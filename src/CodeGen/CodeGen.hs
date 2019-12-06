{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module: CodeGen.CodeGen

Main code generation function: dispatches code generation on the constructors of the AST.
-}

module CodeGen.CodeGen
  ( codeGen, runCodeGen )
  where

-- base
import Prelude
  hiding ( Monad(..) )
import Control.Arrow
  ( second )
import Data.Maybe
  ( fromMaybe )
import Data.Word
  ( Word32 )
import Data.Proxy
  ( Proxy )
import qualified GHC.Stack
import GHC.TypeNats
  ( natVal )

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- containers
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except
  ( throwError )

-- lens
import Control.Lens
  ( view, use, assign )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( toList, knownLength' )

-- fir
import CodeGen.Application
  ( UAST(UAST)
  , traverseASTs, astsLength
  , pattern Applied
  )
import CodeGen.Applicative
  ( idiomatic, pattern Idiomatic )
import CodeGen.Binary
  ( putModule )
import CodeGen.CFG
  ( selection, ifM, switch, while, locally )
import CodeGen.Composite
  ( compositeConstruct, compositeExtract )
import CodeGen.Debug
  ( whenDebugging, putSrcInfo )
import CodeGen.Functions
  ( declareFunction, declareFunctionCall, declareEntryPoint )
import CodeGen.IDs
  ( typeID, constID, bindingID, undefID )
import CodeGen.Images
  ( imageTexel, writeTexel )
import CodeGen.Instruction
  ( ID(ID) )
import CodeGen.Pointers
  ( newVariable, load, store )
import CodeGen.PrimOps
  ( primOp )
import CodeGen.Monad
  ( CGMonad, runCGMonad
  , runExceptTPutM
  )
import CodeGen.Optics
  ( loadThroughAccessChain
  , storeThroughAccessChain
  , extractUsingGetter
  , insertUsingSetter
  , setUsingSetter
  , pattern OpticUse, pattern OpticAssign
  , pattern OpticView, pattern OpticSet
  , IndexedOptic(AnIndexedOptic)
  , IndexedOptic'(AnIndexedOptic')
  , IndexedTypedOptic(AnIndexedTypedOptic)
  , IndexedAssignment(AnIndexedAssignment)
  , ASTs(NilAST,ConsAST)
  )
import CodeGen.State
  ( CGState, CGContext
  , _functionContext
  , _localBinding
  , _userFunction
  , _spirvVersion
  , initialState
  )
import Data.Type.Known
  ( knownValue )
import Data.Type.List
  ( SLength(SSucc), sLengthVal )
import FIR.AST
  ( AST(..), Syntactic(fromAST) )
import FIR.Binding
  ( Permission(Write) )
import FIR.Prim.Array
  ( Array(MkArray) )
import FIR.Prim.Op
  ( PrimOp(opName) )
import FIR.Prim.Singletons
  ( primTy
  , KnownVars(knownVars)
  )
import FIR.Prim.Struct
  ( ASTStructFields(traverseStructASTs) )
import FIR.ProgramState
  ( FunctionContext(TopLevel) )
import FIR.Syntax.AST
  ( )
import FIR.Syntax.Optics
  ( SOptic(..), showSOptic )
import qualified SPIRV.Control as SPIRV
import qualified SPIRV.PrimTy  as SPIRV
import qualified SPIRV.Storage as Storage
import qualified SPIRV.Version as SPIRV

----------------------------------------------------------------------------
-- run the code generator with given context to create a SPIR-V module

runCodeGen :: CGContext -> AST ast -> Either ShortText (ByteString, CGState)
runCodeGen context ast
  = case runCGMonad context (initialState context) (codeGen ast) of

      Right (_, cgState, body)
        -> case runExceptTPutM $ putModule context cgState of
              Right ((), decs) -> Right ( decs <> body, cgState )
              Left err         -> Left err

      Left err -> Left err

----------------------------------------------------------------------------
-- main code generator

codeGen :: AST ast -> CGMonad (ID, SPIRV.PrimTy)
codeGen (Return :$ a) = codeGen a
codeGen (Applied (MkID idTy@(i,ty)) as)
  = case as of
      NilAST -> pure idTy
      _      ->
        case ty of
          SPIRV.Function xs y
            -> let totalArgs = length xs
                   givenArgs = astsLength as
               in case compare totalArgs givenArgs of
                    EQ -> do
                      retTyID <- typeID y
                      funID   <- declareFunctionCall
                                    ( retTyID, y )
                                    idTy
                                    =<< traverseASTs codeGen as
                      pure (funID, y)
                    GT -> throwError $
                            "codeGen: partial application not supported \
                            \(function " <> ShortText.pack (show i) <> ")"
                    LT -> throwError 
                        $ "codeGen: function " <> ShortText.pack (show i) <> " of " <> ShortText.pack (show totalArgs)
                          <> " arguments applied to "
                          <> ShortText.pack (show givenArgs)
                          <> " arguments"
          _ -> throwError $ "codeGen: type " <> ShortText.pack (show ty) <> " used as a function ("<> ShortText.pack (show i) <> ")"
-- constants
codeGen (Lit (a :: ty)) = ( , primTy @ty) <$> constID a
-- perform substitution when possible
codeGen (Lam f :$ a) -- = codeGen (f a)
  = do cg <- codeGen a
       codeGen $ f (MkID cg)
codeGen (Bind :$ a :$ f)
  = do cg <- codeGen a
       codeGen $ (fromAST f) (MkID cg)
codeGen (Applied (PrimOp (_ :: Proxy a) (_ :: Proxy op)) as)
  = primOp (opName @_ @_ @op @a) =<< traverseASTs codeGen as
codeGen (Undefined :: AST a)
  = let undefTy = primTy @a
    in ( , undefTy ) <$> undefID undefTy
-- stateful operations
codeGen (Def (_ :: Proxy name) (_ :: Proxy ps) :$ a)
  = do  let name     = knownValue @name
            writable = Write `elem` (knownValue @ps)
        whenDebugging ( putSrcInfo GHC.Stack.callStack )
        cgRes@(a_ID, a_ty) <- codeGen a
        
        -- check if we should make this into a pointer or not
        let makeMutable
              = case a_ty of
                  SPIRV.Array        {} -> writable
                  SPIRV.RuntimeArray {} -> writable -- should never happen, user cannot define runtime arrays
                  SPIRV.Struct       {} -> writable
                  _                     -> False

        defRes <-
            if makeMutable
            then do
                    ctx <- use _functionContext
                    let storage =
                          case ctx of
                            TopLevel -> Storage.Private
                            _        -> Storage.Function
                    let ptrTy = SPIRV.PointerTy storage a_ty
                    ptrID <- newVariable ptrTy
                    store (name, a_ID) ptrID ptrTy
                    pure (ptrID, SPIRV.pointerTy ptrTy)
            else pure cgRes
        
        assign ( _localBinding name ) ( Just defRes )
        -- addName ( fst defRes ) name -- (not necessarily helpful to assign names as we're using SSA)
        pure cgRes

codeGen (FunDef (_ :: Proxy name) (_ :: Proxy as) (_ :: Proxy b) :$ body)
  = let as     = knownVars  @as
        retTy  = primTy     @b
        name   = knownValue @name
    in do
      whenDebugging ( putSrcInfo GHC.Stack.callStack )
      control <- fromMaybe SPIRV.NoFunctionControl <$> view ( _userFunction name )
      funID   <- declareFunction name control as retTy (codeGen body)
      pure ( funID , SPIRV.Function (map (fst . snd) as) retTy )

codeGen (Entry (_ :: Proxy name) (_ :: Proxy stageInfo) :$ body)
  = let name      = knownValue @name
        stageInfo = knownValue @stageInfo
    in do
      whenDebugging ( putSrcInfo GHC.Stack.callStack )
      entryPointID <- declareEntryPoint name stageInfo Nothing (codeGen body) -- TODO
      pure ( entryPointID, SPIRV.Function [] SPIRV.Unit )

codeGen (OpticUse (AnIndexedOptic singOptic is))
  = case singOptic of

      SBinding (_ :: Proxy name) ->
        do  let varName = knownValue @name
            bd@(bdID, bdTy) <- bindingID varName
            case bdTy of
              SPIRV.Pointer storage eltTy
                -> load (varName, bdID) (SPIRV.PointerTy storage eltTy)
              _ -> pure bd

      -- image
      SImageTexel ( _ :: Proxy name ) ( _ :: Proxy props )
        -> case is of 
            ( ops `ConsAST` coords `ConsAST` NilAST )
              -> do
                    let imgName = knownValue @name
                    bd@(bdID, bdTy) <- bindingID imgName
                    img <- case bdTy of
                              SPIRV.Pointer storage imgTy
                                -> load (imgName, bdID) (SPIRV.PointerTy storage imgTy)
                              _ -> pure bd
                    (cdID, _) <- codeGen coords
                    imageTexel img ops cdID

      SComposeO _ (SBinding (_ :: Proxy name )) getter ->
        do  let varName = knownValue @name
            bd@(bdID, bdTy) <- bindingID varName

            case bdTy of
              SPIRV.Pointer storage eltTy
                -> loadThroughAccessChain (bdID, SPIRV.PointerTy storage eltTy) getter is
              _ -> extractUsingGetter     bd                                    getter is

      SComposeO
        (SSucc (SSucc _))
        (SImageTexel ( _ :: Proxy name ) ( _ :: Proxy props ))
        getter
         -> case is of
              ( ops `ConsAST` coords `ConsAST` nextindices )
                ->
                  do 
                    -- copy-pasted from above
                    let imgName = knownValue @name
                    bd@(bdID, bdTy) <- bindingID imgName
                    img <- case bdTy of
                              SPIRV.Pointer storage imgTy
                                -> load (imgName, bdID) (SPIRV.PointerTy storage imgTy)
                              _ -> pure bd
                    (cdID, _) <- codeGen coords
                    -- end of copy-paste
                    texel <- imageTexel img ops cdID
                    extractUsingGetter texel getter nextindices

      _ -> throwError (   "codeGen: cannot 'use', unsupported optic:\n"
                       <> ShortText.pack (showSOptic singOptic) <> "\n"
                       <> "Optic does not start by accessing a binding (or image texel).\n"
                       <> "Cannot access multiple bindings simultaneously."
                      )
codeGen (Applied (Use sLg singOptic) is)
  = throwError (    "codeGen: optic " <> ShortText.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'use'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: ShortText
          expected = ShortText.pack . show $ sLengthVal sLg
          provided :: ShortText
          provided = ShortText.pack . show $ astsLength is

codeGen (OpticView (AnIndexedOptic' getter is) (UAST s))
  = do base <- codeGen s
       extractUsingGetter base getter is
codeGen (Applied (View sLg singOptic) is)
  = throwError (    "codeGen: optic " <> ShortText.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'view'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: ShortText
          expected = ShortText.pack . show $ sLengthVal sLg
          provided :: ShortText
          provided = ShortText.pack . show . (\i -> if i < 1 then 0 else i-1) $ astsLength is
          -- off by one: the last argument is the object being accessed,
          -- which is not a run-time index

codeGen (OpticAssign (AnIndexedAssignment singOptic is a))
  = case singOptic of

      SBinding (_ :: Proxy name) ->
        do  let varName = knownValue @name
            a_IDTy@(a_ID, _) <- codeGen a
            (bdID, bdTy)     <- bindingID varName

            case bdTy of
              SPIRV.Pointer storage eltTy
                -> store (varName, a_ID) bdID (SPIRV.PointerTy storage eltTy)
              _ -> assign ( _localBinding varName ) (Just a_IDTy)

            pure (ID 0, SPIRV.Unit) -- ID should never be used

      -- image
      SImageTexel ( _ :: Proxy name ) ( _ :: Proxy props )
        -> case is of 
            ( ops `ConsAST` coords `ConsAST` NilAST )
              -> do
                    let imgName = knownValue @name
                    bd@(bdID, bdTy) <- bindingID imgName
                    img <- case bdTy of
                              SPIRV.Pointer storage imgTy
                                -> load (imgName, bdID) (SPIRV.PointerTy storage imgTy)
                              _ -> pure bd
                    (cdID , _) <- codeGen coords
                    (texID, _) <- codeGen a
                    writeTexel img ops cdID texID
                    pure (ID 0, SPIRV.Unit) -- ID should never be used

      SComposeO _ (SBinding (_ :: Proxy name)) setter ->
        do  let varName = knownValue @name
            bd@(bdID, bdTy) <- bindingID varName

            case bdTy of
              SPIRV.Pointer storage eltTy
                -> storeThroughAccessChain   (bdID, SPIRV.PointerTy storage eltTy) a setter is
              _ -> insertUsingSetter varName bd                                    a setter is

            pure (ID 0, SPIRV.Unit) -- ID should never be used

      SComposeO _ SImageTexel {} _
        -> throwError ( "codeGen: cannot write to individual components \
                        \of an image texel"
                      )

      _ -> throwError (   "codeGen: cannot 'assign', unsupported optic:\n"
                       <> ShortText.pack (showSOptic singOptic) <> "\n"
                       <> "Optic does not start by accessing a binding."
                       <> "Cannot update multiple bindings simultaneously."
                      )
codeGen (Applied (Assign sLg singOptic) is)
  = throwError (    "codeGen: optic " <> ShortText.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'assign'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: ShortText
          expected = ShortText.pack . show $ sLengthVal sLg
          provided :: ShortText
          provided = ShortText.pack . show . (\i -> if i < 1 then 0 else i-1) $ astsLength is
          -- off by one: the last argument is the value for the assignment,
          -- which is not a run-time index

codeGen (OpticSet (AnIndexedTypedOptic setter is) a s)
  = setUsingSetter s a setter is
codeGen (Applied (Set sLg singOptic) is)
  = throwError (    "codeGen: optic " <> ShortText.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'set'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: ShortText
          expected = ShortText.pack . show $ sLengthVal sLg
          provided :: ShortText
          provided = ShortText.pack . show . (\i -> if i < 2 then 0 else i-2) $ astsLength is
          -- off by two
codeGen (Applied (MkVector (_ :: Proxy n) (_ :: Proxy ty)) as)
  = do  let n = knownValue @n
        compositeTy
          <- case primTy @ty of
               SPIRV.Scalar s
                 -> pure $ SPIRV.Vector n (SPIRV.Scalar s)
               SPIRV.Vector m (SPIRV.Scalar s)
                 -> pure $ SPIRV.Matrix m n s
               x -> throwError ( "codeGen: unexpected vector constituent "
                               <> ShortText.pack ( show x )
                               )
        (compositeConstruct compositeTy . map fst) =<< traverseASTs codeGen as
codeGen (Struct struct :: AST a)
  = compositeConstruct (primTy @a)
        =<< traverseStructASTs (fmap fst . codeGen) struct
codeGen arr@(Array (MkArray vec))
  = case arr of
      ( _ :: AST (Array n a) ) ->
        let
          n :: Word32
          n = Vector.knownLength' vec ( \px -> fromIntegral ( natVal px ) )
          vecTy :: SPIRV.PrimTy
          vecTy = SPIRV.Vector n ( primTy @a )
        in
        compositeConstruct vecTy
          =<< traverse (fmap fst . codeGen) (Vector.toList vec)
codeGen (GradedMappend :$ (a :: AST a) :$ (b :: AST b))
  = do
      (a_ID, a_ty) <- codeGen a
      (b_ID, b_ty) <- codeGen b
      case (a_ty, b_ty) of
        ( SPIRV.Vector i (SPIRV.Scalar s), SPIRV.Vector j (SPIRV.Scalar t)) ->
          if s /= t
          then throwError "codeGen: graded semigroup operation on incompatible vectors"
          else compositeConstruct (SPIRV.Vector (i+j) (SPIRV.Scalar s)) [a_ID, b_ID]
        ( SPIRV.Matrix m n s, SPIRV.Matrix m' n' s' ) ->
          if m /= m' || s /= s'
          then throwError "codeGen: graded semigroup operation on incompatible matrices"
          else do
            cols1 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..n -1]
            cols2 <- traverse ( \i -> fst <$> compositeExtract (b_ID, b_ty) [i] ) [0..n'-1]
            compositeConstruct (SPIRV.Matrix m (n+n') s) (cols1 ++ cols2)
        ( SPIRV.Struct as _ _, SPIRV.Struct bs _ _ ) ->
          do
            let lg  = fromIntegral (length as)
                lg' = fromIntegral (length bs)
            elts1 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..lg -1]
            elts2 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..lg'-1]
            compositeConstruct
              ( SPIRV.Struct (as ++ bs) Set.empty SPIRV.NotForBuiltins )
              ( elts1 ++ elts2 )
        ( SPIRV.Array lg eltTy _ _, SPIRV.Array lg' eltTy' _ _ ) ->
          if eltTy /= eltTy'
          then throwError "codeGen: graded semigroup operation on incompatible arrays"
          else do
            -- should probably use a loop instead of this, but nevermind
            elts1 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..lg -1]
            elts2 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..lg'-1]
            compositeConstruct
              ( SPIRV.Array (lg + lg') eltTy Set.empty SPIRV.NotForBuiltins )
              ( elts1 ++ elts2 )
        _ -> throwError "codeGen: unsupported graded mappend operation"
-- functor / applicative operations
codeGen (Idiomatic idiom)
  = idiomatic idiom
-- newtype wrapping/unwrapping
codeGen (Mat    :$ m) = codeGen m
codeGen (UnMat  :$ m) = codeGen m
codeGen (Coerce :$ a) = codeGen a
-- control flow
codeGen (Locally :$ a) = locally (codeGen a)
codeGen (Embed   :$ a) = codeGen a
codeGen (If :$ c :$ ( t :: AST a ) :$ f )
  = do
      ver <- view _spirvVersion
      if canUseSelection (primTy @a) ver
      then selection c t f
      else ifM       c t f
    where
      canUseSelection :: SPIRV.PrimTy -> SPIRV.Version -> Bool
      canUseSelection (SPIRV.Scalar    _) _ = True
      canUseSelection (SPIRV.Pointer _ _) _ = True
      canUseSelection ty ver
        | ver < SPIRV.Version 1 4 = False
        | SPIRV.Matrix {} <- ty   = True
        | SPIRV.Vector {} <- ty   = True
        | SPIRV.Array  {} <- ty   = True
        | SPIRV.Struct {} <- ty   = True
        | otherwise               = False
codeGen (IfM :$ c :$ t :$ f) = ifM c t f
codeGen (Switch scrut def cases )
  = codeGen
  ( SwitchM
     ( Return :$ scrut )
     ( Return :$ def   )
     ( map (second (Return :$)) cases )
  )
codeGen ( SwitchM scrut def cases )
  = switch scrut def ( map ( second UAST ) cases )
codeGen (While :$ cond :$ body)
  = while cond body
codeGen (Lam f)
  = throwError ( "codeGen: unexpected lambda abstraction:\n"
                <> ShortText.pack ( show (Lam f) )
               )
codeGen (f :$ a)
  = throwError ( "codeGen: unsupported function application:\n"
                 <> ShortText.pack ( show (f :$ a) )
               )
codeGen other
  = throwError ( "codeGen: non-exhaustive pattern match:\n"
                 <> ShortText.pack ( show other )
               )
