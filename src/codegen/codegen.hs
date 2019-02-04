{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module CodeGen.CodeGen
  ( codeGen, runCodeGen )
  where

-- base
import Control.Arrow(second)
import Control.Monad(when)
import Data.Foldable(toList)
import Data.Maybe(maybe, fromMaybe)
import Data.Word(Word32)
import GHC.TypeLits(symbolVal)
import GHC.TypeNats(natVal)
import qualified GHC.Stack
import Prelude hiding (Monad(..))

-- binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy(ByteString)

-- containers
import Data.Map(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except(throwError)
import Control.Monad.Reader(ask)
import Control.Monad.State(get, put)

-- lens
import Control.Lens(view, use, assign)

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.CFG
  ( block, branch, branchConditional )
import CodeGen.Composite
  ( compositeConstruct, compositeExtract )
import CodeGen.Debug
  ( debug, putSrcInfo )
import CodeGen.Functions
  ( declareFunction, declareFunctionCall, declareEntryPoint )
import CodeGen.IDs
  ( typeID, constID, bindingID )
import CodeGen.Instruction
  ( Args(..)
  , ID(ID), Instruction(..)
  , Pairs(Pairs)
  )
import CodeGen.Phi
  ( phiInstruction, phiInstructions )
import CodeGen.Pointers
  ( newVariable, load, store )
import CodeGen.PrimOps
  ( primOp )
import CodeGen.Monad
  ( CGMonad, runCGMonad
  , MonadFresh(fresh)
  , liftPut
  , note
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
  )
import CodeGen.Put(putASM)
import CodeGen.State
  ( CGState(currentBlock, knownBindings, localBindings)
  , CGContext
  , _currentBlock
  , _knownBindings
  , _localBindings, _localBinding
  , _userFunction
  , _userEntryPoint
  )
import CodeGen.Untyped
  ( UAST(UAST)
  , UASTs(NilUAST)
  , uastsLength
  , pattern Applied
  , codeGenUASTs
  )
import Data.Type.List
  ( sLengthVal )
import FIR.AST(AST(..), Syntactic(fromAST))
import FIR.Binding
  ( Permission(Write)
  , KnownPermissions(permissions)
  )
import FIR.Instances.AST()
import FIR.Instances.Optics(SOptic(..), showSOptic)
import FIR.Prim.Singletons
  (  primTyVal
  , SPrimFunc(..)
  , KnownVars(knownVars)
  )
import Math.Linear(V)
import qualified SPIRV.FunctionControl as SPIRV
import qualified SPIRV.Operation       as SPIRV.Op
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.Stage           as SPIRV
import qualified SPIRV.Storage         as Storage

----------------------------------------------------------------------------
-- main code generator

runCodeGen :: CGContext -> AST a -> Either Text ByteString
runCodeGen context = putASM context . codeGen

codeGen :: AST a -> CGMonad (ID, SPIRV.PrimTy)
codeGen (Return :$ a) = codeGen a
codeGen (Applied (MkID ident@(_,ty)) as)
  = case as of
      NilUAST -> pure ident
      _       ->
        case ty of
          SPIRV.Function xs y
            -> let totalArgs = length xs
                   givenArgs = uastsLength as
               in case compare totalArgs givenArgs of
                    EQ -> do retTyID <- typeID y
                             declareFunctionCall (retTyID, y) ident =<< codeGenUASTs as
                    GT -> throwError "codeGen: partial application not yet supported"
                    LT -> throwError 
                        $ "codeGen: function of " <> Text.pack (show totalArgs)
                          <> " arguments applied to "
                          <> Text.pack (show givenArgs)
                          <> " arguments"
          _ -> throwError $ "codeGen: type " <> Text.pack (show ty) <> " used as a function"
-- constants
codeGen (Lit ty a) = ( , primTyVal ty) <$> constID a
-- perform substitution when possible
codeGen (Lam f :$ a)
  = do cg <- codeGen a
       codeGen $ f (MkID cg)
-- ((  _:$ _ ) :$ (Lam _ ))
codeGen (Bind :$ a :$ f)
  = do cg <- codeGen a
       codeGen $ (fromAST f) (MkID cg)
-- stateful operations
codeGen (Def k perms :$ a)
  = do  let name     = Text.pack ( symbolVal k )
            writable = Write `elem` permissions perms
        debug ( putSrcInfo GHC.Stack.callStack ) 
        cgRes@(a_ID, a_ty) <- codeGen a
        
        -- check if we should make this into a pointer or not
        let makeMutable
              = case a_ty of
                  SPIRV.Array        {} -> writable
                  SPIRV.RuntimeArray {} -> writable
                  SPIRV.Struct       {} -> writable
                  _                     -> False

        defRes <-
            if makeMutable
            then do let ptrTy = SPIRV.PointerTy Storage.Function a_ty
                    ptrID <- newVariable ptrTy
                    store (name, a_ID) ptrID ptrTy
                    pure (ptrID, SPIRV.pointerTy ptrTy)
            else pure cgRes
        
        assign ( _localBinding name ) ( Just defRes )
        pure cgRes

codeGen (FunDef k as b :$ body)
  = let argTys = map (second fst) (knownVars as)
        retTy  = primTyVal b
        name   = Text.pack ( symbolVal k )
    in do
      debug ( putSrcInfo GHC.Stack.callStack )
      control <- fromMaybe SPIRV.noFunctionControl <$> view ( _userFunction name )
      funID   <- declareFunction name control argTys retTy (codeGen body)
      pure ( funID , SPIRV.Function (map snd argTys) retTy )

codeGen (Entry k s :$ body)
  = let name  = Text.pack ( symbolVal k )
        stage = SPIRV.stageVal s
    in do
      debug ( putSrcInfo GHC.Stack.callStack )
      modes        <- maybe Set.empty snd <$> view ( _userEntryPoint name )
      entryPointID <- declareEntryPoint name stage modes (codeGen body)
      pure ( entryPointID, SPIRV.Function [] SPIRV.Unit )

codeGen (OpticUse (AnIndexedOptic singOptic is))
  = case singOptic of

      SBinding k ->
        do  let varName = Text.pack ( symbolVal k )
            bd@(bdID, bdTy) <- bindingID varName
            case bdTy of
              SPIRV.Pointer storage eltTy
                -> load (varName, bdID) (SPIRV.PointerTy storage eltTy)
              _ -> pure bd

      SComposeO _ (SBinding k) getter ->
        do  let varName = Text.pack ( symbolVal k )
            bd@(bdID, bdTy) <- bindingID varName

            case bdTy of
              SPIRV.Pointer storage eltTy
                -> loadThroughAccessChain (bdID, SPIRV.PointerTy storage eltTy) getter is
              _ -> extractUsingGetter     bd                                    getter is

      _ -> throwError (   "codeGen: cannot 'use', unsupported optic:\n"
                       <> Text.pack (showSOptic singOptic) <> "\n"
                       <> "Optic does not start by accessing a binding."
                      )
codeGen (Applied (Use sLg singOptic) is)
  = throwError (    "codeGen: optic " <> Text.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'use'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: Text
          expected = Text.pack . show $ sLengthVal sLg
          provided :: Text
          provided = Text.pack . show $ uastsLength is

codeGen (OpticView (AnIndexedOptic getter is) (UAST s))
  = do base <- codeGen s
       extractUsingGetter base getter is
codeGen (Applied (View sLg singOptic) is)
  = throwError (    "codeGen: optic " <> Text.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'view'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: Text
          expected = Text.pack . show $ sLengthVal sLg
          provided :: Text
          provided = Text.pack . show . (\i -> if i < 1 then 0 else i-1) $ uastsLength is
          -- off by one: the last argument is the object being accessed,
          -- which is not a run-time index

codeGen (OpticAssign (AnIndexedOptic singOptic is) (UAST a))
  = case singOptic of

      SBinding k ->
        do  let varName = Text.pack ( symbolVal k )
            (a_ID, _)       <- codeGen a
            bd@(bdID, bdTy) <- bindingID varName

            case bdTy of
              SPIRV.Pointer storage eltTy
                -> store (varName, a_ID) bdID (SPIRV.PointerTy storage eltTy)
              _ -> assign ( _localBinding varName ) (Just bd)

            pure (ID 0, SPIRV.Unit) -- ID should never be used

      SComposeO _ (SBinding k) setter ->
        do  let varName = Text.pack ( symbolVal k )
            a_IDTy          <- codeGen a
            bd@(bdID, bdTy) <- bindingID varName

            case bdTy of
              SPIRV.Pointer storage eltTy
                -> storeThroughAccessChain   (bdID, (SPIRV.PointerTy storage eltTy)) a_IDTy setter is
              _ -> insertUsingSetter varName bd                                      a_IDTy setter is

            pure (ID 0, SPIRV.Unit) -- ID should never be used

      _ -> throwError (   "codeGen: cannot 'assign', unsupported optic:\n"
                       <> Text.pack (showSOptic singOptic) <> "\n"
                       <> "Optic does not start by accessing a binding."
                      )
codeGen (Applied (Assign sLg singOptic) is)
  = throwError (    "codeGen: optic " <> Text.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'assign'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: Text
          expected = Text.pack . show $ sLengthVal sLg
          provided :: Text
          provided = Text.pack . show . (\i -> if i < 1 then 0 else i-1) $ uastsLength is
          -- off by one: the last argument is the value for the assignment,
          -- which is not a run-time index

codeGen (OpticSet (AnIndexedOptic setter is) (UAST a) (UAST s))
  = do base <- codeGen s
       val  <- codeGen a
       setUsingSetter  base val setter is
codeGen (Applied (Set sLg singOptic) is)
  = throwError (    "codeGen: optic " <> Text.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'set'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: Text
          expected = Text.pack . show $ sLengthVal sLg
          provided :: Text
          provided = Text.pack . show . (\i -> if i < 2 then 0 else i-2) $ uastsLength is
          -- off by two

codeGen (Applied (PrimOp op _) as)
  = primOp op =<< codeGenUASTs as
codeGen (Applied (MkVector n_px ty_px) as)
  = do  let n = fromIntegral (natVal n_px)
        compositeTy
          <- case primTyVal ty_px of
               SPIRV.Scalar s
                 -> pure $ SPIRV.Vector n (SPIRV.Scalar s)
               SPIRV.Vector m (SPIRV.Scalar s)
                 -> pure $ SPIRV.Matrix m n s -- TODO: check ordering of m, n
               x -> throwError ( "codeGen: unexpected vector constituent "
                               <> Text.pack ( show x )
                               )
        (compositeConstruct compositeTy . map fst) =<< codeGenUASTs as
codeGen (Fmap functorSing :$ f :$ a)
  = case functorSing of
      SFuncVector n
        -> do vec@(_,vecTy) <- codeGen a
              constituentTy 
                <- case vecTy of
                      SPIRV.Vector _ ty
                        -> pure ty
                      _ -> throwError ( "codeGen: vector fmap used over non-vector-type "
                                       <> Text.pack (show vecTy)
                                      )
              elems <- traverse
                         ( \i -> compositeExtract constituentTy [i] vec )
                         [ 0 .. fromIntegral (natVal n) - 1 ]
              fmapped <- traverse ( codeGen . (f :$) . MkID ) elems
              compositeConstruct vecTy (map fst . toList $ fmapped)
      SFuncMatrix m n
        -> do mat@(_, matTy) <- codeGen a
              constituentTy 
                <- case matTy of
                      SPIRV.Matrix _ _ ty
                         -> pure ty
                      ty -> throwError ( "codeGen: matrix fmap used over non-matrix-type "
                                        <> Text.pack (show ty)
                                       )
              let colDim = fromIntegral (natVal m)
              cols <- traverse
                         (\i -> compositeExtract
                                  ( SPIRV.Vector colDim (SPIRV.Scalar constituentTy) )
                                  [i]
                                  mat
                         )
                         [ 0 .. fromIntegral (natVal n) - 1 ]
              fmapped <- traverse 
                           ( \ x -> fst <$> codeGen (Fmap (SFuncVector m) :$ f :$ MkID x) ) 
                           cols
              compositeConstruct matTy (toList fmapped)
codeGen (Pure functorSing :$ a)
  = case functorSing of
      SFuncVector n
        -> do (valID, valTy) <- codeGen a
              let dim :: Num a => a
                  dim = fromIntegral (natVal n)
              compositeConstruct
                 ( SPIRV.Vector dim valTy )
                 ( replicate dim valID )
      SFuncMatrix m n
        -> do val@(_,valTy) <- codeGen a
              constituentTy
                <- case valTy of
                      SPIRV.Scalar ty
                         -> pure ty
                      ty -> throwError ( "codeGen: matrix contains non-scalars of type "
                                        <> Text.pack (show ty)
                                       )
              let colDim :: Word32
                  colDim = fromIntegral (natVal m)
                  rowDim :: Num a => a
                  rowDim = fromIntegral (natVal n)
              col <- fst <$> codeGen (Pure (SFuncVector m) :$ MkID val)
              compositeConstruct
                ( SPIRV.Matrix colDim rowDim constituentTy )
                ( replicate rowDim col )
codeGen (Ap functorSing ty_px :$ f :$ a)
  = case functorSing of
      SFuncVector n
        -> case f of
            -- base case
            (Fmap _ :$ g :$ b)
               -> let dim :: Word32
                      dim = fromIntegral (natVal n)
                      t =  fromAST @(AST _ -> AST _ -> AST _) g
                       <$> fromAST @(V _ (AST _)) b
                       <*> fromAST @(V _ (AST _)) a
                  in do ids <- traverse codeGen t                    
                        compositeConstruct
                          ( SPIRV.Vector dim (primTyVal ty_px) )
                          ( map fst (toList ids) )
            -- inductive case
            (Ap _ _ :$ _ :$ _)
              -> throwError "codeGen: applicative support WIP"

            _ -> throwError
                    "codeGen: support for applicatives is very limited, sorry...\n\
                    \only expressions of the form f <$$> a_1 <**> ... <**> a_n are supported"

      SFuncMatrix _ _ -> throwError "codeGen: applicative operations not yet supported on matrices"

-- newtype wrapping
codeGen (Mat :$ m)   = codeGen m
codeGen (UnMat :$ m) = codeGen m
-- control flow
codeGen (Locally :$ a)
  = do
        bindingsBefore <- use _knownBindings
        bindingsLBefore <- use _localBindings
        localBlock <- fresh
        branch localBlock

        block localBlock
        cg <- codeGen a
        outsideBlock <- fresh
        branch outsideBlock

        block outsideBlock
        assign _knownBindings bindingsBefore
        assign _localBindings bindingsLBefore
        pure cg

codeGen (If :$ c :$ t :$ f)
 = codeGen (IfM :$ c :$ (Return :$ t) :$ (Return :$ f))
codeGen (IfM :$ cond :$ bodyTrue :$ bodyFalse)
  = do  headerBlock <- fresh
        trueBlock   <- fresh
        falseBlock  <- fresh
        mergeBlock  <- fresh
        bindingsBefore <- use _knownBindings
        branch headerBlock
        
        -- header block
        block headerBlock
        (condID, condTy) <- codeGen cond
        when ( condTy /= SPIRV.Boolean )
             ( throwError
             $  "codeGen: 'if' expected boolean conditional, but got "
             <> Text.pack (show condTy)
             )
        branchConditional condID trueBlock falseBlock

        -- true block
        block trueBlock
        (trueID, trueTy) <- codeGen bodyTrue
        trueEndBlock <- note ( "codeGen: true branch in if statement escaped CFG" )
                            =<< use _currentBlock
        trueBindings <- use _knownBindings
        trueLBindings <- use _localBindings
        branch mergeBlock

        -- false block
        block falseBlock
        (falseID, falseTy) <- codeGen bodyFalse
        falseEndBlock <- note ( "codeGen: false branch in if statement escaped CFG" )
                            =<< use _currentBlock
        falseBindings <- use _knownBindings
        falseLBindings <- use _knownBindings
        branch mergeBlock

        -- merge block
        block mergeBlock
        phiInstructions
          ( \ bd -> bd `Map.member` bindingsBefore )
          [ trueEndBlock, falseEndBlock ]
          [ trueBindings, falseBindings ]
        phiInstructions
          ( \ bd -> bd `Map.member` bindingsBefore )
          [ trueEndBlock , falseEndBlock  ]
          [ trueLBindings, falseLBindings ]

        -- get the value of the conditional
        if trueTy == falseTy
        then  if trueTy == SPIRV.Unit
              then pure (ID 0, SPIRV.Unit) -- ID should never be used
              else do
                v <- fresh
                phiInstruction (v,trueTy) $ Pairs [(trueID, trueEndBlock), (falseID, falseEndBlock)]
                pure (v, trueTy)
        else throwError "codeGen: true and false branches of conditional have different types"

codeGen (While :$ cond :$ loopBody)
  = do  beforeBlock <- note ( "codeGen: while loop outside of a block" )
                         =<< use _currentBlock
        headerBlock <- fresh
        loopBlock   <- fresh
        mergeBlock  <- fresh -- block where control flow merges back
        branch headerBlock

        -- Need to perform code generation for the loop block first,
        -- as we need to know which phi instructions to put in the header.
        -- However, the loop block (also called the continue block)
        -- needs to appear after the header block in the CFG.
        ctxt  <- ask
        state <- get
        let bindingsBefore, bindingsLBefore :: Map Text (ID, SPIRV.PrimTy)
            bindingsBefore  = knownBindings state
            bindingsLBefore = localBindings state


            -- The first CGState is the one we pass manually and that we want.
            -- The second CGState has the wrong "currentBlock" information,
            -- because we branched to the header block at the end.
            loopGenOutput :: Either Text (CGState, CGState, ByteString)
            loopGenOutput
              = runCGMonad ctxt state
                  do  block loopBlock
                      _ <- codeGen loopBody
                      endState <- get
                      branch headerBlock
                      pure endState

        (loopEndState, loopBodyASM)
          <- case loopGenOutput of
                Left  err     -> throwError err
                Right (s,_,a) -> pure (s,a)
        let mbLoopEndBlock   = currentBlock  loopEndState
            loopEndBindings  = knownBindings loopEndState
            loopEndLBindings = localBindings loopEndState
        loopEndBlock <- note ( "codeGen: while loop escaped CFG")
                          mbLoopEndBlock

        -- Update the state to be the state at the end of the loop
        -- (e.g. don't forget about new constants that were defined),
        -- but reset bindings to what they were before the loop block.
        -- This is because all bindings within the loop remain local to it.
        put loopEndState
        assign _knownBindings bindingsBefore
        assign _localBindings bindingsLBefore

        -- header block
        block headerBlock
        phiInstructions
          ( const True )
          [ beforeBlock   , loopEndBlock    ]
          [ bindingsBefore, loopEndBindings ] -- need loopEndBindings
        phiInstructions
          ( const True )
          [ beforeBlock    , loopEndBlock     ]
          [ bindingsLBefore, loopEndLBindings ] -- and loopEndLBindings
        (condID, condTy) <- codeGen cond
        when ( condTy /= SPIRV.Boolean )
             ( throwError
             $  "codeGen: 'while' expected boolean conditional, but got "
             <> Text.pack (show condTy)
             )
        liftPut $ putInstruction Map.empty
          Instruction
             { operation = SPIRV.Op.LoopMerge
             , resTy = Nothing
             , resID = Nothing
             , args  = Arg mergeBlock
                     $ Arg loopBlock
                     $ Arg (0 :: Word32) -- no loop control
                     EndArgs
             }
        branchConditional condID loopBlock mergeBlock

        -- writing the loop block proper
        liftPut $ Binary.putLazyByteString loopBodyASM

        -- merge block (first block after the loop)
        block mergeBlock
        pure (ID 0, SPIRV.Unit) -- ID should never be used
       
codeGen (Lam f)
  = throwError ( "codeGen: unexpected lambda abstraction:\n"
                <> Text.pack ( show (Lam f) )
               )
codeGen (f :$ a)
  = throwError ( "codeGen: unsupported function application:\n"
                 <> Text.pack ( show (f :$ a) )
               )
codeGen other
  = throwError ( "codeGen: non-exhaustive pattern match:\n"
                 <> Text.pack ( show other )
               )
