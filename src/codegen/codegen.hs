{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module CodeGen.CodeGen
  ( codeGen, runCodeGen )
  where

-- base
import Prelude
  hiding ( Monad(..) )
import Control.Arrow
  ( first )
import Control.Monad
  ( when )
import Data.Maybe
  ( fromMaybe )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )
import qualified GHC.Stack

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except
  ( throwError )
import Control.Monad.Reader
  ( ask )
import Control.Monad.State
  ( get, put )

-- lens
import Control.Lens
  ( view, use, assign, modifying )

-- text-utf8
import "text-utf8" Data.Text
  ( Text )
import qualified "text-utf8" Data.Text as Text

-- fir
import CodeGen.Application
  ( UAST(UAST)
  , traverseASTs, astsLength
  , pattern Applied
  )
import CodeGen.Applicative
  ( idiomatic, pattern Idiomatic )
import CodeGen.Binary
  ( putInstruction )
import CodeGen.CFG
  ( block, branch, branchConditional )
import CodeGen.Composite
  ( compositeConstruct )
import CodeGen.Debug
  ( debug, putSrcInfo )
import CodeGen.Functions
  ( declareFunction, declareFunctionCall, declareEntryPoint )
import CodeGen.IDs
  ( typeID, constID, bindingID )
import CodeGen.Images
  ( imageTexel, writeTexel )
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
  , ASTs(NilAST,ConsAST)
  )
import CodeGen.Put
  ( putASM )
import CodeGen.State
  ( CGState(currentBlock, localBindings)
  , CGContext
  , _currentBlock, _functionContext
  , _localBindings, _localBinding
  , _userFunction
  )
import Data.Type.Known
  ( knownValue )
import Data.Type.List
  ( SLength(SSucc), sLengthVal )
import FIR.AST
  ( AST(..), Syntactic(fromAST) )
import FIR.Binding
  ( Permission(Write) )
import FIR.Instances.AST
  ( )
import FIR.Instances.Optics
  ( SOptic(..), showSOptic )
import FIR.ASTState
  ( FunctionContext(TopLevel) )
import FIR.Prim.Op
  ( PrimOp(opName) )
import FIR.Prim.Singletons
  ( primTyVal
  , KnownVars(knownVars)
  )
import qualified SPIRV.FunctionControl as SPIRV
import qualified SPIRV.Operation       as SPIRV.Op
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.Storage         as Storage

----------------------------------------------------------------------------
-- main code generator

runCodeGen :: CGContext -> AST a -> Either Text ByteString
runCodeGen context = putASM context . codeGen

codeGen :: AST ast -> CGMonad (ID, SPIRV.PrimTy)
codeGen (Return :$ a) = codeGen a
codeGen (Applied (MkID ident@(i,ty)) as)
  = case as of
      NilAST -> pure ident
      _      ->
        case ty of
          SPIRV.Function xs y
            -> let totalArgs = length xs
                   givenArgs = astsLength as
               in case compare totalArgs givenArgs of
                    EQ -> do retTyID <- typeID y
                             declareFunctionCall (retTyID, y) ident =<< traverseASTs codeGen as
                    GT -> throwError $
                            "codeGen: partial application not supported \
                            \(function " <> Text.pack (show i) <> ")"
                    LT -> throwError 
                        $ "codeGen: function " <> Text.pack (show i) <> " of " <> Text.pack (show totalArgs)
                          <> " arguments applied to "
                          <> Text.pack (show givenArgs)
                          <> " arguments"
          _ -> throwError $ "codeGen: type " <> Text.pack (show ty) <> " used as a function ("<> Text.pack (show i) <> ")"
-- constants
codeGen (Lit (a :: ty)) = ( , primTyVal @ty) <$> constID a
-- perform substitution when possible
codeGen (Lam f :$ a) -- = codeGen (f a)
  = do cg <- codeGen a
       codeGen $ f (MkID cg)
codeGen (Bind :$ a :$ f)
  = do cg <- codeGen a
       codeGen $ (fromAST f) (MkID cg)
codeGen (Applied (PrimOp (_ :: Proxy a) (_ :: Proxy op)) as)
  = primOp (opName @_ @_ @op @a) =<< traverseASTs codeGen as
-- stateful operations
codeGen (Def (_ :: Proxy name) (_ :: Proxy ps) :$ a)
  = do  let name     = knownValue @name
            writable = Write `elem` (knownValue @ps)
        debug ( putSrcInfo GHC.Stack.callStack ) 
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
  = let as     = knownVars @as
        retTy  = primTyVal  @b
        name   = knownValue @name
    in do
      debug ( putSrcInfo GHC.Stack.callStack )
      control <- fromMaybe SPIRV.noFunctionControl <$> view ( _userFunction name )
      funID   <- declareFunction name control as retTy (codeGen body)
      pure ( funID , SPIRV.Function (map (fst . snd) as) retTy )

codeGen (Entry (_ :: Proxy name) (_ :: Proxy stageInfo) :$ body)
  = let name      = knownValue @name
        stageInfo = knownValue @stageInfo
    in do
      debug ( putSrcInfo GHC.Stack.callStack )
      entryPointID <- declareEntryPoint name stageInfo (codeGen body)
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
            ( astOps `ConsAST` coords `ConsAST` NilAST )
              -> do
                    let imgName = knownValue @name
                    bd@(bdID, bdTy) <- bindingID imgName
                    img <- case bdTy of
                              SPIRV.Pointer storage imgTy
                                -> load (imgName, bdID) (SPIRV.PointerTy storage imgTy)
                              _ -> pure bd
                    (cdID, _) <- codeGen coords
                    ops <- case astOps of
                              Ops rawOps -> pure rawOps
                              _ -> throwError
                                      "codeGen: image operands not of the expected form"
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
              ( astOps `ConsAST` coords `ConsAST` nextindices )
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
                    ops <- case astOps of
                              Ops rawOps -> pure rawOps
                              _ -> throwError
                                      "codeGen: image operands not of the expected form"
                    -- end of copy-paste
                    texel <- imageTexel img ops cdID
                    extractUsingGetter texel getter nextindices

      _ -> throwError (   "codeGen: cannot 'use', unsupported optic:\n"
                       <> Text.pack (showSOptic singOptic) <> "\n"
                       <> "Optic does not start by accessing a binding (or image texel).\n"
                       <> "Cannot access multiple bindings simultaneously."
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
          provided = Text.pack . show $ astsLength is

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
          provided = Text.pack . show . (\i -> if i < 1 then 0 else i-1) $ astsLength is
          -- off by one: the last argument is the object being accessed,
          -- which is not a run-time index

codeGen (OpticAssign (AnIndexedOptic singOptic is) (UAST a))
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
            ( astOps `ConsAST` coords `ConsAST` NilAST )
              -> do
                    let imgName = knownValue @name
                    bd@(bdID, bdTy) <- bindingID imgName
                    img <- case bdTy of
                              SPIRV.Pointer storage imgTy
                                -> load (imgName, bdID) (SPIRV.PointerTy storage imgTy)
                              _ -> pure bd
                    (cdID , _) <- codeGen coords
                    (texID, _) <- codeGen a
                    ops <- case astOps of
                              Ops rawOps -> pure rawOps
                              _ -> throwError
                                      "codeGen: image operands not of the expected form."
                    writeTexel img ops cdID texID
                    pure (ID 0, SPIRV.Unit) -- ID should never be used

      SComposeO _ (SBinding (_ :: Proxy name)) setter ->
        do  let varName = knownValue @name
            a_IDTy          <- codeGen a
            bd@(bdID, bdTy) <- bindingID varName

            case bdTy of
              SPIRV.Pointer storage eltTy
                -> storeThroughAccessChain   (bdID, (SPIRV.PointerTy storage eltTy)) a_IDTy setter is
              _ -> insertUsingSetter varName bd                                      a_IDTy setter is

            pure (ID 0, SPIRV.Unit) -- ID should never be used

      SComposeO _ SImageTexel {} _
        -> throwError ( "codeGen: cannot write to individual components \
                        \of an image texel"
                      )

      _ -> throwError (   "codeGen: cannot 'assign', unsupported optic:\n"
                       <> Text.pack (showSOptic singOptic) <> "\n"
                       <> "Optic does not start by accessing a binding."
                       <> "Cannot update multiple bindings simultaneously."
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
          provided = Text.pack . show . (\i -> if i < 1 then 0 else i-1) $ astsLength is
          -- off by one: the last argument is the value for the assignment,
          -- which is not a run-time index

codeGen (OpticSet (AnIndexedOptic setter is) (UAST a) (UAST s))
  = do base <- codeGen s
       elt  <- codeGen a
       setUsingSetter  base elt setter is
codeGen (Applied (Set sLg singOptic) is)
  = throwError (    "codeGen: optic " <> Text.pack (showSOptic singOptic)
                 <> " provided with the wrong number of run-time indices by 'set'.\n"
                 <> "Expected " <> expected
                 <> ", but provided " <> provided <> "."
               )
    where expected :: Text
          expected = Text.pack . show $ sLengthVal sLg
          provided :: Text
          provided = Text.pack . show . (\i -> if i < 2 then 0 else i-2) $ astsLength is
          -- off by two
codeGen (Applied (MkVector (_ :: Proxy n) (_ :: Proxy ty)) as)
  = do  let n = knownValue @n
        compositeTy
          <- case primTyVal @ty of
               SPIRV.Scalar s
                 -> pure $ SPIRV.Vector n (SPIRV.Scalar s)
               SPIRV.Vector m (SPIRV.Scalar s)
                 -> pure $ SPIRV.Matrix m n s
               x -> throwError ( "codeGen: unexpected vector constituent "
                               <> Text.pack ( show x )
                               )
        (compositeConstruct compositeTy . map fst) =<< traverseASTs codeGen as
-- functor / applicative operations
codeGen (Idiomatic idiom)
  = idiomatic idiom
-- newtype wrapping/unwrapping
codeGen (Mat    :$ m) = codeGen m
codeGen (UnMat  :$ m) = codeGen m
codeGen (Coerce :$ a) = codeGen a
-- control flow
codeGen (Locally :$ a)
  = do
        bindingsBefore <- use _localBindings
        localBlock <- fresh
        branch localBlock

        block localBlock
        cg <- codeGen a
        outsideBlock <- fresh
        branch outsideBlock

        block outsideBlock
        assign _localBindings bindingsBefore
        pure cg

codeGen (If :$ c :$ t :$ f)
 = codeGen (IfM :$ c :$ (Return :$ t) :$ (Return :$ f))
codeGen (IfM :$ cond :$ bodyTrue :$ bodyFalse)
  = do  headerBlock <- fresh
        trueBlock   <- fresh
        falseBlock  <- fresh
        mergeBlock  <- fresh
        bindingsBefore <- use _localBindings
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
        assign _localBindings bindingsBefore
        (trueID, trueTy) <- codeGen bodyTrue
        trueEndBlock <- note ( "codeGen: true branch in if statement escaped CFG" )
                            =<< use _currentBlock
        trueBindings <- use _localBindings
        branch mergeBlock

        -- false block
        block falseBlock
        assign _localBindings bindingsBefore
        (falseID, falseTy) <- codeGen bodyFalse
        falseEndBlock <- note ( "codeGen: false branch in if statement escaped CFG" )
                            =<< use _currentBlock
        falseBindings <- use _localBindings
        branch mergeBlock

        -- reset local bindings to what they were outside the branches
        assign _localBindings bindingsBefore

        -- merge block
        block mergeBlock
        phiBindings
          <- phiInstructions
              ( \ bd -> bd `Map.member` bindingsBefore )
              [ trueEndBlock, falseEndBlock ]
              [ trueBindings, falseBindings ]

        -- update local bindings to refer to the phi instructions
        Map.traverseWithKey
          ( \ bdName bdID -> modifying ( _localBinding bdName ) ( fmap $ first (const bdID) ) )
          phiBindings

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
        let bindingsBefore :: Map Text (ID, SPIRV.PrimTy)
            bindingsBefore  = localBindings state

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

        (loopEndState, _) -- (loopEndState,loopBodyASM)
          <- case loopGenOutput of
                Left  err     -> throwError err
                Right (s,_,a) -> pure (s,a)
        let mbLoopEndBlock  = currentBlock  loopEndState
            loopEndBindings = localBindings loopEndState
        loopEndBlock <- note ( "codeGen: while loop escaped CFG" )
                          mbLoopEndBlock

        -- Update the state to be the state at the end of the loop
        -- (e.g. don't forget about new constants that were defined),
        -- but reset bindings to what they were before the loop block.
        -- This is because all bindings within the loop remain local to it.
        put loopEndState
        assign _localBindings bindingsBefore

        -- header block
        block headerBlock
        phiLocalBindings <-
          phiInstructions
            ( const True )
            [ beforeBlock   , loopEndBlock    ]
            [ bindingsBefore, loopEndBindings ] -- need loopEndBindings

        -- update local bindings to refer to the phi instructions
        Map.traverseWithKey
          ( \ bdName bdID -> modifying ( _localBinding bdName ) ( fmap $ first (const bdID) ) )
          phiLocalBindings
        updatedLocalBindings <- use _localBindings
        (condID, condTy) <- codeGen cond
        updatedState <- get
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
        {- can't simply use code gen we already did,
        because the bindings don't refer to the phi instructions as they should
        -- liftPut $ Binary.putLazyByteString loopBodyASM
        -}
        -- do code generation a second time for the loop body,
        -- but this time with updated local bindings referring to the phi instructions
        put state
        assign _localBindings updatedLocalBindings
        block loopBlock
        _ <- codeGen loopBody
        branch headerBlock
        put updatedState -- account for what happened in the loop header (e.g. IDs of phi instructions)

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
