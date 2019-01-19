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
import Control.Monad(when, void)
import Data.Foldable(toList)
import Data.Maybe(maybe, fromMaybe)
import Data.Word(Word32)
import GHC.TypeLits(symbolVal)
import GHC.TypeNats(natVal)
import qualified GHC.Stack
import qualified GHC.Stack.Types as GHC.Stack
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
import Control.Monad.Reader(MonadReader, ask)
import Control.Monad.State(get, put)

-- lens
import Control.Lens(view, use, assign)

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

-- fir
import CodeGen.AST
  ( AnyAST(AnyAST)
  , ASTList(NilAST, SnocAST)
  , astListLength, astListHeadTail
  , pattern Applied
  )
import CodeGen.Binary
  ( putInstruction )
import CodeGen.CFG
  ( block, branch, branchConditional )
import CodeGen.Functions
  ( declareFunction, declareFunctionCall, declareEntryPoint )
import CodeGen.IDs
  ( typeID, constID, bindingID, extInstID, stringLitID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID(ID), Instruction(..)
  , Pairs(Pairs)
  )
import CodeGen.Phi
  ( phiInstruction, phiInstructions )
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
  )
import CodeGen.Put(putASM)
import CodeGen.State
  ( CGState(currentBlock, knownBindings, localBindings)
  , CGContext
  , FunctionContext(TopLevel, EntryPoint)
  , _currentBlock
  , _functionContext
  , _knownExtInsts
  , _knownBindings
  , _localBindings, _localBinding
  , _interfaceBinding
  , _userFunction
  , _userEntryPoint
  , _debugMode
  )
import FIR.AST(AST(..), Syntactic(fromAST), toTree)
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
import Math.Linear(V((:.)))
import qualified SPIRV.FunctionControl as SPIRV
import qualified SPIRV.Operation       as SPIRV.Op
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.PrimOp          as SPIRV
import qualified SPIRV.Stage           as SPIRV
import qualified SPIRV.Storage         as Storage

----------------------------------------------------------------------------
-- code generation for the existential AST data types

codeGenASTList :: ASTList -> CGMonad [ (ID, SPIRV.PrimTy)  ]
codeGenASTList = sequence . reverse . go
    where go :: ASTList -> [ CGMonad (ID, SPIRV.PrimTy) ]
          go NilAST           = []
          go (as `SnocAST` a) = codeGen a : go as

codeGenAny :: AnyAST -> CGMonad (ID, SPIRV.PrimTy)
codeGenAny (AnyAST a) = codeGen a

----------------------------------------------------------------------------
-- main code generator

runCodeGen :: CGContext -> AST a -> Either Text ByteString
runCodeGen context = putASM context . codeGen

codeGen :: AST a -> CGMonad (ID, SPIRV.PrimTy)
codeGen (Return :$ a) = codeGen a
codeGen (Applied (MkID ident@(_,ty)) as)
  = case as of
      NilAST -> pure ident
      _      ->
        case ty of
          SPIRV.Function xs y
            -> let totalArgs = length xs
                   givenArgs = astListLength as
               in case compare totalArgs givenArgs of
                    EQ -> do retTyID <- typeID y
                             declareFunctionCall (retTyID, y) ident =<< codeGenASTList as
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
            then do let ptrTy = SPIRV.Pointer Storage.Function a_ty
                    ptrID <- newPointer ptrTy
                    store (name, a_ID) ptrID a_ty
                    pure (ptrID, ptrTy)
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

codeGen (Applied (Use singOptic) is)
  = case singOptic of

      SBinding k ->
        do  let varName = Text.pack ( symbolVal k )
            bd@(bdID, bdTy) <- bindingID varName
            case bdTy of
              SPIRV.Pointer {}
                -> load (varName, bdID) bdTy
              _ -> pure bd

      SComposeO _ (SBinding k) getter ->
        do  let varName = Text.pack ( symbolVal k )
            (bdID, bdTy) <- bindingID varName
            indices <- map fst <$> codeGenASTList is

            case bdTy of
              SPIRV.Pointer {}
                -> loadThroughAccessChain bdID indices getter
              _ -> extractUsingGetter     bdID indices getter

      _ -> throwError (   "codeGen: cannot 'use', unsupported optic:\n"
                       <> Text.pack (showSOptic singOptic) <> "\n"
                       <> "Optic does not start by accessing a binding."
                      )

codeGen (Applied (Assign singOptic) as)
  = do  (a, is) <- note
                      "codeGen: 'assign' not provided any arguments"
                      ( astListHeadTail as )

        indices <- map fst <$> codeGenASTList is

        case singOptic of

          SBinding k ->
            do  let varName = Text.pack ( symbolVal k )
                (a_ID, _)       <- codeGenAny a
                bd@(bdID, bdTy) <- bindingID varName

                case bdTy of
                  SPIRV.Pointer {}
                    -> store (varName, a_ID) bdID bdTy
                  _ -> assign ( _localBinding varName ) (Just bd)

                pure (ID 0, SPIRV.Unit) -- ID should never be used

          SComposeO _ (SBinding k) setter  ->
            do  let varName = Text.pack ( symbolVal k )
                (a_ID, _) <- codeGenAny a
                (bdID, bdTy) <- bindingID varName

                case bdTy of
                  SPIRV.Pointer {}
                    -> storeThroughAccessChain bdID a_ID indices setter 
                  _ -> insertUsingSetter       bdID a_ID indices setter 

                pure (ID 0, SPIRV.Unit) -- ID should never be used

          _ -> throwError (   "codeGen: cannot 'assign', unsupported optic:\n"
                           <> Text.pack (showSOptic singOptic) <> "\n"
                           <> "Optic does not start by accessing a binding."
                          )

codeGen (Applied (PrimOp primOp _) as)
  = codeGenPrimOp primOp =<< codeGenASTList as
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
        (codeGenCompositeConstruct compositeTy . map fst) =<< codeGenASTList as
codeGen (VectorAt ty_px i_px :$ v)
 = codeGenCompositeExtract
      (primTyVal ty_px)
      [fromIntegral ( natVal i_px )]
      =<< codeGen v
codeGen (VectorAt ty_px _ :$ f :$ v)
  = case primTyVal ty_px of
      SPIRV.Function _ _
        -> case fromAST @(V _ ( AST _ -> AST _)) f of
                     (h:._) -> codeGen (h v)
                     _      -> throwError "codeGen: accessing component of dimension 0 vector"
      _ -> throwError "codeGen: trying to apply a vector of non-functions to a vector of arguments"
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
                         (\i -> codeGenCompositeExtract constituentTy [i] vec)
                         [1..fromIntegral (natVal n)] -- off by one issues remain
              fmapped <- traverse ( codeGen . (f :$) . MkID ) elems
              codeGenCompositeConstruct vecTy (map fst . toList $ fmapped)
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
                         (\i -> codeGenCompositeExtract 
                                  ( SPIRV.Vector colDim (SPIRV.Scalar constituentTy) )
                                  [i]
                                  mat
                         )
                         [1..fromIntegral (natVal n)]
              fmapped <- traverse 
                           ( \ x -> fst <$> codeGen (Fmap (SFuncVector m) :$ f :$ MkID x) ) 
                           cols
              codeGenCompositeConstruct matTy (toList fmapped)
codeGen (Pure functorSing :$ a)
  = case functorSing of
      SFuncVector n
        -> do (valID, valTy) <- codeGen a
              let dim :: Num a => a
                  dim = fromIntegral (natVal n)
              codeGenCompositeConstruct
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
              codeGenCompositeConstruct
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
                        codeGenCompositeConstruct
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
                 <> Text.pack ( show (toTree (f :$ a)) )
                )
codeGen other
  = throwError ( "codeGen: non-exhaustive pattern match:\n"
                 <> Text.pack ( show other )
               )

----------------------------------------------------------------------------
-- primops

codeGenPrimOp :: SPIRV.PrimOp -> [ (ID, SPIRV.PrimTy) ] -> CGMonad (ID, SPIRV.PrimTy)
codeGenPrimOp primOp as
  = do  let (op,retTy) = SPIRV.opAndReturnType primOp

        -- check if any extended instruction set is required
        case op of
          SPIRV.Op.ExtCode extInst _
            -> void (extInstID extInst)
          _ -> pure ()

        extInsts <- use _knownExtInsts

        resTyID <- typeID retTy
        v <- fresh
        liftPut $ putInstruction extInsts
          Instruction
            { operation = op
            , resTy = Just resTyID
            , resID = Just v
            , args = toArgs (map fst as)
            }
        pure (v, retTy)

----------------------------------------------------------------------------
-- composite structures

codeGenCompositeConstruct :: SPIRV.PrimTy -> [ ID ] -> CGMonad (ID, SPIRV.PrimTy)
codeGenCompositeConstruct compositeType constituents
  = do tyID <- typeID compositeType
       v <- fresh
       liftPut $ putInstruction Map.empty
         Instruction
           { operation = SPIRV.Op.CompositeConstruct
           , resTy = Just tyID
           , resID = Just v
           , args  = toArgs constituents
           }
       pure (v, compositeType)

codeGenCompositeExtract :: SPIRV.PrimTy
                        -> [ Word32 ]
                        -> (ID, SPIRV.PrimTy)
                        -> CGMonad (ID, SPIRV.PrimTy)
codeGenCompositeExtract constituentTy indices (compositeID, _)
  = do constituentTyID <- typeID constituentTy
       v <- fresh
       liftPut $ putInstruction Map.empty
         Instruction
           { operation = SPIRV.Op.CompositeExtract
           , resTy     = Just constituentTyID
           , resID     = Just v
           , args      = Arg compositeID
                       $ toArgs (map pred indices) -- off by one, TODO: fix
           }
       pure (v, constituentTy)

----------------------------------------------------------------------------
-- debugging annotations

debug :: MonadReader CGContext m => m () -> m ()
debug action = (`when` action) =<< view _debugMode

sourceInfo :: GHC.Stack.CallStack -> Maybe (Text, Word32, Word32)
sourceInfo GHC.Stack.EmptyCallStack = Nothing
sourceInfo (GHC.Stack.PushCallStack _ loc stack)
  = case sourceInfo stack of
      Nothing
        -> Just ( Text.pack    $ GHC.Stack.srcLocFile      loc
                , fromIntegral $ GHC.Stack.srcLocStartLine loc
                , fromIntegral $ GHC.Stack.srcLocStartCol  loc
                )
      Just info
        -> Just info
sourceInfo (GHC.Stack.FreezeCallStack stack) = sourceInfo stack

putSrcInfo :: GHC.Stack.CallStack -> CGMonad ()
putSrcInfo callstack
  = do  (fileName, lineNo, colNo)
          <- note ( "putSrcInfo: cannot find source location \
                    \needed for debug statement"
                  )
                  ( sourceInfo callstack )
        fileID <- stringLitID fileName
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Line
            , resTy = Nothing
            , resID = Nothing
            , args  = Arg fileID
                    $ Arg lineNo
                    $ Arg colNo EndArgs
            }

----------------------------------------------------------------------------
-- load/store through pointers

newPointer :: SPIRV.PrimTy -> CGMonad ID
newPointer ptrTy@(SPIRV.Pointer storage _)
  = do  ptrTyID <- typeID ptrTy -- ensure the pointer type is declared
        v <- fresh
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Variable
            , resTy     = Just ptrTyID
            , resID     = Just v
            , args      = Arg storage EndArgs
            }
        pure v
newPointer nonPtrTy
  = throwError
  $  "codeGen: non-pointer-type "
  <> Text.pack ( show nonPtrTy )
  <> " provided for variable creation"

load :: (Text, ID) -> SPIRV.PrimTy -> CGMonad (ID, SPIRV.PrimTy)
load (loadeeName, loadeeID) ptrTy@(SPIRV.Pointer storage ty)
  = do
      _ <- typeID ptrTy -- ensure the pointer type is declared
      context <- use _functionContext
      case context of
        TopLevel -> throwError "codeGen: load operation not allowed at top level"
        EntryPoint stage entryPointName
          | storage == Storage.Input
          -- add this variable to the interface of the entry point
          -> assign ( _interfaceBinding stage entryPointName loadeeName ) (Just loadeeID)
        _ -> pure ()
      loadInstruction ty loadeeID
load _ nonPtrTy
  = throwError
  $  "codeGen: trying to load through non-pointer of type "
  <> Text.pack ( show nonPtrTy )

loadInstruction :: SPIRV.PrimTy -> ID -> CGMonad (ID, SPIRV.PrimTy)
loadInstruction ty loadeeID
  = do  tyID <- typeID ty
        v <- fresh
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Load
            , resTy = Just tyID
            , resID = Just v
            , args  = Arg loadeeID EndArgs
            }
        pure (v, ty)

store :: (Text, ID) -> ID -> SPIRV.PrimTy -> CGMonad ()
store (storeeName, storeeID) pointerID ptrTy@(SPIRV.Pointer storage _)
  = do
      _ <- typeID ptrTy -- ensure the pointer type is declared
      context <- use _functionContext
      case context of
        TopLevel -> throwError "codeGen: store operation not allowed at top level"
        EntryPoint stage entryPointName
          | storage == Storage.Output
          -- add this variable to the interface of the entry point
          -> assign ( _interfaceBinding stage entryPointName storeeName ) (Just pointerID)
        _ -> pure ()
      storeInstruction pointerID storeeID
store _ _ nonPtrTy
  = throwError
  $  "codeGen: trying to store through non-pointer of type "
  <> Text.pack ( show nonPtrTy )

storeInstruction :: ID -> ID -> CGMonad ()
storeInstruction pointerID storeeID
  = do  liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Store
            , resTy = Nothing
            , resID = Nothing
            , args = Arg pointerID
                   $ Arg storeeID EndArgs
            }
