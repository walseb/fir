{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module CodeGen.CodeGen
  ( codeGen, runCodeGen )
  where

-- base
import Control.Arrow(second)
import Control.Monad((>>=),(>>), when, void)
import Data.Foldable(traverse_, toList)
import Data.List(foldl1')
import Data.Maybe(fromJust)
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
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except(MonadError, throwError)
import Control.Monad.Reader(MonadReader, ask)
import Control.Monad.State(MonadState, get, put)

-- lens
import Control.Lens(Lens', view, use, assign, modifying)

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

-- fir
import CodeGen.Binary(putInstruction, traverseWithKey_)
import CodeGen.Declarations(putASM)
import CodeGen.Monad( CGMonad, runCGMonad
                    , MonadFresh(fresh)
                    , liftPut
                    , create, createRec
                    , tryToUse, tryToUseWith
                    , note
                    )
import CodeGen.State( CGState(currentBlock, knownBindings, localBindings)
                    , CGContext
                    , FunctionContext(TopLevel, Function, EntryPoint)
                    , _currentBlock
                    , _functionContext
                    , _neededCapability
                    , _knownExtInsts, _knownExtInst
                    , _knownStringLit
                    , _names
                    , _knownBindings, _knownBinding
                    , _localBindings, _localBinding
                    , _knownType
                    , _knownConstant
                    , _builtin
                    , _interface
                    , _usedGlobal
                    , _userGlobal
                    , _debugMode
                    )
import CodeGen.Instruction ( Args(..), toArgs
                           , ID(ID), Instruction(..)
                           , Pairs(Pairs)
                           )
import FIR.AST(AST(..), Syntactic(fromAST), toTree)
import FIR.Builtin( Stage, stageVal
                  , stageBuiltins, stageCapabilities
                  )
import FIR.Instances.AST()
import FIR.Instances.Optics(SOptic(..))
import FIR.PrimTy( PrimTy(primTySing)
                 , primTy, primTyVal
                 , SPrimTy(..)
                 , SPrimFunc(..)
                 , aConstant
                 , KnownVars(knownVars)
                 , traverseStruct
                 )
import Math.Linear(V((:.)), M(unM), Matrix(transpose))
import qualified SPIRV.Capability as SPIRV(Capability, primTyCapabilities)
import qualified SPIRV.Extension  as SPIRV
import qualified SPIRV.Operation  as SPIRV.Op
import qualified SPIRV.PrimTy     as SPIRV
import qualified SPIRV.PrimOp     as SPIRV
import qualified SPIRV.Storage    as Storage
import qualified SPIRV.Storage    as SPIRV(StorageClass)

----------------------------------------------------------------------------
-- pattern for applied function with any number of arguments

infixl 5 :&

data ASTList where
  Nil  :: ASTList
  (:&) :: ASTList -> AST a -> ASTList

data AnyAST where
  AnyAST :: AST a -> AnyAST

deriving instance Show ASTList
deriving instance Show AnyAST

pattern Applied :: AST a -> ASTList -> AST b
pattern Applied f as <- (unapply . AnyAST -> (AnyAST f,as))

unapply :: AnyAST -> (AnyAST, ASTList)
unapply (AnyAST (f :$ a))
  = case unapply (AnyAST f) of
        (AnyAST g, as) -> (AnyAST g, as :& a)
unapply (AnyAST f) = (AnyAST f, Nil)

codeGenASTList :: ASTList -> CGMonad [ (ID, SPIRV.PrimTy)  ]
codeGenASTList = sequence . reverse . go
    where go :: ASTList -> [ CGMonad (ID, SPIRV.PrimTy) ]
          go Nil = []
          go (as :& a) = codeGen a : go as

astLength :: ASTList -> Int
astLength Nil        = 0
astLength (as :& _ ) = 1 + astLength as

----------------------------------------------------------------------------
-- main code generator

codeGen :: AST a -> CGMonad (ID, SPIRV.PrimTy)
codeGen (Return :$ a) = codeGen a
codeGen (Applied (MkID ident@(_,ty)) as)
  = case as of
      Nil -> pure ident
      _   ->
        case ty of
          SPIRV.Function xs y
            -> let totalArgs = length xs
                   givenArgs = astLength as
               in case compare totalArgs givenArgs of
                    EQ -> do retTyID <- typeID y
                             codeGenFunctionCall (retTyID, y) ident =<< codeGenASTList as
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
codeGen (Def k _ :$ a)
  = do  let name = Text.pack ( symbolVal k )
        a_ID <- codeGen a
        assign ( _knownBinding name ) (Just a_ID)
        pure a_ID
codeGen (FunDef k as b :$ body)
  = let argTys = map (second fst) (knownVars as)
        retTy  = primTyVal b
    in debug ( putSrcInfo GHC.Stack.callStack )
    >> ( , SPIRV.Function (map snd argTys) retTy) <$>
          declareFunction
            ( Text.pack ( symbolVal k ) )
            argTys
            retTy
            ( codeGen body )
codeGen (Entry k s :$ body)
  = debug ( putSrcInfo GHC.Stack.callStack )
  >>( , SPIRV.Function [] SPIRV.Unit ) <$>
      declareEntryPoint
        ( stageVal s )
        ( Text.pack ( symbolVal k ) )
        ( codeGen body )
{-
codeGen (Get singOptic)
 = case singOptic of

    SName k -> 
      do let varName = Text.pack ( symbolVal k )
         ctxt <- use _functionContext

         case ctxt of

           -- do we need to load a built-in variable?
           EntryPoint stage stageName
             | Just (ty, Storage.Input) <- lookup varName (stageBuiltins stage)
             -> do builtin <- builtinID stage stageName varName
                   loadInstruction ty Storage.Input builtin

           -- are we looking up the value of a function argument?
           Function as
             | Just _ <- lookup varName as
               -> note
                    ( "codeGen: inconsistent local bindings for name " <> varName )
                    =<< use ( _localBinding varName )

           -- are we loading a user-defined variable (e.g. a uniform?)
           _ -> do mbGlobal <- view ( _userGlobal varName )
                   case mbGlobal of
                      Just ty
                        -> do global <- globalID varName ty Storage.Uniform
                              loadInstruction ty Storage.Uniform global

                      Nothing
                        -> do mbKnown <- use ( _knownBinding varName )
                              note
                                ( "codeGen: cannot get variable,\
                                  \ no variable with name " <> varName
                                )
                                mbKnown

    _ -> throwError "codeGen: getter not supported"

  where loadInstruction :: SPIRV.PrimTy -> SPIRV.StorageClass -> ID -> CGMonad (ID, SPIRV.PrimTy)
        loadInstruction ty storage loadeeID
          = do tyID <- typeID ty
               -- inelegantly ensure the TypePointer declaration exists
               -- TODO: include this into the builtinID function?
               _ <- typeID (SPIRV.Pointer storage ty)
               v <- fresh
               liftPut $ putInstruction Map.empty
                 Instruction
                   { operation = SPIRV.Op.Load
                   , resTy = Just tyID
                   , resID = Just v
                   , args  = Arg loadeeID EndArgs
                   }
               pure (v, ty)
codeGen (Put singOptic :$ a)
  = case singOptic of

      SName k ->
        do  let varName = Text.pack ( symbolVal k )
            (a_ID,a_ty) <- codeGen a
            ctxt <- use _functionContext

            case ctxt of

               -- do we need to store into a built-in variable?
               EntryPoint stage stageName
                 | Just (ty, Storage.Output) <- lookup varName (stageBuiltins stage)
                 -> do builtin <- builtinID stage stageName varName
                       -- inelegantly ensure the TypePointer declaration exists
                       -- TODO: include this into the builtinID function?
                       _ <- typeID (SPIRV.Pointer Storage.Output ty)
                       liftPut $ putInstruction Map.empty
                         Instruction
                           { operation = SPIRV.Op.Store
                           , resTy = Nothing
                           , resID = Nothing
                           , args = Arg builtin
                                  $ Arg a_ID EndArgs
                           }

               -- is the variable local to a function?
               Function as
                 | Just _ <- lookup varName as
                 -> assign (_localBinding varName) (Just (a_ID,a_ty))

               -- as we cannot store into user defined variables (e.g. uniforms)
               -- there is only one possible case left to deal with
               _ -> assign ( _knownBinding varName) (Just (a_ID,a_ty))

            pure (ID 0, SPIRV.Unit) -- ID should never be used

      _ -> throwError "codeGen: setter not supported"
-}
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
        _ <- codeGen bodyTrue
        trueEndBlock <- note ( "codeGen: true branch in if statement escaped CFG" )
                            =<< use _currentBlock
        trueBindings <- use _knownBindings
        trueLBindings <- use _localBindings
        branch mergeBlock

        -- false block
        block falseBlock
        _ <- codeGen bodyFalse
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
        
        pure (ID 0, SPIRV.Unit) -- ID should never be used

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
       
codeGen (Lam f) = error ( "codeGen: unexpected lambda abstraction:\n"
                         <> show (Lam f)
                        )
codeGen (f :$ a) = error ( "codeGen: unsupported function application:\n"
                          <> show (toTree (f :$ a))
                         )
codeGen other = error ( "codeGen: non-exhaustive pattern match:\n"
                        <> show other
                      )

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

codeGenFunctionCall :: (ID, SPIRV.PrimTy)
                    -> (ID, SPIRV.PrimTy)
                    -> [ (ID, SPIRV.PrimTy) ]
                    -> CGMonad (ID, SPIRV.PrimTy)
codeGenFunctionCall res func argIDs
  = do v <- fresh
       liftPut $ putInstruction Map.empty
         Instruction
           { operation = SPIRV.Op.FunctionCall
           , resTy = Just (fst res)
           , resID = Just v
           , args  = Arg (fst func)
                   $ toArgs (map fst argIDs)
           }
       pure (v, snd res)

----------------------------------------------------------------------------

runCodeGen :: CGContext -> AST a -> Either Text ByteString
runCodeGen context = putASM context . codeGen

----------------------------------------------------------------------------
-- debugging

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
        fileID <- stringLit fileName
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
-- blocks and branching

block :: ID -> CGMonad ()
block blockID = do
  liftPut $ putInstruction Map.empty
    Instruction
      { operation = SPIRV.Op.Label
      , resTy = Nothing
      , resID = Just blockID
      , args  = EndArgs
      }
  assign _currentBlock (Just blockID)

newBlock :: CGMonad ()
newBlock = fresh >>= block

branch :: ID -> CGMonad ()
branch branchID
  = liftPut $ putInstruction Map.empty
      Instruction
       { operation = SPIRV.Op.Branch
       , resID = Nothing
       , resTy = Nothing
       , args  = Arg branchID EndArgs
       }

branchConditional :: ID -> ID -> ID -> CGMonad ()
branchConditional b t f
  = liftPut $ putInstruction Map.empty
      Instruction
        { operation = SPIRV.Op.BranchConditional
        , resTy = Nothing
        , resID = Nothing
        , args  = Arg b
                $ Arg t
                $ Arg f EndArgs
        }

----------------------------------------------------------------------------
-- function declarations

declareFunction :: Text
                -> [(Text, SPIRV.PrimTy)]
                -> SPIRV.PrimTy
                -> CGMonad (ID, SPIRV.PrimTy)
                -> CGMonad ID
declareFunction funName as b body
  = createRec ( _knownBinding funName )
      ( do resTyID <- typeID b
           fnTyID  <- typeID ( SPIRV.Function (map snd as) b )
           pure (resTyID, fnTyID)
      )
      ( \(resTyID,fnTyID) v -> do
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Function
            , resTy     = Just resTyID
            , resID     = Just v
            , args      = Arg (0 :: Word32) -- no function control information
                        $ Arg fnTyID EndArgs
            }
        (retValID, _) <- inFunctionContext as body
        case b of
          SPIRV.Unit
            -> liftPut $ putInstruction Map.empty
                 Instruction
                   { operation = SPIRV.Op.Return
                   , resTy = Nothing
                   , resID = Nothing
                   , args  = EndArgs
                   }
          _ -> liftPut $ putInstruction Map.empty
                 Instruction
                   { operation = SPIRV.Op.ReturnValue
                   , resTy = Nothing
                   , resID = Just retValID
                   , args  = EndArgs
                   }
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.FunctionEnd
            , resTy     = Nothing
            , resID     = Nothing
            , args      = EndArgs
            }
        pure (v, SPIRV.Function (map snd as) b)
      )


declareArgument :: Text -> SPIRV.PrimTy -> CGMonad ID
declareArgument argName argTy
  = createRec ( _localBinding argName )
     ( ( , argTy) <$> typeID argTy )
     ( \(argTyID,_) v -> do
        liftPut $ putInstruction Map.empty Instruction
          { operation = SPIRV.Op.FunctionParameter
          , resTy = Just argTyID
          , resID = Just v
          , args = EndArgs
          }
        pure (v, argTy)
     )

declareEntryPoint :: Stage -> Text -> CGMonad r -> CGMonad ID
declareEntryPoint stage stageName body
  = createRec ( _knownBinding stageName )
      ( do unitTyID <- typeID SPIRV.Unit
           fnTyID  <- typeID ( SPIRV.Function [] SPIRV.Unit )
           pure (unitTyID, fnTyID)
      )
      ( \(unitTyID,fnTyID) v -> do
        -- initialise entry point with empty interface
        -- uses of 'Get' on builtins will add to the interface as needed
        assign ( _interface stage stageName ) (Just Set.empty)
        -- add the required capabilities
        declareCapabilities ( stageCapabilities stage )
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Function
            , resTy     = Just unitTyID
            , resID     = Just v
            , args      = Arg (0 :: Word32) -- no function control information
                        $ Arg fnTyID EndArgs
            }
        _ <- inEntryPointContext stage stageName body
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Return
            , resTy = Nothing
            , resID = Nothing
            , args  = EndArgs
            }
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.FunctionEnd
            , resTy     = Nothing
            , resID     = Nothing
            , args      = EndArgs
            }
        pure (v, SPIRV.Function [] SPIRV.Unit)
      )

----------------------------------
-- dealing with function context

inFunctionContext :: [(Text, SPIRV.PrimTy)] -> CGMonad a -> CGMonad a
inFunctionContext as action
  = do outsideBindings <- use _localBindings
       traverse_ (uncurry declareArgument) as
       assign _functionContext ( Function as )
       newBlock
       a <- action
       assign _functionContext TopLevel -- functions can't be nested
       assign _localBindings outsideBindings
       pure a

inEntryPointContext :: Stage -> Text -> CGMonad a -> CGMonad a
inEntryPointContext stage stageName action
  = do assign _functionContext ( EntryPoint stage stageName )
       newBlock
       a <- action
       assign _functionContext TopLevel
       pure a

----------------------------------------------------------------------------
-- phi instructions

conflicts :: forall k a. (Ord k, Eq a)
          => ( k -> Bool )
          -> [ Map k a ]
          -> Map k [a]
conflicts keyIsOK
  = Map.mapMaybeWithKey
      ( \k as -> if keyIsOK k && not (all (== head as) as)
                 then Just as
                 else Nothing
      )
  . foldl1'
      ( Map.merge
          Map.dropMissing
          Map.dropMissing
          ( Map.zipWithMatched (const (++)) )
      )
  . map (fmap (:[]))

phiInstructions :: ( Text -> Bool ) -> [ ID ] -> [ Map Text (ID, SPIRV.PrimTy) ] -> CGMonad ()
phiInstructions isRelevant blocks bindings 
  = traverseWithKey_
      ( \ name idsAndTys ->
        case idsAndTys of
          (_,ty) : _
            -> do tyID <- typeID ty
                  v    <- fresh                  
                  let bdAndBlockIDs :: Pairs ID -- has the right traversable instance
                      bdAndBlockIDs 
                        = Pairs $ zipWith 
                                    (\(x_ID, _) blk -> (x_ID, blk))
                                    idsAndTys
                                    blocks
                  liftPut $ putInstruction Map.empty
                    Instruction
                      { operation = SPIRV.Op.Phi
                      , resTy = Just tyID
                      , resID = Just v
                      , args  = toArgs bdAndBlockIDs
                      }
                  assign ( _knownBinding name ) (Just (v, ty))
          _ -> pure ()
      )
      ( conflicts isRelevant bindings )

----------------------------------------------------------------------------
-- instructions generated along the way that need to be floated to the top

-- get extended instruction set ID (or create one if none exist)
extInstID :: (MonadState CGState m, MonadFresh ID m)
          => SPIRV.ExtInst -> m ID
extInstID extInst = 
  tryToUse ( _knownExtInst extInst )
    ( fromJust . resID ) -- ExtInstImport instruction always has a result ID
    ( \ v -> pure
      Instruction
        { operation = SPIRV.Op.ExtInstImport
        , resTy     = Nothing
        , resID     = Just v
        , args      = Arg ( SPIRV.extInstName extInst )
                      EndArgs
        }
    )

-- get an ID for a given type ( result ID of corresponding type constructor instruction )
-- ( if one is known use it, otherwise recursively create fresh IDs for necessary types )
typeID :: forall m.
          ( MonadState CGState m
          , MonadFresh ID m
          , MonadError Text m -- only needed for the constant instruction call for array length
          )
       => SPIRV.PrimTy -> m ID
typeID ty =
  tryToUseWith _knownPrimTy
    ( fromJust . resID ) -- type constructor instructions always have a result ID
    do declareCapabilities ( SPIRV.primTyCapabilities ty )
       case ty of

        SPIRV.Matrix m n a -> 
          createRec _knownPrimTy
            ( typeID (SPIRV.Vector m (SPIRV.Scalar a)) ) -- column type
            ( \ colID 
                  -> mkTyConInstruction ( Arg colID $ Arg n EndArgs )
            )

        SPIRV.Vector n a ->
          createRec _knownPrimTy
            ( typeID a ) -- element type
            ( \ eltID 
                  -> mkTyConInstruction ( Arg eltID $ Arg n EndArgs )
            )

        SPIRV.Function as b ->
          createRec _knownPrimTy
            ( do asIDs <- traverse typeID as -- types of function arguments
                 bID   <- typeID b           -- return type of function
                 pure (asIDs, bID)
            )
            ( \ (asIDs, bID) 
                  -> mkTyConInstruction ( Arg bID $ toArgs asIDs )
            )

        SPIRV.Array l a ->
          createRec _knownPrimTy
            ( do lgID  <- constID l -- array size is the result of a constant instruction
                 eltID <- typeID  a --     as opposed to being a literal number
                 pure (eltID, lgID) -- (I suppose this is to do with specialisation constants)
            )
            ( \(eltID, lgId)
                -> mkTyConInstruction ( Arg eltID $ Arg lgId EndArgs ) 
            )

        SPIRV.RuntimeArray a ->
          createRec _knownPrimTy
            ( typeID a )
            ( \eltID -> mkTyConInstruction ( Arg eltID EndArgs ) )

        SPIRV.Unit    -> create _knownPrimTy ( mkTyConInstruction EndArgs )
        SPIRV.Boolean -> create _knownPrimTy ( mkTyConInstruction EndArgs )

        SPIRV.Scalar (SPIRV.Integer s w)
          -> create _knownPrimTy 
                ( mkTyConInstruction 
                  ( Arg (SPIRV.width w)
                  $ Arg (SPIRV.signedness s) EndArgs
                  )
                )

        SPIRV.Scalar (SPIRV.Floating w)
          -> create _knownPrimTy 
                ( mkTyConInstruction ( Arg (SPIRV.width w) EndArgs ) )

        SPIRV.Struct as
          -> createRec _knownPrimTy
                ( traverse (typeID . snd) as ) -- return IDs of struct member types
                ( \eltIDs structTyID 
                    -> do -- add annotations: name of each struct field
                          traverse_
                            ( uncurry (addMemberName structTyID) )
                            ( zipWith 
                                ( \i (k,_) -> (i,k) ) 
                                [0..]
                                as
                            )
                          -- declare the type
                          mkTyConInstruction (toArgs eltIDs) structTyID
                )

        SPIRV.Pointer storage a ->
          createRec _knownPrimTy
            ( typeID a )
            ( \ tyID -> mkTyConInstruction
                          ( Arg storage $ Arg tyID EndArgs )
            )

  where _knownPrimTy :: Lens' CGState (Maybe Instruction)
        _knownPrimTy = _knownType ty

        mkTyConInstruction :: Args -> ID -> m Instruction
        mkTyConInstruction flds v
          = pure 
              Instruction
                { operation = SPIRV.tyOp ty
                , resTy     = Nothing
                , resID     = Just v
                , args      = flds
                }

-- get the ID for a given constant, or create one if none exist
-- this is the crucial location where we make use of singletons to perform type-case
constID :: forall m a.
           ( MonadState CGState m
           , MonadFresh ID m
           , MonadError Text m
           , PrimTy a
           )
        => a -> m ID
constID a =
  tryToUseWith _knownAConstant
    ( fromJust . resID ) -- constant definition instructions always have a result ID
    do resTyID <- typeID (primTy @a) -- start off by getting an ID for the type!
       let mkConstantInstruction 
             :: SPIRV.Op.Operation -> Args -> ID -> m Instruction
           mkConstantInstruction op flds v
             = pure 
                 Instruction
                   { operation = op
                   , resTy     = Just resTyID
                   , resID     = Just v
                   , args      = flds
                   }
       case primTySing @a of

        SMatrix _ _ _ ->
          createRec _knownAConstant
            ( traverse constID . unM . transpose $ a ) -- get the ID for each column
            ( \ cols -> 
                  mkConstantInstruction 
                    SPIRV.Op.ConstantComposite 
                    ( toArgs cols )
            )

        SVector _ _ ->
          createRec _knownAConstant
            ( traverse constID a ) -- get the result ID for each component
            ( \ eltIDs -> 
                  mkConstantInstruction 
                    SPIRV.Op.ConstantComposite
                    (toArgs eltIDs)
            )

        SScalar _
          -> create _knownAConstant
              ( mkConstantInstruction
                  SPIRV.Op.Constant
                  ( Arg a EndArgs )
              )

        SUnit -> throwError
                    "constId: called on Unit type.\n\
                    \Unit has a unique value, \
                    \and as such does not need to be constructed."

        SBool -> 
          create _knownAConstant
            ( mkConstantInstruction
                ( if a
                  then SPIRV.Op.ConstantTrue
                  else SPIRV.Op.ConstantFalse
                )
                EndArgs
            )

        SRuntimeArray _ ->
            throwError
              "constID: cannot construct runtime arrays.\n\
              \Runtime arrays are only available through uniforms."
        
        SArray _ _ ->
          createRec _knownAConstant
            ( traverse constID a )
            ( \ eltIDs ->
                  mkConstantInstruction
                    SPIRV.Op.ConstantComposite
                    ( toArgs eltIDs )
            )
        
        SStruct _ ->
          createRec _knownAConstant
            ( traverseStruct constID a :: m [ID] )
            ( \ eltIDs ->
                  mkConstantInstruction
                    SPIRV.Op.ConstantComposite
                    ( toArgs eltIDs )
            )

  where _knownAConstant :: Lens' CGState (Maybe Instruction)
        _knownAConstant = _knownConstant ( aConstant a )


builtinID :: (MonadState CGState m, MonadFresh ID m)
          => Stage -> Text -> Text -> m ID
builtinID stage stageName builtinName =
  tryToUse ( _builtin stage stageName builtinName )
    id
    pure


addMemberName :: MonadState CGState m 
              => ID -> Word32 -> Text -> m ()
addMemberName structTyID index name
  = modifying _names
      ( Set.insert (structTyID, Right (index,name)) )


globalID :: (MonadState CGState m, MonadFresh ID m)
         => Text -> SPIRV.PrimTy -> SPIRV.StorageClass -> m ID
globalID globalName ty storage =
  tryToUse ( _usedGlobal globalName )
    fst
    ( pure . ( , (ty,storage) ) )

stringLit :: (MonadState CGState m, MonadFresh ID m)
          => Text -> m ID
stringLit lit =
  tryToUse ( _knownStringLit lit )
    id
    pure

declareCapabilities :: ( MonadState CGState m, Traversable t)
                    => t SPIRV.Capability -> m ()
declareCapabilities
  = traverse_ ( \cap -> assign ( _neededCapability cap ) (Just ()) )