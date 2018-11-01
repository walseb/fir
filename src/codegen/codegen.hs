{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Foldable(traverse_)
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
import Control.Monad.Except(throwError)
import Control.Monad.Reader(MonadReader, ask)
import Control.Monad.State(MonadState, get, put)
--import Control.Monad.State.Class(MonadState)

-- lens
import Control.Lens(Lens', view, use, assign)

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
                           , prependArg, prependArgs
                           , ID(ID), Instruction(..)
                           , Pairs(Pairs)
                           )
import FIR.AST(AST(..))
import FIR.Builtin( Stage, stageVal
                  , stageBuiltins, stageCapabilities
                  )
import FIR.Instances(Syntactic(fromAST))
import FIR.PrimTy( PrimTy(primTySing), primTyVal
                 , SPrimTy(..)
                 , aConstant
                 , scalarTy, sScalarTy
                 , KnownVars(knownVars)
                 )
import Math.Linear(M(unM), Matrix(transpose))
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

pattern Ap :: AST a -> ASTList -> AST b
pattern Ap f as <- (unapply . AnyAST -> (AnyAST f,as))

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
codeGen (Pure :$ a) = codeGen a
codeGen (Ap (MkID v ty) as)
  = case as of
      Nil -> pure (v,ty)
      _   ->
        case ty of
          SPIRV.Function xs y
            -> let totalArgs = length xs
                   givenArgs = astLength as
               in case compare totalArgs givenArgs of
                    EQ -> do retTyID <- typeID y
                             codeGenFunctionCall (retTyID, y) (v,ty) =<< codeGenASTList as
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
       codeGen $ f (uncurry MkID cg)
codeGen (Bind :$ a :$ f)
  = do cg <- codeGen a
       codeGen $ (fromAST f) (uncurry MkID cg)
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
codeGen (Get k)
 = do let varName = Text.pack ( symbolVal k )
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
codeGen (Put k :$ a)
  = do let varName = Text.pack ( symbolVal k )
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
codeGen (Ap (PrimOp primOp _) as)
  = codeGenPrimOp primOp =<< codeGenASTList as
codeGen (Ap (MkVector n_px a_px) as)
  = do  let n = SPIRV.toDim (natVal n_px)
        compositeTy
          <- case primTyVal a_px of
               SPIRV.Scalar s
                 -> pure $ SPIRV.Vector n s
               SPIRV.Vector m s
                 -> pure $ SPIRV.Matrix m n s -- TODO: check ordering of m, n
               x -> throwError ( "codeGen: unexpected vector constituent "
                               <> Text.pack ( show x )
                               )
        (codeGenCompositeConstruct compositeTy . map fst) =<< codeGenASTList as
codeGen (VectorAt a_px i_px :$ v)
 = codeGenCompositeExtract
    (primTyVal a_px)
    [fromIntegral ( natVal i_px )]
    =<< codeGen v
codeGen (FmapVector _) = throwError "codeGen: fmap not yet supported"
codeGen (Mat) = throwError "codeGen: matrix construction not yet supported"
codeGen (UnMat) = throwError "codeGen: matrix type eliminator not yet supported"
-- control flow
codeGen (If :$ c :$ t :$ f)
 = codeGen (IfM :$ c :$ (Pure :$ t) :$ (Pure :$ f))
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
                          <> show (f :$ a)
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
typeID :: forall m. (MonadState CGState m, MonadFresh ID m)
       => SPIRV.PrimTy -> m ID
typeID ty =
  tryToUseWith _knownPrimTy
    ( fromJust . resID ) -- type constructor instructions always have a result ID
    $ declareCapabilities ( SPIRV.primTyCapabilities ty )
    >> case ty of

        SPIRV.Matrix m _ a -> 
          createRec _knownPrimTy
            ( typeID (SPIRV.Vector m a) ) -- column type
            ( \ colID -> pure . prependArg colID . mkTyConInstruction )

        SPIRV.Vector _ a ->
          createRec _knownPrimTy
            ( typeID (SPIRV.Scalar a) ) -- element type
            ( \ eltID -> pure . prependArg eltID . mkTyConInstruction )

        SPIRV.Function as b ->
          createRec _knownPrimTy
            ( do as_IDs <- traverse typeID as -- types of function arguments
                 b_ID   <- typeID b           -- return type of function
                 pure (as_IDs, b_ID)
            )
            ( \ (as_IDs, b_ID) -> pure . prependArgs (b_ID : as_IDs)
                                       . mkTyConInstruction
            )

        SPIRV.Unit     -> create _knownPrimTy ( pure . mkTyConInstruction )
        SPIRV.Boolean  -> create _knownPrimTy ( pure . mkTyConInstruction )
        SPIRV.Scalar _ -> create _knownPrimTy ( pure . mkTyConInstruction )

        SPIRV.Pointer storage a ->
          createRec _knownPrimTy
            ( typeID a )
            ( \ tyID -> pure
                      . prependArg storage
                      . prependArg tyID
                      . mkTyConInstruction
            )

  where _knownPrimTy :: Lens' CGState (Maybe Instruction)
        _knownPrimTy = _knownType ty

        op :: SPIRV.Op.Operation
        staticTyConArgs :: [Word32]
        (op, staticTyConArgs) = SPIRV.tyAndStaticTyConArgs ty

        mkTyConInstruction :: ID -> Instruction
        mkTyConInstruction v
          = Instruction
              { operation = op
              , resTy     = Nothing
              , resID     = Just v
              , args      = toArgs staticTyConArgs
              }

constID :: forall m a.
           ( MonadState CGState m, MonadFresh ID m
           , PrimTy a
           )
        => a -> m ID
constID a =
  tryToUseWith _knownAConstant
    ( fromJust . resID ) -- constant definition instructions always have a result ID
    $ case primTySing @a of

        SMatrix m n eltTySing ->
          createRec _knownAConstant
            ( toArgs <$> ( traverse constID . unM . transpose $ a ) ) -- get the ID for each column
            $ \ cols -> mkConstantInstruction 
                          SPIRV.Op.ConstantComposite 
                          ( SPIRV.Matrix 
                              (SPIRV.sDim $ SPIRV.natSDim m) 
                              (SPIRV.sDim $ SPIRV.natSDim n)
                              (sScalarTy eltTySing)
                          )
                          cols

        SVector n eltTySing -> 
          createRec _knownAConstant
            ( toArgs <$> traverse constID a ) -- get the result ID for each component
            $ \ elts -> mkConstantInstruction 
                          SPIRV.Op.ConstantComposite
                            ( SPIRV.Vector 
                                (SPIRV.sDim $ SPIRV.natSDim n) 
                                (sScalarTy eltTySing)
                            ) 
                            elts

        SScalar _
          -> create _knownAConstant
              ( mkConstantInstruction
                  SPIRV.Op.Constant
                  (SPIRV.Scalar (scalarTy @a))
                  (Arg a EndArgs)
              )

        SUnit -> error "Error: 'constId' called on Unit type.\n\
                       \Unit has a unique value, \
                       \and as such does not need to be constructed."

        SBool -> 
          create _knownAConstant
            $ mkConstantInstruction
                ( if a
                  then SPIRV.Op.ConstantTrue
                  else SPIRV.Op.ConstantFalse
                )
                SPIRV.Boolean
                EndArgs


  where _knownAConstant :: Lens' CGState (Maybe Instruction)
        _knownAConstant = _knownConstant ( aConstant a )

        mkConstantInstruction :: SPIRV.Op.Operation
                              -> SPIRV.PrimTy
                              -> Args
                              -> ID
                              -> m Instruction
        mkConstantInstruction op ty flds v = 
          do resTypeID <- typeID ty
             pure Instruction
                    { operation = op
                    , resTy     = Just resTypeID
                    , resID     = Just v
                    , args      = flds
                    }


builtinID :: (MonadState CGState m, MonadFresh ID m)
          => Stage -> Text -> Text -> m ID
builtinID stage stageName builtinName =
  tryToUse ( _builtin stage stageName builtinName )
    id
    pure


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