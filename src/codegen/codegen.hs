{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module CodeGen.CodeGen where

-- base
import Control.Arrow(second)
import Data.Foldable(traverse_)
import Data.Maybe(fromJust)
import Data.Word(Word32)
import GHC.TypeLits(symbolVal)
import Prelude hiding (Monad(..))

-- bytestring
import Data.ByteString.Lazy(ByteString)

-- containers
import qualified Data.Map as Map
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except(throwError)
import Control.Monad.State.Class(MonadState)

-- lens
import Control.Lens(Lens', view, use, assign)

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

-- fir
import CodeGen.Binary(putInstruction)
import CodeGen.Declarations(putASM)
import CodeGen.Monad( CGMonad, MonadFresh(fresh)
                    , liftPut
                    , create, createRec
                    , tryToUse, tryToUseWith
                    , note
                    )
import CodeGen.State( CGState, CGContext
                    , FunctionContext(TopLevel, Function, EntryPoint)
                    , _functionContext
                    , _knownExtInst
                    , _knownBinding
                    , _localBindings, _localBinding
                    , _knownType
                    , _knownConstant
                    , _builtin
                    , _interface
                    , _usedGlobal
                    , _userGlobal
                    )
import CodeGen.Instruction ( Args(..), toArgs
                           , prependArg, prependArgs
                           , ID(ID), Instruction(..)
                           )
import Control.Monad.Indexed((:=), atKey, Id(runId))
import Data.Binary.Class.Put(Literal(Literal))
import FIR.AST(AST(..))
import FIR.Builtin(Stage, stageVal, stageBuiltins)
import FIR.Instances(Syntactic(fromAST))
import FIR.PrimTy( PrimTy(primTySing), primTyVal
                 , SPrimTy(..)
                 , aConstant
                 , scalarTy, sScalarTy
                 , KnownVars(knownVars)
                 )
import Math.Linear(M(unM), Matrix(transpose))
import qualified SPIRV.Extension as SPIRV
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
import qualified SPIRV.PrimOp    as SPIRV
import qualified SPIRV.Storage   as Storage
import SPIRV.Storage(StorageClass)

----------------------------------------------------------------------------
-- main code generator


codeGen :: AST a -> CGMonad ID
-- ignore indexing information
codeGen (Pure     :$ a) = codeGen a
codeGen (RunId    :$ a) = codeGen a
codeGen (MkAtKey  :$ a) = codeGen a
codeGen (RunAtKey :$ a) = codeGen a
-- constants
codeGen (Lit a) = constID a
-- perform substitution when possible
codeGen (Lam f :$ a) = codeGen (f a)
codeGen (Bind :$ a :$ f)
  = codeGen ( fromAST f
            $ atKey       -- acrobatics
            . runId       -- (to deal with indexing)
            . fromAST @(Id (AST _ := _) _)
            $ a
            )
-- stateful operations
codeGen (Def k _ :$ a)
  = do let name = Text.pack ( symbolVal k )
       a_ID <- codeGen a
       assign ( _knownBinding name ) (Just a_ID)
       pure a_ID
codeGen (FunDef k as b :$ body)
  = declareFunction
      ( Text.pack ( symbolVal k ) )
      ( map (second fst) (knownVars as) )
      ( primTyVal b )
      ( codeGen body )
codeGen (Entry k s :$ body)
  = declareEntryPoint
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
  where loadInstruction :: SPIRV.PrimTy -> StorageClass -> ID -> CGMonad ID
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
               pure v
codeGen (Put k :$ a)
  = do let varName = Text.pack ( symbolVal k )
       a_ID <- codeGen a
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
            -> assign (_localBinding varName) (Just a_ID)

          -- as we cannot store into user defined variables (e.g. uniforms)
          -- there is only one possible case left to deal with
          _ -> assign ( _knownBinding varName) (Just a_ID)

       pure (ID 666666666) -- should never be used
-- primops
codeGen (PrimOp primOp _ :$ a1 :$ a2 :$ a3 :$ a4)
  = codeGenPrimOp primOp =<< sequence [codeGen a1, codeGen a2, codeGen a3, codeGen a4]
codeGen (PrimOp primOp _ :$ a1 :$ a2 :$ a3)
  = codeGenPrimOp primOp =<< sequence [codeGen a1, codeGen a2, codeGen a3]
codeGen (PrimOp primOp _ :$ a1 :$ a2)
  = codeGenPrimOp primOp =<< sequence [codeGen a1, codeGen a2]
codeGen (PrimOp primOp _ :$ a1)
  = codeGenPrimOp primOp =<< sequence [codeGen a1]
codeGen (PrimOp primOp _ )
  = codeGenPrimOp primOp []
codeGen (MkVector _) = throwError "codeGen: vector construction not yet supported"
codeGen (VectorAt _) = throwError "codeGen: vector indexing not yet supported"
codeGen (FmapVector _) = throwError "codeGen: fmap not yet supported"
codeGen (Mat) = throwError "codeGen: matrix construction not yet supported"
codeGen (UnMat) = throwError "codeGen: matrix type eliminator not yet supported"
codeGen (If) = throwError "codeGen: if statements not yet supported"
codeGen (Lam f) = error ( "codeGen: unexpected lambda abstraction:\n"
                          ++ show (Lam f)
                        )
codeGen (f :$ a) = error ( "codeGen: unsupported function application:\n"
                          ++ show (f :$ a)
                         )
-- NamedVar used only for pretty-printing AST
codeGen (NamedVar _) = throwError "codeGen: unexpected 'NamedVar'"
codeGen other = error ( "codeGen: non-exhaustive pattern match:\n"
                        ++ show other
                      )

codeGenPrimOp :: SPIRV.PrimOp -> [ ID ] -> CGMonad ID
codeGenPrimOp primOp as
  = do let (op,retTy) = SPIRV.opAndReturnType primOp
       resTyId <- typeID retTy
       v <- fresh
       liftPut $ putInstruction Map.empty
         Instruction
           { operation = op
           , resTy = Just resTyId
           , resID = Just v
           , args = toArgs as
           }
       pure v

runCodeGen :: CGContext -> AST a -> Either Text ByteString
runCodeGen context = putASM context . codeGen

----------------------------------------------------------------------------
-- function declarations

declareFunction :: Text -> [(Text, SPIRV.PrimTy)] -> SPIRV.PrimTy -> CGMonad r -> CGMonad ID
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
        _ <- inFunctionContext as body
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.FunctionEnd
            , resTy     = Nothing
            , resID     = Nothing
            , args      = EndArgs
            }
        pure v
      )


declareArgument :: Text -> SPIRV.PrimTy -> CGMonad ID
declareArgument argName argTy
  = createRec ( _localBinding argName )
     ( typeID argTy )
     ( \argTyID v -> do
        liftPut $ putInstruction Map.empty Instruction
          { operation = SPIRV.Op.FunctionParameter
          , resTy = Just argTyID
          , resID = Just v
          , args = EndArgs
          }
        pure v
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
            { operation = SPIRV.Op.FunctionEnd
            , resTy     = Nothing
            , resID     = Nothing
            , args      = EndArgs
            }

        pure v
      )

----------------------------------
-- dealing with function context

inFunctionContext :: [(Text, SPIRV.PrimTy)] -> CGMonad a -> CGMonad a
inFunctionContext as action
  = do outsideBindings <- use _localBindings
       traverse_ (uncurry declareArgument) as
       assign _functionContext ( Function as )
       a <- action
       assign _functionContext TopLevel -- functions can't be nested
       assign _localBindings outsideBindings
       pure a

inEntryPointContext :: Stage -> Text -> CGMonad a -> CGMonad a
inEntryPointContext stage stageName action
  = do assign _functionContext ( EntryPoint stage stageName )
       a <- action
       assign _functionContext TopLevel
       pure a

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
        , args      = Arg ( Literal (SPIRV.extInstName extInst) )
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
    $ case ty of

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

        SScalar _ -> create _knownAConstant 
              ( mkConstantInstruction SPIRV.Op.Constant (SPIRV.Scalar (scalarTy @a)) (Arg a EndArgs) )

        SUnit -> error "Error: 'constId' called on Unit type.\n\
                       \Unit has a unique value, and as such does not need to be constructed."

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

        mkConstantInstruction :: SPIRV.Op.Operation -> SPIRV.PrimTy -> Args -> ID -> m Instruction
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
         => Text -> SPIRV.PrimTy -> StorageClass -> m ID
globalID globalName ty storage =
  tryToUse ( _usedGlobal globalName )
    fst
    ( pure . ( , (ty,storage) ) )