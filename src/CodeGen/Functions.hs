{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module CodeGen.Functions where

-- base
import Data.Foldable
  ( traverse_ )
import Data.Word
  ( Word32 )

-- binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- containers
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens
  ( use, assign )

-- mtl
import Control.Monad.Except
  ( throwError )
import Control.Monad.Reader
  ( ask )
import Control.Monad.State
  ( get, put )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.CFG
  ( newBlock )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, TyID
  , Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad, runCGMonad
  , MonadFresh(fresh)
  , liftPut
  , createIDRec
  )
import CodeGen.Pointers
  ( declareVariable )
import CodeGen.State
  ( CGState(localVariables)
  , _functionContext
  , _knownBinding
  , _localBindings, _localBinding
  , _localVariables
  , _interface
  , _entryPoint
  , requireCapabilities
  )
import Data.Containers.Traversals
  ( traverseWithKey_ )
import FIR.Binding
  ( Permissions )
import FIR.ASTState
  ( FunctionContext(..)
  , VLFunctionContext
  , VLInterface
  )
import qualified SPIRV.Control      as SPIRV
  ( FunctionControl, pattern NoFunctionControl )
import qualified SPIRV.Operation    as SPIRV.Op
import qualified SPIRV.PrimTy       as SPIRV
  ( PrimTy(..) )
import qualified SPIRV.Requirements as SPIRV
  ( executionModelCapabilities )
import qualified SPIRV.Stage        as SPIRV
  ( ExecutionModel, ExecutionInfo
  , modelOf
  )

----------------------------------
-- dealing with function context

inFunctionContext :: ShortText -> [(ShortText, (SPIRV.PrimTy, Permissions))] -> CGMonad a -> CGMonad a
inFunctionContext functionName as
  = inContext ( InFunction functionName as ) as

inEntryPointContext
  :: ShortText
  -> SPIRV.ExecutionInfo Word32 model
  -> Maybe VLInterface
  -> CGMonad a
  -> CGMonad a
inEntryPointContext modelName modelInfo mbIface
  = inContext ( InEntryPoint modelName modelInfo mbIface ) []

inContext :: forall a. VLFunctionContext -> [(ShortText, (SPIRV.PrimTy, Permissions))] -> CGMonad a -> CGMonad a
inContext context as body
  = do
      outsideBindings <- use _localBindings
      assign _functionContext context
      traverse_ (uncurry declareArgument) as
      newBlock

      -- need to perform codeGen first to obtain variables defined within this function
      -- and then declare these variables first before the rest of the body
      cgContext <- ask
      cgState   <- get
      let bodyGenOutput :: Either ShortText (a, CGState, ByteString)
          bodyGenOutput = runCGMonad cgContext cgState body
      (a, bodyState, bodyASM)
        <- case bodyGenOutput of
            Left err -> throwError err
            Right (b,s,asm) -> pure (b,s,asm)

      put bodyState

      -- declare the local variables for this function
      traverseWithKey_ declareVariable (localVariables bodyState)

      -- emit assembly for the function body
      liftPut $ Binary.putLazyByteString bodyASM

      -- reset local bindings after end of function
      assign _functionContext TopLevel
      assign _localBindings   outsideBindings
      assign _localVariables  Map.empty
      pure a

----------------------------------
-- declaring function instructions

declareFunctionCall :: (TyID, SPIRV.PrimTy)
                    -> (ID, SPIRV.PrimTy)
                    -> [ (ID, SPIRV.PrimTy) ]
                    -> CGMonad ID
declareFunctionCall res func argIDs
  = do v <- fresh
       liftPut $ putInstruction Map.empty
         Instruction
           { operation = SPIRV.Op.FunctionCall
           , resTy = Just (fst res)
           , resID = Just v
           , args  = Arg (fst func)
                   $ toArgs (map fst argIDs)
           }
       pure v

declareFunction :: ShortText
                -> SPIRV.FunctionControl
                -> [(ShortText, (SPIRV.PrimTy, Permissions))]
                -> SPIRV.PrimTy
                -> CGMonad (ID, SPIRV.PrimTy)
                -> CGMonad ID
declareFunction funName control as b body
  = createIDRec ( _knownBinding funName )
      ( do resTyID <- typeID b
           fnTyID  <- typeID funTy
           pure (resTyID, fnTyID)
      )
      ( \(resTyID,fnTyID) v -> do
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Function
            , resTy     = Just resTyID
            , resID     = Just v
            , args      = Arg control
                        $ Arg fnTyID EndArgs
            }
        (retValID, _) <- inFunctionContext funName as body
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
        pure (v, funTy)
      )
  where
    funTy :: SPIRV.PrimTy
    funTy = SPIRV.Function ( map (fst . snd) as ) b


declareArgument :: ShortText -> (SPIRV.PrimTy, Permissions) -> CGMonad ID
declareArgument argName (argTy, _)
  = createIDRec ( _localBinding argName )
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

declareEntryPoint
  :: ShortText
  -> SPIRV.ExecutionInfo Word32 model
  -> Maybe VLInterface
  -> CGMonad r
  -> CGMonad ID
declareEntryPoint modelName modelInfo mbIface body
  = createIDRec ( _knownBinding modelName )
      ( do unitTyID <- typeID SPIRV.Unit
           fnTyID  <- typeID funTy
           pure (unitTyID, fnTyID)
      )
      ( \(unitTyID,fnTyID) v -> do
        -- declare entry point ID proper
        assign ( _entryPoint modelName model ) (Just v)
        -- initialise entry point with empty interface
        -- (loading/storing should add to the interface as needed)
        assign ( _interface modelName model ) (Just Map.empty)
        -- set the required capabilities
        requireCapabilities ( SPIRV.executionModelCapabilities model )

        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Function
            , resTy     = Just unitTyID
            , resID     = Just v
            , args      = Arg SPIRV.NoFunctionControl
                        $ Arg fnTyID EndArgs
            }
        _ <- inEntryPointContext modelName modelInfo mbIface body
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
        pure (v, funTy)
      )
  where
    model :: SPIRV.ExecutionModel
    model = SPIRV.modelOf modelInfo

    funTy :: SPIRV.PrimTy
    funTy = SPIRV.Function [] SPIRV.Unit
