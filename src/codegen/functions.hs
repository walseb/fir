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
import Data.Set
  ( Set )

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

-- text-utf8
import Data.Text
  ( Text )

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.CFG
  ( newBlock )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, Instruction(..)
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
  , FunctionContext(TopLevel, Function, EntryPoint)
  , _functionContext
  , _knownBinding
  , _localBindings, _localBinding
  , _localVariables
  , _interface
  , _entryPointExecutionModes
  , addCapabilities
  )
import Data.Map.Traverse
  ( traverseWithKey_ )
import qualified SPIRV.ExecutionMode   as SPIRV
import qualified SPIRV.FunctionControl as SPIRV
import qualified SPIRV.Operation       as SPIRV.Op
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.Stage           as SPIRV

----------------------------------
-- dealing with function context

inFunctionContext :: [(Text, SPIRV.PrimTy)] -> CGMonad a -> CGMonad a
inFunctionContext as
  = inContext ( Function as ) as

inEntryPointContext :: SPIRV.Stage -> Text -> CGMonad a -> CGMonad a
inEntryPointContext stage stageName
  = inContext ( EntryPoint stage stageName ) []

inContext :: forall a. FunctionContext -> [(Text, SPIRV.PrimTy)] -> CGMonad a -> CGMonad a
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
      let bodyGenOutput :: Either Text (a, CGState, ByteString)
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

declareFunctionCall :: (ID, SPIRV.PrimTy)
                    -> (ID, SPIRV.PrimTy)
                    -> [ (ID, SPIRV.PrimTy) ]
                    -> CGMonad (ID, SPIRV.PrimTy)
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
       pure (v, snd res)

declareFunction :: Text
                -> SPIRV.FunctionControl
                -> [(Text, SPIRV.PrimTy)]
                -> SPIRV.PrimTy
                -> CGMonad (ID, SPIRV.PrimTy)
                -> CGMonad ID
declareFunction funName control as b body
  = createIDRec ( _knownBinding funName )
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
            , args      = Arg control
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
  :: Text
  -> SPIRV.Stage
  -> Set (SPIRV.ExecutionMode Word32)
  -> CGMonad r
  -> CGMonad ID
declareEntryPoint stageName stage modes body
  = createIDRec ( _knownBinding stageName )
      ( do unitTyID <- typeID SPIRV.Unit
           fnTyID  <- typeID ( SPIRV.Function [] SPIRV.Unit )
           pure (unitTyID, fnTyID)
      )
      ( \(unitTyID,fnTyID) v -> do
        -- initialise entry point with empty interface
        -- loading/storing should add to the interface as needed
        assign ( _interface stage stageName ) (Just Map.empty)
        -- add the required capabilities
        addCapabilities ( SPIRV.stageCapabilities stage )
        -- annotate the execution modes
        assign ( _entryPointExecutionModes stage stageName ) (Just modes)

        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Function
            , resTy     = Just unitTyID
            , resID     = Just v
            , args      = Arg SPIRV.noFunctionControl
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
