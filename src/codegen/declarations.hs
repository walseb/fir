{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module CodeGen.Declarations where

-- binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy(ByteString)

-- containers
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

-- text-utf8
import Data.Text(Text)

-- transformers
import Control.Monad.Except(MonadError , ExceptT, throwError)
import Control.Monad.Trans.Class(lift)

-- fir
import CodeGen.Binary ( putHeader
                      , putCapabilities
                      , putExtendedInstructions
                      , putMemoryModel
                      , putEntryPoints
                      , putBindingAnnotations
                      , putDecorations
                      , putExecutionModes
                      , putTyDecs
                      )
import CodeGen.Instruction ( ID(..)
                           , EntryPoint(..)
                           )
import CodeGen.Monad ( CGState(..), CGContext(..)
                     , CGMonad, runCGMonad, runExceptTPutM
                     )

----------------------------------------------------------------------------
-- writing the declarations at the top, after codegen is finished

putDecs :: CGContext -> CGState -> ExceptT Text Binary.PutM ()
putDecs 
  CGContext { .. }
  CGState   { .. }
  = do entryPointsWithIDs <- identifyEntryPoints knownBindings entryPoints
       lift $
         do putHeader               ( idNumber currentID )
            putCapabilities         neededCapabilities
            putExtendedInstructions knownExtInsts
            putMemoryModel
            putEntryPoints          entryPointsWithIDs
            putExecutionModes       entryPointsWithIDs
            putBindingAnnotations   knownBindings
            putDecorations          undefined --todo
            putTyDecs               knownTypes

putASM :: CGContext -> CGMonad r -> Either Text ByteString
putASM context mr
  = case runCGMonad context mr of

      Right (_, cgState, body)
        -> case runExceptTPutM $ putDecs context cgState of
              Right ((), decs) -> Right ( decs <> body )
              Left err         -> Left err

      Left err -> Left err

----------------------------------------------------------------------------
-- managing entry point ID data

-- TODO: move this somewhere sensible
note :: MonadError e m => e -> Maybe a -> m a
note _ (Just a) = pure a
note e Nothing  = throwError e

-- fill in the ID of an entry point and of all its bindings in its interface
identifyEntryPoint
  :: forall m. ( MonadError Text m )
  => Map Text ID 
  -> EntryPoint Text
  -> m (EntryPoint ID)
identifyEntryPoint
  bindingIDs
  entryPoint@EntryPoint
    { entryPointName      = entryName
    , entryPointInterface = interface
    }  
  = do
      entryID <- 
        note
          (    "No entry point with name "
            <> entryName
            <> " was found during code generation."
          )
          ( Map.lookup entryName bindingIDs )

      interfaceIDs <- 
        traverse
          ( \ bindingName -> note
            (    "No binding with name "
              <> bindingName
              <> " was found during code generation."
            )
            ( Map.lookup bindingName bindingIDs )
          )
          interface
      
      pure ( entryPoint 
              { entryPointID        = entryID
              , entryPointInterface = interfaceIDs
              }
           )

identifyEntryPoints
  :: forall m. ( MonadError Text m )
  => Map Text ID
  -> Map Text (EntryPoint Text)
  -> m (Map Text (EntryPoint ID))
identifyEntryPoints = traverse . identifyEntryPoint