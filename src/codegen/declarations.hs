{-# LANGUAGE RecordWildCards #-}

module CodeGen.Declarations where

-- binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy(ByteString)

-- text-utf8
import Data.Text(Text)

-- transformers
import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)

-- fir
import CodeGen.Binary ( putHeader
                      , putCapabilities
                      , putExtendedInstructions
                      , putMemoryModel
                      , putEntryPoints
                      , putBindingAnnotations
                      --, putDecorations
                      --, putExecutionModes
                      , putInstructionsInOrder
                      , putGlobals
                      )
import CodeGen.Instruction (ID(..))
import CodeGen.Monad(CGMonad, runCGMonad, runExceptTPutM)
import CodeGen.State(CGState(..), CGContext(..))

----------------------------------------------------------------------------
-- writing the declarations at the top, after codegen is finished

putDecs :: CGContext -> CGState -> ExceptT Text Binary.PutM ()
putDecs 
  CGContext { .. }
  CGState   { .. }
  = do
    lift $ do putHeader               ( idNumber currentID )
              putCapabilities         neededCapabilities
              putExtendedInstructions knownExtInsts
              putMemoryModel
    putEntryPoints (fmap fst knownBindings) usedGlobals interfaces    
    lift $ do --putExecutionModes knownBindings interfaces
              putBindingAnnotations   (fmap fst knownBindings)
              putBindingAnnotations   (fmap fst usedGlobals)
              --putDecorations
              putInstructionsInOrder  knownTypes
              putInstructionsInOrder  knownConstants
    putGlobals knownTypes usedGlobals
     
putASM :: CGContext -> CGMonad r -> Either Text ByteString
putASM context mr
  = case runCGMonad context mr of

      Right (_, cgState, body)
        -> case runExceptTPutM $ putDecs context cgState of
              Right ((), decs) -> Right ( decs <> body )
              Left err         -> Left err

      Left err -> Left err