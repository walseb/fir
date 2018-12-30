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
import CodeGen.Binary
  ( putHeader
  , putCapabilities
  , putExtendedInstructions
  , putMemoryModel
  , putEntryPoints
  , putKnownStringLits
  , putBindingAnnotations                      
  , putNames
--, putDecorations
--, putExecutionModes
  , putTypesAndConstants
  , putGlobals
  )
import CodeGen.Instruction (ID(..))
import CodeGen.Monad(CGMonad, runCGMonad, runExceptTPutM)
import CodeGen.State(CGState(..), CGContext(..), initialState)

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
    putEntryPoints (fmap fst knownBindings) interfaces
    lift $ do --putExecutionModes knownBindings interfaces
              putKnownStringLits      knownStringLits
              putBindingAnnotations   (fmap fst knownBindings)
              putBindingAnnotations   (fmap fst usedGlobals)
              putNames                names
              --putDecorations

              -- Type and constant declarations need to be interleaved.
              -- For instance, an array type needs to have
              -- its length (a constant) defined earlier.
              putTypesAndConstants    knownTypes knownConstants

    putGlobals knownTypes usedGlobals
     
putASM :: CGContext -> CGMonad r -> Either Text ByteString
putASM context mr
  = case runCGMonad context initialState mr of

      Right (_, cgState, body)
        -> case runExceptTPutM $ putDecs context cgState of
              Right ((), decs) -> Right ( decs <> body )
              Left err         -> Left err

      Left err -> Left err