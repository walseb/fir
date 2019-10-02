{-# LANGUAGE RecordWildCards #-}

module CodeGen.Put
  ( putASM )
  where

-- binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- text-short
import Data.Text.Short
  ( ShortText )

-- transformers
import Control.Monad.Except
  ( ExceptT )
import Control.Monad.Trans.Class
  ( lift )

-- fir
import CodeGen.Binary
  ( putHeader
  , putCapabilities
  , putExtensions
  , putExtendedInstructions
  , putMemoryModel
  , putEntryPoints
  , putExecutionModes
  , putKnownStringLits
  , putBindingAnnotations
  , putNames
  , putDecorations
  , putMemberDecorations
  , putTypesAndConstants
  , putUndefineds
  , putGlobals
  )
import CodeGen.Instruction
  ( ID(..) )
import CodeGen.Monad
  ( CGMonad, runCGMonad, runExceptTPutM )
import CodeGen.State
  ( CGState(..), CGContext(..), initialState )

----------------------------------------------------------------------------
-- emit SPIR-V assembly (after code generation)
     
putASM :: CGContext -> CGMonad r -> Either ShortText ByteString
putASM context mr
  = case runCGMonad context (initialState context) mr of

      Right (_, cgState, body)
        -> case runExceptTPutM $ putDecs context cgState of
              Right ((), decs) -> Right ( decs <> body )
              Left err         -> Left err

      Left err -> Left err

----------------------------------------------------------------------------
-- floating various declarations to the top

putDecs :: CGContext -> CGState -> ExceptT ShortText Binary.PutM ()
putDecs 
  CGContext { .. }
  CGState   { .. }
  = do
    lift $ do putHeader               ( idNumber currentID )
              putCapabilities         neededCapabilities
              putExtensions           neededExtensions
              putExtendedInstructions knownExtInsts
              putMemoryModel
    let knownBindingIDs = fmap fst knownBindings
        usedGlobalIDs   = fmap fst usedGlobals
    putEntryPoints    entryPoints interfaces
    putExecutionModes entryPoints userEntryPoints
    lift $ do
              putKnownStringLits      knownStringLits
              putBindingAnnotations   knownBindingIDs
              putBindingAnnotations   usedGlobalIDs
              putNames                names
              putDecorations          decorations
              putMemberDecorations    memberDecorations

              -- Type and constant declarations need to be interleaved.
              -- For instance, an array type needs to have
              -- its length (a constant) defined earlier.
              putTypesAndConstants    knownTypes knownConstants

              putUndefineds           knownUndefineds

    putGlobals knownTypes usedGlobals
