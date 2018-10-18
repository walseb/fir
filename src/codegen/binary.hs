{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module CodeGen.Binary where

-- base
import Data.Maybe(maybe)
import Data.List(sortBy)
import Data.Foldable(traverse_)
import Data.Ord(comparing)
import Data.Word(Word32)
import qualified Data.Bits as Bits

-- binary
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary

-- containers
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

-- text
import Data.Text(Text)

-- fir
import CodeGen.Instruction ( Args(..), arity, putArgs, argsList
                           , ID(..), Instruction(..)
                           , EntryPoint(..)
                           )
import qualified SPIRV.Types        as SPIRVTy
import qualified SPIRV.OpCodes      as SPIRV
import qualified SPIRV.Capabilities as SPIRV

----------------------------------------------------------------------------

putInstruction :: Map SPIRV.Extension Instruction -> Instruction -> Binary.Put
putInstruction extInstrs Instruction { code, resTy, resID = res, args }
  = case code of

      SPIRV.OpCode opCode ->
        let n = 1                          -- OpCode and number of arguments
              + maybe 0 (const 1) resTy    -- result type + ID, if operation produces a result
              + maybe 0 (const 1) res
              + arity args                 -- one per argument
        in do Binary.putWord32be ( Bits.shift n 16 + fromIntegral opCode )
              traverse_ Binary.put resTy
              traverse_ Binary.put res
              putArgs args
           
      SPIRV.ExtOpCode ext extOpCode ->
        case resID <$> Map.lookup ext extInstrs of
          Nothing    -> pure ()
          Just extID ->
            let n = 1                           -- OpCode and number of arguments
                  + maybe 0 (const 1) resTy     -- result type + ID 
                  + maybe 0 (const 1) res       -- (these should always be present in this case)
                  + 2                           -- instruction set ID, instruction ID
                  + arity args                  -- one per argument
            in do Binary.putWord32be ( Bits.shift n 16 + 12 ) -- OpExtInst has OpCode 12
                  traverse_ Binary.put resTy
                  traverse_ Binary.put res
                  Binary.put extID
                  Binary.putWord32be extOpCode
                  putArgs args


putHeader :: Word32 -> Binary.Put
putHeader bound
  = traverse_
      Binary.putWord32be
      [ 0x07230203   -- magic number
      , 0x00010000   -- version 1.0 ( 0 | 1 | 0 | 0 )
      , 0x46495221   -- FIR!
      , bound
      , 0            -- always 0
      ]

putCapabilities :: [SPIRV.Capability] -> Binary.Put
putCapabilities
  = traverse_
      ( \cap -> putInstruction Map.empty Instruction 
        { name  = "Capability"
        , code  = SPIRV.OpCode 17
        , resTy = Nothing
        , resID = Nothing
        , args  = Arg @Word32 ( fromIntegral ( fromEnum cap ) )
                  EndArgs
        }
      )

putExtendedInstructions :: Map SPIRV.Extension Instruction -> Binary.Put
putExtendedInstructions
  = traverse_ ( putInstruction Map.empty )
      
putMemoryModel :: Binary.Put
putMemoryModel = putInstruction Map.empty Instruction
  { name  = "MemoryModel"
  , code  = SPIRV.OpCode 14
  , resTy = Nothing
  , resID = Nothing
  , args  = Arg @Word32 0 -- logical addressing
          $ Arg @Word32 1 -- GLSL450 memory model
          EndArgs
  }

putEntryPoints :: Map Text (EntryPoint ID) -> Binary.Put
putEntryPoints
  = traverse_
      ( \ EntryPoint { entryPointName, entryPointModel, entryPointID, entryPointInterface }
         -> putInstruction Map.empty Instruction
              { name  = "EntryPoint"
              , code  = SPIRV.OpCode 15
              -- slight kludge to account for unusual parameters for OpEntryPoint
              -- instead of result type, resTy field holds the ExecutionModel value
              , resTy = Just $ ID (fromIntegral . fromEnum $ entryPointModel)
              , resID = Just entryPointID
              , args  = Arg entryPointName
                      $ argsList entryPointInterface
              }
      )

putExecutionModes :: Map Text (EntryPoint ID) -> Binary.Put
putExecutionModes
  = traverse_
    ( \ EntryPoint { entryPointID, executionMode, executionModeArgs }
       -> putInstruction Map.empty Instruction
            { name = "ExecutionMode"
            , code = SPIRV.OpCode 16
            , resTy = Nothing
            , resID = Nothing
            , args  = Arg entryPointID
                    $ Arg @Word32 (fromIntegral . fromEnum $ executionMode)
                    $ argsList executionModeArgs 
            }
    )

-- assumes the map of types is well-founded,
-- e.g. if a vector type is declared, the component type is too
putTyDecs :: Map SPIRVTy.PrimTy Instruction -> Binary.Put
putTyDecs
  = traverse_ ( putInstruction Map.empty )
  . sortBy (comparing resID)
  . Map.elems

-- TODO: execution modes for each entry point
-- ( e.g. number of invocations for a geometry shader,
--   or vertex orientation for a tessellation shader )
-- see page 30 of the SPIR-V spec for necessary modes

-- TODO: debug instructions
-- annotations (decorations)
-- types, constants, global variables
-- functions
-- rest of instructions