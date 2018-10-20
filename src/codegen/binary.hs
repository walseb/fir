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
import CodeGen.Instruction ( Args(..), arity, putArgs, toArgs
                           , ID(..), Instruction(..)
                           , EntryPoint(..)
                           )
import qualified SPIRV.Capability    as SPIRV
import qualified SPIRV.ExecutionMode as SPIRV
import qualified SPIRV.Extension     as SPIRV
import qualified SPIRV.Operation     as SPIRV.Op

----------------------------------------------------------------------------

traverseWithKey_ :: Applicative t => (k -> v -> t a) -> Map k v -> t ()
traverseWithKey_ f = Map.foldrWithKey (\k a b -> f k a *> b) (pure ())

----------------------------------------------------------------------------

putInstruction :: Map SPIRV.ExtInst Instruction -> Instruction -> Binary.Put
putInstruction extInsts
  Instruction { operation = op, resTy = opResTy, resID = opResID, args = opArgs }
    = case op of

      SPIRV.Op.OpCode opCode ->
        let n = 1                          -- OpCode and number of arguments
              + maybe 0 (const 1) opResTy  -- result type + ID, if operation produces a result
              + maybe 0 (const 1) opResID
              + arity opArgs               -- one per argument
        in do Binary.putWord32be ( Bits.shift n 16 + fromIntegral opCode )
              traverse_ Binary.put opResTy
              traverse_ Binary.put opResID
              putArgs opArgs
           
      SPIRV.Op.ExtOpCode ext extOpCode ->
        case resID <$> Map.lookup ext extInsts of
          Nothing    -> pure ()
          Just extID ->
            putInstruction Map.empty
              Instruction
                { operation = SPIRV.Op.ExtInst
                , resTy     = opResTy
                , resID     = opResID
                , args      = Arg extID
                            $ Arg extOpCode
                            $ opArgs
                }


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
      ( \cap -> putInstruction Map.empty 
        Instruction 
          { operation = SPIRV.Op.Capability
          , resTy     = Nothing
          , resID     = Nothing
          , args      = Arg cap
                        EndArgs
          }
      )

putExtendedInstructions :: Map SPIRV.ExtInst Instruction -> Binary.Put
putExtendedInstructions
  = traverse_ ( putInstruction Map.empty )
      
putMemoryModel :: Binary.Put
putMemoryModel 
  = putInstruction Map.empty
      Instruction
        { operation = SPIRV.Op.MemoryModel
        , resTy     = Nothing
        , resID     = Nothing
        , args      = Arg @Word32 0 -- logical addressing
                    $ Arg @Word32 1 -- GLSL450 memory model
                    EndArgs
        }

putEntryPoints :: Map Text (EntryPoint ID) -> Binary.Put
putEntryPoints
  = traverse_
      ( \ EntryPoint { entryPointName
                     , entryPointModel = SPIRV.ExecutionModel executionModel
                     , entryPointID
                     , entryPointInterface
                     }
         -> putInstruction Map.empty
              Instruction
                { operation = SPIRV.Op.EntryPoint
                -- slight kludge to account for unusual parameters for OpEntryPoint
                -- instead of result type, resTy field holds the ExecutionModel value
                , resTy     = Just ( ID executionModel )
                , resID     = Just entryPointID
                , args      = Arg entryPointName
                            $ toArgs entryPointInterface
                }
      )

putExecutionModes :: Map Text (EntryPoint ID) -> Binary.Put
putExecutionModes
  = traverse_
      ( \ EntryPoint { entryPointID, executionMode, executionModeArgs }
         -> putInstruction Map.empty
              Instruction
                { operation = SPIRV.Op.ExecutionMode
                , resTy     = Nothing
                , resID     = Nothing
                , args      = Arg entryPointID
                            $ Arg executionMode
                            $ toArgs executionModeArgs 
                }
      )


putBindingAnnotations :: Map Text ID -> Binary.Put
putBindingAnnotations
  = traverseWithKey_
      ( \ name ident -> putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.Name
          , resTy     = Nothing
          , resID     = Nothing
          , args      = Arg ident
                      $ Arg name EndArgs
          }
      )

putDecorations :: todo -> Binary.Put
putDecorations = error "todo"

putInstructionsInOrder :: Map a Instruction -> Binary.Put
putInstructionsInOrder
  = traverse_ ( putInstruction Map.empty )
  . sortBy ( comparing resID )
  . Map.elems

-- TODO: debug instructions
-- annotations (decorations)
-- global variables