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
import Data.Set(Set)
import qualified Data.Set as Set

-- text
import Data.Text(Text)

-- transformers
import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)

-- fir
import CodeGen.Instruction ( Args(..), putArgs, toArgs
                           , ID(..), Instruction(..)
                           , wordCount
                           )
import CodeGen.Monad(note)
import Data.Binary.Class.Put(Put(put), Literal(Literal))
import FIR.Builtin(Stage, executionModel)
import qualified SPIRV.Capability    as SPIRV
import qualified SPIRV.ExecutionMode as SPIRV
import qualified SPIRV.Extension     as SPIRV
import qualified SPIRV.Operation     as SPIRV.Op
import qualified SPIRV.PrimTy        as SPIRV
import qualified SPIRV.Storage       as SPIRV

import Debug.Trace(trace)
import Numeric(showHex)

----------------------------------------------------------------------------

traverseWithKey_ :: Applicative t => (k -> v -> t a) -> Map k v -> t ()
traverseWithKey_ f = Map.foldrWithKey (\k a b -> f k a *> b) (pure ())

----------------------------------------------------------------------------

putInstruction :: Map SPIRV.ExtInst Instruction -> Instruction -> Binary.Put
putInstruction extInsts
  Instruction { operation = op, resTy = opResTy, resID = opResID, args = opArgs }
    = case op of

      SPIRV.Op.Code opCode ->
        let n :: Word32
            n = 1                          -- OpCode and word count (first byte)
              + maybe 0 (const 1) opResTy  -- result type (if present)
              + maybe 0 (const 1) opResID  -- ID (if present)
              + wordCount opArgs
        in do put @Word32 ( Bits.shift n 16 + fromIntegral opCode)
              traverse_ put opResTy
              traverse_ put opResID
              trace (show (SPIRV.Op.Code opCode) ++ " ( hex = " ++ showHex opCode "" ++ ")") (pure ())
              putArgs opArgs

           
      SPIRV.Op.ExtCode ext extOpCode ->
        case resID =<< Map.lookup ext extInsts of
          Nothing    -> pure ()
          Just extID ->
            putInstruction Map.empty
              Instruction
                { operation = SPIRV.Op.ExtInst
                , resTy     = opResTy
                , resID     = opResID
                , args      = Arg extID
                            $ Arg extOpCode
                            opArgs
                }


putHeader :: Word32 -> Binary.Put
putHeader bound
  = traverse_
      (put @Word32)
      [ 0x07230203   -- magic number
      , 0x00010000   -- version 1.0 ( 0 | 1 | 0 | 0 )
      , 0x21524946   -- FIR!
      , bound
      , 0            -- always 0
      ]

putCapabilities :: Set SPIRV.Capability -> Binary.Put
putCapabilities
  = Set.foldr' ( \cap p -> p >> putCap cap ) (pure ()) -- traverse_ for sets
      where 
        putCap :: SPIRV.Capability -> Binary.Put
        putCap cap 
          = putInstruction Map.empty 
              Instruction 
                { operation = SPIRV.Op.Capability
                , resTy     = Nothing
                , resID     = Nothing
                , args      = Arg cap
                              EndArgs
                }

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

putEntryPoint :: Stage -> Text -> ID -> [ID] -> Binary.Put
putEntryPoint stage stageName entryPointID interface
  = putInstruction Map.empty
      Instruction
        { operation = SPIRV.Op.EntryPoint
        -- slight kludge to account for unusual parameters for OpEntryPoint
        -- instead of result type, resTy field holds the ExecutionModel value
        , resTy     = Just ( ID executionID )
        , resID     = Just entryPointID
        , args      = Arg (Literal stageName)
                    $ toArgs interface
        }
    where SPIRV.ExecutionModel executionID = executionModel stage

putEntryPoints :: Map Text ID -> Map Text (ID, p) -> Map (Stage, Text) (Set Text) -> ExceptT Text Binary.PutM ()
putEntryPoints bindings globals
  = traverseWithKey_
      ( \(stage, stageName) builtins -> do
        builtin_IDs <-
          traverse
            ( \builtin -> 
              note
                ( "putEntryPoints: builtin " <> builtin <> " not bound to any ID." )
                ( fst <$> Map.lookup builtin globals)
            )
            (Set.toList builtins)
        entryPointID <- note
                     ( "putEntryPoints: entry point " <> stageName <> "not bound to any ID." )
                     ( Map.lookup stageName bindings )
        lift ( putEntryPoint stage stageName entryPointID builtin_IDs )
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
                      $ Arg (Literal name) EndArgs
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

putGlobals :: Map SPIRV.PrimTy Instruction
           -> Map Text (ID, (SPIRV.PrimTy,SPIRV.StorageClass))          
           -> ExceptT Text Binary.PutM ()
putGlobals typeIDs
  = traverse_
      ( \(globalID, (ty, storage)) -> 
        do let ptrTy = SPIRV.Pointer storage ty
           ptrTyID 
             <- note
                  ( error ( "putGlobals: pointer type " ++ show ptrTy ++ " not bound to any ID." ) )
                  ( resID =<< Map.lookup ptrTy typeIDs )
           lift $ putInstruction Map.empty
                 Instruction
                   { operation = SPIRV.Op.Variable
                   , resTy = Just ptrTyID
                   , resID = Just globalID
                   , args  = Arg storage EndArgs
                   }
      )
                