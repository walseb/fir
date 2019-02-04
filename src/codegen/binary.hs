{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module CodeGen.Binary where

-- base
import Data.Maybe(maybe)
import Data.List(sortOn)
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
import qualified Data.Text as Text

-- transformers
import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)

-- fir
import CodeGen.Instruction
  ( Args(..), putArgs, toArgs
  , ID(..), Instruction(..)
  , wordCount
  )
import CodeGen.Monad(note)
import Data.Binary.Class.Put(Put(put))
import Data.Map.Traverse(traverseWithKey_)
import qualified SPIRV.Capability    as SPIRV
import qualified SPIRV.Decoration    as SPIRV
import qualified SPIRV.ExecutionMode as SPIRV
import qualified SPIRV.Extension     as SPIRV
import qualified SPIRV.Operation     as SPIRV.Op
import qualified SPIRV.PrimTy        as SPIRV
import qualified SPIRV.Stage         as SPIRV

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

putEntryPoint :: SPIRV.Stage -> Text -> ID -> Map Text ID -> Binary.Put
putEntryPoint stage stageName entryPointID interface
  = putInstruction Map.empty
      Instruction
        { operation = SPIRV.Op.EntryPoint
        -- slight kludge to account for unusual parameters for OpEntryPoint
        -- instead of result type, resTy field holds the ExecutionModel value
        , resTy     = Just stageID
        , resID     = Just entryPointID
        , args      = Arg stageName
                    $ toArgs interface -- 'Map Text ID' has the right traversable instance
        }
    where stageID :: ID
          stageID = ID . fromIntegral . fromEnum $ stage

putEntryPoints
  :: Map Text ID
  -> Map (SPIRV.Stage, Text) (Map Text ID)
  -> ExceptT Text Binary.PutM ()
putEntryPoints bindings
  = traverseWithKey_
      ( \(stage, stageName) interface -> do
        entryPointID
          <- note
              ( "putEntryPoints: entry point " <> stageName <> "not bound to any ID." )
              ( Map.lookup stageName bindings )
        lift ( putEntryPoint stage stageName entryPointID interface )
      )

putStageExecutionModes :: ID -> Set (SPIRV.ExecutionMode Word32) -> Binary.Put
putStageExecutionModes stageID
  = traverse_
      ( \mode -> putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.ExecutionMode
            , resTy     = Nothing
            , resID     = Nothing
            , args      = Arg stageID
                        $ Arg mode EndArgs
            }
      )

putExecutionModes
  :: Map Text ID
  -> Map (SPIRV.Stage, Text) (Set (SPIRV.ExecutionMode Word32))
  -> ExceptT Text Binary.PutM ()
putExecutionModes bindings
  = traverseWithKey_
      ( \(_, stageName) executionModes -> do
        entryPointID
          <- note
              ( "putExecutionModes: entry point " <> stageName <> "not bound to any ID." )
              ( Map.lookup stageName bindings )
        lift ( putStageExecutionModes entryPointID executionModes )
      )

putKnownStringLits :: Map Text ID -> Binary.Put
putKnownStringLits
  = traverseWithKey_
      ( \ lit ident -> putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.String
          , resTy     = Nothing
          , resID     = Just ident
          , args      = Arg lit EndArgs
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

putNames :: Set ( ID, Either Text (Word32, Text) ) -> Binary.Put
putNames = traverse_
  ( \case

      ( ident, Left name )
        -> putInstruction Map.empty
              Instruction
                { operation = SPIRV.Op.Name
                , resTy     = Nothing
                , resID     = Nothing
                , args      = Arg ident
                            $ Arg name EndArgs
                }

      ( ident, Right (index,name) )
        -> putInstruction Map.empty
             Instruction
               { operation = SPIRV.Op.MemberName
               , resTy     = Nothing
               , resID     = Nothing
               , args      = Arg ident
                           $ Arg index
                           $ Arg name EndArgs
               }
  )

putDecorations :: Map ID (Set (SPIRV.Decoration Word32)) -> Binary.Put
putDecorations
  = traverseWithKey_
      ( \ decoratee ->
          traverse_
            ( \ dec ->
                putInstruction Map.empty
                  Instruction
                    { operation = SPIRV.Op.Decorate
                    , resTy     = Nothing
                    , resID     = Nothing
                    , args      = Arg decoratee
                                $ Arg dec EndArgs
                    }
            )
      )

putMemberDecorations :: Map (ID,Word32) (Set (SPIRV.Decoration Word32)) -> Binary.Put
putMemberDecorations
  = traverseWithKey_
      ( \ (structID, index) ->
           traverse_
             ( \dec ->
                 putInstruction Map.empty
                   Instruction
                     { operation = SPIRV.Op.MemberDecorate
                     , resTy     = Nothing
                     , resID     = Nothing
                     , args      = Arg structID
                                 $ Arg index
                                 $ Arg dec EndArgs
                     }
             )
      )

putInstructionsInOrder :: Map a Instruction -> Binary.Put
putInstructionsInOrder
  = traverse_ ( putInstruction Map.empty )
  . sortOn resID
  . Map.elems

putTypesAndConstants :: Map a Instruction -> Map b Instruction -> Binary.Put
putTypesAndConstants as bs
  = traverse_ ( putInstruction Map.empty )
      ( sortOn resID $ Map.elems as ++ Map.elems bs )

putGlobals :: Map SPIRV.PrimTy Instruction
           -> Map Text (ID, SPIRV.PointerTy)
           -> ExceptT Text Binary.PutM ()
putGlobals typeIDs
  = traverse_
      ( \(globalID, ptrTy@(SPIRV.PointerTy storage _)) ->
        do  ptrTyID
              <- note
                   ( "putGlobals: pointer type " <> Text.pack (show ptrTy) <> " not bound to any ID." )
                   ( resID =<< Map.lookup (SPIRV.pointerTy ptrTy) typeIDs )
            lift $ putInstruction Map.empty
                  Instruction
                    { operation = SPIRV.Op.Variable
                    , resTy = Just ptrTyID
                    , resID = Just globalID
                    , args  = Arg storage EndArgs
                    }
      )
