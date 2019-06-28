{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module CodeGen.Binary where

-- base
import Data.List
  ( sortOn)
import Data.Foldable
  ( traverse_)
import Data.Word
  ( Word32 )
import qualified Data.Bits as Bits

-- binary
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except
  ( ExceptT )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- transformers
import Control.Monad.Trans.Class
  ( lift )

-- fir
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID(..), Instruction(..)
  )
import CodeGen.Monad
  ( note )
import Data.Binary.Class.Put
  ( Put(put, wordCount) )
import Data.Map.Traverse
  ( traverseWithKey_ )
import qualified SPIRV.Capability    as SPIRV
import qualified SPIRV.Decoration    as SPIRV
import qualified SPIRV.ExecutionMode as SPIRV
import qualified SPIRV.Extension     as SPIRV
import qualified SPIRV.Operation     as SPIRV.Op
import qualified SPIRV.PrimTy        as SPIRV
import qualified SPIRV.Stage         as SPIRV

----------------------------------------------------------------------------

putInstruction :: Map SPIRV.ExtInst ID -> Instruction -> Binary.Put
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
              put opArgs

           
      SPIRV.Op.ExtCode ext extOpCode ->
        case Map.lookup ext extInsts of
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

putExtendedInstructions :: Map SPIRV.ExtInst ID -> Binary.Put
putExtendedInstructions
  = traverseWithKey_ \ extInst extInstID ->
      putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.ExtInstImport
          , resTy     = Nothing
          , resID     = Just extInstID
          , args      = Arg ( SPIRV.extInstName extInst ) EndArgs
          }
      
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

putEntryPoint :: SPIRV.ExecutionModel -> ShortText -> ID -> Map ShortText ID -> Binary.Put
putEntryPoint model modelName entryPointID interface
  = putInstruction Map.empty
      Instruction
        { operation = SPIRV.Op.EntryPoint
        -- slight kludge to account for unusual parameters for OpEntryPoint
        -- instead of result type, resTy field holds the ExecutionModel value
        , resTy     = Just . ID $ SPIRV.executionModelID model
        , resID     = Just entryPointID
        , args      = Arg modelName
                    $ toArgs interface -- 'Map ShortText ID' has the appropriate traversable instance
        }

putEntryPoints
  :: Map (ShortText, SPIRV.ExecutionModel) ID
  -> Map (ShortText, SPIRV.ExecutionModel) (Map ShortText ID)
  -> ExceptT ShortText Binary.PutM ()
putEntryPoints entryPointIDs
  = traverseWithKey_
      ( \(modelName, model) interface -> do
        entryPointID
          <- note
              (  "putEntryPoints: " <> ShortText.pack (show model)
              <> " entry point named \"" <> modelName
              <> "\" not bound to any ID."
              )
              ( Map.lookup (modelName, model) entryPointIDs )
        lift ( putEntryPoint model modelName entryPointID interface )
      )

putModelExecutionModes :: ID -> SPIRV.ExecutionModes -> Binary.Put
putModelExecutionModes modelID
  = traverse_
      ( \case
          SPIRV.MaxPatchVertices {} -> pure () -- custom execution mode that doesn't exist in SPIR-V
          mode -> putInstruction Map.empty
            Instruction
              { operation = SPIRV.Op.ExecutionMode
              , resTy     = Nothing
              , resID     = Nothing
              , args      = Arg modelID
                          $ Arg mode EndArgs
              }
      )

putExecutionModes
  :: Map (ShortText, SPIRV.ExecutionModel) ID
  -> Map (ShortText, SPIRV.ExecutionModel) SPIRV.ExecutionModes
  -> ExceptT ShortText Binary.PutM ()
putExecutionModes entryPointIDs
  = traverseWithKey_
      ( \(modelName, model) executionModes -> do
        entryPointID
          <- note
              (  "putExecutionModes: " <> ShortText.pack (show model)
              <> " entry point named \"" <> modelName
              <> "\" not bound to any ID."
              )
              ( Map.lookup (modelName, model) entryPointIDs )
        lift ( putModelExecutionModes entryPointID executionModes )
      )

putKnownStringLits :: Map ShortText ID -> Binary.Put
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

putBindingAnnotations :: Map ShortText ID -> Binary.Put
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

putNames :: Set ( ID, Either ShortText (Word32, ShortText) ) -> Binary.Put
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

putDecorations :: Map ID SPIRV.Decorations -> Binary.Put
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

putMemberDecorations :: Map (ID,Word32) SPIRV.Decorations -> Binary.Put
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

putTypesAndConstants
  :: Map types     Instruction
  -> Map constants Instruction
  -> Binary.Put
putTypesAndConstants ts cs
  = traverse_ ( putInstruction Map.empty )
      ( sortOn resID $ Map.elems ts ++ Map.elems cs )

putGlobals :: Map SPIRV.PrimTy Instruction
           -> Map ShortText (ID, SPIRV.PointerTy)
           -> ExceptT ShortText Binary.PutM ()
putGlobals typeIDs
  = traverse_
      ( \(globalID, ptrTy@(SPIRV.PointerTy storage _)) ->
        do  ptrTyID
              <- note
                   ( "putGlobals: pointer type " <> ShortText.pack (show ptrTy) <> " not bound to any ID." )
                   ( resID =<< Map.lookup (SPIRV.pointerTy ptrTy) typeIDs )
            lift $ putInstruction Map.empty
                  Instruction
                    { operation = SPIRV.Op.Variable
                    , resTy = Just ptrTyID
                    , resID = Just globalID
                    , args  = Arg storage EndArgs
                    }
      )
