{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}

module CodeGen where

-- base
import Data.Maybe(maybe)
import Data.Foldable(traverse_)
import Data.Word(Word32)
import qualified Data.Bits as Bits

-- binary
import Data.Binary(Binary)
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary

-- mtl
import Control.Monad.State.Strict(State)
import Control.Monad.State.Class(MonadState)

-- containers
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens( Lens', lens
                   , use, assign, (<<%=)
                   , at
                   )

-- fir
import AST(AST(..))
import qualified SPIRV.PrimOps      as SPIRV
import qualified SPIRV.Types        as SPIRV
import qualified SPIRV.OpCodes      as SPIRV
import qualified SPIRV.Capabilities as SPIRV

----------------------------------------------------------------------------

newtype ID = ID { identifier :: Word32 }
  deriving ( Eq, Ord, Enum, Binary )
  -- derived binary instance is big-endian

data Args where
  Args :: Binary a => [a] -> Args

data Instruction
  = Instruction
    { name  :: String
    , code  :: SPIRV.OpCode
    , resTy :: Maybe ID
    , resID :: Maybe ID
    , args  :: Args
    }


data GenState
  = GenState
    { currentID          :: ID
    , knownTypes         :: Map SPIRV.PrimTy     ID
    , knownExts          :: Map SPIRV.Extension  ID
    , neededCapabilities :: [SPIRV.Capability]
    }

data EntryPoint 
  = EntryPoint
      { entryPoint :: String
      , entryModel :: SPIRV.ExecutionModel
      , entryID    :: ID
      , interface  :: [ID]
      }

----------------------------------------------------------------------------
-- lenses

_currentID :: Lens' GenState ID
_currentID = lens currentID ( \s v -> s { currentID = v } )

_knownTypes :: Lens' GenState (Map SPIRV.PrimTy ID)
_knownTypes = lens knownTypes ( \s v -> s { knownTypes = v } )

_knownExts :: Lens' GenState (Map SPIRV.Extension ID)
_knownExts = lens knownExts ( \s v -> s { knownExts = v } )

_knownType :: SPIRV.PrimTy -> Lens' GenState (Maybe ID)
_knownType primTy = _knownTypes . at primTy

_knownExt :: SPIRV.Extension -> Lens' GenState (Maybe ID)
_knownExt ext = _knownExts . at ext

----------------------------------------------------------------------------
-- fresh name generation

class MonadFresh v m where
  fresh :: m v

class HasSupply v s where
  supply :: Lens' s v

instance HasSupply ID GenState where
  supply = _currentID

newtype FreshSucc s v = FreshSucc { freshSucc :: State s v }
  deriving ( Functor, Applicative, Monad, MonadState s )

instance (HasSupply v s, Enum v) => MonadFresh v (FreshSucc s) where
  fresh = supply <<%= succ -- <<%= means: "modify, and return the _old_ value"

useOrCreate :: (MonadFresh v m, MonadState s m)
            => Lens' s (Maybe v) -> m v
useOrCreate key =
  use key >>= \case
    Just v  -> pure v
    Nothing ->
      do v <- fresh
         assign key (Just v)
         pure v
  

----------------------------------------------------------------------------


genCode :: AST a -> [Instruction]
genCode = error "lul"

toBinary :: Map SPIRV.Extension ID -> Instruction -> Binary.Put
toBinary extIDs Instruction { code, resTy, resID, args = Args args }
  = case code of

      SPIRV.OpCode opCode ->
        let n = 1                          -- OpCode and number of arguments
              + maybe 0 (const 1) resTy    -- result type + ID, if operation produces a result
              + maybe 0 (const 1) resID
              + fromIntegral (length args) -- one per argument              
        in do Binary.putWord32be ( Bits.shift n 16 + fromIntegral opCode )
              traverse_ Binary.put resTy
              traverse_ Binary.put resID
              traverse_ Binary.put args
           
      SPIRV.ExtOpCode ext extOpCode ->
        let n = 1                           -- OpCode and number of arguments
              + maybe 0 (const 1) resTy     -- result type + ID 
              + maybe 0 (const 1) resID     -- (these should always be present in this case)
              + 2                           -- instruction set ID, instruction ID
              + fromIntegral (length args)  -- one per argument
        in do Binary.putWord32be ( Bits.shift n 16 + 12 ) -- OpExtInst has OpCode 12
              traverse_ Binary.put resTy
              traverse_ Binary.put resID
              Binary.put (extIDs Map.! ext)
              Binary.putWord32be extOpCode
              traverse_ Binary.put args


header :: Word32 -> Binary.Put
header bound
  = traverse_
      Binary.putWord32be
      [ 0x07230203   -- magic number
      , 0x00010000   -- version 1.0 ( 0 | 1 | 0 | 0 )
      , 0x46495200   -- fir, version 0.0
      , bound
      , 0            -- always 0
      ]

capabilities :: [SPIRV.Capability] -> Binary.Put
capabilities
  = traverse_
      ( \cap -> toBinary Map.empty Instruction 
        { name  = "Capability"
        , code  = SPIRV.OpCode 17 -- OpCapability has OpCode 17
        , resTy = Nothing
        , resID = Nothing
        , args  = Args @Word32 [ fromIntegral ( fromEnum cap ) ]
        }
      )

extendedInstructions :: Map SPIRV.Extension ID -> Binary.Put
extendedInstructions
  = Map.foldrWithKey -- poor man's traverseWithKey_
      ( \ext ext_ID r -> toBinary Map.empty Instruction
        { name  = "ExtInstImport"
        , code  = SPIRV.OpCode 11 -- OpExtInstImport has OpCode 11
        , resTy = Nothing
        , resID = Just ext_ID
        , args  = Args [ SPIRV.extensionName ext ] -- TODO: 'Put' instance for strings might be wrong
        }
        >> r
      )
      (pure ())

memoryModel :: Binary.Put
memoryModel = toBinary Map.empty Instruction
  { name  = "MemoryModel"
  , code  = SPIRV.OpCode 14 -- OpMemoryModel has OpCode 14
  , resTy = Nothing
  , resID = Nothing
  , args  = Args @Word32
              [ 0 -- logical addressing
              , 1 -- GLSL450 memory model
              ]
  }

entryPoints :: [EntryPoint] -> Binary.Put
entryPoints
  = traverse_
      ( \ EntryPoint { entryPoint, entryModel, entryID, interface }
         -> let n = 4 + fromIntegral ( length interface )
            in do
                Binary.putWord32be ( Bits.shift n 16 + 15 ) -- OpEntryPoint has OpCode 15
                Binary.putWord32be . fromIntegral . fromEnum $ entryModel
                Binary.put entryID
                Binary.put entryPoint -- TODO: 'Put' instance for strings might be wrong
                traverse_ Binary.put interface
      )

-- TODO: execution modes for each entry point 
-- ( e.g. number of invocations for a geometry shader,
--   or vertex orientation for a tessellation shader ) 
-- see page 30 of the SPIR-V spec for necessary modes

-- TODO: debug instructions
-- annotations (decorations)
-- types, constants, global variables
-- functions
-- rest of instructions