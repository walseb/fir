{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module CodeGen.AST where

-- base
import Data.Word(Word32)

-- binary
import Data.Binary(Binary)

-- mtl
import Control.Monad.State.Strict(State)
import Control.Monad.State.Class(MonadState)

-- containers
import Data.Map.Strict(Map)
--import qualified Data.Map.Strict as Map

-- lens
import Control.Lens( Lens', lens
                   , use, assign, (<<%=)
                   , at
                   )

-- fir
--import AST(AST(..))
--import qualified SPIRV.PrimOps      as SPIRV
import qualified SPIRV.Types        as SPIRV
import qualified SPIRV.OpCodes      as SPIRV
import qualified SPIRV.Capabilities as SPIRV

----------------------------------------------------------------------------

newtype ID = ID { identifier :: Word32 }
  deriving ( Eq, Ord, Enum, Binary )
  -- derived binary instance is big-endian

data Args where
  EndArgs :: Args
  Arg     :: Binary a => a -> Args -> Args

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
    , knownTypes         :: Map SPIRV.PrimTy    ID
    , knownExts          :: Map SPIRV.Extension ID
    , neededCapabilities :: [ SPIRV.Capability ]
    }

data EntryPoint 
  = EntryPoint
      { entryPoint        :: String
      , entryModel        :: SPIRV.ExecutionModel
      , entryID           :: ID
      , interface         :: [ ID ]
      , executionMode     :: SPIRV.ExecutionMode
      , executionModeArgs :: [ Word32 ]
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