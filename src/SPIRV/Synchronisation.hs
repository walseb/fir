{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module: SPIRV.Synchronisation

Defines synchronisation scopes and related memory lock semantics.

See the SPIR-V specification:
  
  * §3.25 __Memory Semantics__,
  * §3.27 __Scope__ (synchronisation scopes).

-}

module SPIRV.Synchronisation
  ( SynchronisationScope(..)
  , MemorySemantics(..)
  , OrderingSemantics(..)
  , LockSemantics(..)
  , MemoryType(..)
  , synchronisationScope
  , memorySemanticsBitmask
  ) where

-- base
import Data.Bits
  ( Bits ( (.|.), bit ) )
import Data.Word
  ( Word32 )

-- fir
import Data.Binary.Class.Put
  ( Put(..), PutWord32Enum(..) )

--------------------------------------------------

data SynchronisationScope
  = CrossDevice
  | Device
  | Workgroup
  | Subgroup
  | Invocation
  | QueueFamily
  deriving stock ( Eq, Ord, Show, Enum, Bounded )
  deriving Put via ( PutWord32Enum SynchronisationScope )

synchronisationScope :: SynchronisationScope -> Word32
synchronisationScope = fromIntegral . fromEnum

data MemorySemantics
  = MemorySemantics
    { orderingSemantics :: OrderingSemantics
    , memoryTypes       :: [ MemoryType ]
    }
  deriving stock ( Eq, Show )

data OrderingSemantics
  = Lock [ LockSemantics ]
  | SequentiallyConsistent
  deriving stock ( Eq, Show )

ordBitmask :: OrderingSemantics -> Word32
ordBitmask = \case
  SequentiallyConsistent -> 16
  Lock locks -> case ( Acquire `elem` locks, Release `elem` locks ) of
    ( True , True  ) -> 8
    ( True , False ) -> 2
    ( False, True  ) -> 4
    ( False, False ) -> 0

memorySemanticsBitmask :: MemorySemantics -> Word32
memorySemanticsBitmask ( MemorySemantics ord mems ) = ordBitmask ord .|. memBitmask mems

data LockSemantics
  = Acquire
  | Release
  deriving stock ( Eq, Show )

instance Put MemorySemantics where
  wordCount _ = 1
  put memSem  = put ( memorySemanticsBitmask memSem )

instance Put (Maybe MemorySemantics) where
  wordCount _ = 1
  put Nothing    = put @Word32 0
  put (Just sem) = put sem

data MemoryType
  = UniformMemory
  | SubgroupMemory
  | WorkgroupMemory
  | CrossWorkgroupMemory
  | AtomicCounterMemory
  | ImageMemory
  | OutputMemory
  deriving stock ( Eq, Ord, Show, Enum, Bounded )

memBitmask :: [MemoryType] -> Word32
memBitmask = foldl (.|.) 0 . map memBit

memBit :: MemoryType -> Word32
memBit = bit . ( + 6 ) . fromEnum
