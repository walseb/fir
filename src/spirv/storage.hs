{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE PolyKinds           #-}

module SPIRV.Storage where

-- fir
import Data.Binary.Class.Put(Put, PutWord32Enum(..))

--------------------------------------------------

data StorageClass
  = UniformConstant
  | Input
  | Uniform
  | Output
  | Workgroup
  | CrossWorkgroup
  | Private
  | Function
  | Generic
  | PushConstant
  | AtomicCounter
  | Image
  | StorageBuffer
  deriving (Show, Eq, Ord, Enum, Bounded)
  deriving Put via (PutWord32Enum StorageClass)

class KnownStorage (storage :: StorageClass) where
  storage :: StorageClass

instance KnownStorage UniformConstant where
  storage = UniformConstant
instance KnownStorage Input where
  storage = Input
instance KnownStorage Uniform where
  storage = Uniform
instance KnownStorage Output where
  storage = Output
instance KnownStorage Workgroup where
  storage = Workgroup
instance KnownStorage CrossWorkgroup where
  storage = CrossWorkgroup
instance KnownStorage Private where
  storage = Private
instance KnownStorage Function where
  storage = Function
instance KnownStorage Generic where
  storage = Generic
instance KnownStorage PushConstant where
  storage = PushConstant
instance KnownStorage AtomicCounter where
  storage = AtomicCounter
instance KnownStorage Image where
  storage = Image
instance KnownStorage StorageBuffer where
  storage = StorageBuffer
