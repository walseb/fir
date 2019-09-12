{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module: SPIRV.Storage

This module enumerates SPIR-V storage classes.

These are used both at the type-level and at the value-level.

See the SPIR-V specification, §3.7 __Storage Class__.

-}

module SPIRV.Storage where

-- fir
import Data.Binary.Class.Put
  ( Put, PutWord32Enum(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known) )

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

instance Demotable StorageClass where
  type Demote StorageClass = StorageClass

instance Known StorageClass UniformConstant where
  known = UniformConstant
instance Known StorageClass Input where
  known = Input
instance Known StorageClass Uniform where
  known = Uniform
instance Known StorageClass Output where
  known = Output
instance Known StorageClass Workgroup where
  known = Workgroup
instance Known StorageClass CrossWorkgroup where
  known = CrossWorkgroup
instance Known StorageClass Private where
  known = Private
instance Known StorageClass Function where
  known = Function
instance Known StorageClass Generic where
  known = Generic
instance Known StorageClass PushConstant where
  known = PushConstant
instance Known StorageClass AtomicCounter where
  known = AtomicCounter
instance Known StorageClass Image where
  known = Image
instance Known StorageClass StorageBuffer where
  known = StorageBuffer
