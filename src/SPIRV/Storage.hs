{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: SPIRV.Storage

This module enumerates SPIR-V storage classes.

These are used both at the type-level and at the value-level.

See the SPIR-V specification, §3.7 __Storage Class__.

-}

module SPIRV.Storage where

-- base
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.Ord
  ( POrd(..) )

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
  | RayStorage RayStorage
  deriving stock (Show, Eq, Ord)

instance Put StorageClass where
  put UniformConstant = put @Word32 0
  put Input           = put @Word32 1
  put Uniform         = put @Word32 2
  put Output          = put @Word32 3
  put Workgroup       = put @Word32 4
  put CrossWorkgroup  = put @Word32 5
  put Private         = put @Word32 6
  put Function        = put @Word32 7
  put Generic         = put @Word32 8
  put PushConstant    = put @Word32 9
  put AtomicCounter   = put @Word32 10
  put Image           = put @Word32 11
  put StorageBuffer   = put @Word32 12
  put (RayStorage s)  = put s
  wordCount _         = 1

data DataOrigin
  = Lifetime
  | Incoming
  deriving stock (Show, Eq, Ord)

data RayStorage
  = CallableData DataOrigin
  | RayPayload   DataOrigin
  | HitAttribute
  | ShaderRecordBuffer
  deriving stock (Show, Eq, Ord)

instance Put RayStorage where
  put (CallableData Lifetime) = put @Word32 5328
  put (CallableData Incoming) = put @Word32 5329
  put (RayPayload   Lifetime) = put @Word32 5338
  put (RayPayload   Incoming) = put @Word32 5342
  put HitAttribute            = put @Word32 5339
  put ShaderRecordBuffer      = put @Word32 5343
  wordCount _                 = 1

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
instance Known RayStorage s => Known StorageClass ('RayStorage s) where
  known = RayStorage (knownValue @s)

instance Demotable DataOrigin where
  type Demote DataOrigin = DataOrigin

instance Known DataOrigin Lifetime where
  known = Lifetime
instance Known DataOrigin Incoming where
  known = Incoming

instance Demotable RayStorage where
  type Demote RayStorage = RayStorage

instance Known DataOrigin origin => Known RayStorage (CallableData origin) where
  known = CallableData (knownValue @origin)
instance Known DataOrigin origin => Known RayStorage (RayPayload origin) where
  known = RayPayload (knownValue @origin)
instance Known RayStorage HitAttribute where
  known = HitAttribute
instance Known RayStorage ShaderRecordBuffer where
  known = ShaderRecordBuffer



instance POrd StorageClass where
  type Compare x y = Compare (StorageClassToNat x) (StorageClassToNat y)

type family StorageClassToNat (s :: StorageClass) :: Nat where
  StorageClassToNat UniformConstant = 0
  StorageClassToNat Input           = 1
  StorageClassToNat Uniform         = 2
  StorageClassToNat Output          = 3
  StorageClassToNat Workgroup       = 4
  StorageClassToNat CrossWorkgroup  = 5
  StorageClassToNat Private         = 6
  StorageClassToNat Function        = 7
  StorageClassToNat Generic         = 8
  StorageClassToNat PushConstant    = 9
  StorageClassToNat AtomicCounter   = 10
  StorageClassToNat Image           = 11
  StorageClassToNat StorageBuffer   = 12
  StorageClassToNat ('RayStorage r) = RayStorageClassToNat r

type family RayStorageClassToNat (r :: RayStorage) :: Nat where
  RayStorageClassToNat (CallableData Lifetime) = 5328
  RayStorageClassToNat (CallableData Incoming) = 5329
  RayStorageClassToNat (RayPayload   Lifetime) = 5338
  RayStorageClassToNat (RayPayload   Incoming) = 5342
  RayStorageClassToNat HitAttribute            = 5339
  RayStorageClassToNat ShaderRecordBuffer      = 5343
