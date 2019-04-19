{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module SPIRV.Stage where

-- base
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat, KnownNat, CmpNat )

-- fir
import Data.Binary.Class.Put
  ( Put, PutWord32Enum(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import SPIRV.Capability
  ( Capability )
import qualified SPIRV.Capability as Capability

-------------------------------------------------------
-- stages (called execution models in SPIR-V)

data Stage
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | GLCompute
  | Kernel
  deriving ( Show, Eq, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum Stage)

stageCapabilities :: Stage -> [ Capability ]
stageCapabilities Vertex                 = [ Capability.Shader       ]
stageCapabilities TessellationControl    = [ Capability.Tessellation ] --- |
stageCapabilities TessellationEvaluation = [ Capability.Tessellation ] --- |-- shader capability implied
stageCapabilities Geometry               = [ Capability.Geometry     ] --- |
stageCapabilities Fragment               = [ Capability.Shader       ]
stageCapabilities GLCompute              = [ Capability.Shader       ]
stageCapabilities Kernel                 = [ Capability.Kernel       ]

instance Demotable Stage where
  type Demote Stage = Stage

instance Known Stage Vertex where
  known = Vertex
instance Known Stage TessellationControl where
  known = TessellationControl
instance Known Stage TessellationEvaluation where
  known = TessellationEvaluation
instance Known Stage Geometry where
  known = Geometry
instance Known Stage Fragment where
  known = Fragment
instance Known Stage GLCompute where
  known = GLCompute
instance Known Stage Kernel where
  known = Kernel

type family FromEnumStage (s :: Stage) :: Nat where
  FromEnumStage Vertex                 = 0
  FromEnumStage TessellationControl    = 1
  FromEnumStage TessellationEvaluation = 2
  FromEnumStage Geometry               = 3
  FromEnumStage Fragment               = 4
  FromEnumStage GLCompute              = 5
  FromEnumStage Kernel                 = 6

type CmpStage (s1 :: Stage) (s2 :: Stage)
  = FromEnumStage s1 `CmpNat` FromEnumStage s2

--------------------------------------------------------------------------
-- additional stage information that needs to be known at the type-level

data StageInfo (n :: Type) (s :: Stage) where
  VertexInfo
    :: StageInfo n 'Vertex
  TessellationControlInfo
    :: n -- input size
    -> n -- output size
    -> StageInfo n 'TessellationControl
  TessellationEvaluationInfo
    :: n -- input size
    -> StageInfo n 'TessellationEvaluation
  GeometryInfo
    :: n -- input size
    -> StageInfo n 'Geometry
  FragmentInfo
    :: StageInfo n 'Fragment
  GLComputeInfo
    :: StageInfo n 'GLCompute
  KernelInfo
    :: StageInfo n 'Kernel

instance Demotable (StageInfo Nat s) where
  type Demote (StageInfo Nat s) = StageInfo Word32 s

instance Known (StageInfo Nat 'Vertex) 'VertexInfo where
  known = VertexInfo
instance (KnownNat inputSize, KnownNat outputSize)
      => Known (StageInfo Nat 'TessellationControl) ('TessellationControlInfo inputSize outputSize) where
  known = TessellationControlInfo (knownValue @inputSize) (knownValue @outputSize)
instance KnownNat inputSize
      => Known (StageInfo Nat 'TessellationEvaluation) ('TessellationEvaluationInfo inputSize) where
  known = TessellationEvaluationInfo (knownValue @inputSize)
instance KnownNat inputSize
      => Known (StageInfo Nat 'Geometry) ('GeometryInfo inputSize) where
  known = GeometryInfo (knownValue @inputSize)
instance Known (StageInfo Nat 'Fragment) 'FragmentInfo where
  known = FragmentInfo
instance Known (StageInfo Nat 'GLCompute) 'GLComputeInfo where
  known = GLComputeInfo
instance Known (StageInfo Nat 'Kernel) 'KernelInfo where
  known = KernelInfo
