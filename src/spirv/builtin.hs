{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeApplications    #-}

module SPIRV.Builtin where

-- base
import Data.Word(Word32)

-- fir
import Data.Binary.Class.Put(Put(..))
import SPIRV.Capability(Capability)
import qualified SPIRV.Capability as Capability

--------------------------------------------------

data Builtin
  = Position
  | PointSize
  --
  | ClipDistance
  | CullDistance
  | VertexId
  | InstanceId
  | PrimitiveId
  | InvocationId
  | Layer
  | ViewportIndex
  | TessLevelOuter
  | TessLevelInner
  | TessCoord
  | PatchVertices
  | FragCoord
  | PointCoord
  | FrontFacing
  | SampleId
  | SamplePosition
  | SampleMask
  --
  | FragDepth
  | HelperInvocation
  | NumWorkgroups
  | WorkgroupSize
  | WorkgroupId
  | LocalInvocationId
  | GlobalInvocationId
  | LocalInvocationIndex
  | WorkDim
  | GlobalSize
  | EnqueuedWorkgroupSize
  | GlobalOffset
  | GlobalLinearId
  --
  | SubgroupSize
  | SubgroupMaxSize
  | NumSubgroups
  | NumEnqueuedSubgroups
  | SubgroupId
  | SubgroupLocalInvocationId
  | VertexIndex
  | InstanceIndex
  deriving ( Show, Eq )

instance Put Builtin where
  sizeOf _ = 1

  put Position                  = put @Word32  0
  put PointSize                 = put @Word32  1
  --
  put ClipDistance              = put @Word32  3
  put CullDistance              = put @Word32  4
  put VertexId                  = put @Word32  5
  put InstanceId                = put @Word32  6
  put PrimitiveId               = put @Word32  7
  put InvocationId              = put @Word32  8
  put Layer                     = put @Word32  9
  put ViewportIndex             = put @Word32 10
  put TessLevelOuter            = put @Word32 11
  put TessLevelInner            = put @Word32 12
  put TessCoord                 = put @Word32 13
  put PatchVertices             = put @Word32 14
  put FragCoord                 = put @Word32 15
  put PointCoord                = put @Word32 16
  put FrontFacing               = put @Word32 17
  put SampleId                  = put @Word32 18
  put SamplePosition            = put @Word32 19
  put SampleMask                = put @Word32 20
  --
  put FragDepth                 = put @Word32 22
  put HelperInvocation          = put @Word32 23
  put NumWorkgroups             = put @Word32 24
  put WorkgroupSize             = put @Word32 25
  put WorkgroupId               = put @Word32 26
  put LocalInvocationId         = put @Word32 27
  put GlobalInvocationId        = put @Word32 28
  put LocalInvocationIndex      = put @Word32 29
  put WorkDim                   = put @Word32 30
  put GlobalSize                = put @Word32 31
  put EnqueuedWorkgroupSize     = put @Word32 32
  put GlobalOffset              = put @Word32 33
  put GlobalLinearId            = put @Word32 34
  --
  put SubgroupSize              = put @Word32 36
  put SubgroupMaxSize           = put @Word32 37
  put NumSubgroups              = put @Word32 38
  put NumEnqueuedSubgroups      = put @Word32 39
  put SubgroupId                = put @Word32 40
  put SubgroupLocalInvocationId = put @Word32 41
  put VertexIndex               = put @Word32 42
  put InstanceIndex             = put @Word32 43

class KnownBuiltin (builtin :: Builtin) where
  builtin :: Builtin

instance KnownBuiltin Position where
  builtin = Position
instance KnownBuiltin PointSize where
  builtin = PointSize
instance KnownBuiltin ClipDistance where
  builtin = ClipDistance
instance KnownBuiltin CullDistance where
  builtin = CullDistance
instance KnownBuiltin VertexId where
  builtin = VertexId
instance KnownBuiltin InstanceId where
  builtin = InstanceId
instance KnownBuiltin PrimitiveId where
  builtin = PrimitiveId
instance KnownBuiltin InvocationId where
  builtin = InvocationId
instance KnownBuiltin Layer where
  builtin = Layer
instance KnownBuiltin ViewportIndex where
  builtin = ViewportIndex
instance KnownBuiltin TessLevelOuter where
  builtin = TessLevelOuter
instance KnownBuiltin TessLevelInner where
  builtin = TessLevelInner
instance KnownBuiltin TessCoord where
  builtin = TessCoord
instance KnownBuiltin PatchVertices where
  builtin = PatchVertices
instance KnownBuiltin FragCoord where
  builtin = FragCoord
instance KnownBuiltin PointCoord where
  builtin = PointCoord
instance KnownBuiltin FrontFacing where
  builtin = FrontFacing
instance KnownBuiltin SampleId where
  builtin = SampleId
instance KnownBuiltin SamplePosition where
  builtin = SamplePosition
instance KnownBuiltin SampleMask where
  builtin = SampleMask
instance KnownBuiltin FragDepth where
  builtin = FragDepth
instance KnownBuiltin HelperInvocation where
  builtin = HelperInvocation
instance KnownBuiltin NumWorkgroups where
  builtin = NumWorkgroups
instance KnownBuiltin WorkgroupSize where
  builtin = WorkgroupSize
instance KnownBuiltin WorkgroupId where
  builtin = WorkgroupId
instance KnownBuiltin LocalInvocationId where
  builtin = LocalInvocationId
instance KnownBuiltin GlobalInvocationId where
  builtin = GlobalInvocationId
instance KnownBuiltin LocalInvocationIndex where
  builtin = LocalInvocationIndex
instance KnownBuiltin WorkDim where
  builtin = WorkDim
instance KnownBuiltin GlobalSize where
  builtin = GlobalSize
instance KnownBuiltin EnqueuedWorkgroupSize where
  builtin = EnqueuedWorkgroupSize
instance KnownBuiltin GlobalOffset where
  builtin = GlobalOffset
instance KnownBuiltin GlobalLinearId where
  builtin = GlobalLinearId
instance KnownBuiltin SubgroupSize where
  builtin = SubgroupSize
instance KnownBuiltin SubgroupMaxSize where
  builtin = SubgroupMaxSize
instance KnownBuiltin NumSubgroups where
  builtin = NumSubgroups
instance KnownBuiltin NumEnqueuedSubgroups where
  builtin = NumEnqueuedSubgroups
instance KnownBuiltin SubgroupId where
  builtin = SubgroupId
instance KnownBuiltin SubgroupLocalInvocationId where
  builtin = SubgroupLocalInvocationId
instance KnownBuiltin VertexIndex where
  builtin = VertexIndex
instance KnownBuiltin InstanceIndex where
  builtin = InstanceIndex


capabilityBuiltins :: Capability -> [ Builtin ]
capabilityBuiltins Capability.Shader            = [ Position, PointSize, VertexId, InstanceId
                                                  , FragCoord, PointCoord, FrontFacing
                                                  , SampleMask, FragDepth, HelperInvocation
                                                  ]
capabilityBuiltins Capability.ClipDistance      = [ ClipDistance ]
capabilityBuiltins Capability.CullDistance      = [ CullDistance ]
capabilityBuiltins Capability.Geometry          = [ PrimitiveId, InvocationId, Layer ]
capabilityBuiltins Capability.Tessellation      = [ PrimitiveId, InvocationId
                                                  , TessLevelOuter, TessLevelInner
                                                  , TessCoord, PatchVertices
                                                  ]
capabilityBuiltins Capability.MultiViewport     = [ ViewportIndex ]
capabilityBuiltins Capability.SampleRateShading = [ SampleId, SamplePosition ]
capabilityBuiltins _                            = [ ]