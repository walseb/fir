{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module SPIRV.Builtin
  ( Builtin(..)
  , readBuiltin
  )
  where

-- base
import Data.Word
  ( Word32 )

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known) )

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
  deriving ( Show, Eq, Ord )

readBuiltin :: Text -> Maybe Builtin
readBuiltin "gl_Position"                  = Just Position
readBuiltin "gl_PointSize"                 = Just PointSize
readBuiltin "gl_ClipDistance"              = Just ClipDistance
readBuiltin "gl_CullDistance"              = Just CullDistance
readBuiltin "gl_VertexID"                  = Just VertexId
readBuiltin "gl_InstanceID"                = Just InstanceId
readBuiltin "gl_PrimitiveID"               = Just PrimitiveId
readBuiltin "gl_InvocationID"              = Just InvocationId
readBuiltin "gl_Layer"                     = Just Layer
readBuiltin "gl_ViewportIndex"             = Just ViewportIndex
readBuiltin "gl_TessLevelOuter"            = Just TessLevelOuter
readBuiltin "gl_TessLevelInner"            = Just TessLevelInner
readBuiltin "gl_TessCoord"                 = Just TessCoord
readBuiltin "gl_PatchVertices"             = Just PatchVertices
readBuiltin "gl_FragCoord"                 = Just FragCoord
readBuiltin "gl_PointCoord"                = Just PointCoord
readBuiltin "gl_FrontFacing"               = Just FrontFacing
readBuiltin "gl_SampleID"                  = Just SampleId
readBuiltin "gl_SamplePosition"            = Just SamplePosition
readBuiltin "gl_SampleMask"                = Just SampleMask
readBuiltin "gl_FragDepth"                 = Just FragDepth
readBuiltin "gl_HelperInvocation"          = Just HelperInvocation
readBuiltin "gl_NumWorkgroups"             = Just NumWorkgroups
readBuiltin "gl_WorkgroupSize"             = Just WorkgroupSize
readBuiltin "gl_WorkgroupID"               = Just WorkgroupId
readBuiltin "gl_LocalInvocationID"         = Just LocalInvocationId
readBuiltin "gl_GlobalInvocationID"        = Just GlobalInvocationId
readBuiltin "gl_LocalInvocationIndex"      = Just LocalInvocationIndex
readBuiltin "gl_WorkDim"                   = Just WorkDim
readBuiltin "gl_GlobalSize"                = Just GlobalSize
readBuiltin "gl_EnqueuedWorkgroupSize"     = Just EnqueuedWorkgroupSize
readBuiltin "gl_GlobalOffset"              = Just GlobalOffset
readBuiltin "gl_GlobalLinearID"            = Just GlobalLinearId
readBuiltin "gl_SubgroupSize"              = Just SubgroupSize
readBuiltin "gl_SubgroupMaxSize"           = Just SubgroupMaxSize
readBuiltin "gl_NumSubgroups"              = Just NumSubgroups
readBuiltin "gl_NumEnqueuedSubgroups"      = Just NumEnqueuedSubgroups
readBuiltin "gl_SubgroupID"                = Just SubgroupId
readBuiltin "gl_SubgroupLocalInvocationID" = Just SubgroupLocalInvocationId
readBuiltin "gl_VertexIndex"               = Just VertexIndex
readBuiltin "gl_InstanceIndex"             = Just InstanceIndex
readBuiltin _ = Nothing

instance Put Builtin where
  wordCount _ = 1

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

instance Demotable Builtin where
  type Demote Builtin = Builtin
instance Known Builtin Position where
  known = Position
instance Known Builtin PointSize where
  known = PointSize
instance Known Builtin ClipDistance where
  known = ClipDistance
instance Known Builtin CullDistance where
  known = CullDistance
instance Known Builtin VertexId where
  known = VertexId
instance Known Builtin InstanceId where
  known = InstanceId
instance Known Builtin PrimitiveId where
  known = PrimitiveId
instance Known Builtin InvocationId where
  known = InvocationId
instance Known Builtin Layer where
  known = Layer
instance Known Builtin ViewportIndex where
  known = ViewportIndex
instance Known Builtin TessLevelOuter where
  known = TessLevelOuter
instance Known Builtin TessLevelInner where
  known = TessLevelInner
instance Known Builtin TessCoord where
  known = TessCoord
instance Known Builtin PatchVertices where
  known = PatchVertices
instance Known Builtin FragCoord where
  known = FragCoord
instance Known Builtin PointCoord where
  known = PointCoord
instance Known Builtin FrontFacing where
  known = FrontFacing
instance Known Builtin SampleId where
  known = SampleId
instance Known Builtin SamplePosition where
  known = SamplePosition
instance Known Builtin SampleMask where
  known = SampleMask
instance Known Builtin FragDepth where
  known = FragDepth
instance Known Builtin HelperInvocation where
  known = HelperInvocation
instance Known Builtin NumWorkgroups where
  known = NumWorkgroups
instance Known Builtin WorkgroupSize where
  known = WorkgroupSize
instance Known Builtin WorkgroupId where
  known = WorkgroupId
instance Known Builtin LocalInvocationId where
  known = LocalInvocationId
instance Known Builtin GlobalInvocationId where
  known = GlobalInvocationId
instance Known Builtin LocalInvocationIndex where
  known = LocalInvocationIndex
instance Known Builtin WorkDim where
  known = WorkDim
instance Known Builtin GlobalSize where
  known = GlobalSize
instance Known Builtin EnqueuedWorkgroupSize where
  known = EnqueuedWorkgroupSize
instance Known Builtin GlobalOffset where
  known = GlobalOffset
instance Known Builtin GlobalLinearId where
  known = GlobalLinearId
instance Known Builtin SubgroupSize where
  known = SubgroupSize
instance Known Builtin SubgroupMaxSize where
  known = SubgroupMaxSize
instance Known Builtin NumSubgroups where
  known = NumSubgroups
instance Known Builtin NumEnqueuedSubgroups where
  known = NumEnqueuedSubgroups
instance Known Builtin SubgroupId where
  known = SubgroupId
instance Known Builtin SubgroupLocalInvocationId where
  known = SubgroupLocalInvocationId
instance Known Builtin VertexIndex where
  known = VertexIndex
instance Known Builtin InstanceIndex where
  known = InstanceIndex
