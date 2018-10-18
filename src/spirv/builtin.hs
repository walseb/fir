{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

module SPIRV.Builtin where

-- base
import Data.Word(Word32)

-- binary
import Data.Binary(Binary)

-- fir
import qualified SPIRV.Capability as Cap
import SPIRV.ExecutionMode

--------------------------------------------------

newtype Builtin = Builtin Word32
  deriving ( Eq, Binary )

instance Show Builtin where
  show builtin = "Builtin " ++ showBuiltin builtin


pattern Position :: Builtin
pattern Position = Builtin 0

pattern PointSize :: Builtin
pattern PointSize = Builtin 1

-- no 2

pattern ClipDistance :: Builtin
pattern ClipDistance = Builtin 3

pattern CullDistance :: Builtin
pattern CullDistance = Builtin 4

pattern VertexId :: Builtin
pattern VertexId = Builtin 5

pattern InstanceId :: Builtin
pattern InstanceId = Builtin 6

pattern PrimitiveId :: Builtin
pattern PrimitiveId = Builtin 7

pattern InvocationId :: Builtin
pattern InvocationId = Builtin 8

pattern Layer :: Builtin
pattern Layer = Builtin 9

pattern ViewportIndex :: Builtin
pattern ViewportIndex = Builtin 10

pattern TessLevelOuter :: Builtin
pattern TessLevelOuter = Builtin 11

pattern TessLevelInner :: Builtin
pattern TessLevelInner = Builtin 12

pattern TessCoord :: Builtin
pattern TessCoord = Builtin 13

pattern PatchVertices :: Builtin
pattern PatchVertices = Builtin 14

pattern FragCoord :: Builtin
pattern FragCoord = Builtin 15

pattern PointCoord :: Builtin
pattern PointCoord = Builtin 16

pattern FrontFacing :: Builtin
pattern FrontFacing = Builtin 17

pattern SampleId :: Builtin
pattern SampleId = Builtin 18

pattern SamplePosition :: Builtin
pattern SamplePosition = Builtin 19

pattern SampleMask :: Builtin
pattern SampleMask = Builtin 20

-- no 21

pattern FragDepth :: Builtin
pattern FragDepth = Builtin 22

pattern HelperInvocation :: Builtin
pattern HelperInvocation = Builtin 23

pattern NumWorkgroups :: Builtin
pattern NumWorkgroups = Builtin 24

pattern WorkgroupSize :: Builtin
pattern WorkgroupSize = Builtin 25

pattern WorkgroupId :: Builtin
pattern WorkgroupId = Builtin 26

pattern LocalInvocationId :: Builtin
pattern LocalInvocationId = Builtin 27

pattern GlobalInvocationId :: Builtin
pattern GlobalInvocationId = Builtin 28

pattern LocalInvocationIndex :: Builtin
pattern LocalInvocationIndex = Builtin 29

pattern WorkDim :: Builtin
pattern WorkDim = Builtin 30

pattern GlobalSize :: Builtin
pattern GlobalSize = Builtin 31

pattern EnqueuedWorkgroupSize :: Builtin
pattern EnqueuedWorkgroupSize = Builtin 32

pattern GlobalOffset :: Builtin
pattern GlobalOffset = Builtin 33

pattern GlobalLinearId :: Builtin
pattern GlobalLinearId = Builtin 34

-- no 35

pattern SubgroupSize :: Builtin
pattern SubgroupSize = Builtin 36

pattern SubgroupMaxSize :: Builtin
pattern SubgroupMaxSize = Builtin 37

pattern NumSubgroups :: Builtin
pattern NumSubgroups = Builtin 38

pattern NumEnqueuedSubgroups :: Builtin
pattern NumEnqueuedSubgroups = Builtin 39

pattern SubgroupId :: Builtin
pattern SubgroupId = Builtin 40

pattern SubgroupLocalInvocationId :: Builtin
pattern SubgroupLocalInvocationId = Builtin 41

pattern VertexIndex :: Builtin
pattern VertexIndex = Builtin 42

pattern InstanceIndex :: Builtin
pattern InstanceIndex = Builtin 43


showBuiltin :: Builtin -> String
showBuiltin Position = "Position"
showBuiltin PointSize = "PointSize"
showBuiltin ClipDistance = "ClipDistance"
showBuiltin CullDistance = "CullDistance"
showBuiltin VertexId = "VertexId"
showBuiltin InstanceId = "InstanceId"
showBuiltin PrimitiveId = "PrimitiveId"
showBuiltin InvocationId = "InvocationId"
showBuiltin Layer = "Layer"
showBuiltin ViewportIndex = "ViewportIndex"
showBuiltin TessLevelOuter = "TessLevelOuter"
showBuiltin TessLevelInner = "TessLevelInner"
showBuiltin TessCoord = "TessCoord"
showBuiltin PatchVertices = "PatchVertices"
showBuiltin FragCoord = "FragCoord"
showBuiltin PointCoord = "PointCoord"
showBuiltin FrontFacing = "FrontFacing"
showBuiltin SampleId = "SampleId"
showBuiltin SamplePosition = "SamplePosition"
showBuiltin SampleMask = "SampleMask"
showBuiltin FragDepth = "FragDepth"
showBuiltin HelperInvocation = "HelperInvocation"
showBuiltin NumWorkgroups = "NumWorkgroups"
showBuiltin WorkgroupSize = "WorkgroupSize"
showBuiltin WorkgroupId = "WorkgroupId"
showBuiltin LocalInvocationId = "LocalInvocationId"
showBuiltin GlobalInvocationId = "GlobalInvocationId"
showBuiltin LocalInvocationIndex = "LocalInvocationIndex"
showBuiltin WorkDim = "WorkDim"
showBuiltin GlobalSize = "GlobalSize"
showBuiltin EnqueuedWorkgroupSize = "EnqueuedWorkgroupSize"
showBuiltin GlobalOffset = "GlobalOffset"
showBuiltin GlobalLinearId = "GlobalLinearId"
showBuiltin SubgroupSize = "SubgroupSize"
showBuiltin SubgroupMaxSize = "SubgroupMaxSize"
showBuiltin NumSubgroups = "NumSubgroups"
showBuiltin NumEnqueuedSubgroups = "NumEnqueuedSubgroups"
showBuiltin SubgroupId = "SubgroupId"
showBuiltin SubgroupLocalInvocationId = "SubgroupLocalInvocationId"
showBuiltin VertexIndex = "VertexIndex"
showBuiltin InstanceIndex = "InstanceIndex"
showBuiltin (Builtin i) = show i


capabilityBuiltins :: Cap.Capability -> [ Builtin ]
capabilityBuiltins Cap.Shader            = [ Position, PointSize, VertexId, InstanceId
                                           , FragCoord, PointCoord, FrontFacing
                                           , SampleMask, FragDepth, HelperInvocation
                                           ]
capabilityBuiltins Cap.ClipDistance      = [ ClipDistance ]
capabilityBuiltins Cap.CullDistance      = [ CullDistance ]
capabilityBuiltins Cap.Geometry          = [ PrimitiveId, InvocationId, Layer ]
capabilityBuiltins Cap.Tessellation      = [ PrimitiveId, InvocationId
                                           , TessLevelOuter, TessLevelInner
                                           , TessCoord, PatchVertices
                                           ]
capabilityBuiltins Cap.MultiViewport     = [ ViewportIndex ]
capabilityBuiltins Cap.SampleRateShading = [ SampleId, SamplePosition ]
capabilityBuiltins _                     = [ ]

data InOut = Input | Output

modelBuiltins :: ExecutionModel -> InOut -> [ Builtin ]
modelBuiltins Vertex                 Input  = [ VertexId, InstanceId ]
modelBuiltins Vertex                 Output = [ Position, PointSize ]
modelBuiltins TessellationControl    Input  = [ InvocationId, PatchVertices, PrimitiveId ]
modelBuiltins TessellationControl    Output = [ Position, PointSize
                                              , TessLevelOuter, TessLevelInner
                                              ]
modelBuiltins TessellationEvaluation Input  = [ TessCoord, PatchVertices, PrimitiveId ]
modelBuiltins TessellationEvaluation Output = [ Position, PointSize ]
modelBuiltins Geometry               Input  = [ PrimitiveId, InvocationId ]
modelBuiltins Geometry               Output = [ Position, PointSize
                                              , Layer, ViewportIndex
                                              ]
modelBuiltins Fragment               Input  = [ Layer, ViewportIndex
                                              , FragCoord, PointCoord
                                              , FrontFacing
                                              , SampleId, SamplePosition
                                              , SampleMask
                                              ]
modelBuiltins Fragment               Output = [ SampleMask, FragDepth ]
modelBuiltins GLCompute              Input  = [ NumWorkgroups, WorkgroupSize
                                              , WorkgroupId
                                              , LocalInvocationId, GlobalInvocationId
                                              , LocalInvocationIndex
                                              ]
modelBuiltins GLCompute              Output = [ ]
modelBuiltins Kernel                 Input  = [ NumWorkgroups, WorkgroupSize
                                              , WorkgroupId
                                              , LocalInvocationId, GlobalInvocationId
                                              , LocalInvocationIndex
                                              , WorkDim, GlobalSize
                                              , EnqueuedWorkgroupSize
                                              , GlobalOffset, GlobalLinearId
                                              , SubgroupSize, SubgroupMaxSize
                                              , NumSubgroups, NumEnqueuedSubgroups
                                              , SubgroupId, SubgroupLocalInvocationId
                                              ]
modelBuiltins Kernel                 Output = [ ]
modelBuiltins _                      _      = [ ]

                                    
