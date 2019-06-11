{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module SPIRV.Stage where

-- base
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol, AppendSymbol )
import GHC.TypeNats
  ( Nat, KnownNat, CmpNat )

-- fir
import Data.Binary.Class.Put
  ( Put(..), PutWord32Enum(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.Ord
  ( POrd(Compare) )
import SPIRV.Capability
  ( Capability )
import qualified SPIRV.Capability as Capability

-------------------------------------------------------
-- execution models / stages

data ExecutionModel
  = Stage Stage
  | Kernel -- OpenCL compute kernel
  deriving ( Show, Eq, Ord )

data Stage
  = ShaderStage Shader
  | MeshStage   MeshShader
  | RayStage    RayShader
  deriving ( Show, Eq, Ord )

data Shader
  = VertexShader
  | TessellationControlShader
  | TessellationEvaluationShader
  | GeometryShader
  | FragmentShader
  | ComputeShader -- GLCompute in SPIR-V
  deriving ( Show, Eq, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum Shader)

data MeshShader
  = TaskShader
  | MeshShader
  deriving ( Show, Eq, Ord, Enum, Bounded )

instance Put MeshShader where
  put = put @Word32 . (+5267) . fromIntegral . fromEnum
  wordCount _ = 1

data RayShader
  = RayGenerationShader
  | IntersectionShader
  | AnyHitShader
  | ClosestHitShader
  | MissShader
  | CallableShader
  deriving ( Show, Eq, Ord, Enum, Bounded )

instance Put RayShader where
  put = put @Word32 . (+5313) . fromIntegral . fromEnum
  wordCount _ = 1

instance Put Stage where
  put (ShaderStage s) = put s
  put (MeshStage   s) = put s
  put (RayStage    s) = put s
  wordCount _ = 1

instance Put ExecutionModel where
  put (Stage s) = put s
  put Kernel = put @Word32 6
  wordCount _ = 1

stageID :: Stage -> Word32
stageID (ShaderStage s) = fromIntegral ( fromEnum s )
stageID (MeshStage   s) = fromIntegral ( fromEnum s + 5267 )
stageID (RayStage    s) = fromIntegral ( fromEnum s + 5313 )

executionModelID :: ExecutionModel -> Word32
executionModelID (Stage s) = stageID s
executionModelID Kernel    = 6

shaderCapabilities :: Shader -> [ Capability ]
shaderCapabilities VertexShader                 = [ Capability.Shader       ]
shaderCapabilities TessellationControlShader    = [ Capability.Tessellation ] --- |
shaderCapabilities TessellationEvaluationShader = [ Capability.Tessellation ] --- |-- shader capability implied
shaderCapabilities GeometryShader               = [ Capability.Geometry     ] --- |
shaderCapabilities FragmentShader               = [ Capability.Shader       ]
shaderCapabilities ComputeShader                = [ Capability.Shader       ]

stageCapabilities :: Stage -> [ Capability ]
stageCapabilities (ShaderStage s) = shaderCapabilities s
stageCapabilities (MeshStage   _) = [ Capability.MeshShadingNV ]
stageCapabilities (RayStage    _) = [ Capability.RayTracingNV  ]

executionModelCapabilities :: ExecutionModel -> [ Capability ]
executionModelCapabilities (Stage s) = stageCapabilities s
executionModelCapabilities Kernel    = [ Capability.Kernel ]



type Vertex                 = 'Stage ('ShaderStage 'VertexShader)
type TessellationControl    = 'Stage ('ShaderStage 'TessellationControlShader)
type TessellationEvaluation = 'Stage ('ShaderStage 'TessellationEvaluationShader)
type Geometry               = 'Stage ('ShaderStage 'GeometryShader)
type Fragment               = 'Stage ('ShaderStage 'FragmentShader)
type Compute                = 'Stage ('ShaderStage 'ComputeShader)

type Task = 'Stage ('MeshStage 'TaskShader)
type Mesh = 'Stage ('MeshStage 'MeshShader)

type RayGeneration = 'Stage ('RayStage 'RayGenerationShader)
type Intersection  = 'Stage ('RayStage 'IntersectionShader)
type AnyHit        = 'Stage ('RayStage 'AnyHitShader)
type ClosestHit    = 'Stage ('RayStage 'ClosestHitShader)
type Miss          = 'Stage ('RayStage 'MissShader)
type Callable      = 'Stage ('RayStage 'CallableShader)


instance Demotable Shader where
  type Demote Shader = Shader

instance Known Shader VertexShader where
  known = VertexShader
instance Known Shader TessellationControlShader where
  known = TessellationControlShader
instance Known Shader TessellationEvaluationShader where
  known = TessellationEvaluationShader
instance Known Shader GeometryShader where
  known = GeometryShader
instance Known Shader FragmentShader where
  known = FragmentShader
instance Known Shader ComputeShader where
  known = ComputeShader

instance Demotable MeshShader where
  type Demote MeshShader = MeshShader

instance Known MeshShader 'TaskShader where
  known = TaskShader
instance Known MeshShader 'MeshShader where
  known = MeshShader

instance Demotable RayShader where
  type Demote RayShader = RayShader

instance Known RayShader RayGenerationShader where
  known = RayGenerationShader
instance Known RayShader IntersectionShader where
  known = IntersectionShader
instance Known RayShader AnyHitShader where
  known = AnyHitShader
instance Known RayShader ClosestHitShader where
  known = ClosestHitShader
instance Known RayShader MissShader where
  known = MissShader
instance Known RayShader CallableShader where
  known = CallableShader

instance Demotable Stage where
  type Demote Stage = Stage

instance Known Shader shader => Known Stage (ShaderStage shader) where
  known = ShaderStage ( knownValue @shader )
instance Known MeshShader meshShader => Known Stage (MeshStage meshShader) where
  known = MeshStage ( knownValue @meshShader )
instance Known RayShader rayShader => Known Stage (RayStage rayShader) where
  known = RayStage ( knownValue @rayShader )

instance Demotable ExecutionModel where
  type Demote ExecutionModel = ExecutionModel

instance Known Stage stage => Known ExecutionModel ('Stage stage) where
  known = Stage ( knownValue @stage )
instance Known ExecutionModel Kernel where
  known = Kernel


type family FromEnumShader (s :: Shader) :: Nat where
  FromEnumShader VertexShader                 = 0
  FromEnumShader TessellationControlShader    = 1
  FromEnumShader TessellationEvaluationShader = 2
  FromEnumShader GeometryShader               = 3
  FromEnumShader FragmentShader               = 4
  FromEnumShader ComputeShader                = 5

type family FromEnumMeshShader (s :: MeshShader) :: Nat where
  FromEnumMeshShader 'TaskShader = 5267
  FromEnumMeshShader 'MeshShader = 5268

type family FromEnumRayShader (s :: RayShader) :: Nat where
  FromEnumRayShader RayGenerationShader = 5313
  FromEnumRayShader IntersectionShader  = 5314
  FromEnumRayShader AnyHitShader        = 5315
  FromEnumRayShader ClosestHitShader    = 5316
  FromEnumRayShader MissShader          = 5317
  FromEnumRayShader CallableShader      = 5318

type family FromEnumStage (s :: Stage) :: Nat where
  FromEnumStage (ShaderStage s) = FromEnumShader     s
  FromEnumStage (MeshStage   s) = FromEnumMeshShader s
  FromEnumStage (RayStage    s) = FromEnumRayShader  s

type family FromEnumExecutionModel (em :: ExecutionModel) :: Nat where
  FromEnumExecutionModel ('Stage s) = FromEnumStage s
  FromEnumExecutionModel Kernel     = 6

instance POrd ExecutionModel where
  type Compare s1 s2 = FromEnumExecutionModel s1 `CmpNat` FromEnumExecutionModel s2

type family ShaderName (s :: Shader) :: Symbol where
  ShaderName VertexShader                 = "Vertex shader"
  ShaderName TessellationControlShader    = "Tessellation control shader"
  ShaderName TessellationEvaluationShader = "Tessellation evaluation shader"
  ShaderName GeometryShader = "Geometry shader"
  ShaderName FragmentShader = "Fragment shader"
  ShaderName ComputeShader  = "Compute shader"

type family StageName (s :: Stage) :: Symbol where
  StageName (ShaderStage s)                = ShaderName s
  StageName (MeshStage 'TaskShader)        = "Task shader"
  StageName (MeshStage 'MeshShader)        = "Mesh shader"
  StageName (RayStage RayGenerationShader) = "Ray generation shader"
  StageName (RayStage IntersectionShader)  = "Ray intersection shader"
  StageName (RayStage AnyHitShader)        = "Any hit ray shader"
  StageName (RayStage ClosestHitShader)    = "Closest hit ray shader"
  StageName (RayStage MissShader)          = "Ray miss shader"
  StageName (RayStage CallableShader)      = "Ray callable shader"

type family ExecutionModelName (em :: ExecutionModel) :: Symbol where
  ExecutionModelName ('Stage s) = StageName s
  ExecutionModelName Kernel     = "Compute kernel"

type family NamedShader (k :: Symbol) (s :: Shader) :: Symbol where
  NamedShader k s = ShaderName s `AppendSymbol` "named \"" `AppendSymbol` k `AppendSymbol` "\""

type family NamedStage (k :: Symbol) (s :: Stage) :: Symbol where
  NamedStage k s = StageName s `AppendSymbol` "named \"" `AppendSymbol` k `AppendSymbol` "\""

type family NamedExecutionModel (k :: Symbol) (em :: ExecutionModel) :: Symbol where
  NamedExecutionModel k em = ExecutionModelName em `AppendSymbol` "named \"" `AppendSymbol` k `AppendSymbol` "\""

--------------------------------------------------------------------------
-- additional execution model information that needs to be known at the type-level

data TessellationMode
  = ModeTriangles
  | ModeQuads
  | ModeIsolines
  | ModePoints
  deriving (Show, Eq, Ord, Enum, Bounded)

type family ShowTessellationMode (mode :: TessellationMode) :: Symbol where
  ShowTessellationMode ModeTriangles = "Triangles"
  ShowTessellationMode ModeQuads     = "Quads"
  ShowTessellationMode ModeIsolines  = "Isolines"
  ShowTessellationMode ModePoints    = "Points"

data GeometryInputMode
  = InputModePoints
  | InputModeLines
  | InputModeLinesAdjacency
  | InputModeTriangles
  | InputModeTrianglesAdjacency
  deriving (Show, Eq, Ord, Enum, Bounded)

type family ShowGeometryInputMode (mode :: GeometryInputMode) :: Symbol where
  ShowGeometryInputMode InputModePoints
    = "InputPoints"
  ShowGeometryInputMode InputModeLines
    = "InputLines"
  ShowGeometryInputMode InputModeLinesAdjacency
    = "InputLinesAdjacency"
  ShowGeometryInputMode InputModeTriangles
    = "(Input)Triangles"
  ShowGeometryInputMode InputModeTrianglesAdjacency
    = "InputTrianglesAdjacency"

instance Demotable TessellationMode where
  type Demote TessellationMode = TessellationMode

instance Demotable GeometryInputMode where
  type Demote GeometryInputMode = GeometryInputMode

instance Known TessellationMode 'ModeTriangles where
  known = ModeTriangles
instance Known TessellationMode 'ModeQuads     where
  known = ModeQuads
instance Known TessellationMode 'ModeIsolines  where
  known = ModeIsolines
instance Known TessellationMode 'ModePoints    where
  known = ModePoints

instance Known GeometryInputMode 'InputModePoints             where
  known = InputModePoints
instance Known GeometryInputMode 'InputModeLines              where
  known = InputModeLines
instance Known GeometryInputMode 'InputModeLinesAdjacency     where
  known = InputModeLinesAdjacency
instance Known GeometryInputMode 'InputModeTriangles          where
  known = InputModeTriangles
instance Known GeometryInputMode 'InputModeTrianglesAdjacency where
  known = InputModeTrianglesAdjacency

data ShaderInfo (n :: Type) (s :: Shader) where
  VertexShaderInfo
    :: ShaderInfo n VertexShader
  TessellationControlShaderInfo
    :: n -- input size
    -> n -- output size
    -> Maybe TessellationMode
    -> ShaderInfo n TessellationControlShader
  TessellationEvaluationShaderInfo
    :: n -- input size
    -> TessellationMode
    -> ShaderInfo n TessellationEvaluationShader
  GeometryShaderInfo
    :: n -- input size
    -> GeometryInputMode
    -> ShaderInfo n GeometryShader
  FragmentShaderInfo
    :: ShaderInfo n FragmentShader
  ComputeShaderInfo
    :: ShaderInfo n ComputeShader

deriving instance Show n => Show (ShaderInfo n s)
deriving instance Eq   n => Eq   (ShaderInfo n s)
deriving instance Ord  n => Ord  (ShaderInfo n s)

data ExecutionInfo (n :: Type) (em :: ExecutionModel) where
  ShaderExecutionInfo :: ShaderInfo n s -> ExecutionInfo n ('Stage ('ShaderStage s))
  -- other infos not yet implemented

type VertexInfo = 'ShaderExecutionInfo 'VertexShaderInfo
type TessellationControlInfo i j m = 'ShaderExecutionInfo ( 'TessellationControlShaderInfo i j m)
type TessellationEvaluationInfo i m = 'ShaderExecutionInfo ( 'TessellationEvaluationShaderInfo i m)
type GeometryInfo i m = 'ShaderExecutionInfo ('GeometryShaderInfo i m)
type FragmentInfo = 'ShaderExecutionInfo 'FragmentShaderInfo
type ComputeInfo  = 'ShaderExecutionInfo 'ComputeShaderInfo

deriving instance Show n => Show (ExecutionInfo n s)
deriving instance Eq   n => Eq   (ExecutionInfo n s)
deriving instance Ord  n => Ord  (ExecutionInfo n s)

modelOf :: ExecutionInfo n em -> ExecutionModel
modelOf (ShaderExecutionInfo VertexShaderInfo)
  = Stage (ShaderStage VertexShader)
modelOf (ShaderExecutionInfo (TessellationControlShaderInfo {}))
  = Stage (ShaderStage TessellationControlShader)
modelOf (ShaderExecutionInfo (TessellationEvaluationShaderInfo {}))
  = Stage (ShaderStage TessellationEvaluationShader)
modelOf (ShaderExecutionInfo (GeometryShaderInfo {}))
  = Stage (ShaderStage GeometryShader)
modelOf (ShaderExecutionInfo FragmentShaderInfo)
  = Stage (ShaderStage FragmentShader)
modelOf (ShaderExecutionInfo ComputeShaderInfo)
  = Stage (ShaderStage ComputeShader)


instance Demotable (ShaderInfo Nat s) where
  type Demote (ShaderInfo Nat s) = ShaderInfo Word32 s

instance Known (ShaderInfo Nat VertexShader) VertexShaderInfo where
  known = VertexShaderInfo
instance ( KnownNat inputSize
         , KnownNat outputSize
         , Known (Maybe TessellationMode) mbMode
         )
      => Known
            (ShaderInfo Nat TessellationControlShader)
            (TessellationControlShaderInfo inputSize outputSize mbMode) where
  known = TessellationControlShaderInfo
              ( knownValue @inputSize  )
              ( knownValue @outputSize )
              ( knownValue @mbMode     )
instance (KnownNat inputSize, Known TessellationMode mode)
      => Known
            (ShaderInfo Nat TessellationEvaluationShader)
            (TessellationEvaluationShaderInfo inputSize mode) where
  known = TessellationEvaluationShaderInfo
              ( knownValue @inputSize )
              ( knownValue @mode      )
instance (KnownNat inputSize, Known GeometryInputMode mode)
      => Known (ShaderInfo Nat GeometryShader)
            (GeometryShaderInfo inputSize mode) where
  known = GeometryShaderInfo
             ( knownValue @inputSize )
             ( knownValue @mode      )
instance Known (ShaderInfo Nat FragmentShader) FragmentShaderInfo where
  known = FragmentShaderInfo
instance Known (ShaderInfo Nat ComputeShader) ComputeShaderInfo where
  known = ComputeShaderInfo

instance Demotable (ExecutionInfo Nat s) where
  type Demote (ExecutionInfo Nat s) = ExecutionInfo Word32 s

instance Known (ShaderInfo Nat s) info
      => Known (ExecutionInfo Nat ('Stage ('ShaderStage s)))
           (ShaderExecutionInfo info)
      where
  known = ShaderExecutionInfo ( knownValue @info )
