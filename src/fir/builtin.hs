{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module FIR.Builtin where

-- base
import Data.Int(Int32)
import Data.Proxy(Proxy)
import Data.Word(Word32)
import GHC.TypeLits(Symbol)

-- fir
import Data.Type.Bindings( BindingsMap, type (:->)
                         , FromList, Union
                         , Var, R, W --, RW
                         )
import Math.Linear(V)
import SPIRV.ExecutionMode(ExecutionMode(ExecutionMode))



data Stage
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | GLCompute
  | Kernel
  deriving ( Show, Eq, Ord, Enum, Bounded )


type family GetAllBuiltins (entryPoints :: [( Symbol, Stage )]) :: BindingsMap where
  GetAllBuiltins '[]                    = '[ ]
  GetAllBuiltins ( '( _, stage) ': ps ) = Union ( StageBuiltins stage ) ( GetAllBuiltins ps )

class KnownStage (s :: Stage) where
  stageVal :: Proxy s -> Stage

instance KnownStage Vertex where
  stageVal _ = Vertex
instance KnownStage TessellationControl where
  stageVal _ = TessellationControl
instance KnownStage TessellationEvaluation where
  stageVal _ = TessellationEvaluation
instance KnownStage Geometry where
  stageVal _ = Geometry
instance KnownStage Fragment where
  stageVal _ = Fragment
instance KnownStage GLCompute where
  stageVal _ = GLCompute
instance KnownStage Kernel where
  stageVal _ = Kernel

executionMode :: Stage -> ExecutionMode
executionMode = ExecutionMode . fromIntegral . fromEnum

stage :: ExecutionMode -> Stage
stage (ExecutionMode i) = toEnum ( fromIntegral i )

type family StageBuiltins (stage :: Stage) :: BindingsMap where
  StageBuiltins stage = FromList ( StageBuiltins' stage )

type family StageBuiltins' (stage :: Stage) :: BindingsMap where
  StageBuiltins' Vertex                 = '[ "gl_VertexId"       :-> Var R Int32
                                           , "gl_InstanceId"     :-> Var R Int32
                                           , "gl_Position"       :-> Var W (V 4 Float)
                                           , "gl_PointSize"      :-> Var W Float
                                           ]
  StageBuiltins' TessellationControl    = '[ "gl_InvocationId"   :-> Var R Int32
                                           , "gl_PatchVertices"  :-> Var R Int32
                                           , "gl_PrimitiveId"    :-> Var R Int32
                                        -- , "gl_perVertex"      :-> Var RW (Array (Struct "gl_Position" "gl_PointSize"))
                                        -- , "gl_TessLevelOuter" :-> Var W (Array Float)
                                        -- , "gl_TessLevelInner" :-> Var W (Array Float)
                                           ]
  StageBuiltins' TessellationEvaluation = '[ "gl_TessCoord"      :-> Var R (V 3 Float)
                                           , "gl_PatchVertices"  :-> Var R Int32
                                           , "gl_PrimitiveId"    :-> Var R Int32
                                        -- , "gl_perVertex"      :-> Var R (Array (Struct "gl_Position" "gl_PointSize"))
                                           , "gl_Position"       :-> Var W (V 4 Float)
                                           , "gl_PointSize"      :-> Var W Float
                                           ]
  StageBuiltins' Geometry               = '[ "gl_PrimitiveId"    :-> Var R Int32
                                           , "gl_InvocationId"   :-> Var R Int32
                                        -- , "gl_perVertex"      :-> Var R (Array (Struct "gl_Position" "gl_PointSize"))
                                           , "gl_Position"       :-> Var W (V 4 Float)
                                           , "gl_PointSize"      :-> Var W Float
                                           , "gl_Layer"          :-> Var W Int32
                                           , "gl_ViewportIndex"  :-> Var W Int32
                                           ]                                        
  StageBuiltins' Fragment               = '[ "gl_Layer"          :-> Var R Int32
                                           , "gl_ViewportIndex"  :-> Var R Int32
                                           , "gl_FragCoord"      :-> Var R (V 4 Float)
                                           , "gl_PointCoord"     :-> Var R (V 2 Float)
                                           , "gl_FrontFacing"    :-> Var R Bool
                                           , "gl_SampleId"       :-> Var R Int32
                                           , "gl_SamplePosition" :-> Var R (V 2 Float)
                                        -- , "gl_SampleMask"     :-> Var W (Array Int32)
                                           , "gl_FragDepth"      :-> Var W Float
                                           ]
  StageBuiltins' GLCompute              = '[ "gl_NumWorkgroups"        :-> Var R (V 3 Word32)
                                           , "gl_WorkgroupSize"        :-> Var R (V 3 Word32)
                                           , "gl_WorkgroupId"          :-> Var R (V 3 Word32)
                                           , "gl_LocalInvocationId"    :-> Var R (V 3 Word32)
                                           , "gl_GlobalInvocationId"   :-> Var R (V 3 Word32)
                                           , "gl_LocalInvocationIndex" :-> Var R Word32
                                           ]
  StageBuiltins' Kernel                 = '[ "gl_NumWorkgroups"             :-> Var R Word32
                                           , "gl_WorkgroupSize"             :-> Var R Word32
                                           , "gl_WorkgroupId"               :-> Var R Word32
                                           , "gl_LocalInvocationId"         :-> Var R Word32
                                           , "gl_GlobalInvocationId"        :-> Var R Word32
                                           , "gl_LocalInvocationIndex"      :-> Var R Word32
                                           , "gl_WorkDim"                   :-> Var R Word32
                                           , "gl_GlobalSize"                :-> Var R Word32
                                           , "gl_EnqueuedWorkgroupSize"     :-> Var R Word32
                                           , "gl_GlobalOffset"              :-> Var R Word32
                                           , "gl_GlobalLinearId"            :-> Var R Word32
                                           , "gl_SubgroupSize"              :-> Var R Word32
                                           , "gl_SubgroupMaxSize"           :-> Var R Word32
                                           , "gl_NumSubgroups"              :-> Var R Word32
                                           , "gl_NumEnqueuedSubgroups"      :-> Var R Word32
                                           , "gl_SubgroupId"                :-> Var R Word32
                                           , "gl_SubgroupLocalInvocationId" :-> Var R Word32
                                           ]
  StageBuiltins' _                      = '[ ]