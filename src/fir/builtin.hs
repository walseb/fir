{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module FIR.Builtin where

-- base
import Control.Arrow
  ( second )
import Data.Int
  ( Int32 )
import Data.Maybe
  ( maybe )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat )

-- containers
import Data.Set
  ( Set )
import qualified Data.Set as Set

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import Data.Type.Map
  ( (:->)((:->))
  , InsertionSort
  )
import FIR.Binding
  ( Binding, BindingsMap, Var, R, W )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Singletons
  ( KnownInterface(knownInterface) )
import FIR.Prim.Struct
  ( Struct )
import Math.Linear
  ( V )
import qualified SPIRV.Builtin       as SPIRV
  ( Builtin(TessLevelInner, TessLevelOuter)
  , readBuiltin
  )
import qualified SPIRV.Decoration    as SPIRV
  ( Decoration(Block, Builtin, Patch) )
import           SPIRV.ExecutionMode as SPIRV
  ( ExecutionMode(..)
  , StageExecutionInfo(..), ValidateExecutionModes
  )
import qualified SPIRV.PrimTy        as SPIRV
  ( PrimTy, PointerTy, pattern PointerTy )
import qualified SPIRV.Storage       as SPIRV
  ( StorageClass )
import SPIRV.Stage
  ( Stage(..) )

--------------------------------------------------------------------------

type StageBuiltins (k :: Symbol) (stage :: Stage) (modes :: [ExecutionMode Nat])
  = ( InsertionSort ( StageBuiltins' (ValidateExecutionModes k stage modes) ) :: BindingsMap )

type family StageBuiltins' (info :: StageExecutionInfo stage) :: [ Symbol :-> Binding ] where
  StageBuiltins' VertexInfo
    = '[ "gl_VertexId"       ':-> Var R Int32
       , "gl_InstanceId"     ':-> Var R Int32
       , "gl_Position"       ':-> Var W ( V 4 Float )
       , "gl_PointSize"      ':-> Var W Float
       ]
  StageBuiltins' (TessellationControlInfo inputSize outputSize)
    = '[ "gl_InvocationId"   ':-> Var R Int32
       , "gl_PatchVertices"  ':-> Var R Int32
       , "gl_PrimitiveId"    ':-> Var R Int32
       , "gl_in"
            ':-> Var R
                  ( Array inputSize
                    ( Struct '[ "gl_Position"  ':-> V 4 Float
                              , "gl_PointSize" ':-> Float
                              ]
                    )
                  )
       , "gl_out"
            ':-> Var W
                  ( Array outputSize
                    ( Struct '[ "gl_Position"  ':-> V 4 Float
                              , "gl_PointSize" ':-> Float
                              ]
                    )
                  )
       , "gl_TessLevelOuter" ':-> Var W ( Array 4 Float )
       , "gl_TessLevelInner" ':-> Var W ( Array 2 Float )
       ]
  StageBuiltins' (TessellationEvaluationInfo inputSize)
    = '[ "gl_TessCoord"      ':-> Var R ( V 3 Float )
       , "gl_PatchVertices"  ':-> Var R Int32
       , "gl_PrimitiveId"    ':-> Var R Int32
       , "gl_PerVertex"
           ':-> Var R
                  ( Array inputSize
                    ( Struct '[ "gl_Position"  ':-> V 4 Float
                              , "gl_PointSize" ':-> Float
                              ]
                    )
                  )
       , "gl_Position"       ':-> Var W ( V 4 Float )
       , "gl_PointSize"      ':-> Var W Float
       ]
  StageBuiltins' (GeometryInfo inputSize)
    = '[ "gl_PrimitiveId"    ':-> Var R Int32
       , "gl_InvocationId"   ':-> Var R Int32
       , "gl_PerVertex"
           ':-> Var R
                  ( Array inputSize
                    ( Struct '[ "gl_Position"  ':-> V 4 Float
                              , "gl_PointSize" ':-> Float
                              ]
                    )
                  )
       , "gl_Position"       ':-> Var W ( V 4 Float )
       , "gl_PointSize"      ':-> Var W Float
       , "gl_Layer"          ':-> Var W Int32
       , "gl_ViewportIndex"  ':-> Var W Int32
       ]
  StageBuiltins' FragmentInfo
    = '[ "gl_Layer"          ':-> Var R Int32
       , "gl_ViewportIndex"  ':-> Var R Int32
       , "gl_FragCoord"      ':-> Var R ( V 4 Float )
       , "gl_PointCoord"     ':-> Var R ( V 2 Float )
       , "gl_FrontFacing"    ':-> Var R Bool
       , "gl_SampleId"       ':-> Var R Int32
       , "gl_SamplePosition" ':-> Var R ( V 2 Float )
       , "gl_FragDepth"      ':-> Var W Float
       ]
  StageBuiltins' GLComputeInfo
    = '[ "gl_NumWorkgroups"        ':-> Var R ( V 3 Word32 )
       , "gl_WorkgroupSize"        ':-> Var R ( V 3 Word32 )
       , "gl_WorkgroupId"          ':-> Var R ( V 3 Word32 )
       , "gl_LocalInvocationId"    ':-> Var R ( V 3 Word32 )
       , "gl_GlobalInvocationId"   ':-> Var R ( V 3 Word32 )
       , "gl_LocalInvocationIndex" ':-> Var R Word32
       ]
  StageBuiltins' KernelInfo
    = '[ "gl_NumWorkgroups"             ':-> Var R Word32
       , "gl_WorkgroupSize"             ':-> Var R Word32
       , "gl_WorkgroupId"               ':-> Var R Word32
       , "gl_LocalInvocationId"         ':-> Var R Word32
       , "gl_GlobalInvocationId"        ':-> Var R Word32
       , "gl_LocalInvocationIndex"      ':-> Var R Word32
       , "gl_WorkDim"                   ':-> Var R Word32
       , "gl_GlobalSize"                ':-> Var R Word32
       , "gl_EnqueuedWorkgroupSize"     ':-> Var R Word32
       , "gl_GlobalOffset"              ':-> Var R Word32
       , "gl_GlobalLinearId"            ':-> Var R Word32
       , "gl_SubgroupSize"              ':-> Var R Word32
       , "gl_SubgroupMaxSize"           ':-> Var R Word32
       , "gl_NumSubgroups"              ':-> Var R Word32
       , "gl_NumEnqueuedSubgroups"      ':-> Var R Word32
       , "gl_SubgroupId"                ':-> Var R Word32
       , "gl_SubgroupLocalInvocationId" ':-> Var R Word32
       ]

stageBuiltins :: Stage -> [ SPIRV.ExecutionMode Word32 ] -> [ (Text, SPIRV.PointerTy) ]
stageBuiltins Vertex _
  = builtinPointer $ knownInterface @(StageBuiltins' VertexInfo)
stageBuiltins TessellationControl _
  = builtinPointer $ knownInterface @(StageBuiltins' ( TessellationControlInfo 65535 65535 ) ) -- TODO
stageBuiltins TessellationEvaluation _
  = builtinPointer $ knownInterface @(StageBuiltins' ( TessellationEvaluationInfo 65535 ) ) -- TODO
stageBuiltins Geometry modes
  | InputPoints         `elem` modes = builtinPointer $ knownInterface @(StageBuiltins' ( GeometryInfo 1 ))
  | InputLines          `elem` modes = builtinPointer $ knownInterface @(StageBuiltins' ( GeometryInfo 2 ))
  | Triangles           `elem` modes = builtinPointer $ knownInterface @(StageBuiltins' ( GeometryInfo 3 ))
  | InputLinesAdjacency `elem` modes = builtinPointer $ knownInterface @(StageBuiltins' ( GeometryInfo 4 ))
  | otherwise                        = builtinPointer $ knownInterface @(StageBuiltins' ( GeometryInfo 6 ))
stageBuiltins Fragment _
  = builtinPointer $ knownInterface @(StageBuiltins' FragmentInfo )
stageBuiltins GLCompute _
  = builtinPointer $ knownInterface @(StageBuiltins' GLComputeInfo )
stageBuiltins Kernel _
  = builtinPointer $ knownInterface @(StageBuiltins' KernelInfo )

builtinPointer :: [ (Text, (SPIRV.PrimTy, SPIRV.StorageClass)) ]
               -> [ (Text, SPIRV.PointerTy) ]
builtinPointer = map
  ( second ( \ (ty, storage) -> SPIRV.PointerTy storage ty ) )


--------------------------------------------------------------------------
-- decoration of builtins
-- slight indirection to account for complexities with 'gl_in', 'gl_out', 'gl_perVertex'
-- (that is, inputs/outputs that are given as arrays of structs)

builtinDecorations :: Text -> Set (SPIRV.Decoration Word32)
builtinDecorations "gl_TessLevelInner"
  = Set.fromAscList [ SPIRV.Builtin SPIRV.TessLevelInner, SPIRV.Patch ]
builtinDecorations "gl_TessLevelOuter"
  = Set.fromAscList [ SPIRV.Builtin SPIRV.TessLevelOuter, SPIRV.Patch ]
builtinDecorations perVertex
  | perVertex `elem` [ "gl_in", "gl_out", "gl_PerVertex" ]
  -- workaround: in this case the 'Builtin' decoration is applied when we create the relevant struct
  = Set.fromAscList [ SPIRV.Block ]
builtinDecorations builtin
  = maybe
      Set.empty
      ( Set.singleton . SPIRV.Builtin )
      ( SPIRV.readBuiltin builtin )
