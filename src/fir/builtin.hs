{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module FIR.Builtin where

-- base
import Control.Arrow
  ( second )
import Data.Maybe
  ( maybe )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat, SomeNat(SomeNat), someNatVal )
import Numeric.Natural
  ( Natural )

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
  ( ExecutionMode(..) )
import qualified SPIRV.PrimTy        as SPIRV
  ( PrimTy, PointerTy, pattern PointerTy )
import qualified SPIRV.Storage       as SPIRV
  ( StorageClass )
import SPIRV.Stage
  ( Stage(..), StageInfo(..) )

--------------------------------------------------------------------------

type StageBuiltins (info :: StageInfo Nat stage)
  = ( InsertionSort ( StageBuiltins' info ) :: BindingsMap )

type family StageBuiltins' (info :: StageInfo Nat stage) :: [ Symbol :-> Binding ] where
  StageBuiltins' VertexInfo
    = '[ "gl_VertexID"       ':-> Var R Word32
       , "gl_InstanceID"     ':-> Var R Word32
       , "gl_Position"       ':-> Var W ( V 4 Float )
       , "gl_PointSize"      ':-> Var W Float
       ]
  StageBuiltins' (TessellationControlInfo inputSize outputSize)
    = '[ "gl_InvocationID"   ':-> Var R Word32
       , "gl_PatchVertices"  ':-> Var R Word32
       , "gl_PrimitiveID"    ':-> Var R Word32
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
       , "gl_PatchVertices"  ':-> Var R Word32
       , "gl_PrimitiveID"    ':-> Var R Word32
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
    = '[ "gl_PrimitiveID"    ':-> Var R Word32
       , "gl_InvocationID"   ':-> Var R Word32
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
       , "gl_Layer"          ':-> Var W Word32
       , "gl_ViewportIndex"  ':-> Var W Word32
       ]
  StageBuiltins' FragmentInfo
    = '[ "gl_Layer"          ':-> Var R Word32
       , "gl_ViewportIndex"  ':-> Var R Word32
       , "gl_FragCoord"      ':-> Var R ( V 4 Float )
       , "gl_PointCoord"     ':-> Var R ( V 2 Float )
       , "gl_FrontFacing"    ':-> Var R Bool
       , "gl_SampleID"       ':-> Var R Word32
       , "gl_SamplePosition" ':-> Var R ( V 2 Float )
       , "gl_FragDepth"      ':-> Var W Float
       ]
  StageBuiltins' GLComputeInfo
    = '[ "gl_NumWorkgroups"        ':-> Var R ( V 3 Word32 )
       , "gl_WorkgroupSize"        ':-> Var R ( V 3 Word32 )
       , "gl_WorkgroupID"          ':-> Var R ( V 3 Word32 )
       , "gl_LocalInvocationID"    ':-> Var R ( V 3 Word32 )
       , "gl_GlobalInvocationID"   ':-> Var R ( V 3 Word32 )
       , "gl_LocalInvocationIndex" ':-> Var R Word32
       ]
  StageBuiltins' KernelInfo
    = '[ "gl_NumWorkgroups"             ':-> Var R Word32
       , "gl_WorkgroupSize"             ':-> Var R Word32
       , "gl_WorkgroupID"               ':-> Var R Word32
       , "gl_LocalInvocationID"         ':-> Var R Word32
       , "gl_GlobalInvocationID"        ':-> Var R Word32
       , "gl_LocalInvocationIndex"      ':-> Var R Word32
       , "gl_WorkDim"                   ':-> Var R Word32
       , "gl_GlobalSize"                ':-> Var R Word32
       , "gl_EnqueuedWorkgroupSize"     ':-> Var R Word32
       , "gl_GlobalOffset"              ':-> Var R Word32
       , "gl_GlobalLinearID"            ':-> Var R Word32
       , "gl_SubgroupSize"              ':-> Var R Word32
       , "gl_SubgroupMaxSize"           ':-> Var R Word32
       , "gl_NumSubgroups"              ':-> Var R Word32
       , "gl_NumEnqueuedSubgroups"      ':-> Var R Word32
       , "gl_SubgroupID"                ':-> Var R Word32
       , "gl_SubgroupLocalInvocationID" ':-> Var R Word32
       ]

-- some code duplication happening here with the logic in "SPIRV.ExecutionMode"
stageBuiltins :: Stage -> [ SPIRV.ExecutionMode Word32 ] -> [ (Text, SPIRV.PointerTy) ]
stageBuiltins Vertex _
  = builtinPointer $ knownInterface @(StageBuiltins' VertexInfo)
stageBuiltins TessellationControl modes =
  let
    maxPatchVertices :: [ SPIRV.ExecutionMode Word32 ] -> Natural
    maxPatchVertices [] = 32
    maxPatchVertices ( MaxPatchVertices i : _ ) = fromIntegral i
    maxPatchVertices ( _ : next ) = maxPatchVertices next
    outputVertices :: [ SPIRV.ExecutionMode Word32 ] -> Natural
    outputVertices [] = 32
    outputVertices ( OutputVertices i : _ ) = fromIntegral i
    outputVertices ( _ : next ) = outputVertices next
    res :: SomeNat -> SomeNat -> [ (Text, SPIRV.PointerTy) ]
    res ( SomeNat (_ :: Proxy maxPatchVertices) ) ( SomeNat (_ :: Proxy outputVertices) )
      = builtinPointer
      $ knownInterface @(StageBuiltins' (TessellationControlInfo maxPatchVertices outputVertices))
  in res (someNatVal (maxPatchVertices modes)) (someNatVal (outputVertices modes))
stageBuiltins TessellationEvaluation modes =
  let
    inputVertices :: [ SPIRV.ExecutionMode Word32 ] -> Natural
    inputVertices [] = 32
    inputVertices ( OutputVertices i : _ ) = fromIntegral i -- 'InputVertices' is a synonym for 'OutputVertices' (weird I know, sorry)
    inputVertices ( _ : next ) = inputVertices next
    res :: SomeNat -> [ (Text, SPIRV.PointerTy) ]
    res ( SomeNat (_ :: Proxy inputVertices) )
      = builtinPointer
      $ knownInterface @(StageBuiltins' (TessellationEvaluationInfo inputVertices))
  in res (someNatVal (inputVertices modes))
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
