{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Builtin

This module describes the built-in variables that are provided
for each execution model in SPIR-V.

For instance, in a vertex shader, one can always write position information
into the built-in variable named @gl_Position@, of type @V 4 Float@.
-}

module FIR.Builtin where

-- base
import Control.Arrow
  ( second )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat, SomeNat(SomeNat), someNatVal )

-- containers
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except
  ( runExcept )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

-- fir
import Data.Type.List
  ( type (:++:) )
import Data.Type.Map
  ( (:->)((:->))
  , InsertionSort
  )
import FIR.Binding
  ( Binding, BindingsMap, Var, R, W )
import FIR.Layout
  ( inferPointerLayout )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Singletons
  ( KnownInterface(knownInterface) )
import FIR.Prim.Struct
  ( Struct )
import Math.Linear
  ( V, M )
import qualified SPIRV.Builtin       as SPIRV
  ( Builtin(TessLevelInner, TessLevelOuter)
  , readBuiltin
  )
import qualified SPIRV.Decoration    as SPIRV
  ( Decoration(Builtin, Patch), Decorations )
import qualified SPIRV.PrimTy        as SPIRV
  ( AggregateUsage(..), PrimTy, PointerTy(PointerTy) )
import qualified SPIRV.Storage       as SPIRV
  ( StorageClass(..) )
import SPIRV.Stage
  ( TessellationMode(..)
  , GeometryInputMode(..)
  , ShaderInfo(..)
  , ExecutionInfo(..)
  , VertexInfo
  , TessellationControlInfo
  , TessellationEvaluationInfo
  , GeometryInfo
  , FragmentInfo
  , ComputeInfo
  , RayShader(..), RayShaderInfo(..)
  )

--------------------------------------------------------------------------

type ModelBuiltins (info :: ExecutionInfo Nat stage)
  = ( InsertionSort ( ModelBuiltins' info ) :: BindingsMap )

type family ModelBuiltins' (info :: ExecutionInfo Nat stage) :: [ Symbol :-> Binding ] where
  ModelBuiltins' VertexInfo
    = '[ "gl_VertexID"       ':-> Var R Word32
       , "gl_InstanceID"     ':-> Var R Word32
       , "gl_Position"       ':-> Var W ( V 4 Float )
       , "gl_PointSize"      ':-> Var W Float
       ]
  ModelBuiltins' (TessellationControlInfo inputSize outputSize _)
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
  ModelBuiltins' (TessellationEvaluationInfo inputSize _)
    = '[ "gl_TessCoord"      ':-> Var R ( V 3 Float )
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
       , "gl_Position"       ':-> Var W ( V 4 Float )
       , "gl_PointSize"      ':-> Var W Float
       ]
  ModelBuiltins' (GeometryInfo inputSize _)
    = '[ "gl_PrimitiveID"    ':-> Var R Word32
       , "gl_InvocationID"   ':-> Var R Word32
       , "gl_in"
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
  ModelBuiltins' FragmentInfo
    = '[ "gl_Layer"          ':-> Var R Word32
       , "gl_ViewportIndex"  ':-> Var R Word32
       , "gl_FragCoord"      ':-> Var R ( V 4 Float )
       , "gl_PointCoord"     ':-> Var R ( V 2 Float )
       , "gl_FrontFacing"    ':-> Var R Bool
       , "gl_SampleID"       ':-> Var R Word32
       , "gl_SamplePosition" ':-> Var R ( V 2 Float )
       , "gl_FragDepth"      ':-> Var W Float
       ]
  ModelBuiltins' ( ComputeInfo _ )
    = '[ "gl_NumWorkgroups"        ':-> Var R ( V 3 Word32 )
       , "gl_WorkgroupSize"        ':-> Var R ( V 3 Word32 )
       , "gl_WorkgroupID"          ':-> Var R ( V 3 Word32 )
       , "gl_LocalInvocationID"    ':-> Var R ( V 3 Word32 )
       , "gl_GlobalInvocationID"   ':-> Var R ( V 3 Word32 )
       , "gl_LocalInvocationIndex" ':-> Var R Word32
       ]
  ModelBuiltins' ( KernelInfo _ )
    = '[ "cl_WorkDim"            ':-> Var R Word32
       , "cl_GlobalSize"         ':-> Var R (V 3 Word32)
       , "cl_GlobalInvocationID" ':-> Var R (V 3 Word32)
       , "cl_LocalInvocationID"  ':-> Var R (V 3 Word32)
       , "cl_NumWorkgroups"      ':-> Var R (V 3 Word32)
       , "cl_WorkgroupID"        ':-> Var R (V 3 Word32)
       , "cl_GlobalOffset"       ':-> Var R (V 3 Word32)
       , "cl_GlobalLinearID"     ':-> Var R Word32
       ]
  ModelBuiltins' ( 'RayShaderInfo ( _ :: RayShaderInfo rayShader ) )
    = RayShaderBuiltins rayShader

type family RayShaderBuiltins ( shader :: RayShader ) :: [ Symbol :-> Binding ] where
  RayShaderBuiltins RayGenerationShader
    = '[ "gl_LaunchID"   ':-> Var R (V 3 Word32)
       , "gl_LaunchSize" ':-> Var R (V 3 Word32)
       ]
  RayShaderBuiltins IntersectionShader
    = '[ "gl_LaunchID"            ':-> Var R (V 3 Word32)
       , "gl_LaunchSize"          ':-> Var R (V 3 Word32)
       , "gl_PrimitiveID"         ':-> Var R Word32
       , "gl_InstanceID"          ':-> Var R Word32
       , "gl_InstanceCustomIndex" ':-> Var R Word32
       , "gl_GeometryIndex"       ':-> Var R Word32
       , "gl_WorldRayOrigin"      ':-> Var R (V 3 Float)
       , "gl_WorldRayDirection"   ':-> Var R (V 3 Float)
       , "gl_ObjectRayOrigin"     ':-> Var R (V 3 Float)
       , "gl_ObjectRayDirection"  ':-> Var R (V 3 Float)
       , "gl_RayTMin"             ':-> Var R Float
       , "gl_RayTMax"             ':-> Var R Float
       , "gl_IncomingRayFlags"    ':-> Var R Word32
       , "gl_ObjectToWorld"       ':-> Var R (M 3 4 Float)
       , "gl_WorldToObject"       ':-> Var R (M 3 4 Float)
       , "gl_ObjectToWorld3x4"    ':-> Var R (M 4 3 Float)
       , "gl_WorldToObject3x4"    ':-> Var R (M 4 3 Float)
       ]
  RayShaderBuiltins AnyHitShader = RayShaderBuiltins IntersectionShader :++:
      '[ "gl_HitT"    ':-> Var R Float
       , "gl_HitKind" ':-> Var R Word32
       ]
  RayShaderBuiltins ClosestHitShader = RayShaderBuiltins AnyHitShader
  RayShaderBuiltins MissShader
    = '[ "gl_LaunchID"            ':-> Var R (V 3 Word32)
       , "gl_LaunchSize"          ':-> Var R (V 3 Word32)
       , "gl_WorldRayOrigin"      ':-> Var R (V 3 Float)
       , "gl_WorldRayDirection"   ':-> Var R (V 3 Float)
       , "gl_RayTMin"             ':-> Var R Float
       , "gl_RayTMax"             ':-> Var R Float
       , "gl_IncomingRayFlags"    ':-> Var R Word32
       ]
  RayShaderBuiltins CallableShader
    = '[ "gl_LaunchID"            ':-> Var R (V 3 Word32)
       , "gl_LaunchSize"          ':-> Var R (V 3 Word32)
       ]

shaderBuiltins :: ShaderInfo Word32 shader -> [ (ShortText, SPIRV.PointerTy) ]
shaderBuiltins VertexShaderInfo
  = builtinPointer ( knownInterface @(ModelBuiltins' VertexInfo) )
shaderBuiltins (TessellationControlShaderInfo inputSize outputSize _)
  = case ( someNatVal (fromIntegral inputSize), someNatVal (fromIntegral outputSize) ) of
      ( SomeNat ( _ :: Proxy inputSize ), SomeNat ( _ :: Proxy outputSize ) )
        -> builtinPointer ( knownInterface @(ModelBuiltins' (TessellationControlInfo inputSize outputSize 'Nothing)) )
shaderBuiltins (TessellationEvaluationShaderInfo inputSize _)
  = case someNatVal (fromIntegral inputSize) of
      SomeNat ( _ :: Proxy inputSize )
        -> builtinPointer ( knownInterface @(ModelBuiltins' (TessellationEvaluationInfo inputSize 'ModeTriangles)) ) -- dummy mode that is not used
shaderBuiltins (GeometryShaderInfo inputSize _)
  = case someNatVal (fromIntegral inputSize) of
      SomeNat ( _ :: Proxy inputSize )
        -> builtinPointer ( knownInterface @(ModelBuiltins' (GeometryInfo inputSize 'InputModeTriangles)) ) -- another dummy
shaderBuiltins FragmentShaderInfo
  = builtinPointer ( knownInterface @(ModelBuiltins' FragmentInfo) )
shaderBuiltins (ComputeShaderInfo _)
  = builtinPointer ( knownInterface @(ModelBuiltins' (ComputeInfo '(1,1,1))) ) -- yet another

rayShaderBuiltins :: RayShaderInfo shader -> [ (ShortText, SPIRV.PointerTy) ]
rayShaderBuiltins RayGenerationShaderInfo = builtinPointer ( knownInterface @(RayShaderBuiltins RayGenerationShader) )
rayShaderBuiltins IntersectionShaderInfo  = builtinPointer ( knownInterface @(RayShaderBuiltins IntersectionShader) )
rayShaderBuiltins AnyHitShaderInfo        = builtinPointer ( knownInterface @(RayShaderBuiltins AnyHitShader) )
rayShaderBuiltins ClosestHitShaderInfo    = builtinPointer ( knownInterface @(RayShaderBuiltins ClosestHitShader) )
rayShaderBuiltins MissShaderInfo          = builtinPointer ( knownInterface @(RayShaderBuiltins MissShader) )
rayShaderBuiltins CallableShaderInfo      = builtinPointer ( knownInterface @(RayShaderBuiltins CallableShader) )

modelBuiltins :: ExecutionInfo Word32 model -> [ (ShortText, SPIRV.PointerTy) ]
modelBuiltins (ShaderExecutionInfo shaderInfo) = shaderBuiltins shaderInfo
modelBuiltins (KernelInfo _)
  = builtinPointer ( knownInterface @(ModelBuiltins' (KernelInfo '(1,1,1))) ) -- dummy again
modelBuiltins (RayShaderInfo rayInfo) = rayShaderBuiltins rayInfo

builtinPointer
    :: [ (ShortText, (SPIRV.PrimTy, SPIRV.StorageClass)) ]
    -> [ (ShortText, SPIRV.PointerTy) ]
builtinPointer = map
  ( second $ \ (ty, storage)
      -> let usage =
                case storage of
                  SPIRV.Input  -> SPIRV.ForInputBuiltins
                  SPIRV.Output -> SPIRV.ForOutputBuiltins
                  _            -> SPIRV.NotForBuiltins
         in case runExcept $ inferPointerLayout usage Set.empty (SPIRV.PointerTy storage ty) of
              Left err ->
                error
                  ( "'modelBuiltins' bug: cannot infer layout of builtins.\n\
                    \Reason provided: " <> ShortText.unpack err
                  )
              Right laidOutPtr -> laidOutPtr
  )

--------------------------------------------------------------------------
-- decoration of builtins
-- slight indirection to account for complexities with 'gl_in', 'gl_out'
-- (that is, inputs/outputs that are given as arrays of structs)

builtinDecorations :: ShortText -> SPIRV.Decorations
builtinDecorations "gl_TessLevelInner"
  = Set.fromAscList [ SPIRV.Builtin SPIRV.TessLevelInner, SPIRV.Patch ]
builtinDecorations "gl_TessLevelOuter"
  = Set.fromAscList [ SPIRV.Builtin SPIRV.TessLevelOuter, SPIRV.Patch ]
builtinDecorations perVertex
  | perVertex `elem` [ "gl_in", "gl_out" ]
  -- workaround: in this case the 'Builtin' decoration is applied when we create the relevant struct
  = Set.empty
builtinDecorations builtin
  = maybe
      Set.empty
      ( Set.singleton . SPIRV.Builtin )
      ( SPIRV.readBuiltin builtin )
