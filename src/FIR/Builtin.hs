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

-- containers
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except
  ( runExcept )

-- text-utf8
import "text-utf8" Data.Text
  ( Text )
import qualified "text-utf8" Data.Text as Text

-- fir
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
  ( V )
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
  ( StageInfo(..) )

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
  StageBuiltins' (GeometryInfo inputSize)
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

stageBuiltins :: StageInfo Word32 stage -> [ (Text, SPIRV.PointerTy) ]
stageBuiltins VertexInfo
  = builtinPointer ( knownInterface @(StageBuiltins' VertexInfo) )
stageBuiltins (TessellationControlInfo inputSize outputSize)
  = case ( someNatVal (fromIntegral inputSize), someNatVal (fromIntegral outputSize) ) of
      ( SomeNat ( _ :: Proxy inputSize ), SomeNat ( _ :: Proxy outputSize ) )
        -> builtinPointer ( knownInterface @(StageBuiltins' (TessellationControlInfo inputSize outputSize)) )
stageBuiltins (TessellationEvaluationInfo inputSize)
  = case someNatVal (fromIntegral inputSize) of
      SomeNat ( _ :: Proxy inputSize )
        -> builtinPointer ( knownInterface @(StageBuiltins' (TessellationEvaluationInfo inputSize)) )
stageBuiltins (GeometryInfo inputSize)
  = case someNatVal (fromIntegral inputSize) of
      SomeNat ( _ :: Proxy inputSize )
        -> builtinPointer ( knownInterface @(StageBuiltins' (GeometryInfo inputSize)) )
stageBuiltins FragmentInfo
  = builtinPointer ( knownInterface @(StageBuiltins' FragmentInfo) )
stageBuiltins GLComputeInfo
  = builtinPointer ( knownInterface @(StageBuiltins' GLComputeInfo) )
stageBuiltins KernelInfo
  = builtinPointer ( knownInterface @(StageBuiltins' KernelInfo) )

builtinPointer
    :: [ (Text, (SPIRV.PrimTy, SPIRV.StorageClass)) ]
    -> [ (Text, SPIRV.PointerTy) ]
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
                  ( "'stageBuiltins' bug: cannot infer layout of builtins.\n\
                    \Reason provided: " <> Text.unpack err
                  )
              Right laidOutPtr -> laidOutPtr
  )

--------------------------------------------------------------------------
-- decoration of builtins
-- slight indirection to account for complexities with 'gl_in', 'gl_out'
-- (that is, inputs/outputs that are given as arrays of structs)

builtinDecorations :: Text -> SPIRV.Decorations
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
