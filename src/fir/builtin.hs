{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
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

-- containers
import Data.Set
  ( Set )
import qualified Data.Set as Set

-- text-utf8
import Data.Text
  ( Text )

-- fir
import Data.Type.Map
  ( (:->)((:->))
  , InsertionSort, Union
  )
import FIR.Binding
  ( BindingsMap, Var, R, W )
import FIR.Prim.Array
  ( RuntimeArray )
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
  ( Decoration(Builtin, Patch) )
import qualified SPIRV.PrimTy        as SPIRV
  ( PrimTy, PointerTy, pattern PointerTy )
import qualified SPIRV.Storage       as SPIRV
  ( StorageClass )
import SPIRV.Stage
  ( Stage(..) )

--------------------------------------------------------------------------

type family GetAllBuiltins (entryPoints :: [( Symbol, Stage )]) :: BindingsMap where
  GetAllBuiltins '[]                    = '[ ]
  GetAllBuiltins ( '( _, stage) ': ps ) = Union ( StageBuiltins stage ) ( GetAllBuiltins ps )

type StageBuiltins (stage :: Stage)
  = ( InsertionSort ( StageBuiltins' stage ) :: BindingsMap )

type family StageBuiltins' (stage :: Stage) :: BindingsMap where
  StageBuiltins' Vertex
    = '[ "gl_VertexId"       ':-> Var R Int32
       , "gl_InstanceId"     ':-> Var R Int32
       , "gl_Position"       ':-> Var W ( V 4 Float )
       , "gl_PointSize"      ':-> Var W Float
       ]
  StageBuiltins' TessellationControl
    = '[ "gl_InvocationId"   ':-> Var R Int32
       , "gl_PatchVertices"  ':-> Var R Int32
       , "gl_PrimitiveId"    ':-> Var R Int32
       , "gl_in"
            ':-> Var R
                  ( RuntimeArray 
                    ( Struct '[ "gl_Position"  ':-> V 4 Float
                              , "gl_PointSize" ':-> Float
                              ]
                    )
                  )
       , "gl_out"
            ':-> Var W
                  ( RuntimeArray 
                    ( Struct '[ "gl_Position"  ':-> V 4 Float
                              , "gl_PointSize" ':-> Float
                              ]
                    )
                  )
       , "gl_TessLevelOuter" ':-> Var W ( RuntimeArray Float )
       , "gl_TessLevelInner" ':-> Var W ( RuntimeArray Float )
       ]
  StageBuiltins' TessellationEvaluation
    = '[ "gl_TessCoord"      ':-> Var R ( V 3 Float )
       , "gl_PatchVertices"  ':-> Var R Int32
       , "gl_PrimitiveId"    ':-> Var R Int32
       , "gl_perVertex" 
           ':-> Var R
                  ( RuntimeArray 
                    ( Struct '[ "gl_Position"  ':-> V 4 Float
                              , "gl_PointSize" ':-> Float
                              ]
                    )
                  )
       , "gl_Position"       ':-> Var W ( V 4 Float )
       , "gl_PointSize"      ':-> Var W Float
       ]
  StageBuiltins' Geometry 
    = '[ "gl_PrimitiveId"    ':-> Var R Int32
       , "gl_InvocationId"   ':-> Var R Int32
       , "gl_perVertex" 
           ':-> Var R
                  ( RuntimeArray 
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
  StageBuiltins' Fragment 
    = '[ "gl_Layer"          ':-> Var R Int32
       , "gl_ViewportIndex"  ':-> Var R Int32
       , "gl_FragCoord"      ':-> Var R ( V 4 Float )
       , "gl_PointCoord"     ':-> Var R ( V 2 Float )
       , "gl_FrontFacing"    ':-> Var R Bool
       , "gl_SampleId"       ':-> Var R Int32
       , "gl_SamplePosition" ':-> Var R ( V 2 Float )
       , "gl_SampleMask"     ':-> Var W ( RuntimeArray Int32 )
       , "gl_FragDepth"      ':-> Var W Float
       ]
  StageBuiltins' GLCompute 
    = '[ "gl_NumWorkgroups"        ':-> Var R ( V 3 Word32 )
       , "gl_WorkgroupSize"        ':-> Var R ( V 3 Word32 )
       , "gl_WorkgroupId"          ':-> Var R ( V 3 Word32 )
       , "gl_LocalInvocationId"    ':-> Var R ( V 3 Word32 )
       , "gl_GlobalInvocationId"   ':-> Var R ( V 3 Word32 )
       , "gl_LocalInvocationIndex" ':-> Var R Word32
       ]
  StageBuiltins' Kernel
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

stageBuiltins :: Stage -> [ (Text, SPIRV.PointerTy) ]
stageBuiltins Vertex                 = builtinPointer $ knownInterface @(StageBuiltins Vertex                )
stageBuiltins TessellationControl    = builtinPointer $ knownInterface @(StageBuiltins TessellationControl   )
stageBuiltins TessellationEvaluation = builtinPointer $ knownInterface @(StageBuiltins TessellationEvaluation)
stageBuiltins Geometry               = builtinPointer $ knownInterface @(StageBuiltins Geometry              )
stageBuiltins Fragment               = builtinPointer $ knownInterface @(StageBuiltins Fragment              )
stageBuiltins GLCompute              = builtinPointer $ knownInterface @(StageBuiltins GLCompute             )
stageBuiltins Kernel                 = builtinPointer $ knownInterface @(StageBuiltins Kernel                )

builtinPointer :: [ (Text, (SPIRV.PrimTy, SPIRV.StorageClass)) ]
               -> [ (Text, SPIRV.PointerTy) ]
builtinPointer
  = map 
      ( second
          ( \ (ty, storage) -> SPIRV.PointerTy storage ty )
      )

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
  | perVertex `elem` [ "gl_in", "gl_out", "gl_perVertex" ]
  = Set.empty -- workaround: we decorate in this case when we create the relevant struct
builtinDecorations builtin
  = maybe
      Set.empty
      ( Set.singleton . SPIRV.Builtin )
      ( SPIRV.readBuiltin builtin )
