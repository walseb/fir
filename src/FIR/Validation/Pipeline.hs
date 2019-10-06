{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Validation.Pipeline

Validation of graphics pipelines at compile-time.

This module validates graphics pipelines and computes information
necessary for the creation of a graphics pipeline in @Vulkan@.
-}

module FIR.Validation.Pipeline where

-- base
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If, type (&&) )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( Symbol, AppendSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat
  , type (+), type (*), Div
  )

-- fir
import Data.Type.List
  ( Replicate )
import Data.Type.Map
  ( Map, (:->)((:->))
  , Values
  )
import Data.Type.Ord
  ( POrd(Max) )
import Data.Type.String
  ( ShowNat )
import FIR.ASTState
  ( ASTState(ASTState)
  , TLInterface, TLInterfaceVariable
  , EntryPointInfo(EntryPointInfo)
  )
import FIR.Definition
  ( Definition(Global) )
import {-# SOURCE #-} FIR.Pipeline
  ( PrimitiveConnectedness(..), PrimitiveTopology(..)
  , VertexLocationDescriptions
  , PipelineInfo(TopOfPipe,Into)
  )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Singletons
  ( SKPrimTy, ScalarFromSScalar )
import FIR.Prim.Struct
  ( LocationSlot(LocationSlot), ShowLocation )
import FIR.Validation.Layout
  ( Slots, SlotProvenance(Provenance) )
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.Image      as SPIRV
  ( ImageFormat(ImageFormat) )
import qualified SPIRV.Image
import qualified SPIRV.ScalarTy   as SPIRV
import qualified SPIRV.Stage      as SPIRV
import SPIRV.Stage
  ( NamedShader )
import qualified SPIRV.Storage    as SPIRV

-------------------------------------------------

-- | Check that a pipeline is valid:
--
--  - starts with a vertex shader,
--  - ends with a fragment shader,
--  - the sequence of shaders is valid,
--  - the interfaces between consecutive shaders match.
type family ValidPipelineInfo
              ( info :: PipelineInfo          )
              ( top  :: PrimitiveTopology Nat )
            :: Constraint where
  ValidPipelineInfo info top
    = ( StartsWithVertex info
      , EndsWithFragment info
      , ValidInterfaces  top info
      , ValidTopology    top
      )

-- | Check that a pipeline starts with a vertex shader.
type family StartsWithVertex ( info :: PipelineInfo ) :: Constraint where
  StartsWithVertex
    ( TopOfPipe `Into` ( 'EntryPointInfo _ (_ :: SPIRV.ExecutionInfo Nat SPIRV.Vertex) _ _ ) )
      = ()
  StartsWithVertex ( info `Into` _)
    = StartsWithVertex info
  StartsWithVertex _
    = TypeError
        ( Text "Pipeline does not begin with a vertex shader." )

-- | Check that a pipeline ends with a fragment shader.
type family EndsWithFragment ( info :: PipelineInfo ) :: Constraint where
  EndsWithFragment
    ( _ `Into` ( 'EntryPointInfo _ (_ :: SPIRV.ExecutionInfo Nat SPIRV.Fragment) _ _ ) )
      = ()
  EndsWithFragment _
    = TypeError
        ( Text "Pipeline does not end with a fragment shader." )

type family ValidTopology (top :: PrimitiveTopology n) :: Constraint where
  ValidTopology (Line Fan)
    = TypeError ( Text "Invalid topology: fan of lines." )
  ValidTopology _
    = ()

-- | Check that consecutive shaders in the pipeline match up correctly:
--
--  - the sequence makes sense (e.g. can't have a tessellation shader following a geometry shader),
--  - the sequence is compatible with the supplied primitive topology,
--  - the interfaces match.
type family ValidInterfaces
              ( top  :: PrimitiveTopology Nat )
              ( info :: PipelineInfo          )
            :: Constraint where
  ValidInterfaces _ TopOfPipe = ()
  ValidInterfaces _ (TopOfPipe `Into` _) = ()
  ValidInterfaces top
    (      pipelineStages
    `Into` pipelineStage1
    `Into` pipelineStage2
    )
      = ( ValidPipelineInterface top pipelineStage1 pipelineStage2
        , ValidInterfaces top ( pipelineStages `Into` pipelineStage1 )
        )

-- | Check that a given pair of shaders sequences correctly.
type family ValidPipelineInterface
              ( top    :: PrimitiveTopology Nat )
              ( stage1 :: EntryPointInfo     )
              ( stage2 :: EntryPointInfo     )
            :: Constraint
            where
  ValidPipelineInterface top
    ( 'EntryPointInfo
        name1
        ( 'SPIRV.ShaderExecutionInfo info1
            :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader1))
        )
        '( _, outputs1 )
        _
    )
    ( 'EntryPointInfo
        name2
        ( 'SPIRV.ShaderExecutionInfo info2
            :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader2))
        )
        '( inputs2, _ )
        _
    )
      = ( ValidInterface
          shader1 info1 name1 (Values outputs1)
          shader2 info2 name2 (Values inputs2 )
        , ValidSequence top
            shader1 info1
            shader2 info2
        )

-- | Checks that the sequencing of shaders is valid.
--
-- Checks that:
--
--   - the sequence of stages is an allowed stage sequence,
--     e.g. a Geometry shader must be followed by a Fragment shader,
--
--   - the stages fit the given primitive topology,
--     e.g. no tessellation shaders if primitive topology is not 'Patches',
--
--   - subsequent stages are in the correct mode,
--     e.g. the Tessellation Evaluation shader outputs geometry
--     in the appropriate mode for the Geometry shader that follows.
type family ValidSequence
              ( top     :: PrimitiveTopology Nat        )
              ( shader1 :: SPIRV.Shader                 )
              ( info1   :: SPIRV.ShaderInfo Nat shader1 )
              ( shader2 :: SPIRV.Shader                 )
              ( info2   :: SPIRV.ShaderInfo Nat shader2 )
            :: Constraint where
  ValidSequence ('PatchesOfSize n)
    SPIRV.VertexShader               _
    SPIRV.TessellationControlShader  _
      = ()
  ValidSequence top
    SPIRV.VertexShader               _
    SPIRV.TessellationControlShader  _
      = TypeError
        (    Text "Primitive topology is " :<>: ShowType top :<>: Text ","
        :$$: Text "which cannot be used in the presence of tessellation shaders."
        )
  ValidSequence ('PatchesOfSize n)
    SPIRV.VertexShader   _
    SPIRV.GeometryShader _
      = TypeError
        (    Text "Primitive topology is 'Patches' – "
        :<>: Text "which requires the use of tessellation – "
        :$$: Text "but the supplied shader pipeline does not contain\
                  \ any tessellation shaders."
        )
  ValidSequence top
    SPIRV.VertexShader   _
    SPIRV.GeometryShader geomInfo
      = CompatibleGeometryTopology top 'Nothing geomInfo
  ValidSequence ('PatchesOfSize n)
    SPIRV.VertexShader   _
    SPIRV.FragmentShader _
      = TypeError
        (    Text "Primitive topology is 'Patches' – "
        :<>: Text "which requires the use of tessellation – "
        :$$: Text "but the supplied shader pipeline does not contain\
                  \ any tessellation shaders."
        )
  ValidSequence _
    SPIRV.VertexShader   _
    SPIRV.FragmentShader _
      = ()
  ValidSequence _
    SPIRV.TessellationControlShader    info1
    SPIRV.TessellationEvaluationShader info2
      = CompatibleTessellationStages info1 info2
  ValidSequence top
    SPIRV.TessellationEvaluationShader info1
    SPIRV.GeometryShader               info2
      = CompatibleGeometryTopology top ('Just info1) info2
  ValidSequence _
    SPIRV.TessellationEvaluationShader _
    SPIRV.FragmentShader               _
      = ()
  ValidSequence _
    SPIRV.GeometryShader _
    SPIRV.FragmentShader _
      = ()
  ValidSequence _ shader1 _ shader2 _
    = TypeError
        ( Text "Invalid pipeline:"
         :$$: ShowType shader2 :<>: Text " cannot follow "
         :<>: ShowType shader1 :<>: Text "."
        )

type family CompatibleGeometryTopology
              ( top :: PrimitiveTopology Nat )
              ( mbTess :: Maybe
                  ( SPIRV.ShaderInfo Nat SPIRV.TessellationEvaluationShader )
              )
              ( geom :: SPIRV.ShaderInfo Nat SPIRV.GeometryShader )
            :: Constraint
            where
  CompatibleGeometryTopology
    ( 'PatchesOfSize n )
    ( 'Just ( SPIRV.TessellationEvaluationShaderInfo _ teseMode ) )
    ( SPIRV.GeometryShaderInfo _ geomMode )
      = ( CompatibleTessellationGeometryModes teseMode geomMode )
  CompatibleGeometryTopology
    top
    'Nothing
    ( SPIRV.GeometryShaderInfo _ geomMode )
      = ( CompatibleBasicGeometryTopology top geomMode )


-- | Check that the Geometry shader is getting the correct primitive topology,
-- in a pipeline without any tessellation.
type family CompatibleBasicGeometryTopology
                ( top      :: PrimitiveTopology Nat   )
                ( geomMode :: SPIRV.GeometryInputMode )
              :: Constraint
              where
  CompatibleBasicGeometryTopology
    Points
    SPIRV.InputModePoints
      = ()
  CompatibleBasicGeometryTopology
    (Line List)
    SPIRV.InputModeLines
      = ()
  CompatibleBasicGeometryTopology
    (Line AdjacencyStrip)
    SPIRV.InputModeLinesAdjacency
      = ()
  CompatibleBasicGeometryTopology
    (Triangle List)
    SPIRV.InputModeTriangles
      = ()
  CompatibleBasicGeometryTopology
    (Triangle Strip)
    SPIRV.InputModeTriangles
      = ()
  CompatibleBasicGeometryTopology
    (Triangle Fan)
    SPIRV.InputModeTriangles
      = ()
  CompatibleBasicGeometryTopology
    (Triangle AdjacencyList)
    SPIRV.InputModeTrianglesAdjacency
      = ()
  CompatibleBasicGeometryTopology
    (Triangle AdjacencyStrip)
    SPIRV.InputModeTrianglesAdjacency
      = ()
  CompatibleBasicGeometryTopology
    top
    geomMode
      = TypeError
      (    Text "Geometry shader input mode " :<>: Text (SPIRV.ShowGeometryInputMode geomMode)
      :$$: Text "is incompatible with the primitive topology "
      :<>: ShowType top :<>: Text "."
      )

-- | Check that the Geometry shader is getting the correct primitive topology,
-- in a pipeline which does contain tessellation.
type family CompatibleTessellationGeometryModes
              ( teseMode :: SPIRV.TessellationMode  )
              ( geomMode :: SPIRV.GeometryInputMode )
            :: Constraint
            where
  CompatibleTessellationGeometryModes
    SPIRV.ModePoints
    SPIRV.InputModePoints
      = ()
  CompatibleTessellationGeometryModes
    SPIRV.ModeIsolines
    SPIRV.InputModeLines
      = ()
  CompatibleTessellationGeometryModes
    SPIRV.ModeTriangles
    SPIRV.InputModeTriangles
      = ()
  CompatibleTessellationGeometryModes
    SPIRV.ModeQuads
    SPIRV.InputModeTriangles
      = ()
  CompatibleTessellationGeometryModes
    teseMode
    geomMode
      = TypeError
        (    Text "Geometry shader input mode " :<>: Text (SPIRV.ShowGeometryInputMode geomMode)
        :$$: Text "is incompatible with the preceding tessellation output mode "
        :<>: Text (SPIRV.ShowTessellationMode teseMode) :<>: Text "."
        )

-- | Check that the two tessellation stages are using the same tessellation mode.
type family CompatibleTessellationStages
              ( tesc :: SPIRV.ShaderInfo Nat SPIRV.TessellationControlShader    )
              ( tese :: SPIRV.ShaderInfo Nat SPIRV.TessellationEvaluationShader )
            :: Constraint
            where
  CompatibleTessellationStages
    ( SPIRV.TessellationControlShaderInfo  _ _ 'Nothing )
    ( SPIRV.TessellationEvaluationShaderInfo _ _        )
      = ()
  CompatibleTessellationStages
    ( SPIRV.TessellationControlShaderInfo  _ _ ('Just mode) )
    ( SPIRV.TessellationEvaluationShaderInfo _ mode         )
      = ()
  CompatibleTessellationStages
    ( SPIRV.TessellationControlShaderInfo  _ _ ('Just tescMode) )
    ( SPIRV.TessellationEvaluationShaderInfo _ teseMode         )
      = TypeError
        (    Text "Conflicting tessellation modes:"
        :$$: Text "  - tessellation control shader specifies "
        :<>: Text (SPIRV.ShowTessellationMode tescMode) :<>: Text ","
        :$$: Text "  - tessellation evaluation shader specifies "
        :<>: Text (SPIRV.ShowTessellationMode teseMode) :<>: Text "."
        )


-- | Grabs the input/output variables in the given definition.
--
-- This is a workaround for the fact that shader interfaces are not kept track of
-- at the type level.
-- This is permissible as long as the interface for the entry-point consists precisely
-- of the 'Input'/'Output' variables provided in the top-level annotation.
-- See [FIR issue #51](https://gitlab.com/sheaf/fir/issues/51).
type family VariablesWithStorage
              ( storage :: SPIRV.StorageClass      )
              ( defs    :: [Symbol :-> Definition] )
            :: [ Symbol :-> TLInterfaceVariable]
            where
  VariablesWithStorage storage '[] = '[]
  VariablesWithStorage storage ( ( varName ':-> Global storage decs ty) ': defs )
    = ( varName ':-> '(decs, ty) ) ': VariablesWithStorage storage defs
  VariablesWithStorage storage ( _ ': defs )
    = VariablesWithStorage storage defs

type family GetExecutionInfo
              ( shader :: SPIRV.Shader )
              ( name   :: Symbol       )
              ( state  :: ASTState     )
          :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader))
          where
  GetExecutionInfo shader name ('ASTState _ _ _ eps)
    = GetExecutionInfoOf shader name eps

type family GetExecutionInfoOf
             ( shader :: SPIRV.Shader       )
             ( name   :: Symbol             )
             ( eps    :: [ EntryPointInfo ] )
          :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader))
          where
  GetExecutionInfoOf shader name '[]
    = TypeError
        (      Text "Program does not define a "
          :<>: Text (NamedShader name shader)
          :<>: Text "."
        )
  GetExecutionInfoOf shader name
    ( 'EntryPointInfo
         name
         ( nfo :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader)) )
         _
         _
    ': _
    )
      = nfo
  GetExecutionInfoOf shader name ( _ ': eps )
    = GetExecutionInfoOf shader name eps

type family GetInterface
              ( shader :: SPIRV.Shader )
              ( name   :: Symbol       )
              ( state  :: ASTState     )
          :: TLInterface
          where
  GetInterface shader name ('ASTState _ _ _ eps)
    = GetInterfaceOf shader name eps

type family GetInterfaceOf
              ( shader :: SPIRV.Shader       )
              ( name   :: Symbol             )
              ( eps    :: [ EntryPointInfo ] )
          :: TLInterface
          where
  GetInterfaceOf shader name '[]
    = TypeError
        (      Text "Program does not define a "
          :<>: Text (NamedShader name shader)
          :<>: Text "."
        )
  GetInterfaceOf shader name
    ( 'EntryPointInfo
         name
         _
         iface
         _
    ': _
    )
      = iface
  GetInterfaceOf shader name ( _ ': eps )
    = GetInterfaceOf shader name eps

type family ValidInterface
              ( stage1     :: SPIRV.Shader                )
              ( stageInfo1 :: SPIRV.ShaderInfo Nat stage1 )
              ( stageName1 :: Symbol                      )
              ( outputs    :: [TLInterfaceVariable]       )
              ( stage2     :: SPIRV.Shader                )
              ( stageInfo2 :: SPIRV.ShaderInfo Nat stage2 )
              ( stageName2 :: Symbol                      )
              ( inputs     :: [TLInterfaceVariable]       )
            :: Constraint
            where
  ValidInterface
    stage1 info1 name1 outputs
    stage2 info2 name2 inputs
      = MatchingInterface
          stage1 name1 (OutputArrayness stage1 info1) outputs
          stage2 name2 (InputArrayness  stage2 info2) inputs

type family OutputArrayness
              ( stage     :: SPIRV.Shader               )
              ( stageInfo :: SPIRV.ShaderInfo Nat stage )
            :: Maybe Nat
            where
  OutputArrayness SPIRV.TessellationControlShader (SPIRV.TessellationControlShaderInfo _ j _)
    = Just j
  OutputArrayness _ _
    = Nothing

type family InputArrayness
              ( stage     :: SPIRV.Shader               )
              ( stageInfo :: SPIRV.ShaderInfo Nat stage )
            :: Maybe Nat
            where
  InputArrayness SPIRV.TessellationControlShader (SPIRV.TessellationControlShaderInfo i _ _)
    = Just i
  InputArrayness SPIRV.TessellationEvaluationShader (SPIRV.TessellationEvaluationShaderInfo i _)
    = Just i
  InputArrayness SPIRV.GeometryShader (SPIRV.GeometryShaderInfo i _)
    = Just i
  InputArrayness _ _
    = Nothing

type family MatchingInterface
              ( stage1          :: SPIRV.Shader          )
              ( stageName1      :: Symbol                )
              ( outputArrayness :: Maybe Nat             )
              ( outputs         :: [TLInterfaceVariable] )
              ( stage2          :: SPIRV.Shader          )
              ( stageName2      :: Symbol                )
              ( inputArrayness  :: Maybe Nat             )
              ( inputs          :: [TLInterfaceVariable] )
            :: Constraint
            where
  MatchingInterface
    _ _ _ '[]
    _ _ _ '[]
      = ()
  MatchingInterface
    stage1 name1 Nothing '[]
    stage2 name2 Nothing ( '(decs2, ty2) ': _ )
      = TypeError
          (      Text "Input of type " :<>: ShowType ty2
            :<>: Text " of " :<>: Text ( NamedShader name2 stage2 ) :<>: Text ","
            :$$: Text "with decorations " :<>: ShowType decs2 :<>: Text ","
            :$$: Text "does not match any output of "
            :<>: Text (NamedShader name1 stage1) :<>: Text "."
          )
  MatchingInterface
    stage1 name1 arrayness1 '[]
    stage2 name2 arrayness2 ( '(decs2, ty2) ': _ )
      = TypeError
          (      Text "Input of type " :<>: ShowType ty2
            :<>: Text " of " :<>: Text ( NamedShader name2 stage2 ) :<>: Text ","
            :$$: Text "with decorations " :<>: ShowType decs2 :<>: Text ","
            :$$: Text "does not match any output of "
            :<>: Text (NamedShader name1 stage1) :<>: Text "."
            :$$: (ArraynessNote stage1 name1 arrayness1 stage2 name2 arrayness2)
          )
  MatchingInterface
    stage1 name1 arrayness1 ( output ': outputs )
    stage2 name2 arrayness2 inputs
      = MatchingInterface
          stage1 name1 arrayness1 outputs
          stage2 name2 arrayness2
            ( RemoveMatch
                stage1 name1 arrayness1 output
                stage2 name2 arrayness2 inputs
            )

type family RemoveMatch
              ( stage1     :: SPIRV.Shader          )
              ( stageName1 :: Symbol                )
              ( arrayness1 :: Maybe Nat             )
              ( output     :: TLInterfaceVariable   )
              ( stage2     :: SPIRV.Shader          )
              ( stageName2 :: Symbol                )
              ( arrayness2 :: Maybe Nat             )
              ( inputs     :: [TLInterfaceVariable] )
            :: [TLInterfaceVariable]
            where
  RemoveMatch
    stage1 name1 Nothing '(decs1, ty1)
    stage2 name2 Nothing '[]
      = TypeError
          (      Text "Output of type " :<>: ShowType ty1
            :<>: Text " of " :<>: Text ( NamedShader name1 stage1 ) :<>: Text ","
            :$$: Text "with decorations " :<>: ShowType decs1 :<>: Text ","
            :$$: Text "does not match any input of "
            :<>: Text (NamedShader name2 stage2) :<>: Text "."
          )
  RemoveMatch
    stage1 name1 arrayness1 '(decs1, ty1)
    stage2 name2 arrayness2 '[]
      = TypeError
          (      Text "Output of type " :<>: ShowType ty1
            :<>: Text " of " :<>: Text ( NamedShader name1 stage1 ) :<>: Text ","
            :$$: Text "with decorations " :<>: ShowType decs1 :<>: Text ","
            :$$: Text "does not match any input of "
            :<>: Text (NamedShader name2 stage2) :<>: Text "."
            :$$: (ArraynessNote stage1 name1 arrayness2 stage2 name2 arrayness2)
          )
  RemoveMatch
    stage1 name1 arrayness1 '(decs1, ty1)
    stage2 name2 arrayness2 ( '(decs2, ty2) ': inputs )
    = RemoveThisMatchOrContinue
        ( SameLocation
            (Location decs1) (Component decs1)
            (Location decs2) (Component decs2)
        )
        stage1 name1 arrayness1 '(decs1, ty1)
        stage2 name2 arrayness2 '(decs2, ty2) inputs

type family RemoveThisMatchOrContinue
              ( mbLocation :: Maybe (Nat, Nat)      )
              ( stage1     :: SPIRV.Shader          )
              ( name1      :: Symbol                )
              ( arrayness1 :: Maybe Nat             )
              ( output     :: TLInterfaceVariable   )
              ( stage2     :: SPIRV.Shader          )
              ( name2      :: Symbol                )
              ( arrayness2 :: Maybe Nat             )
              ( input      :: TLInterfaceVariable   )
              ( inputs     :: [TLInterfaceVariable] )
          :: [TLInterfaceVariable]
          where
  RemoveThisMatchOrContinue (Just loc)
    stage1 name1 arrayness1 output
    stage2 name2 arrayness2 input  inputs
      = If
          ( Matches loc
              stage1 name1 arrayness1 output
              stage2 name2 arrayness2 input
          )
          ( inputs )
          ( '[] ) -- unreachable

  RemoveThisMatchOrContinue Nothing
    stage1 name1 arrayness1 output
    stage2 name2 arrayness2 input inputs
      = input ':
          RemoveMatch
            stage1 name1 arrayness1 output
            stage1 name2 arrayness2 inputs

type family ArraynessNote
              ( stage1     :: SPIRV.Shader )
              ( name1      :: Symbol       )
              ( arrayness1 :: Maybe Nat    )
              ( stage2     :: SPIRV.Shader )
              ( name2      :: Symbol       )
              ( arrayness2 :: Maybe Nat    )
            :: ErrorMessage
            where
  ArraynessNote
    stage1 name1 (Just i1)
    _      _     Nothing
      = Text "Note that per-vertex outputs of " :<>: Text ( NamedShader name1 stage1 )
      :<>: Text " must be arrays of size " :<>: Text (ShowNat i1) :<>: Text "."
  ArraynessNote
    _      _     Nothing
    stage2 name2 (Just i2)
      = Text "Note that per-vertex inputs of " :<>: Text ( NamedShader name2 stage2 )
      :<>: Text " must be arrays of size " :<>: ShowType i2 :<>: Text "."
  ArraynessNote
    stage1 name1 (Just i1)
    stage2 name2 (Just i2)
      =    Text "Note that:"
      :$$: Text "  - per-vertex outputs of " :<>: Text ( NamedShader name1 stage1 )
      :<>: Text " must be arrays of size " :<>: Text (ShowNat i1) :<>: Text ","
      :$$: Text "  - per-vertex inputs of " :<>: Text ( NamedShader name2 stage2 )
      :<>: Text " must be arrays of size " :<>: Text (ShowNat i2) :<>: Text "."
  ArraynessNote
    _ _ Nothing
    _ _ Nothing
      = Text ""


type family Matches
              ( location   :: (Nat,Nat)           )
              ( stage1     :: SPIRV.Shader        )
              ( name1      :: Symbol              )
              ( arrayness1 :: Maybe Nat           )
              ( output     :: TLInterfaceVariable )
              ( stage2     :: SPIRV.Shader        )
              ( name2      :: Symbol              )
              ( arrayness2 :: Maybe Nat           )
              ( input      :: TLInterfaceVariable )
            :: Bool
            where
  Matches loc
    stage1 name1 arrayness1 '(decs1, ty1)
    stage2 name2 arrayness2 '(decs2, ty2)
      =   MatchingTypes loc
            stage1 name1 (Arrayness arrayness1 decs1) ty1
            stage2 name2 (Arrayness arrayness2 decs2) ty2
      &&  MatchingDecorations loc
            stage1 name1 decs1
            stage2 name2 decs2

type family SameLocation
              ( loc1  :: Maybe Nat )
              ( comp1 :: Nat       )
              ( loc2  :: Maybe Nat )
              ( comp2 :: Nat       )
            :: Maybe (Nat, Nat)
            where
  SameLocation (Just loc) comp (Just loc) comp
    = Just '(loc, comp)
  SameLocation _ _ _ _
    = Nothing

type family Location (decs :: [SPIRV.Decoration Nat]) :: Maybe Nat where
  Location ('SPIRV.Location i ': _)
    = Just i
  Location (_ ': decs)
    = Location decs
  Location '[]
    = Nothing

type family Component (decs :: [SPIRV.Decoration Nat]) :: Nat where
  Component ('SPIRV.Component i ': _) = i
  Component (_ ': decs) = Component decs
  Component '[] = 0

-- | This computes the level of arrayness needed, noting that
-- per-patch variables don't have any implicit levels of arrayness.
type family Arrayness
              ( baseArrayness :: Maybe Nat              )
              ( decorations   :: [SPIRV.Decoration Nat] )
            :: Maybe Nat
            where
  Arrayness Nothing _
    = Nothing
  Arrayness (Just i) '[]
    = Just i
  Arrayness (Just i) ( SPIRV.Patch ': _ )
    = Nothing
  Arrayness arrayness ( _ ': decs )
    = Arrayness arrayness decs

type family MatchingTypes
              ( loc        :: (Nat, Nat)   )
              ( stage1     :: SPIRV.Shader )
              ( name1      :: Symbol       )
              ( arrayness1 :: Maybe Nat    )
              ( type1      :: Type         )
              ( stage2     :: SPIRV.Shader )
              ( name2      :: Symbol       )
              ( arrayness2 :: Maybe Nat    )
              ( type2      :: Type         )
            :: Bool
            where
  MatchingTypes loc
    stage1 name1 arrayness1 ty1
    stage2 name2 arrayness2 ty2
      = MatchingUnderlyingTypes loc
          stage1 name1 (UnderlyingType "Output" stage1 name1 arrayness1 ty1)
          stage2 name2 (UnderlyingType "Input" stage2 name2 arrayness2 ty2)

type family MatchingUnderlyingTypes
              ( loc        :: (Nat, Nat)   )
              ( stage1     :: SPIRV.Shader )
              ( name1      :: Symbol       )
              ( type1      :: Type         )
              ( stage2     :: SPIRV.Shader )
              ( name2      :: Symbol       )
              ( type2      :: Type         )
            :: Bool
            where
  MatchingUnderlyingTypes _
    _ _ ty
    _ _ ty
      = 'True
  MatchingUnderlyingTypes loc
    stage1 name1 ty1
    stage2 name2 ty2
    = TypeError
        ( MismatchLocation loc
        :$$: Text "Output of " :<>: Text (NamedShader name1 stage1)
        :<>: Text " is of type " :<>: ShowType ty1 :<>: Text ","
        :$$: Text "whereas input of " :<>: Text (NamedShader name2 stage2)
        :<>: Text " is of type " :<>: ShowType ty2 :<>: Text "."
        )

type family MismatchLocation ( loc :: (Nat,Nat) ) :: ErrorMessage where
  MismatchLocation '(loc, 0)
    =    Text "Interface mismatch at location " :<>: Text (ShowNat loc)
    :<>: Text "."
  MismatchLocation '(loc, comp)
    =    Text "Interface mismatch at location " :<>: Text (ShowNat loc)
    :<>: Text ", component " :<>: Text (ShowNat loc) :<>: Text "."

type family UnderlyingType
              ( inout     :: Symbol       )
              ( stage     :: SPIRV.Shader )
              ( name      :: Symbol       )
              ( arrayness :: Maybe Nat    )
              ( ty        :: Type         )
            :: Type
            where
  UnderlyingType _ _ _ Nothing ty
    = ty
  UnderlyingType _ _ _ (Just i) (Array j ty) -- ignore the actual array length, as Vulkan does
    = ty
  UnderlyingType inout stage name (Just i) ty
    = TypeError
        ( Text inout :<>: Text " per-vertex variable of " :<>: Text (NamedShader name stage)
          :<>: Text " should be an array,"
          :$$: Text "but its type is " :<>: ShowType ty :<>: Text "."
        )

type family MatchingDecorations
              ( loc        :: (Nat, Nat)             )
              ( stage1     :: SPIRV.Shader           )
              ( name1      :: Symbol                 )
              ( decs1      :: [SPIRV.Decoration Nat] )
              ( stage2     :: SPIRV.Shader           )
              ( name2      :: Symbol                 )
              ( decs2      :: [SPIRV.Decoration Nat] )
            :: Bool
            where
  MatchingDecorations _
    _ _ '[]
    _ _ '[]
      = 'True
  MatchingDecorations loc
    stage1 name1 '[]
    stage2 name2 (dec2 ': _)
      = TypeError
          ( MismatchLocation loc
           :$$: Text "Input to " :<>: Text (NamedShader name2 stage2)
           :<>: Text "is decorated with " :<>: ShowType dec2 :<>: Text ","
           :$$: Text "but this decoration does not appear in the output of "
           :<>: Text (NamedShader name1 stage1) :<>: Text "."
          )
  MatchingDecorations loc
    stage1 name1 ( dec1 ': decs1 )
    stage2 name2 decs2
      = MatchingDecorations loc
          stage1 name2 decs1
          stage2 name2
            ( FindAndRemoveDecoration loc
                stage1 name1 dec1
                stage2 name2 decs2
            )

type family FindAndRemoveDecoration
              ( loc        :: (Nat, Nat)             )
              ( stage1     :: SPIRV.Shader           )
              ( name1      :: Symbol                 )
              ( dec1       :: SPIRV.Decoration Nat   )
              ( stage2     :: SPIRV.Shader           )
              ( name2      :: Symbol                 )
              ( decs2      :: [SPIRV.Decoration Nat] )
            :: [SPIRV.Decoration Nat]
            where
  FindAndRemoveDecoration _
    _ _ dec1
    _ _ (dec1 ': decs2)
      = decs2
  FindAndRemoveDecoration loc
    stage1 name1 dec1
    stage2 name2 (dec2 ': decs2)
      = dec2 ': FindAndRemoveDecoration loc
                  stage1 name1 dec1
                  stage2 name2 decs2
  FindAndRemoveDecoration loc
    stage1 name1 dec1
    stage2 name2 '[]
      = TypeError
          ( MismatchLocation loc
           :$$: Text "Output to " :<>: Text (NamedShader name1 stage1)
           :<>: Text " is decorated with " :<>: ShowType dec1 :<>: Text ","
           :$$: Text "but this decoration does not appear in the input of "
           :<>: Text (NamedShader name2 stage2) :<>: Text "."
          )

type family LocationDescriptionsOfStruct
      ( as :: [ LocationSlot Nat :-> Type ] )
    :: [ Nat :-> SPIRV.ImageFormat Nat ]
    where
  LocationDescriptionsOfStruct as = ComputeFormats (Slots as)

type family ComputeFormats
              ( slots :: [ LocationSlot Nat :-> SlotProvenance ] )
            :: [ Nat :-> SPIRV.ImageFormat Nat ]
            where
  ComputeFormats '[] = '[]
  ComputeFormats ( ( loc ':-> Provenance loc ( _ :: SKPrimTy ty ) scalar ) ': slots )
    = ComputeFormatsFromBaseSlot loc ty ( ScalarFromSScalar scalar ) 1 slots
  ComputeFormats ( ( loc ':-> Provenance loc' ( _ :: SKPrimTy ty ) _ ) ': _ )
    = TypeError
    (    Text "Internal error: slot provenance has not been encountered."
    :$$: Text ( "When computing format at " `AppendSymbol` ShowLocation loc `AppendSymbol` "," )
    :$$: Text "based off " :<>: ShowType ty
    :<>: Text ( " at " `AppendSymbol` ShowLocation loc `AppendSymbol` "." )
    )

-- | Accumulates all the locations until it finds a new base component
-- with component 0.
type family ComputeFormatsFromBaseSlot
              ( baseLoc    :: LocationSlot Nat )
              ( baseTy     :: Type             )
              ( baseScalar :: SPIRV.ScalarTy   )
              ( nbComps    :: Nat              )
              ( slots      :: Map (LocationSlot Nat) SlotProvenance )
            :: Map Nat (SPIRV.ImageFormat Nat)
            where
  ComputeFormatsFromBaseSlot baseLoc _ baseScalar nbComps '[]
    = '[ FormatFromComponents baseLoc baseScalar nbComps ]
  ComputeFormatsFromBaseSlot
    baseLoc _ baseScalar nbComps
    ( ( 'LocationSlot l 0 ':-> Provenance ('LocationSlot l 0 ) ( _ :: SKPrimTy newTy ) newScalar ) ': slots )
  -- found new base slot with component 0
      = FormatFromComponents baseLoc baseScalar nbComps
        ': ComputeFormatsFromBaseSlot
            ( 'LocationSlot l 0 )
            newTy
            ( ScalarFromSScalar newScalar )
            1
            slots
  ComputeFormatsFromBaseSlot
    baseLoc baseTy baseScalar nbComps
    ( ( loc ':-> Provenance otherBase ( _ :: SKPrimTy otherTy ) otherScalar ) ': slots )
      = ComputeFormatsFromBaseSlot
          baseLoc baseTy baseScalar
          ( SuccNbComponentsIfMatching
              baseLoc   baseTy  baseScalar nbComps
              otherBase otherTy ( ScalarFromSScalar otherScalar )
              loc
              ( baseScalar == ScalarFromSScalar otherScalar )
          )
          slots

type family SuccNbComponentsIfMatching
              ( baseLoc      :: LocationSlot Nat )
              ( baseTy       :: Type             )
              ( baseScalar   :: SPIRV.ScalarTy   )
              ( nbComps      :: Nat              )
              ( otherBase    :: LocationSlot Nat )
              ( otherTy      :: Type             )
              ( otherScalar  :: SPIRV.ScalarTy   )
              ( loc          :: LocationSlot Nat )
              ( equalScalars :: Bool             )
            :: Nat
            where
  SuccNbComponentsIfMatching
    baseLoc baseTy baseScalar _
    otherBase otherTy otherScalar
    loc
    'False
      = TypeError
      (    Text ( "Mismatched components at " `AppendSymbol` ShowLocation loc `AppendSymbol` ":" )
      :$$: Text "  - object of type " :<>: ShowType baseTy
      :$$: Text ( "    based at " `AppendSymbol` ShowLocation baseLoc `AppendSymbol` ":" )
      :$$: Text "    uses components of type " :<>: ShowType baseScalar :<>: Text ","
      :$$: Text "  - object of type " :<>: ShowType otherTy
      :$$: Text ( "    based at " `AppendSymbol` ShowLocation otherBase `AppendSymbol` ":" )
      :$$: Text "    uses components of type " :<>: ShowType otherScalar :<>: Text "."
      :$$: Text ""
      :$$: Text "Note that all components within a given location must use the same component type."
      )
  SuccNbComponentsIfMatching
    _ _ _ nbComps
    _ _ _
    _
    _
      = nbComps + 1

type family FormatFromComponents
              ( baseLoc    :: LocationSlot Nat )
              ( baseScalar :: SPIRV.ScalarTy   )
              ( nbComps    :: Nat              )
            :: ( Nat :-> SPIRV.ImageFormat Nat )
            where
  FormatFromComponents
    ('LocationSlot l c) scalarTy nbComps
      = l ':->
        FormatForComponentsOfWidth
          ( nbComps + c )
          ( ComponentOf scalarTy )
          ( SPIRV.WidthToNat (SPIRV.ScalarWidth scalarTy ) )

type family ComponentOf (scalar :: SPIRV.ScalarTy) :: SPIRV.Image.Component where
  ComponentOf (SPIRV.Integer SPIRV.Unsigned _) = SPIRV.Image.UI
  ComponentOf (SPIRV.Integer SPIRV.Signed   _) = SPIRV.Image.I
  ComponentOf (SPIRV.Floating _              ) = SPIRV.Image.F

type family FormatForComponentsOfWidth
              ( nbComponents :: Nat                   )
              ( component    :: SPIRV.Image.Component )
              ( width        :: Nat                   )
            :: ( SPIRV.ImageFormat Nat ) where
  FormatForComponentsOfWidth n comp w
    = 'SPIRV.ImageFormat comp ( Replicate (n `Div` (Max w 32 `Div` 32)) w )

type family StructLocationDescriptions
              ( bdNo :: Nat )
              ( as :: [LocationSlot Nat :-> Type] )
            :: VertexLocationDescriptions
            where
  StructLocationDescriptions bdNo as
    = AnnotateLocationsWithBindingAndOffset
        bdNo
        ( LocationDescriptionsOfStruct as )

type family AnnotateLocationsWithBindingAndOffset
              ( bdNo :: Nat )
              ( locationFormats :: [ Nat :-> SPIRV.ImageFormat Nat ] )
           :: [ Nat :-> ( Nat, Nat, SPIRV.ImageFormat Nat ) ]
           where
  AnnotateLocationsWithBindingAndOffset _ '[] = '[]
  AnnotateLocationsWithBindingAndOffset bdNo
    ( ( loc ':-> fmt ) ': locs )
      = ( loc ':-> '( bdNo, 16 * loc, fmt ) )
      ': AnnotateLocationsWithBindingAndOffset bdNo locs
