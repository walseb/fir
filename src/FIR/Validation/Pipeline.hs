{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

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
  ( type (&&) )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Error
  ( Assert )
import Data.Type.Map
  ( (:->)((:->)), Lookup )
import Data.Type.Maybe
  ( SequenceMaybe )
import Data.Type.String
  ( ShowNat )
import {-# SOURCE #-} FIR.Pipeline
  ( PrimitiveConnectedness(..), PrimitiveTopology(..)
  , PipelineInfo(VertexInputInfo, Into)
  , VertexLocationDescriptions, BindingStrides
  )
import FIR.ProgramState
  ( ProgramState(ProgramState)
  , TLInterface, TLInterfaceVariable
  , EntryPointInfo(EntryPointInfo)
  )
import FIR.Validation.Arrayness
  ( UnderlyingType )
import qualified SPIRV.Decoration as SPIRV
  ( Decoration(..) )
import qualified SPIRV.Stage      as SPIRV
import SPIRV.Stage
  ( NamedShader )
import qualified SPIRV.Storage    as SPIRV
  ( StorageClass(Input, Output) )

-----------------------------------------------------------------------------------------

-- | Check that a pipeline is valid:
--
--  - the first shader is a vertex shader,
--  - the last shader is a fragment shader,
--  - the ordering of the shaders is allowed,
--  - the interfaces between consecutive shaders match,
--  - the supplied primitive topology is valid.
type ValidPipelineInfo ( info :: PipelineInfo )
  = ( ValidPipelineInfoWithTopology info (GetTopologyInfo info) :: Constraint )

type ValidPipelineInfoWithTopology
        ( info :: PipelineInfo )
        ( top  :: PrimitiveTopology Nat )
  = ( (   StartsWithVertex   info
        , EndsWithFragment   info
        , MatchingInterfaces top info
        , ValidTopology      top
        ) :: Constraint )

type family GetVertexInputInfo ( info :: PipelineInfo )
             :: ( PrimitiveTopology Nat, VertexLocationDescriptions, BindingStrides )
             where
  GetVertexInputInfo
    ( VertexInputInfo top descs strides ) = '( top, descs, strides )
  GetVertexInputInfo
    ( pipe `Into` _ ) = GetVertexInputInfo pipe

type family GetTopologyInfo ( info :: PipelineInfo ) :: PrimitiveTopology Nat where
  GetTopologyInfo info = Proj31 (GetVertexInputInfo info)

type family Proj31 ( p :: (a,b,c) ) :: a where
  Proj31 '(a,_,_) = a

-- | Check that a pipeline starts with a vertex shader.
type family StartsWithVertex ( info :: PipelineInfo ) :: Constraint where
  StartsWithVertex
    ( VertexInputInfo _ _ _ `Into` '( _, 'EntryPointInfo (_ :: SPIRV.ExecutionInfo Nat SPIRV.Vertex) _ _ ) )
      = ()
  StartsWithVertex ( info `Into` _ )
    = StartsWithVertex info
  StartsWithVertex _
    = TypeError
        ( Text "Pipeline does not begin with a vertex shader." )

-- | Check that a pipeline ends with a fragment shader.
type family EndsWithFragment ( info :: PipelineInfo ) :: Constraint where
  EndsWithFragment
    ( _ `Into` '( _, 'EntryPointInfo (_ :: SPIRV.ExecutionInfo Nat SPIRV.Fragment) _ _ ) )
      = ()
  EndsWithFragment _
    = TypeError
        ( Text "Pipeline does not end with a fragment shader." )

-- | Check that the supplied primitive topology is valid.
type family ValidTopology (top :: PrimitiveTopology n) :: Constraint where
  ValidTopology (Line Fan)
    = TypeError ( Text "Invalid topology: fan of lines." )
  ValidTopology _
    = ()

-----------------------------------------------------------------------------------------
-- Validation of shader sequence / matching of interfaces.

-- | Check that consecutive shaders in the pipeline match up correctly:
--
--  - the sequence makes sense (e.g. can't have a tessellation shader following a geometry shader),
--  - the sequence is compatible with the supplied primitive topology,
--  - the interfaces match.
type family MatchingInterfaces
              ( top  :: PrimitiveTopology Nat )
              ( info :: PipelineInfo          )
            :: Constraint where
  MatchingInterfaces _ (VertexInputInfo _ _ _ ) = ()
  MatchingInterfaces _ (VertexInputInfo _ _ _ `Into` _) = ()
  MatchingInterfaces top
    (      pipelineStages
    `Into` pipelineStage1
    `Into` pipelineStage2
    )
      = ( ValidPipelineInterface top pipelineStage1 pipelineStage2
        , MatchingInterfaces top ( pipelineStages `Into` pipelineStage1 )
        )

---------------------------
-- Sequencing of shaders.

-- | Check that a given pair of shaders sequences correctly.
type family ValidPipelineInterface
              ( top    :: PrimitiveTopology Nat )
              ( stage1 :: (Symbol, EntryPointInfo) )
              ( stage2 :: (Symbol, EntryPointInfo) )
            :: Constraint
            where
  ValidPipelineInterface top
    '( name1,
        'EntryPointInfo
          ( 'SPIRV.ShaderExecutionInfo info1
              :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader1))
          )
          ifaceVars1
          _
    )
    '( name2,
        'EntryPointInfo
          ( 'SPIRV.ShaderExecutionInfo info2
              :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader2))
          )
          ifaceVars2
          _
    )
      = ( MatchingInterface
          name1 info1 ( SequenceMaybe ( Lookup SPIRV.Output ifaceVars1 ) )
          name2 info2 ( SequenceMaybe ( Lookup SPIRV.Input  ifaceVars2 ) )
        , ValidSequence top
            name1 shader1 info1
            name2 shader2 info2
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
              ( name1   :: Symbol                       )
              ( shader1 :: SPIRV.Shader                 )
              ( info1   :: SPIRV.ShaderInfo Nat shader1 )
              ( name2   :: Symbol                       )
              ( shader2 :: SPIRV.Shader                 )
              ( info2   :: SPIRV.ShaderInfo Nat shader2 )
            :: Constraint where
  ValidSequence ('PatchesOfSize n)
    _ SPIRV.VertexShader               _
    _ SPIRV.TessellationControlShader  _
      = ()
  ValidSequence top
    _ SPIRV.VertexShader               _
    _ SPIRV.TessellationControlShader  _
      = TypeError
        (    Text "Primitive topology is " :<>: ShowType top :<>: Text ","
        :$$: Text "which cannot be used in the presence of tessellation shaders."
        )
  ValidSequence ('PatchesOfSize n)
    _ SPIRV.VertexShader   _
    _ SPIRV.GeometryShader _
      = TypeError
        (    Text "Primitive topology is 'Patches' – "
        :<>: Text "which requires the use of tessellation – "
        :$$: Text "but the supplied shader pipeline does not contain\
                  \ any tessellation shaders."
        )
  ValidSequence top
    _     SPIRV.VertexShader   _
    name2 SPIRV.GeometryShader geomInfo
      = CompatibleGeometryTopology top 'Nothing name2 geomInfo
  ValidSequence ('PatchesOfSize n)
    _ SPIRV.VertexShader   _
    _ SPIRV.FragmentShader _
      = TypeError
        (    Text "Primitive topology is 'Patches' – "
        :<>: Text "which requires the use of tessellation – "
        :$$: Text "but the supplied shader pipeline does not contain\
                  \ any tessellation shaders."
        )
  ValidSequence _
    _ SPIRV.VertexShader   _
    _ SPIRV.FragmentShader _
      = ()
  ValidSequence _
    name1 SPIRV.TessellationControlShader    info1
    name2 SPIRV.TessellationEvaluationShader info2
      = CompatibleTessellationStages name1 info1 name2 info2
  ValidSequence top
    name1 SPIRV.TessellationEvaluationShader info1
    name2 SPIRV.GeometryShader               info2
      = CompatibleGeometryTopology top (Just '(name1, info1)) name2 info2
  ValidSequence _
    _ SPIRV.TessellationEvaluationShader _
    _ SPIRV.FragmentShader               _
      = ()
  ValidSequence _
    _ SPIRV.GeometryShader _
    _ SPIRV.FragmentShader _
      = ()
  ValidSequence _ name1 shader1 _ name2 shader2 _
    = TypeError
        ( Text "Invalid pipeline:"
         :$$: Text (NamedShader name2 shader2) :<>: Text " cannot follow "
         :<>: Text (NamedShader name1 shader1) :<>: Text "."
        )

type family CompatibleGeometryTopology
              ( top :: PrimitiveTopology Nat )
              ( mbTess :: Maybe
                  ( Symbol, SPIRV.ShaderInfo Nat SPIRV.TessellationEvaluationShader )
              )
              ( geomName :: Symbol )
              ( geom :: SPIRV.ShaderInfo Nat SPIRV.GeometryShader )
            :: Constraint
            where
  CompatibleGeometryTopology
    ( 'PatchesOfSize n )
    ( 'Just '( teseName, SPIRV.TessellationEvaluationShaderInfo _ teseMode ) )
    geomName
    ( SPIRV.GeometryShaderInfo _ geomMode )
      = ( CompatibleTessellationGeometryModes teseName teseMode geomName geomMode )
  CompatibleGeometryTopology
    top
    'Nothing
    geomName
    ( SPIRV.GeometryShaderInfo _ geomMode )
      = ( CompatibleBasicGeometryTopology top geomName geomMode )


-- | Check that the Geometry shader is getting the correct primitive topology,
-- in a pipeline without any tessellation.
type family CompatibleBasicGeometryTopology
                ( top      :: PrimitiveTopology Nat   )
                ( geomName :: Symbol                  )
                ( geomMode :: SPIRV.GeometryInputMode )
              :: Constraint
              where
  CompatibleBasicGeometryTopology
    Points
    _
    SPIRV.InputModePoints
      = ()
  CompatibleBasicGeometryTopology
    (Line List)
    _
    SPIRV.InputModeLines
      = ()
  CompatibleBasicGeometryTopology
    (Line AdjacencyStrip)
    _
    SPIRV.InputModeLinesAdjacency
      = ()
  CompatibleBasicGeometryTopology
    (Triangle List)
    _
    SPIRV.InputModeTriangles
      = ()
  CompatibleBasicGeometryTopology
    (Triangle Strip)
    _
    SPIRV.InputModeTriangles
      = ()
  CompatibleBasicGeometryTopology
    (Triangle Fan)
    _
    SPIRV.InputModeTriangles
      = ()
  CompatibleBasicGeometryTopology
    (Triangle AdjacencyList)
    _
    SPIRV.InputModeTrianglesAdjacency
      = ()
  CompatibleBasicGeometryTopology
    (Triangle AdjacencyStrip)
    _
    SPIRV.InputModeTrianglesAdjacency
      = ()
  CompatibleBasicGeometryTopology
    top
    geomName
    geomMode
      = TypeError
      (    Text (NamedShader geomName SPIRV.GeometryShader)
      :<>: Text " input mode " :<>: Text (SPIRV.ShowGeometryInputMode geomMode)
      :$$: Text "is incompatible with the primitive topology "
      :<>: ShowType top :<>: Text "."
      )

-- | Check that the Geometry shader is getting the correct primitive topology,
-- in a pipeline which does contain tessellation.
type family CompatibleTessellationGeometryModes
              ( teseName :: Symbol                  )
              ( teseMode :: SPIRV.TessellationMode  )
              ( geomName :: Symbol                  )
              ( geomMode :: SPIRV.GeometryInputMode )
            :: Constraint
            where
  CompatibleTessellationGeometryModes
    _ SPIRV.ModePoints
    _ SPIRV.InputModePoints
      = ()
  CompatibleTessellationGeometryModes
    _ SPIRV.ModeIsolines
    _ SPIRV.InputModeLines
      = ()
  CompatibleTessellationGeometryModes
    _ SPIRV.ModeTriangles
    _ SPIRV.InputModeTriangles
      = ()
  CompatibleTessellationGeometryModes
    _ SPIRV.ModeQuads
    _ SPIRV.InputModeTriangles
      = ()
  CompatibleTessellationGeometryModes
    teseName teseMode
    geomName geomMode
      = TypeError
        (    Text (NamedShader geomName SPIRV.GeometryShader)
        :<>: Text " input mode " :<>: Text (SPIRV.ShowGeometryInputMode geomMode)
        :$$: Text "is incompatible with the preceding "
        :<>: Text (NamedShader teseName SPIRV.TessellationEvaluationShader)
        :<>: Text " output mode "
        :<>: Text (SPIRV.ShowTessellationMode teseMode) :<>: Text "."
        )

-- | Check that the two tessellation stages are using the same tessellation mode.
type family CompatibleTessellationStages
              ( tescName :: Symbol )
              ( tesc :: SPIRV.ShaderInfo Nat SPIRV.TessellationControlShader    )
              ( teseName :: Symbol )
              ( tese :: SPIRV.ShaderInfo Nat SPIRV.TessellationEvaluationShader )
            :: Constraint
            where
  CompatibleTessellationStages
    _ ( SPIRV.TessellationControlShaderInfo  _ _ 'Nothing )
    _ ( SPIRV.TessellationEvaluationShaderInfo _ _        )
      = ()
  CompatibleTessellationStages
    _ ( SPIRV.TessellationControlShaderInfo  _ _ ('Just mode) )
    _ ( SPIRV.TessellationEvaluationShaderInfo _ mode         )
      = ()
  CompatibleTessellationStages
    tescName ( SPIRV.TessellationControlShaderInfo  _ _ ('Just tescMode) )
    teseName ( SPIRV.TessellationEvaluationShaderInfo _ teseMode         )
      = TypeError
        (    Text "Conflicting tessellation modes:"
        :$$: Text "  - " :<>: Text (NamedShader tescName SPIRV.TessellationControlShader)
        :<>: Text" specifies "
        :<>: Text (SPIRV.ShowTessellationMode tescMode) :<>: Text ","
        :$$: Text "  - " :<>: Text (NamedShader teseName SPIRV.TessellationEvaluationShader)
        :<>: Text" specifies "
        :<>: Text (SPIRV.ShowTessellationMode teseMode) :<>: Text "."
        )


type family GetExecutionInfo
              ( shader :: SPIRV.Shader )
              ( name   :: Symbol       )
              ( state  :: ProgramState )
          :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader))
          where
  GetExecutionInfo shader name ('ProgramState _ _ _ _ eps _ _ _)
    = GetExecutionInfoOf shader name eps

type family GetExecutionInfoOf
             ( shader :: SPIRV.Shader )
             ( name   :: Symbol       )
             ( eps    :: [ Symbol :-> EntryPointInfo ] )
          :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader))
          where
  GetExecutionInfoOf shader name '[]
    = TypeError
        (      Text "Module does not define a "
          :<>: Text (NamedShader name shader)
          :<>: Text "."
        )
  GetExecutionInfoOf shader name
    ( ( name ':->
        ( 'EntryPointInfo
           ( nfo :: SPIRV.ExecutionInfo Nat ('SPIRV.Stage ('SPIRV.ShaderStage shader)) )
           _
           _
        )
      )
    ': _
    )
      = nfo
  GetExecutionInfoOf shader name ( _ ': eps )
    = GetExecutionInfoOf shader name eps

type family GetInterface
              ( shader :: SPIRV.Shader )
              ( name   :: Symbol       )
              ( state  :: ProgramState )
          :: TLInterface
          where
  GetInterface shader name ('ProgramState _ _ _ _ eps _ _ _)
    = GetInterfaceOf shader name eps

type family GetInterfaceOf
              ( shader :: SPIRV.Shader )
              ( name   :: Symbol       )
              ( eps    :: [ Symbol :-> EntryPointInfo ] )
          :: TLInterface
          where
  GetInterfaceOf shader name '[]
    = TypeError
        (      Text "Module does not define a "
          :<>: Text (NamedShader name shader)
          :<>: Text "."
        )
  GetInterfaceOf shader name
    ( ( name ':->
        ( 'EntryPointInfo
           _
           iface
           _
        )
      )
    ': _
    )
      = iface
  GetInterfaceOf shader name ( _ ': eps )
    = GetInterfaceOf shader name eps

---------------------------
-- Interface matching.

type family MatchingInterface
              ( stageName1 :: Symbol                           )
              ( stageInfo1 :: SPIRV.ShaderInfo Nat stage1      )
              ( outputs    :: [Symbol :-> TLInterfaceVariable] )
              ( stageName2 :: Symbol                           )
              ( stageInfo2 :: SPIRV.ShaderInfo Nat stage2      )
              ( inputs     :: [Symbol :-> TLInterfaceVariable] )
            :: Constraint
            where
  MatchingInterface
    _ _ '[]
    _ _ '[]
      = ()
  MatchingInterface
    stageName1 ( stageInfo1 :: SPIRV.ShaderInfo Nat stage1 ) '[]
    stageName2 ( stageInfo2 :: SPIRV.ShaderInfo Nat stage2 ) ( ( inputName ':-> '(_, ty2) ) ': _ )
      = TypeError
          (      Text "Input " :<>: ShowType inputName
            :<>: Text " of type " :<>: ShowType ty2
            :<>: Text " of " :<>: Text ( NamedShader stageName1 stage2 ) :<>: Text ","
            :$$: Text "does not match any output of "
            :<>: Text (NamedShader stageName2 stage1) :<>: Text "."
          )
  MatchingInterface
    stageName1 stageInfo1 ( ( outputName ':-> output ) ': outputs )
    stageName2 stageInfo2 inputs
      = MatchingInterface
          stageName1 stageInfo1 outputs
          stageName2 stageInfo2
            ( RemoveMatch
                stageName1 stageInfo1 outputName output
                stageName2 stageInfo2            inputs
            )

type family RemoveMatch
              ( stageName1 :: Symbol                            )
              ( stageInfo1 :: SPIRV.ShaderInfo Nat stage1       )
              ( outputName :: Symbol                            )
              ( output     :: TLInterfaceVariable               )
              ( stageName2 :: Symbol                            )
              ( stageInfo2 :: SPIRV.ShaderInfo Nat stage2       )
              ( inputs     :: [ Symbol :-> TLInterfaceVariable] )
            :: [ Symbol :-> TLInterfaceVariable]
            where
  RemoveMatch
    stageName1 ( stageInfo1 :: SPIRV.ShaderInfo Nat stage1 ) outputName '(_, ty1)
    stageName2 ( stageInfo2 :: SPIRV.ShaderInfo Nat stage2 )            '[]
      = TypeError
          (      Text "Output " :<>: ShowType outputName
            :<>: Text " of type " :<>: ShowType ty1
            :<>: Text " of " :<>: Text ( NamedShader stageName1 stage1 ) :<>: Text ","
            :$$: Text "does not match any input of "
            :<>: Text (NamedShader stageName2 stage2) :<>: Text "."
          )
  RemoveMatch
    stageName1 stageInfo1 outputName '(decs1, ty1)
    stageName2 stageInfo2            ( ( inputName ':-> '(decs2, ty2) ) ': inputs )
    = RemoveThisMatchOrContinue
        ( SameLocation
            (Location decs1) (Component decs1)
            (Location decs2) (Component decs2)
        )
        stageName1 stageInfo1 outputName '(decs1, ty1)
        stageName2 stageInfo2 inputName  '(decs2, ty2)
        inputs

type family RemoveThisMatchOrContinue
              ( mbLocation :: Maybe (Nat, Nat)                  )
              ( stageName1 :: Symbol                            )
              ( stageInfo1 :: SPIRV.ShaderInfo Nat shader1      )
              ( outputName :: Symbol                            )
              ( output     :: TLInterfaceVariable               )
              ( stageName2 :: Symbol                            )
              ( stageInfo2 :: SPIRV.ShaderInfo Nat shader2      )
              ( inputName  :: Symbol                            )
              ( input      :: TLInterfaceVariable               )
              ( inputs     :: [ Symbol :-> TLInterfaceVariable] )
          :: [ Symbol :-> TLInterfaceVariable ]
          where
  RemoveThisMatchOrContinue (Just loc)
    stageName1 stageInfo1 outputName output
    stageName2 stageInfo2 inputName  input  inputs
      = Assert
          ( Matches loc
              stageName1 stageInfo1 outputName output
              stageName2 stageInfo2 inputName  input
          )
          inputs
  RemoveThisMatchOrContinue Nothing
    stageName1 stageInfo1 outputName output
    stageName2 stageInfo2 inputName  input  inputs
      = ( inputName ':-> input ) ':
          RemoveMatch
            stageName1 stageInfo1 outputName output
            stageName2 stageInfo2            inputs

type family Matches
              ( location   :: (Nat,Nat)                    )
              ( stageName1 :: Symbol                       )
              ( stage1     :: SPIRV.ShaderInfo Nat shader1 )
              ( outputName :: Symbol                       )
              ( output     :: TLInterfaceVariable          )
              ( stageName2 :: Symbol                       )
              ( stage2     :: SPIRV.ShaderInfo Nat shader2 )
              ( inputName  :: Symbol                       )
              ( input      :: TLInterfaceVariable          )
            :: Bool
            where
  Matches loc
    stageName1 ( stageInfo1 :: SPIRV.ShaderInfo Nat stage1 )
    outputName '(decs1, ty1)
    stageName2 ( stageInfo2 :: SPIRV.ShaderInfo Nat stage2 )
    inputName  '(decs2, ty2)
      =   MatchingUnderlyingTypes loc
            stageName1 stage1
            outputName
            ( UnderlyingType outputName SPIRV.Output decs1 stageName1 ty1
                ( SPIRV.ShaderExecutionInfo stageInfo1 )
            )
            stageName2 stage2
            inputName
            ( UnderlyingType inputName  SPIRV.Input  decs2 stageName2 ty2
                ( SPIRV.ShaderExecutionInfo stageInfo2 )
            )
      &&  MatchingDecorations loc
            stageName1 stage1 outputName decs1
            stageName2 stage2 inputName  decs2

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

type family MatchingUnderlyingTypes
              ( loc        :: (Nat, Nat)   )
              ( stageName1 :: Symbol       )
              ( stage1     :: SPIRV.Shader )
              ( varName1   :: Symbol       )
              ( u_type1    :: Type         )
              ( stageName2 :: Symbol       )
              ( stage2     :: SPIRV.Shader )
              ( varName2   :: Symbol       )
              ( u_type2    :: Type         )
            :: Bool
            where
  MatchingUnderlyingTypes _
    _ _ _ u_ty1
    _ _ _ u_ty1
      = 'True
  MatchingUnderlyingTypes loc
    stageName1 stage1 varName1 u_ty1
    stageName2 stage2 varName2 u_ty2
    = TypeError
        (    MismatchLocation loc
        :$$: Text "Output named " :<>: ShowType varName1
        :<>: Text " of " :<>: Text (NamedShader stageName1 stage1)
        :<>: Text " has underlying type " :<>: ShowType u_ty1 :<>: Text ","
        :$$: Text "whereas input named " :<>: ShowType varName2
        :<>: Text " of " :<>: Text (NamedShader stageName2 stage2)
        :<>: Text " has underlying type " :<>: ShowType u_ty2 :<>: Text "."
        )

type family MismatchLocation ( loc :: (Nat,Nat) ) :: ErrorMessage where
  MismatchLocation '(loc, 0)
    =    Text "Interface mismatch at location " :<>: Text (ShowNat loc)
    :<>: Text "."
  MismatchLocation '(loc, comp)
    =    Text "Interface mismatch at location " :<>: Text (ShowNat loc)
    :<>: Text ", component " :<>: Text (ShowNat loc) :<>: Text "."

type family MatchingDecorations
              ( loc        :: (Nat, Nat)             )
              ( stageName1 :: Symbol                 )
              ( stage1     :: SPIRV.Shader           )
              ( inputName  :: Symbol                 )
              ( decs1      :: [SPIRV.Decoration Nat] )
              ( stageName2 :: Symbol                 )
              ( stage2     :: SPIRV.Shader           )
              ( outputName :: Symbol                 )
              ( decs2      :: [SPIRV.Decoration Nat] )
            :: Bool
            where
  MatchingDecorations _
    _ _ _ '[]
    _ _ _ '[]
      = 'True
  MatchingDecorations loc
    stageName1 stage1 outputName '[]
    stageName2 stage2 inputName  (dec2 ': _)
      = TypeError
          ( MismatchLocation loc
           :$$: Text "Input named " :<>: ShowType inputName
           :<>: Text " to " :<>: Text (NamedShader stageName2 stage2)
           :<>: Text " is decorated with " :<>: ShowType dec2 :<>: Text ","
           :$$: Text "but matching output " :<>: ShowType outputName
           :<>: Text " to " :<>: Text (NamedShader stageName1 stage1)
           :<>: Text " is missing such a decoration."
          )
  MatchingDecorations loc
    stageName1 stage1 outputName ( dec1 ': decs1 )
    stageName2 stage2 inputName  decs2
      = MatchingDecorations loc
          stageName1 stage1 outputName decs1
          stageName2 stage2 inputName
            ( FindAndRemoveDecoration loc
                stageName1 stage1 outputName dec1
                stageName2 stage2 inputName  decs2
            )

type family FindAndRemoveDecoration
              ( loc        :: (Nat, Nat)             )
              ( stageName1 :: Symbol                 )
              ( stage1     :: SPIRV.Shader           )
              ( outputName :: Symbol                 )
              ( dec1       :: SPIRV.Decoration Nat   )
              ( stageName2 :: Symbol                 )
              ( stage2     :: SPIRV.Shader           )
              ( inputName  :: Symbol                 )
              ( decs2      :: [SPIRV.Decoration Nat] )
            :: [SPIRV.Decoration Nat]
            where
  FindAndRemoveDecoration _
    _ _ _ dec1
    _ _ _ (dec1 ': decs2)
      = decs2
  FindAndRemoveDecoration loc
    stageName1 stage1 outputName  dec1
    stageName2 stage2 inputName   (dec2 ': decs2)
      = dec2 ': FindAndRemoveDecoration loc
                  stageName1 stage1 outputName dec1
                  stageName2 stage2 inputName  decs2
  FindAndRemoveDecoration loc
    stageName1 stage1 outputName dec1
    stageName2 stage2 inputName  '[]
      = TypeError
          ( MismatchLocation loc
           :$$: Text "Output named " :<>: ShowType outputName
           :<>: Text " to " :<>: Text (NamedShader stageName1 stage1)
           :<>: Text " is decorated with " :<>: ShowType dec1 :<>: Text ","
           :$$: Text "but matching input " :<>: ShowType inputName
           :<>: Text " to " :<>: Text (NamedShader stageName2 stage2)
           :<>: Text " is missing such a decoration."
          )
