{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
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

{-|
Module: FIR.Pipeline

This module encodes the Vulkan concept of graphics pipelines
as a sequence of shader programs.

See "FIR.Validation.Pipeline" for the validation of such pipelines.
-}

module FIR.Pipeline
  ( PipelineInfo(..)
  , PipelineStages(..), pipelineStages
  , ShaderPipeline(..), pipelineShaders
  , PrimitiveConnectedness(..)
  , PrimitiveTopology(..)
  , BindingStrides
  , VertexLocationDescriptions
  , GetVertexInputInfo, GetTopologyInfo -- (re-exports)
  )
  where

-- base
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat, KnownNat )

-- fir
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.Map
  ( (:->) )
import FIR.Definition
  ( Variables )
import FIR.Module
  ( ShaderModule )
import FIR.ProgramState
  ( Definedness(Defined)
  , EntryPointInfo(EntryPointInfo)
  )
import FIR.Validation.Pipeline
  ( ValidPipelineInfo
  , GetExecutionInfo
  , GetVertexInputInfo
  , GetTopologyInfo
  )
import qualified SPIRV.Image as SPIRV
  ( ImageFormat )
import qualified SPIRV.Stage as SPIRV

--------------------------------------------------------------------------
-- * Primitive topologies

data PrimitiveConnectedness
  = List
  | Strip
  | Fan
  | AdjacencyList
  | AdjacencyStrip
  deriving stock ( Eq, Ord, Show, Enum, Bounded )

data PrimitiveTopology (n :: Type)
  = Points
  | Line          PrimitiveConnectedness
  | Triangle      PrimitiveConnectedness
  | PatchesOfSize n
  deriving stock ( Eq, Show )

instance Demotable PrimitiveConnectedness where
  type Demote PrimitiveConnectedness = PrimitiveConnectedness

instance Known PrimitiveConnectedness 'List where
  known = List
instance Known PrimitiveConnectedness 'Strip where
  known = Strip
instance Known PrimitiveConnectedness 'Fan where
  known = Fan
instance Known PrimitiveConnectedness 'AdjacencyList where
  known = AdjacencyList
instance Known PrimitiveConnectedness 'AdjacencyStrip where
  known = AdjacencyStrip

instance Demotable (PrimitiveTopology Nat) where
  type Demote (PrimitiveTopology Nat) = PrimitiveTopology Word32

instance Known (PrimitiveTopology Nat) 'Points where
  known = Points
instance Known PrimitiveConnectedness conn
      => Known (PrimitiveTopology Nat) ('Line conn) where
  known = Line ( knownValue @conn )
instance Known PrimitiveConnectedness conn
      => Known (PrimitiveTopology Nat) ('Triangle conn) where
  known = Triangle ( knownValue @conn )
instance KnownNat n => Known (PrimitiveTopology Nat) ('PatchesOfSize n) where
  known = PatchesOfSize ( knownValue @n )

-------------------------------------------------------
-- * Graphics pipelines

-- | For each location:
--   - a binding index,
--   - the offset (in bytes) within the buffer bound at that index,
--   - the image format of the location.
type VertexLocationDescriptions = [ Nat :-> ( Nat, Nat, SPIRV.ImageFormat Nat ) ]

-- | For each binding, the corresponding stride to use.
type BindingStrides = [ Nat :-> Nat ]

-- | Pipeline information data kind.
--
-- Records information about a pipeline which should be kept track of at the type level,
-- to be used for validation.
--
--   - Primitive topology used for the pipeline,
--   - layout information (vertex location descriptions, binding strides),
--   - type-level information associated to each shader in the pipeline,
--     such as the type of shader (vertex shader, geometry shader, ...)
--     and the input/output interfaces (for interface matching).
--
-- Represented as a snoc-list, beginning with vertex input information,
-- and then the sequence of shaders in order.
data PipelineInfo where
  VertexInputInfo
    :: PrimitiveTopology Nat
    -> VertexLocationDescriptions
    -> BindingStrides
    -> PipelineInfo
  Into
    :: PipelineInfo
    -> ( Symbol, EntryPointInfo )
    -> PipelineInfo

-- | Value-level sequence of pipeline stages.
--
-- Specified as a snoc-list:
--
-- > VertexInput @top @descs @strides :>-> (shader_1, filepath_1) :>-> ... (shader_n, filepath_n)
--
-- Keeps track of type-level information necessary for validation.
data PipelineStages (info :: PipelineInfo) (stageData :: Type) where
  VertexInput
    :: forall
          ( top       :: PrimitiveTopology Nat      )
          ( descs     :: VertexLocationDescriptions )
          ( strides   :: BindingStrides             )
          ( stageData :: Type                       )
    .  ( Known (PrimitiveTopology Nat)    top
       , Known VertexLocationDescriptions descs
       , Known BindingStrides             strides
       )
    => PipelineStages (VertexInputInfo top descs strides) stageData
  (:>->) :: ( Known SPIRV.Shader shader )
         => PipelineStages info stageData
         -> ( ShaderModule name shader defs endState, stageData )
         -> PipelineStages
              ( info `Into`
                '( name
                 , 'EntryPointInfo
                      ( GetExecutionInfo shader name endState )
                      ( Variables defs )
                      'Defined
                 )
              )
              stageData

deriving stock instance Functor     (PipelineStages info)
deriving stock instance Foldable    (PipelineStages info)
deriving stock instance Traversable (PipelineStages info)

-- | Smart wrapper for a shader pipeline,
-- performing type-level validation on a sequence of pipeline stages.
data ShaderPipeline (stageData :: Type) where
  ShaderPipeline
    :: forall
        ( info      :: PipelineInfo               )
        ( top       :: PrimitiveTopology Nat      )
        ( descs     :: VertexLocationDescriptions )
        ( strides   :: BindingStrides             )
        ( stageData :: Type                       )
    . ( ValidPipelineInfo info
      , '(top, descs, strides) ~ GetVertexInputInfo info
      , Known (PrimitiveTopology Nat)    top
      , Known VertexLocationDescriptions descs
      , Known BindingStrides             strides
      )
    => PipelineStages info stageData
    -> ShaderPipeline stageData

deriving stock instance Functor     ShaderPipeline
deriving stock instance Foldable    ShaderPipeline
deriving stock instance Traversable ShaderPipeline

pipelineStages :: PipelineStages info stageData -> [(SPIRV.Shader, stageData)]
pipelineStages = reverse . go []
  where
    go :: [(SPIRV.Shader, stageData)] -> PipelineStages info2 stageData -> [(SPIRV.Shader, stageData)]
    go shaders VertexInput = shaders
    go shaders ( info :>-> ( (_ :: ShaderModule name shader defs endState) , stageData) )
      = go ( (knownValue @shader, stageData) : shaders) info

pipelineShaders :: ShaderPipeline stageData -> [(SPIRV.Shader, stageData)]
pipelineShaders (ShaderPipeline stages) = pipelineStages stages
