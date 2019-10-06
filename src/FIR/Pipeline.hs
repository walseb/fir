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
Module: FIR.Pipeline

This module encodes the Vulkan concept of graphics pipelines
as a sequence of shader programs.

See "FIR.Validation.Pipeline" for the validation of such pipelines.
-}

module FIR.Pipeline
  ( PipelineInfo(..)
  , ShaderPipelineWithInfo(..), pipelineInfoShaders
  , ShaderPipeline(WithVertexInput), pipelineShaders
  , withVertexInput, withStructInput
  , ShaderStage(..)
  , PrimitiveConnectedness(..)
  , PrimitiveTopology(..)
  , BindingStrides
  , VertexLocationDescriptions
  )
  where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
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
  ( (:->)((:->)) )
import FIR.ASTState
  ( ASTState
  , Definedness(Defined)
  , EntryPointInfo(EntryPointInfo)
  )
import FIR.Definition
  ( Definition, StartState )
import FIR.Layout
  ( Layout(Locations)
  , Poke(SizeOf)
  )
import FIR.Prim.Struct
  ( Struct, LocationSlot )
import FIR.Program
  ( CodensityProgram )
import FIR.Validation.Pipeline
  ( ValidPipelineInfo
  , GetExecutionInfo
  , VariablesWithStorage
  , StructLocationDescriptions
  )
import qualified SPIRV.Image      as SPIRV
  ( ImageFormat )
import qualified SPIRV.Stage      as SPIRV
import qualified SPIRV.Storage    as SPIRV

--------------------------------------------------------------------------
-- * Graphics pipelines

data PrimitiveConnectedness
  = List
  | Strip
  | Fan
  | AdjacencyList
  | AdjacencyStrip
  deriving ( Eq, Ord, Show, Enum, Bounded )

data PrimitiveTopology (n :: Type)
  = Points
  | Line          PrimitiveConnectedness
  | Triangle      PrimitiveConnectedness
  | PatchesOfSize n

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

data PipelineInfo where
  TopOfPipe :: PipelineInfo
  Into :: PipelineInfo
       -> EntryPointInfo
       -> PipelineInfo

data ShaderStage
        ( name     :: Symbol                  )
        ( stage    :: SPIRV.Shader            )
        ( defs     :: [Symbol :-> Definition] )
        ( endState :: ASTState                )
        where
  ShaderStage :: forall name stage defs endState.
                 CodensityProgram (StartState defs) endState ()
              -> ShaderStage name stage defs endState

data ShaderPipelineWithInfo (info :: PipelineInfo) where
  StartPipeline :: ShaderPipelineWithInfo TopOfPipe
  (:>->) :: ( Known SPIRV.Shader shader )
         => ShaderPipelineWithInfo info
         -> (ShaderStage name shader defs endState, String)
         -> ShaderPipelineWithInfo
              ( info `Into`
                ( 'EntryPointInfo
                      name
                      ( GetExecutionInfo shader name endState )
                      '( VariablesWithStorage SPIRV.Input  defs
                       , VariablesWithStorage SPIRV.Output defs
                       )
                      'Defined
                )
              )

pipelineInfoShaders :: ShaderPipelineWithInfo info -> [(SPIRV.Shader, String)]
pipelineInfoShaders = reverse . go []
  where
    go :: [(SPIRV.Shader, String)] -> ShaderPipelineWithInfo info2 -> [(SPIRV.Shader, String)]
    go paths StartPipeline = paths
    go paths ( info :>-> ( (_ :: ShaderStage name shader defs endState) , path) )
      = go ( (knownValue @shader, path) : paths) info

type BindingStrides = [ Nat :-> Nat ]

-- | For each location:
--   - a binding index,
--   - the offset (in bytes) within the buffer bound at that index,
--   - the image format of the location.
type VertexLocationDescriptions = [ Nat :-> ( Nat, Nat, SPIRV.ImageFormat Nat ) ]

data ShaderPipeline where
  WithVertexInput
    :: forall
         ( bds   :: BindingStrides )
         ( descs :: VertexLocationDescriptions )
         ( top   :: PrimitiveTopology Nat )
         ( info  :: PipelineInfo )
    . ( ValidPipelineInfo info top
      --, ValidVertexInput (VertexInputs info) descs
      , Known [Nat :-> Nat] bds
      , Known VertexLocationDescriptions descs
      , Known (PrimitiveTopology Nat) top
      )
    => Proxy bds
    -> Proxy descs
    -> Proxy top
    -> ShaderPipelineWithInfo info
    -> ShaderPipeline

pipelineShaders :: ShaderPipeline -> [(SPIRV.Shader, String)]
pipelineShaders (WithVertexInput _ _ _ info) = pipelineInfoShaders info

withVertexInput
  :: forall
       ( bds   :: BindingStrides )
       ( descs :: VertexLocationDescriptions )
       ( top   :: PrimitiveTopology Nat )
       ( info  :: PipelineInfo )
  . ( ValidPipelineInfo info top
    --, ValidVertexInput (VertexInputs info) descs
    , Known [Nat :-> Nat] bds
    , Known VertexLocationDescriptions descs
    , Known (PrimitiveTopology Nat) top
    )
  => ShaderPipelineWithInfo info
  -> ShaderPipeline
withVertexInput = WithVertexInput @bds @descs @top @info Proxy Proxy Proxy

withStructInput
  :: forall
       ( as     :: [LocationSlot Nat :-> Type] )
       ( top    :: PrimitiveTopology Nat       )
       ( info   :: PipelineInfo                )
       ( descs  :: VertexLocationDescriptions  )
       ( stride :: Nat                         )
       ( bds    :: BindingStrides              )
  . ( ValidPipelineInfo info top
    , descs ~ StructLocationDescriptions 0 as
    , KnownNat stride
    , stride ~ ( SizeOf Locations (Struct as) )
    , bds ~ '[ 0 ':-> stride ]
    --, ValidVertexInput (VertexInputs info) descs
    , Known VertexLocationDescriptions descs
    , Known (PrimitiveTopology Nat) top
    )
  => ShaderPipelineWithInfo info -> ShaderPipeline
withStructInput = WithVertexInput @bds @descs @top @info Proxy Proxy Proxy
