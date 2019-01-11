{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

module SPIRV.Stage where

-- base
import Data.Proxy(Proxy)

-- fir
import Data.Binary.Class.Put(Put, PutWord32Enum(..))
import SPIRV.Capability(Capability)
import qualified SPIRV.Capability as Capability

-------------------------------------------------------
-- stages (called execution models in SPIR-V)

data Stage
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | GLCompute
  | Kernel
  deriving ( Show, Eq, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum Stage)

stageCapabilities :: Stage -> [ Capability ]
stageCapabilities Vertex                 = [ Capability.Shader ]
stageCapabilities TessellationControl    = [ Capability.Shader , Capability.Tessellation ]
stageCapabilities TessellationEvaluation = [ Capability.Shader , Capability.Tessellation ]
stageCapabilities Geometry               = [ Capability.Shader , Capability.Geometry     ]
stageCapabilities Fragment               = [ Capability.Shader ]
stageCapabilities GLCompute              = [ Capability.Shader ]
stageCapabilities Kernel                 = [ Capability.Kernel ]

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