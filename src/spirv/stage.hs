{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module SPIRV.Stage where

-- fir
import Data.Binary.Class.Put
  ( Put, PutWord32Enum(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known) )
import SPIRV.Capability
  ( Capability )
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

instance Demotable Stage where
  type Demote Stage = Stage

instance Known Stage Vertex where
  known = Vertex
instance Known Stage TessellationControl where
  known = TessellationControl
instance Known Stage TessellationEvaluation where
  known = TessellationEvaluation
instance Known Stage Geometry where
  known = Geometry
instance Known Stage Fragment where
  known = Fragment
instance Known Stage GLCompute where
  known = GLCompute
instance Known Stage Kernel where
  known = Kernel
