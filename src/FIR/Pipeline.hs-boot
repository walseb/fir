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

module FIR.Pipeline
  ( PipelineInfo(..)
  , PrimitiveConnectedness(..)
  , PrimitiveTopology(..)
  , VertexLocationDescriptions
  )
  where

-- base
import Data.Kind
  ( Type )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Map
  ( (:->) )
import FIR.ASTState
  ( EntryPointInfo )
import qualified SPIRV.Image as SPIRV

--------------------------------------------------------------------------

data PrimitiveConnectedness
  = List
  | Strip
  | Fan
  | AdjacencyList
  | AdjacencyStrip

data PrimitiveTopology (n :: Type)
  = Points
  | Line          PrimitiveConnectedness
  | Triangle      PrimitiveConnectedness
  | PatchesOfSize n

data PipelineInfo where
  TopOfPipe :: PipelineInfo
  Into :: PipelineInfo
       -> EntryPointInfo
       -> PipelineInfo

type VertexLocationDescriptions = [ Nat :-> ( Nat, Nat, SPIRV.ImageFormat Nat ) ]
