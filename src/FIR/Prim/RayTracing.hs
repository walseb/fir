{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module: FIR.Prim.RayTracing

This module defines the opaque 'AccelerationStructure' and 'RayQuery' types,
to be used to refer to such objects in ray tracing operations.
-}

module FIR.Prim.RayTracing where

-- base
import Data.Word
  ( Word64 )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import Data.Type.Known
  ( Demotable(..), Known(..) )

------------------------------------------------------------

newtype AccelerationStructure = AccelerationStructureFromWord64 Word64
  deriving stock ( Show, Eq, Ord )
newtype RayQuery = RayQuery ShortText -- refer to the ray query by its name internally (not exported by the library)
  deriving stock ( Show, Eq, Ord )

data RayQueryState
  = RayQueryState
      ( Maybe RayQueryProceedState )
      ( Maybe RayQueryCommittedIntersection )
  deriving stock ( Show, Eq, Ord )

data RayQueryProceedState
  = Complete
  | Incomplete ( Maybe RayQueryCandidateIntersection )
  deriving stock ( Show, Eq, Ord )

data IntersectionType
  = Candidate
  | Committed
  deriving stock ( Show, Eq, Ord )

data RayQueryCandidateIntersection 
  = TriangleCandidateIntersection
  | AABBCandidateIntersection
  deriving stock ( Show, Eq, Ord )
data RayQueryCommittedIntersection
  = NoIntersection
  | TriangleIntersection
  | GeneratedIntersection
  deriving stock ( Show, Eq, Ord )

instance Demotable IntersectionType where
  type Demote IntersectionType = IntersectionType

instance Known IntersectionType Candidate where
  known = Candidate
instance Known IntersectionType Committed where
  known = Committed
