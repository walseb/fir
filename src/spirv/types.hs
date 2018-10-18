{-# LANGUAGE GADTs #-}

module SPIRV.Types
  ( Width(..)
  , Signedness(..)
  , PrimTy(..)
  , Ty(..)
  , ExecutionModel(..)
  ) where

--------------------------------------------------
-- SPIR-V types

data Width
  = W8
  | W16
  | W32
  | W64
  deriving ( Show, Eq, Ord, Enum, Bounded )

data Signedness
  = Unsigned
  | Signed
  deriving ( Show, Eq, Ord, Enum, Bounded )

data PrimTy where
  Unit     ::                         PrimTy -- known as Void in the SPIR-V specification
  Boolean  ::                         PrimTy
  Integer  :: Signedness -> Width  -> PrimTy
  Floating ::               Width  -> PrimTy
  Vec      :: Int        -> PrimTy -> PrimTy
  Mat      :: Int -> Int -> PrimTy -> PrimTy
  -- todo: records, arrays, opaque types, ...
  deriving ( Show, Eq, Ord )

--------------------------------------------------
-- execution models

data ExecutionModel
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | GLCompute
  | Kernel
  deriving ( Eq, Show, Ord, Enum, Bounded )

--------------------------------------------------
-- SPIR-V type constructors

data Ty
  = Void
  | Bool
  | Int
  | Float
  | Vector
  | Matrix
  -- | Image
  -- | Sampler
  -- | SampledImage
  -- | Array
  -- | RuntimeArray
  -- | Struct
  -- | Opaque
  -- | Pointer
  | Function
  -- | Event
  -- | DeviceEvent
  -- | ReserveId
  -- | Queue
  -- | Pipe
  -- | ForwardPointer
  deriving Show