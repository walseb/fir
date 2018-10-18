{-# LANGUAGE GADTs #-}

module SPIRV.Types
  ( Width(..), width
  , Signedness(..), signedness
  , PrimTy(..)
  , Ty(..), tyAndSomeTyConArgs, ty
  , ExecutionModel(..)
  , ExecutionMode(..)
  ) where

-- base
import Data.Word(Word32)

--------------------------------------------------
-- SPIR-V types

data Width
  = W8
  | W16
  | W32
  | W64
  deriving ( Show, Eq, Ord, Enum, Bounded )

width :: Width -> Word32
width W8  = 8
width W16 = 16
width W32 = 32
width W64 = 64

data Signedness
  = Unsigned
  | Signed
  deriving ( Show, Eq, Ord, Enum, Bounded )

signedness :: Signedness -> Word32
signedness Unsigned = 0
signedness Signed   = 1

data PrimTy where
  Unit     ::                         PrimTy -- known as Void in the SPIR-V specification
  Boolean  ::                         PrimTy
  Integer  :: Signedness -> Width  -> PrimTy
  Floating ::               Width  -> PrimTy
  Vec      :: Int        -> PrimTy -> PrimTy
  Mat      :: Int -> Int -> PrimTy -> PrimTy
  -- todo: records, arrays, opaque types, ...
  deriving ( Show, Eq, Ord )

tyAndSomeTyConArgs :: PrimTy -> (Ty, [Word32])
tyAndSomeTyConArgs Unit          = (Void  , [ ] )
tyAndSomeTyConArgs Boolean       = (Bool  , [ ] )
tyAndSomeTyConArgs (Integer s w) = (Int   , [ width w, signedness s ] )
tyAndSomeTyConArgs (Floating  w) = (Float , [ width w] )
tyAndSomeTyConArgs (Vec   n   _) = (Vector, [ fromIntegral n ] ) -- element type is separate
tyAndSomeTyConArgs (Mat   _ m _) = (Matrix, [ fromIntegral m ] ) -- only number of columns... column type is separate

ty :: PrimTy -> Ty
ty = fst . tyAndSomeTyConArgs

--------------------------------------------------
-- execution

data ExecutionModel
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | GLCompute
  | Kernel
  deriving ( Eq, Show, Ord, Enum, Bounded )

data ExecutionMode
  = Invocations
  | SpacingEqual
  | SpacingFractionalEven
  | SpacingFractionalOdd
  | VertexOrderCw
  | VertexOrderCcw
  | PixelCenterInteger
  | OriginUpperLeft
  | OriginLowerLeft
  | EarlyFragmentTests
  | PointMode
  | Xfb
  | DepthReplacing
  | UNDEFINED_EXECUTION_MODE
  | DepthGreater
  | DepthLess
  | DepthUnchanged
  | LocalSize
  | LocalSizeHint
  | InputPoints
  | InputLines
  | InputLinesAdjacency
  | Triangles
  | InputTrianglesAdjacency
  | Quads
  | Isoline
  | OutputVertices
  | OutputPoints
  | OutputLineStrip
  | OutputTriangleStrip
  | VecTypeHint
  | ContractionOff
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
  | Image
  | Sampler
  | SampledImage
  | Array
  | RuntimeArray
  | Struct
  | Opaque
  | Pointer
  | Function
  | Event
  | DeviceEvent
  | ReserveId
  | Queue
  | Pipe
  | ForwardPointer
  deriving ( Show, Eq, Ord, Enum )