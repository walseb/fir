{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module SPIRV.ExecutionMode where

-- base
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )

--------------------------------------------------
-- execution modes

data ExecutionMode a
  = Invocations a
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
  | DepthGreater
  | DepthLess
  | DepthUnchanged
  | LocalSize a a a
  | LocalSizeHint a a a
  | InputPoints
  | InputLines
  | InputLinesAdjacency
  | Triangles
  | InputTrianglesAdjacency
  | Quads
  | Isoline
  | OutputVertices a
  | OutputPoints
  | OutputLineStrip
  | OutputTriangleStrip
  | VecTypeHint a
  | ContractionOff
  deriving ( Show, Eq, Ord )

instance Put (ExecutionMode Word32) where
  put (Invocations i)         = put @Word32 0 *> put i
  put SpacingEqual            = put @Word32 1
  put SpacingFractionalEven   = put @Word32 2
  put SpacingFractionalOdd    = put @Word32 3
  put VertexOrderCw           = put @Word32 4
  put VertexOrderCcw          = put @Word32 5
  put PixelCenterInteger      = put @Word32 6
  put OriginUpperLeft         = put @Word32 7
  put OriginLowerLeft         = put @Word32 8
  put EarlyFragmentTests      = put @Word32 9
  put PointMode               = put @Word32 10
  put Xfb                     = put @Word32 11
  put DepthReplacing          = put @Word32 12
  put DepthGreater            = put @Word32 14
  put DepthLess               = put @Word32 15
  put DepthUnchanged          = put @Word32 16
  put (LocalSize     x y z)   = put @Word32 17 *> put x *> put y *> put z
  put (LocalSizeHint x y z)   = put @Word32 18 *> put x *> put y *> put z
  put InputPoints             = put @Word32 19
  put InputLines              = put @Word32 20
  put InputLinesAdjacency     = put @Word32 21
  put Triangles               = put @Word32 22
  put InputTrianglesAdjacency = put @Word32 23
  put Quads                   = put @Word32 24
  put Isoline                 = put @Word32 25
  put (OutputVertices i)      = put @Word32 26 *> put i
  put OutputPoints            = put @Word32 27
  put OutputLineStrip         = put @Word32 28
  put OutputTriangleStrip     = put @Word32 29
  put (VecTypeHint i)         = put @Word32 30 *> put i
  put ContractionOff          = put @Word32 31

  sizeOf Invocations    {} = 2
  sizeOf LocalSize      {} = 4
  sizeOf LocalSizeHint  {} = 4
  sizeOf OutputVertices {} = 2
  sizeOf VecTypeHint    {} = 2
  sizeOf _                 = 1

instance Demotable (ExecutionMode Nat) where
  type Demote (ExecutionMode Nat) = ExecutionMode Word32

instance Known Nat i => Known (ExecutionMode Nat) (Invocations i) where
  known = Invocations ( knownValue @i )
instance Known (ExecutionMode Nat) SpacingEqual where
  known = SpacingEqual
instance Known (ExecutionMode Nat) SpacingFractionalEven where
  known = SpacingFractionalEven
instance Known (ExecutionMode Nat) SpacingFractionalOdd where
  known = SpacingFractionalOdd
instance Known (ExecutionMode Nat) VertexOrderCw where
  known = VertexOrderCw
instance Known (ExecutionMode Nat) VertexOrderCcw where
  known = VertexOrderCcw
instance Known (ExecutionMode Nat) PixelCenterInteger where
  known = PixelCenterInteger
instance Known (ExecutionMode Nat) OriginUpperLeft where
  known = OriginUpperLeft
instance Known (ExecutionMode Nat) OriginLowerLeft where
  known = OriginLowerLeft
instance Known (ExecutionMode Nat) EarlyFragmentTests where
  known = EarlyFragmentTests
instance Known (ExecutionMode Nat) PointMode where
  known = PointMode
instance Known (ExecutionMode Nat) Xfb where
  known = Xfb
instance Known (ExecutionMode Nat) DepthReplacing where
  known = DepthReplacing
instance Known (ExecutionMode Nat) DepthGreater where
  known = DepthGreater
instance Known (ExecutionMode Nat) DepthLess where
  known = DepthLess
instance Known (ExecutionMode Nat) DepthUnchanged where
  known = DepthUnchanged
instance (Known Nat x, Known Nat y, Known Nat z)
        => Known (ExecutionMode Nat) (LocalSize x y z) where
  known = LocalSize
                    ( knownValue @x )
                    ( knownValue @y )
                    ( knownValue @z )
instance (Known Nat x, Known Nat y, Known Nat z)
        => Known (ExecutionMode Nat) (LocalSizeHint x y z) where
  known = LocalSizeHint
                    ( knownValue @x )
                    ( knownValue @y )
                    ( knownValue @z )
instance Known (ExecutionMode Nat) InputPoints where
  known = InputPoints
instance Known (ExecutionMode Nat) InputLines where
  known = InputLines
instance Known (ExecutionMode Nat) InputLinesAdjacency where
  known = InputLinesAdjacency
instance Known (ExecutionMode Nat) Triangles where
  known = Triangles
instance Known (ExecutionMode Nat) InputTrianglesAdjacency where
  known = InputTrianglesAdjacency
instance Known (ExecutionMode Nat) Quads where
  known = Quads
instance Known (ExecutionMode Nat) Isoline where
  known = Isoline
instance Known Nat i => Known (ExecutionMode Nat) (OutputVertices i) where
  known = OutputVertices ( knownValue @i )
instance Known (ExecutionMode Nat) OutputPoints where
  known = OutputPoints
instance Known (ExecutionMode Nat) OutputLineStrip where
  known = OutputLineStrip
instance Known (ExecutionMode Nat) OutputTriangleStrip where
  known = OutputTriangleStrip
instance Known Nat i => Known (ExecutionMode Nat) (VecTypeHint i) where
  known = VecTypeHint ( knownValue @i )
instance Known (ExecutionMode Nat) ContractionOff where
  known = ContractionOff

