{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module SPIRV.ExecutionMode where

-- base
import Data.Proxy(Proxy(Proxy))
import Data.Word(Word32)
import GHC.TypeNats(KnownNat, natVal)

-- fir
import Data.Binary.Class.Put(Put(..))

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

class KnownExecutionMode (mode :: ExecutionMode a) where
  executionMode :: ExecutionMode Word32

instance KnownNat i => KnownExecutionMode (Invocations i) where
  executionMode = Invocations ( fromIntegral . natVal $ Proxy @i )
instance KnownExecutionMode SpacingEqual where
  executionMode = SpacingEqual
instance KnownExecutionMode SpacingFractionalEven where
  executionMode = SpacingFractionalEven
instance KnownExecutionMode SpacingFractionalOdd where
  executionMode = SpacingFractionalOdd
instance KnownExecutionMode VertexOrderCw where
  executionMode = VertexOrderCw
instance KnownExecutionMode VertexOrderCcw where
  executionMode = VertexOrderCcw
instance KnownExecutionMode PixelCenterInteger where
  executionMode = PixelCenterInteger
instance KnownExecutionMode OriginUpperLeft where
  executionMode = OriginUpperLeft
instance KnownExecutionMode OriginLowerLeft where
  executionMode = OriginLowerLeft
instance KnownExecutionMode EarlyFragmentTests where
  executionMode = EarlyFragmentTests
instance KnownExecutionMode PointMode where
  executionMode = PointMode
instance KnownExecutionMode Xfb where
  executionMode = Xfb
instance KnownExecutionMode DepthReplacing where
  executionMode = DepthReplacing
instance KnownExecutionMode DepthGreater where
  executionMode = DepthGreater
instance KnownExecutionMode DepthLess where
  executionMode = DepthLess
instance KnownExecutionMode DepthUnchanged where
  executionMode = DepthUnchanged
instance (KnownNat x, KnownNat y, KnownNat z)
        => KnownExecutionMode (LocalSize x y z) where
  executionMode = LocalSize
                    ( fromIntegral . natVal $ Proxy @x )
                    ( fromIntegral . natVal $ Proxy @y )
                    ( fromIntegral . natVal $ Proxy @z )
instance (KnownNat x, KnownNat y, KnownNat z)
        => KnownExecutionMode (LocalSizeHint x y z) where
  executionMode = LocalSizeHint
                    ( fromIntegral . natVal $ Proxy @x )
                    ( fromIntegral . natVal $ Proxy @y )
                    ( fromIntegral . natVal $ Proxy @z )
instance KnownExecutionMode InputPoints where
  executionMode = InputPoints
instance KnownExecutionMode InputLines where
  executionMode = InputLines
instance KnownExecutionMode InputLinesAdjacency where
  executionMode = InputLinesAdjacency
instance KnownExecutionMode Triangles where
  executionMode = Triangles
instance KnownExecutionMode InputTrianglesAdjacency where
  executionMode = InputTrianglesAdjacency
instance KnownExecutionMode Quads where
  executionMode = Quads
instance KnownExecutionMode Isoline where
  executionMode = Isoline
instance KnownNat i => KnownExecutionMode (OutputVertices i) where
  executionMode = OutputVertices ( fromIntegral . natVal $ Proxy @i )
instance KnownExecutionMode OutputPoints where
  executionMode = OutputPoints
instance KnownExecutionMode OutputLineStrip where
  executionMode = OutputLineStrip
instance KnownExecutionMode OutputTriangleStrip where
  executionMode = OutputTriangleStrip
instance KnownNat i => KnownExecutionMode (VecTypeHint i) where
  executionMode = VecTypeHint ( fromIntegral . natVal $ Proxy @i )
instance KnownExecutionMode ContractionOff where
  executionMode = ContractionOff



class KnownExecutionModes (modes :: [ExecutionMode a]) where
  executionModes :: [ExecutionMode Word32]

instance KnownExecutionModes '[] where
  executionModes = []

instance (KnownExecutionMode a, KnownExecutionModes as)
      => KnownExecutionModes (a ': as) where
  executionModes = executionMode @_ @a : executionModes @_ @as
