{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

module SPIRV.ExecutionMode where

-- base
import Data.Word(Word32)

-- binary
import Data.Binary(Binary)

--------------------------------------------------

newtype ExecutionModel = ExecutionModel Word32
  deriving ( Eq, Binary )

newtype ExecutionMode = ExecutionMode Word32
  deriving ( Eq, Binary )

instance Show ExecutionModel where
  show executionModel = "ExecutionModel " ++ showExecutionModel executionModel

instance Show ExecutionMode where
  show executionMode = "ExecutionMode " ++ showExecutionMode executionMode


-- execution models

pattern Vertex :: ExecutionModel
pattern Vertex = ExecutionModel 0

pattern TessellationControl :: ExecutionModel
pattern TessellationControl = ExecutionModel 1

pattern TessellationEvaluation :: ExecutionModel
pattern TessellationEvaluation = ExecutionModel 2

pattern Geometry :: ExecutionModel
pattern Geometry = ExecutionModel 3

pattern Fragment :: ExecutionModel
pattern Fragment = ExecutionModel 4

pattern GLCompute :: ExecutionModel
pattern GLCompute = ExecutionModel 5

pattern Kernel :: ExecutionModel
pattern Kernel = ExecutionModel 6


showExecutionModel :: ExecutionModel -> String
showExecutionModel Vertex = "Vertex"
showExecutionModel TessellationControl = "TessellationControl"
showExecutionModel TessellationEvaluation = "TessellationEvaluation"
showExecutionModel Geometry = "Geometry"
showExecutionModel Fragment = "Fragment"
showExecutionModel GLCompute = "GLCompute"
showExecutionModel Kernel = "Kernel"
showExecutionModel (ExecutionModel i) = show i


-- execution modes

pattern Invocations :: ExecutionMode
pattern Invocations = ExecutionMode 0

pattern SpacingEqual :: ExecutionMode
pattern SpacingEqual = ExecutionMode 1

pattern SpacingFractionalEven :: ExecutionMode
pattern SpacingFractionalEven = ExecutionMode 2

pattern SpacingFractionalOdd :: ExecutionMode
pattern SpacingFractionalOdd = ExecutionMode 3

pattern VertexOrderCw :: ExecutionMode
pattern VertexOrderCw = ExecutionMode 4

pattern VertexOrderCcw :: ExecutionMode
pattern VertexOrderCcw = ExecutionMode 5

pattern PixelCenterInteger :: ExecutionMode
pattern PixelCenterInteger = ExecutionMode 6

pattern OriginUpperLeft :: ExecutionMode
pattern OriginUpperLeft = ExecutionMode 7

pattern OriginLowerLeft :: ExecutionMode
pattern OriginLowerLeft = ExecutionMode 8

pattern EarlyFragmentTests :: ExecutionMode
pattern EarlyFragmentTests = ExecutionMode 9

pattern PointMode :: ExecutionMode
pattern PointMode = ExecutionMode 10

pattern Xfb :: ExecutionMode
pattern Xfb = ExecutionMode 11

pattern DepthReplacing :: ExecutionMode
pattern DepthReplacing = ExecutionMode 12

-- no 13

pattern DepthGreater :: ExecutionMode
pattern DepthGreater = ExecutionMode 14

pattern DepthLess :: ExecutionMode
pattern DepthLess = ExecutionMode 15

pattern DepthUnchanged :: ExecutionMode
pattern DepthUnchanged = ExecutionMode 16

pattern LocalSize :: ExecutionMode
pattern LocalSize = ExecutionMode 17

pattern LocalSizeHint :: ExecutionMode
pattern LocalSizeHint = ExecutionMode 18

pattern InputPoints :: ExecutionMode
pattern InputPoints = ExecutionMode 19

pattern InputLines :: ExecutionMode
pattern InputLines = ExecutionMode 20

pattern InputLinesAdjacency :: ExecutionMode
pattern InputLinesAdjacency = ExecutionMode 21

pattern Triangles :: ExecutionMode
pattern Triangles = ExecutionMode 22

pattern InputTrianglesAdjacency :: ExecutionMode
pattern InputTrianglesAdjacency = ExecutionMode 23

pattern Quads :: ExecutionMode
pattern Quads = ExecutionMode 24

pattern Isoline :: ExecutionMode
pattern Isoline = ExecutionMode 25

pattern OutputVertices :: ExecutionMode
pattern OutputVertices = ExecutionMode 26

pattern OutputPoints :: ExecutionMode
pattern OutputPoints = ExecutionMode 27

pattern OutputLineStrip :: ExecutionMode
pattern OutputLineStrip = ExecutionMode 28

pattern OutputTriangleStrip :: ExecutionMode
pattern OutputTriangleStrip = ExecutionMode 29

pattern VecTypeHint :: ExecutionMode
pattern VecTypeHint = ExecutionMode 30

pattern ContractionOff :: ExecutionMode
pattern ContractionOff = ExecutionMode 31



showExecutionMode :: ExecutionMode -> String
showExecutionMode Invocations = "Invocations"
showExecutionMode SpacingEqual = "SpacingEqual"
showExecutionMode SpacingFractionalEven = "SpacingFractionalEven"
showExecutionMode SpacingFractionalOdd = "SpacingFractionalOdd"
showExecutionMode VertexOrderCw = "VertexOrderCw"
showExecutionMode VertexOrderCcw = "VertexOrderCcw"
showExecutionMode PixelCenterInteger = "PixelCenterInteger"
showExecutionMode OriginUpperLeft = "OriginUpperLeft"
showExecutionMode OriginLowerLeft = "OriginLowerLeft"
showExecutionMode EarlyFragmentTests = "EarlyFragmentTests"
showExecutionMode Xfb = "Xfb"
showExecutionMode DepthReplacing = "DepthReplacing"
showExecutionMode DepthGreater = "DepthGreater"
showExecutionMode DepthLess = "DepthLess"
showExecutionMode DepthUnchanged = "DepthUnchanged"
showExecutionMode LocalSize = "LocalSize"
showExecutionMode LocalSizeHint = "LocalSizeHint"
showExecutionMode InputPoints = "InputPoints"
showExecutionMode InputLines = "InputLines"
showExecutionMode InputLinesAdjacency = "InputLinesAdjacency"
showExecutionMode Triangles = "Triangles"
showExecutionMode InputTrianglesAdjacency = "InputTrianglesAdjacency"
showExecutionMode Quads = "Quads"
showExecutionMode Isoline = "Isoline"
showExecutionMode OutputVertices = "OutputVertices"
showExecutionMode OutputPoints = "OutputPoints"
showExecutionMode OutputLineStrip = "OutputLineStrip"
showExecutionMode OutputTriangleStrip = "OutputTriangleStrip"
showExecutionMode VecTypeHint = "VecTypeHint"
showExecutionMode ContractionOff = "ContractionOff"
showExecutionMode (ExecutionMode i) = show i