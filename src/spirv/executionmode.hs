{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
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

module SPIRV.ExecutionMode where

-- base
import Data.Type.Bool
  ( If, type (&&) )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol, AppendSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.List
  ( Elem )
import Data.Type.Maybe
  ( IsJust )
import SPIRV.Stage
  ( Stage(..), StageInfo(..) )

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
  | Isolines
  | OutputVertices a
  | OutputPoints
  | OutputLineStrip
  | OutputTriangleStrip
  | VecTypeHint a
  | ContractionOff
  -- custom information that does not align with the SPIR-V spec
  | MaxPatchVertices a
  deriving ( Show, Eq, Ord )

type InputVertices i = OutputVertices i
  -- synonym for providing number of input vertices for a tessellation evaluation shader

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
  put Isolines                = put @Word32 25
  put (OutputVertices i)      = put @Word32 26 *> put i
  put OutputPoints            = put @Word32 27
  put OutputLineStrip         = put @Word32 28
  put OutputTriangleStrip     = put @Word32 29
  put (VecTypeHint i)         = put @Word32 30 *> put i
  put ContractionOff          = put @Word32 31
  put (MaxPatchVertices _)
    = error "Cannot put custom 'MaxPatchVertices' execution mode."

  sizeOf Invocations      {} = 2
  sizeOf LocalSize        {} = 4
  sizeOf LocalSizeHint    {} = 4
  sizeOf OutputVertices   {} = 2
  sizeOf VecTypeHint      {} = 2
  sizeOf MaxPatchVertices {} = error "Cannot compute size of custom 'MaxPatchVertices' execution mode."
  sizeOf _                   = 1

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
instance Known (ExecutionMode Nat) Isolines where
  known = Isolines
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
instance Known Nat i => Known (ExecutionMode Nat) (MaxPatchVertices i) where
  known = MaxPatchVertices ( knownValue @i )

--------------------------------------------------
-- validation of execution modes (at the type level)

type family ValidStagesFor (mode :: ExecutionMode Nat) :: [ Stage ] where
  ValidStagesFor (Invocations _)         = '[ Geometry ]
  ValidStagesFor SpacingEqual            = '[ TessellationControl, TessellationEvaluation ]
  ValidStagesFor SpacingFractionalEven   = '[ TessellationControl, TessellationEvaluation ]
  ValidStagesFor SpacingFractionalOdd    = '[ TessellationControl, TessellationEvaluation ]
  ValidStagesFor VertexOrderCw           = '[ TessellationControl, TessellationEvaluation ]
  ValidStagesFor VertexOrderCcw          = '[ TessellationControl, TessellationEvaluation ]
  ValidStagesFor PixelCenterInteger      = '[ Fragment ]
  ValidStagesFor OriginUpperLeft         = '[ Fragment ]
  ValidStagesFor OriginLowerLeft         = '[ Fragment ]
  ValidStagesFor EarlyFragmentTests      = '[ Fragment ]
  ValidStagesFor PointMode               = '[ TessellationControl, TessellationEvaluation ]
  ValidStagesFor DepthReplacing          = '[ Fragment ]
  ValidStagesFor DepthGreater            = '[ Fragment ]
  ValidStagesFor DepthLess               = '[ Fragment ]
  ValidStagesFor DepthUnchanged          = '[ Fragment ]
  ValidStagesFor (LocalSize _ _ _)       = '[ GLCompute, Kernel ]
  ValidStagesFor (LocalSizeHint _ _ _)   = '[ Kernel ]
  ValidStagesFor InputPoints             = '[ Geometry ]
  ValidStagesFor InputLines              = '[ Geometry ]
  ValidStagesFor InputLinesAdjacency     = '[ Geometry ]
  ValidStagesFor Triangles               = '[ Geometry, TessellationControl, TessellationEvaluation ]
  ValidStagesFor InputTrianglesAdjacency = '[ Geometry ]
  ValidStagesFor Quads                   = '[ TessellationControl, TessellationEvaluation ]
  ValidStagesFor Isolines                = '[ TessellationControl, TessellationEvaluation ]
  ValidStagesFor (OutputVertices _)      = '[ Geometry, TessellationControl, TessellationEvaluation ]
  ValidStagesFor OutputPoints            = '[ Geometry ]
  ValidStagesFor OutputLineStrip         = '[ Geometry ]
  ValidStagesFor OutputTriangleStrip     = '[ Geometry ]
  ValidStagesFor (VecTypeHint _)         = '[ Kernel ]
  ValidStagesFor ContractionOff          = '[ Kernel ]
  ValidStagesFor (MaxPatchVertices _)    = '[ TessellationControl ]
  ValidStagesFor _
    -- for the moment, anything else is considered valid (no validity checks)
    = '[ Vertex, TessellationControl, TessellationEvaluation, Geometry, Fragment, GLCompute, Kernel ]


type family Unreachable :: k where

type family Quote (k :: Symbol) :: Symbol where
  Quote k = "\"" `AppendSymbol` k `AppendSymbol` "\""

type family Named (k :: Symbol) (s :: Stage) :: Symbol where
  Named k Vertex   = "Vertex shader named" `AppendSymbol` Quote k
  Named k TessellationControl = "Tessellation control shader named " `AppendSymbol` Quote k
  Named k TessellationEvaluation = "Tessellation evaluation shader named " `AppendSymbol` Quote k
  Named k Geometry = "Geometry shader named " `AppendSymbol` Quote k
  Named k Fragment = "Fragment shader named " `AppendSymbol` Quote k
  Named k GLCompute = "Compute shader named " `AppendSymbol` Quote k
  Named k Kernel = "Compute kernel named " `AppendSymbol` Quote k

type family ValidateExecutionModes (k :: Symbol) ( s :: Stage ) ( modes :: [ExecutionMode Nat] ) :: Maybe (StageInfo Nat s) where
  ValidateExecutionModes k Fragment modes
    = If (  HasOneOf k Fragment '[ OriginLowerLeft, OriginUpperLeft ] modes
         && HasAtMostOneOf k Fragment '[ DepthGreater, DepthLess, DepthUnchanged ] modes
         && PertainTo k Fragment modes
         )
        ( Just FragmentInfo )
        Nothing
  ValidateExecutionModes k Vertex modes
    = If ( PertainTo k Vertex modes )
         ( Just VertexInfo )
         Nothing
  ValidateExecutionModes k TessellationControl modes
  -- require that the tessellation control shader specifies tessellation info
  -- (will ignore all conflicting info provided by the tessellation evaluation shader)
    = If (  HasOneOf       k TessellationControl '[ SpacingEqual, SpacingFractionalEven, SpacingFractionalOdd ] modes
         && HasAtMostOneOf k TessellationControl '[ Triangles, Quads, Isolines ] modes
         && HasAtMostOneOf k TessellationControl '[ VertexOrderCw, VertexOrderCcw ] modes
         && PertainTo      k TessellationEvaluation modes
         )
        ( Just 
            ( TessellationControlInfo
              ( TessellationControlMaxPatchVertices k modes )
              ( TessellationControlOutputSize       k modes )
            )
        )
        Nothing
  ValidateExecutionModes k TessellationEvaluation modes
    = If (  HasAtMostOneOf k TessellationEvaluation '[ SpacingEqual, SpacingFractionalEven, SpacingFractionalOdd ] modes
         && HasOneOf       k TessellationEvaluation '[ Triangles, Quads, Isolines ] modes
         && HasAtMostOneOf k TessellationEvaluation '[ VertexOrderCw, VertexOrderCcw ] modes
         && PertainTo      k TessellationEvaluation modes
         )
        ( Just ( TessellationEvaluationInfo 32 ) )
        --        default value  ----^^ 
        Nothing
  ValidateExecutionModes k Geometry modes
    = If ( HasOneOf k Geometry '[ OutputPoints, OutputLineStrip, OutputTriangleStrip ] modes )
        ( Just ( GeometryInfo (GeometryInputSize k modes) ) )
        Nothing
  ValidateExecutionModes k GLCompute modes
    = If ( PertainTo k GLCompute modes )
        ( Just GLComputeInfo )
        Nothing
  ValidateExecutionModes k Kernel modes
    = If ( PertainTo k Kernel modes )
        ( Just KernelInfo )
        Nothing

type family PertainTo (k :: Symbol) (s :: Stage) (modes :: [ExecutionMode Nat]) :: Bool where
  PertainTo _ _ '[] = True
  PertainTo k s (mode ': modes)
    = Pertains k s mode (s `Elem` ValidStagesFor mode) && PertainTo k s modes

type family Pertains (k :: Symbol) (s :: Stage) (mode :: m) (ok :: Bool) :: Bool where
  Pertains _ _ _ True = True
  Pertains k s mode False
    = TypeError
        ( Text (Named k s) :<>: Text " does not support execution mode " :<>: ShowType mode )

type family HasOneOf (k :: Symbol) (s :: Stage) (oneOf :: [ m ] ) (modes :: [ m ]) :: Bool where
  HasOneOf k s oneOf modes
    = If ( IsJust ( LookupOneOf k s oneOf modes ) )
        True
        ( TypeError
            ( Text (Named k s) :<>: Text " needs to be given one of the following execution modes:"
             :$$: ShowType oneOf
            )
        )

type family HasAtMostOneOf (k :: Symbol) (s :: Stage) (oneOf :: [ m ] ) (modes :: [ m ]) :: Bool where
  HasAtMostOneOf k s oneOf modes
    = If ( IsJust ( LookupOneOf k s oneOf modes ) ) -- computes LookupOneOf which will error if there are conflicts
        True
        True

type family LookupOneOf (k :: Symbol) (s :: Stage) (oneOf :: [ m ] ) (modes :: [ m ]) :: Maybe m where
  LookupOneOf  k s oneOf '[] = Nothing
  LookupOneOf  k s oneOf (mode ': modes)
    = If ( mode `Elem` oneOf )
        ( Just (NoConflictWithLookupOneOf k s mode ( LookupOneOf k s oneOf modes ) ) )
        ( LookupOneOf k s oneOf modes )

type family NoConflictWithLookupOneOf (k :: Symbol) (s :: Stage) (already :: m) (another :: Maybe m) :: m where
  NoConflictWithLookupOneOf _ _ already Nothing = already
  NoConflictWithLookupOneOf k s already (Just another)
    = TypeError
        (    Text (Named k s) :<>: Text " has conflicting execution modes: "
        :<>: ShowType already :<>: Text " and " :<>: ShowType another :<>: Text "."
        )


type family GeometryInputSize ( k :: Symbol ) ( modes :: [ ExecutionMode Nat ] ) :: Nat where
  GeometryInputSize k '[]
    = TypeError
      (    Text (Named k Geometry) :<>: Text " is under-specified: missing input information."
      :$$: Text "Must specify exactly one of 'Triangles', 'InputTrianglesAdjacency',\
                \ 'InputPoints', 'InputLines', 'InputLinesAdjacency'."
      )
  GeometryInputSize k ( InputPoints ': modes )
    = If ( NoSizes k Geometry InputPoints modes )
        1
        Unreachable
  GeometryInputSize k ( InputLines ': modes )
    = If ( NoSizes k Geometry InputLines modes )
        2
        Unreachable
  GeometryInputSize k ( InputLinesAdjacency ': modes )
    = If ( NoSizes k Geometry InputLinesAdjacency modes )
        4
        Unreachable
  GeometryInputSize k ( Triangles ': modes )
    = If ( NoSizes k Geometry Triangles modes )
        3
        Unreachable
  GeometryInputSize k ( InputTrianglesAdjacency ': modes )
    = If ( NoSizes k Geometry Triangles modes )
        6
        Unreachable
  GeometryInputSize k ( _ ': modes )
    = GeometryInputSize k modes

type family TessellationControlOutputSize ( k :: Symbol ) ( modes :: [ ExecutionMode Nat ] ) :: Nat where
  TessellationControlOutputSize k '[]
    = TypeError
      (   Text (Named k TessellationControl) :<>: Text " is under-specified: missing number of output points."
      :$$: Text "The 'OutputVertices' execution mode must be provided."
      )
  TessellationControlOutputSize k ( OutputVertices verts ': modes )
    = If ( NoOutputVertices k TessellationControl verts modes )
        verts
        Unreachable
  TessellationControlOutputSize k (_ ': modes )
    = TessellationControlOutputSize k modes

type family TessellationControlMaxPatchVertices ( k :: Symbol ) ( modes :: [ ExecutionMode Nat ] ) :: Nat where
  TessellationControlMaxPatchVertices k '[] = 32 -- default as used by SPIR-V
  TessellationControlMaxPatchVertices k ( MaxPatchVertices verts ': _ )
    = verts
  TessellationControlMaxPatchVertices k (_ ': modes)
    = TessellationControlMaxPatchVertices k modes

type family NoSizes
              ( k         :: Symbol                )
              ( stage     :: Stage                 )
              ( givenSize :: ExecutionMode Nat     )
              ( modes     :: [ ExecutionMode Nat ] )
              :: Bool
              where
  NoSizes _ _     _     '[]
    = 'True
  NoSizes k stage given (InputPoints ': _ )
    = TypeError
        (   Text (Named k stage) :<>: Text " conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType InputPoints :<>: Text "."
        )
  NoSizes k stage given (InputLines ': _ )
    = TypeError
        (   Text (Named k stage) :<>: Text " conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType InputLines :<>: Text "."
        )
  NoSizes k stage given (InputLinesAdjacency ': _ )
    = TypeError
        (   Text (Named k stage) :<>: Text " conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType InputLinesAdjacency :<>: Text "."
        )
  NoSizes k stage given (Triangles ': _ )
    = TypeError
        (   Text (Named k stage) :<>: Text " conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType Triangles :<>: Text "."
        )
  NoSizes k stage given (InputTrianglesAdjacency ': _ )
    = TypeError
        (   Text (Named k stage) :<>: Text " conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType InputTrianglesAdjacency :<>: Text "."
        )
  NoSizes k stage given (Quads ': _ )
    = TypeError
        (   Text (Named k stage) :<>: Text " conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType Quads :<>: Text "."
        )
  NoSizes k stage given (Isolines ': _ )
    = TypeError
        (   Text (Named k stage) :<>: Text " conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType Isolines :<>: Text "."
        )
  NoSizes k stage given ( _ ': modes )
    = NoSizes k stage given modes

type family NoOutputVertices
              ( k          :: Symbol                )
              ( stage      :: Stage                 )
              ( givenVerts :: Nat                   )
              ( modes      :: [ ExecutionMode Nat ] )
            :: Bool
            where
  NoOutputVertices _ _     _        '[] = True
  NoOutputVertices k stage givenVerts ( OutputVertices verts ': _ )
    = TypeError
        (   Text (Named k stage) :<>: Text " conflicting number of output vertices."
        :$$:  Text "Provided both " :<>: ShowType givenVerts :<>: Text " and " :<>: ShowType verts :<>: Text "."
        )
  NoOutputVertices k stage given ( _ ': modes )
    = NoOutputVertices k stage given modes
