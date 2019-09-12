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

{-|
Module: SPIRV.ExecutionMode

This module enumerates SPIR-V execution modes. These are used both at the type-level and value-level.

These annotate execution models / shader stages (see "SPIRV.Stage") with further information,
such as the geometry type to use in a tessellation shader (points, triangles, quads, isolines),
or the workgroup size in a compute shader.

See the SPIR-V specification §3.6 __Execution Modes__.

-}

module SPIRV.ExecutionMode where

-- base
import Data.Type.Bool
  ( If, type (&&) )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- containers
import Data.Set
  ( Set )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.List
  ( Elem )
import Data.Type.Maybe
  ( IsJust, FromMaybe )
import SPIRV.Stage

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

type ExecutionModes = Set (ExecutionMode Word32)

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

  wordCount Invocations      {} = 2
  wordCount LocalSize        {} = 4
  wordCount LocalSizeHint    {} = 4
  wordCount OutputVertices   {} = 2
  wordCount VecTypeHint      {} = 2
  wordCount MaxPatchVertices {} = error "Cannot compute size of custom 'MaxPatchVertices' execution mode."
  wordCount _                   = 1

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

type family ValidModelsFor (mode :: ExecutionMode Nat) :: [ ExecutionModel ] where
  ValidModelsFor (Invocations _)         = '[ Geometry ]
  ValidModelsFor SpacingEqual            = '[ TessellationControl, TessellationEvaluation ]
  ValidModelsFor SpacingFractionalEven   = '[ TessellationControl, TessellationEvaluation ]
  ValidModelsFor SpacingFractionalOdd    = '[ TessellationControl, TessellationEvaluation ]
  ValidModelsFor VertexOrderCw           = '[ TessellationControl, TessellationEvaluation ]
  ValidModelsFor VertexOrderCcw          = '[ TessellationControl, TessellationEvaluation ]
  ValidModelsFor PixelCenterInteger      = '[ Fragment ]
  ValidModelsFor OriginUpperLeft         = '[ Fragment ]
  ValidModelsFor OriginLowerLeft         = '[ Fragment ]
  ValidModelsFor EarlyFragmentTests      = '[ Fragment ]
  ValidModelsFor PointMode               = '[ TessellationControl, TessellationEvaluation ]
  ValidModelsFor DepthReplacing          = '[ Fragment ]
  ValidModelsFor DepthGreater            = '[ Fragment ]
  ValidModelsFor DepthLess               = '[ Fragment ]
  ValidModelsFor DepthUnchanged          = '[ Fragment ]
  ValidModelsFor (LocalSize _ _ _)       = '[ Compute, Kernel ]
  ValidModelsFor (LocalSizeHint _ _ _)   = '[ Kernel ]
  ValidModelsFor InputPoints             = '[ Geometry ]
  ValidModelsFor InputLines              = '[ Geometry ]
  ValidModelsFor InputLinesAdjacency     = '[ Geometry ]
  ValidModelsFor Triangles               = '[ Geometry, TessellationControl, TessellationEvaluation ]
  ValidModelsFor InputTrianglesAdjacency = '[ Geometry ]
  ValidModelsFor Quads                   = '[ TessellationControl, TessellationEvaluation ]
  ValidModelsFor Isolines                = '[ TessellationControl, TessellationEvaluation ]
  ValidModelsFor (OutputVertices _)      = '[ Geometry, TessellationControl, TessellationEvaluation ]
  ValidModelsFor OutputPoints            = '[ Geometry ]
  ValidModelsFor OutputLineStrip         = '[ Geometry ]
  ValidModelsFor OutputTriangleStrip     = '[ Geometry ]
  ValidModelsFor (VecTypeHint _)         = '[ Kernel ]
  ValidModelsFor ContractionOff          = '[ Kernel ]
  ValidModelsFor (MaxPatchVertices _)    = '[ TessellationControl ]
  ValidModelsFor _
    -- for the moment, anything else is considered valid (no validity checks)
    = '[ Vertex, TessellationControl, TessellationEvaluation, Geometry, Fragment, Compute, Kernel ]


type family Unreachable :: k where

type family ValidateExecutionModes
                ( k     :: Symbol              )
                ( em    :: ExecutionModel      )
                ( modes :: [ExecutionMode Nat] )
              :: Maybe (ExecutionInfo Nat em)
              where
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
    = If (  HasOneOf       k TessellationControl '[ SpacingEqual, SpacingFractionalEven, SpacingFractionalOdd ] modes
         && HasAtMostOneOf k TessellationControl '[ VertexOrderCw, VertexOrderCcw ] modes
         && PertainTo      k TessellationControl modes
         )
        ( Just
            ( TessellationControlInfo
              ( TessellationControlMaxPatchVertices k modes )
              ( TessellationControlOutputSize       k modes )
              ( TessellationControlMbMode           k modes )
            )
        )
        Nothing
  ValidateExecutionModes k TessellationEvaluation modes
    = If (  HasAtMostOneOf k TessellationEvaluation '[ SpacingEqual, SpacingFractionalEven, SpacingFractionalOdd ] modes
         && HasAtMostOneOf k TessellationEvaluation '[ VertexOrderCw, VertexOrderCcw ] modes
         && PertainTo      k TessellationEvaluation modes
         )
        ( Just
          ( TessellationEvaluationInfo
              32 -- default value
              ( TessellationEvaluationMode k modes )
          )
        )
        Nothing
  ValidateExecutionModes k Geometry modes
    = If (  HasOneOf k Geometry '[ OutputPoints, OutputLineStrip, OutputTriangleStrip ] modes
         && HasInvocations k modes
         )
        ( Just
          ( GeometryInputInfo k modes )
        )
        Nothing
  ValidateExecutionModes k Compute modes
    = If ( PertainTo k Compute modes )
        ( Just (ComputeInfo (LocalSizes k Compute modes) ) )
        Nothing
  ValidateExecutionModes k em _
    = TypeError ( Text "Unsupported " :<>: Text (NamedExecutionModel k em) )

type family PertainTo (k :: Symbol) (em :: ExecutionModel) (modes :: [ExecutionMode Nat]) :: Bool where
  PertainTo _ _ '[] = True
  PertainTo k s (mode ': modes)
    = Pertains k s mode (s `Elem` ValidModelsFor mode) && PertainTo k s modes

type family Pertains (k :: Symbol) (em :: ExecutionModel) (mode :: m) (ok :: Bool) :: Bool where
  Pertains _ _ _ True = True
  Pertains k em mode False
    = TypeError
        (    Text (NamedExecutionModel k em) :<>: Text " does not support execution mode "
        :<>: ShowType mode :<>: Text "."
        )

type family HasOneOf (k :: Symbol) (em :: ExecutionModel) (oneOf :: [ m ] ) (modes :: [ m ]) :: Bool where
  HasOneOf k em oneOf modes
    = If ( IsJust ( LookupOneOf k em oneOf modes ) )
        True
        ( TypeError
            ( Text (NamedExecutionModel k em) :<>: Text " needs to be given one of the following execution modes:"
             :$$: ShowType oneOf
            )
        )

type family HasAtMostOneOf (k :: Symbol) (em :: ExecutionModel) (oneOf :: [ m ]) (modes :: [ m ]) :: Bool where
  HasAtMostOneOf k em oneOf modes
    = If ( IsJust ( LookupOneOf k em oneOf modes ) ) -- computes LookupOneOf which will error if there are conflicts
        True
        True

type family LookupOneOf (k :: Symbol) (em :: ExecutionModel) (oneOf :: [ m ]) (modes :: [ m ]) :: Maybe m where
  LookupOneOf k em oneOf '[] = Nothing
  LookupOneOf k em oneOf (mode ': modes)
    = If ( mode `Elem` oneOf )
        ( Just (NoConflictWithLookupOneOf k em mode ( LookupOneOf k em oneOf modes ) ) )
        ( LookupOneOf k em oneOf modes )

type family NoConflictWithLookupOneOf (k :: Symbol) (em :: ExecutionModel) (already :: m) (another :: Maybe m) :: m where
  NoConflictWithLookupOneOf _ _  already Nothing = already
  NoConflictWithLookupOneOf k em already (Just another)
    = TypeError
        (    Text (NamedExecutionModel k em) :<>: Text " has conflicting execution modes: "
        :<>: ShowType already :<>: Text " and " :<>: ShowType another :<>: Text "."
        )


type family GeometryInputInfo
              ( k :: Symbol )
              ( modes :: [ ExecutionMode Nat ] )
              :: (ExecutionInfo Nat Geometry)  where
  GeometryInputInfo k '[]
    = TypeError
      (    Text (NamedExecutionModel k Geometry) :<>: Text " is under-specified: missing input information."
      :$$: Text "Must specify exactly one of 'Triangles', 'InputTrianglesAdjacency',\
                \ 'InputPoints', 'InputLines', 'InputLinesAdjacency'."
      )
  GeometryInputInfo k ( InputPoints ': modes )
    = If ( NoSizes k Geometry InputPoints modes )
        ( GeometryInfo 1 InputModePoints )
        Unreachable
  GeometryInputInfo k ( InputLines ': modes )
    = If ( NoSizes k Geometry InputLines modes )
        ( GeometryInfo 2 InputModeLines )
        Unreachable
  GeometryInputInfo k ( InputLinesAdjacency ': modes )
    = If ( NoSizes k Geometry InputLinesAdjacency modes )
        ( GeometryInfo 4 InputModeLinesAdjacency )
        Unreachable
  GeometryInputInfo k ( Triangles ': modes )
    = If ( NoSizes k Geometry Triangles modes )
        ( GeometryInfo 3 InputModeTriangles )
        Unreachable
  GeometryInputInfo k ( InputTrianglesAdjacency ': modes )
    = If ( NoSizes k Geometry Triangles modes )
        ( GeometryInfo 6 InputModeTrianglesAdjacency )
        Unreachable
  GeometryInputInfo k ( _ ': modes )
    = GeometryInputInfo k modes

type family TessellationControlMbMode
              ( k     :: Symbol                )
              ( modes :: [ ExecutionMode Nat ] )
            :: Maybe TessellationMode
            where
  TessellationControlMbMode k modes
    = FmapReadTessellationMode
        ( LookupOneOf k TessellationControl
            '[ Triangles, Quads, Isolines, PointMode ]
            modes
        )

type family FmapReadTessellationMode
               ( m :: Maybe (ExecutionMode Nat) )
            :: Maybe TessellationMode
            where
  FmapReadTessellationMode 'Nothing = 'Nothing
  FmapReadTessellationMode ('Just mode) = 'Just (ReadTessellationMode mode)

type family ReadTessellationMode (m :: ExecutionMode Nat) :: TessellationMode where
  ReadTessellationMode Triangles = ModeTriangles
  ReadTessellationMode Quads = ModeQuads
  ReadTessellationMode Isolines = ModeIsolines
  ReadTessellationMode PointMode = ModePoints
  ReadTessellationMode nonMode
    = TypeError
      (    Text "Expected tessellation mode, but got " :<>: ShowType nonMode
      :<>: Text " instead."
      )


type family TessellationEvaluationMode
              ( k     :: Symbol                )
              ( modes :: [ ExecutionMode Nat ] )
            :: TessellationMode
            where
  TessellationEvaluationMode k modes
    = ReadTessellationMode
        ( FromMaybe
          ( LookupOneOf k TessellationEvaluation
            '[ Triangles, Quads, Isolines, PointMode ]
            modes
          )
          ( TypeError
            (    Text (NamedExecutionModel k TessellationEvaluation)
            :<>: Text " does not specify its tessellation mode."
            :$$: Text "One of 'Triangles', 'Quads', 'Isolines' or 'PointMode' must be provided."
            )
          )
        )

type family TessellationControlOutputSize
              ( k     :: Symbol                )
              ( modes :: [ ExecutionMode Nat ] )
            :: Nat
            where
  TessellationControlOutputSize k '[]
    = TypeError
      (   Text (NamedExecutionModel k TessellationControl) :<>: Text " is under-specified: missing number of output points."
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
              ( em        :: ExecutionModel        )
              ( givenSize :: ExecutionMode Nat     )
              ( modes     :: [ ExecutionMode Nat ] )
              :: Bool
              where
  NoSizes _ _  _     '[]
    = 'True
  NoSizes k em given (InputPoints ': _ )
    = TypeError
        (   Text (NamedExecutionModel k em) :<>: Text " has conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType InputPoints :<>: Text "."
        )
  NoSizes k em given (InputLines ': _ )
    = TypeError
        (   Text (NamedExecutionModel k em) :<>: Text " has conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType InputLines :<>: Text "."
        )
  NoSizes k em given (InputLinesAdjacency ': _ )
    = TypeError
        (   Text (NamedExecutionModel k em) :<>: Text " has conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType InputLinesAdjacency :<>: Text "."
        )
  NoSizes k em given (Triangles ': _ )
    = TypeError
        (   Text (NamedExecutionModel k em) :<>: Text " has conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType Triangles :<>: Text "."
        )
  NoSizes k em given (InputTrianglesAdjacency ': _ )
    = TypeError
        (   Text (NamedExecutionModel k em) :<>: Text " has conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType InputTrianglesAdjacency :<>: Text "."
        )
  NoSizes k em given (Quads ': _ )
    = TypeError
        (   Text (NamedExecutionModel k em) :<>: Text " has conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType Quads :<>: Text "."
        )
  NoSizes k em given (Isolines ': _ )
    = TypeError
        (   Text (NamedExecutionModel k em) :<>: Text " has conflicting inputs."
        :$$:  Text "Provided both " :<>: ShowType given :<>: Text " and " :<>: ShowType Isolines :<>: Text "."
        )
  NoSizes k em given ( _ ': modes )
    = NoSizes k em given modes

type family NoOutputVertices
              ( k          :: Symbol                )
              ( em         :: ExecutionModel        )
              ( givenVerts :: Nat                   )
              ( modes      :: [ ExecutionMode Nat ] )
            :: Bool
            where
  NoOutputVertices _ _  _        '[] = True
  NoOutputVertices k em givenVerts ( OutputVertices verts ': _ )
    = TypeError
        (   Text (NamedExecutionModel k em) :<>: Text "has conflicting numbers of output vertices."
        :$$:  Text "Provided both " :<>: ShowType givenVerts :<>: Text " and " :<>: ShowType verts :<>: Text "."
        )
  NoOutputVertices k em given ( _ ': modes )
    = NoOutputVertices k em given modes

type family LocalSizes
              ( k     :: Symbol                )
              ( em    :: ExecutionModel        )
              ( modes :: [ ExecutionMode Nat ] )
            :: (Nat, Nat, Nat)
            where
  LocalSizes k em '[]
    = TypeError
    ( Text (NamedExecutionModel k em) :<>: Text " is missing 'LocalSize' information." )
  LocalSizes k em ( LocalSize x y z ': modes )
    = If ( NoLocalSizes k em '(x,y,z) modes )
      ( '(x,y,z) )
      ( '(1,1,1) ) -- unreachable
  LocalSizes k em ( _ ': modes ) = LocalSizes k em modes

type family NoLocalSizes
              ( k     :: Symbol                )
              ( em    :: ExecutionModel        )
              ( xyz   :: (Nat,Nat,Nat)         )
              ( modes :: [ ExecutionMode Nat ] )
            :: Bool
            where
  NoLocalSizes _ _ _ '[] = 'True
  NoLocalSizes k em xyz (LocalSize x' y' z' ': _)
    = TypeError
    ( Text (NamedExecutionModel k em) :<>: Text " has conflicting local size information."
    :$$: Text "Provided both " :<>: ShowType xyz
    :<>: Text " and " :<>: ShowType '(x',y',z') :<>: Text "."
    )
  NoLocalSizes k em xyz ( _ ': modes )
    = NoLocalSizes k em xyz modes

type family HasInvocations
              ( k     :: Symbol                )
              ( modes :: [ ExecutionMode Nat ] )
           :: Bool
           where
  HasInvocations k '[]
    = TypeError
    (    Text (NamedExecutionModel k Geometry) :<>: Text " is missing the number of invocations."
    :$$: Text "Expecting an 'Invocations i' execution mode, with 0 < i <= maxGeometryShaderInvocations."
    )
  HasInvocations k ( Invocations 0 ': _ )
    = TypeError
    ( Text (NamedExecutionModel k Geometry) :<>: Text ": number of invocations cannot be 0." )
  HasInvocations k ( Invocations _ ': _ ) = True
  HasInvocations k ( _ ': modes ) = HasInvocations k modes
