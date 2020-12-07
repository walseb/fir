{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Definitions

Validate user-provided execution modes.

For instance, this checks that compute shaders
are decorated with local group sizes,
or that a geometry shader specifies its output
primitive type.

-}

module FIR.Validation.ExecutionModes where

-- base
import Data.Type.Bool
  ( If, type (&&) )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Error
  ( Assert )
import Data.Type.List
  ( Elem )
import Data.Type.Maybe
  ( IsJust, FromMaybe )
import SPIRV.ExecutionMode
import SPIRV.Stage

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
  ValidateExecutionModes k Kernel modes
    = If ( PertainTo k Kernel modes )
        ( Just (KernelInfo (LocalSizes k Kernel modes) ) )
        Nothing
  ValidateExecutionModes k ( 'Stage ( 'RayStage rayShader ) ) modes
    = If ( PertainTo k ( 'Stage ( 'RayStage rayShader ) ) modes )
      ( Just ( 'RayShaderInfo ( MkRayShaderInfo rayShader ) ) )
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

type family HasOneOf (k :: Symbol) (em :: ExecutionModel) (oneOf :: [ m ]) (modes :: [ m ]) :: Bool where
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
    = Assert ( NoSizes k Geometry InputPoints modes )
        ( GeometryInfo 1 InputModePoints )
  GeometryInputInfo k ( InputLines ': modes )
    = Assert ( NoSizes k Geometry InputLines modes )
        ( GeometryInfo 2 InputModeLines )
  GeometryInputInfo k ( InputLinesAdjacency ': modes )
    = Assert ( NoSizes k Geometry InputLinesAdjacency modes )
        ( GeometryInfo 4 InputModeLinesAdjacency )
  GeometryInputInfo k ( Triangles ': modes )
    = Assert ( NoSizes k Geometry Triangles modes )
        ( GeometryInfo 3 InputModeTriangles )
  GeometryInputInfo k ( InputTrianglesAdjacency ': modes )
    = Assert ( NoSizes k Geometry Triangles modes )
        ( GeometryInfo 6 InputModeTrianglesAdjacency )
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
    = Assert ( NoOutputVertices k TessellationControl verts modes )
        verts
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
    = Assert ( NoLocalSizes k em '(x,y,z) modes )
        ( '(x,y,z) )
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
