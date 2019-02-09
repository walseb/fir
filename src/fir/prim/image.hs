{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Prim.Image
  ( -- * Images
    -- ** Image properties
    ImageProperties(Properties)
  -- ** Opaque image data type
  , Image
  -- ** Type reflection of image properties
  -- $reflection
  , knownImageComponent
  , knownImage

  -- * Image operands
  , OperandName(..), ImageOperands(..)
  -- ** Specific data type for gathering operations
  , GatherInfo(..)

  -- * Compute the coordinate and texel type of an image
  , ImageCoordinates, ImageData
  )
  where

-- base
import Prelude
  hiding ( Floating, Integral )
import Data.Int
  ( Int32 )
import Data.Kind
  ( Type, Constraint )
import Data.Word
  ( Word32 )
import Data.Type.Bool
  ( If )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.List
  ( Elem )
import {-# SOURCE #-} FIR.AST
  ( AST )
import FIR.Prim.Array
  ( Array )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( PrimTy  , primTy
  , ScalarTy, scalarTy
  )
import Math.Algebra.Class
  ( Floating, Integral )
import Math.Linear
  ( V )
import SPIRV.Image
  ( Arrayness(..)
  , Dimensionality(..)
  , HasDepth(..)
  , ImageUsage(..)
  , ImageFormat(..), I, UI
  , LODOperand
  , MultiSampling(..)
  , Operand(LODOperand)
  , Projection(..)
  )
import qualified SPIRV.Image  as SPIRV
  ( Image(..), Operand(..), LODOperand(..) )
import qualified SPIRV.PrimTy as SPIRV
  ( PrimTy )

--------------------------------------------------

-- | Properties of an image, used as a data kind
-- to describe the properties of an image at the type-level.
--
-- This allows type-checking that the image properties
-- comply with the @SPIR-V@ specification, as well
-- as check that operations done with the image are valid.
data ImageProperties where
  Properties
    :: Type -- ^ Component type of image coordinates.
    -> Type -- ^ Texel component type.
    -> Dimensionality -- ^ Dimensionality of the image (1D, 2D, 3D, cubemap, ...).
    -> Maybe HasDepth -- ^ Whether the image is a depth image.
    -> Arrayness      -- ^ Whether the image has an extra array component.
    -> MultiSampling  -- ^ Whether the image is multisampled.
    -> ImageUsage     -- ^ Is the image sampled or a storage image?
    -> Maybe (ImageFormat Nat) -- ^ 'SPIRV.Image.ImageFormat' of the image.
    -> ImageProperties

-- Access the component type.
type family ImageComponent ( props :: ImageProperties ) :: Type where
  ImageComponent ( Properties a _ _ _ _ _ _ _ ) = a

-- | Abstract handle to an image.
-- 
-- This type is uninhabited, but is tagged
-- with a type describing its properties.
data Image (props :: ImageProperties) where


-- newtype to retain injectivity of 'Demote' type family
newtype ImageAndComponent
  = ImageAndComponent (SPIRV.Image, SPIRV.PrimTy)

instance Demotable ImageProperties where
  type Demote ImageProperties = ImageAndComponent

instance ( ScalarTy                        component
         , PrimTy                          result
         , Known Dimensionality            dimensionality
         , Known (Maybe HasDepth)          hasDepth
         , Known Arrayness                 arrayness
         , Known MultiSampling             multiSampling
         , Known ImageUsage                imageUsage
         , Known (Maybe (ImageFormat Nat)) imageFormat
         , MatchesFormat imageFormat result
         )
  => Known ImageProperties
      ( 'Properties
           component result
           dimensionality hasDepth
           arrayness multiSampling
           imageUsage imageFormat
      )
  where
  known
    = ImageAndComponent
        ( SPIRV.Image
            { SPIRV.component        = scalarTy   @component
            , SPIRV.dimensionality   = knownValue @dimensionality
            , SPIRV.hasDepth         = knownValue @hasDepth
            , SPIRV.arrayness        = knownValue @arrayness
            , SPIRV.multiSampling    = knownValue @multiSampling
            , SPIRV.imageUsage = Just (knownValue @imageUsage)
            , SPIRV.imageFormat      = knownValue @imageFormat
            }
        , primTy @result
        )


-- $reflection
--
-- As image properties are provided at the type level,
-- we use the 'Data.Type.Known.Known' mechanism to reflect
-- this type-level information to the value level.

-- | Provided image properties at the type-level,
-- return the component type of the image's texels.
knownImageComponent :: forall props. Known ImageProperties props
                          => SPIRV.PrimTy
knownImageComponent
  = case knownValue @props of
      ImageAndComponent (_, comp) -> comp

-- | Return the 'SPIRV.Image.Image' type with the given properties.
knownImage :: forall props. Known ImageProperties props => SPIRV.Image
knownImage
  = case knownValue @props of
      ImageAndComponent (img, _) -> img

--------------------------------------------------
-- Image operands

-- | Names of image operands.
--
-- These are used to keep track at the type level of the operands
-- which have been provided.
--
-- This is necessary so that we can type check their usage,
-- and know which types are being used in conjunction with the image.
--
-- For instance, if a depth-comparison is being performed,
-- the return of an image sampling operation is a scalar,
-- whereas otherwise it is a 4-vector.
data OperandName
  = DepthComparison  -- ^ Whether a depth-comparison is being performed.
  | ProjectiveCoords -- ^ Whether to use projective coordinates.
  | BaseOperand SPIRV.Image.Operand -- A @SPIR-V@ image "SPIRV.Image.Operand".
  deriving ( Show, Eq, Ord )

-- | Image operands.
--
-- This is essentially a linked list of operands, with a /twist/:
--
--   - the type of an image operand in the list can depend
--     on which operands appear further down in the list.
--
-- Consider for instance the image operands:
--
--    @ Grad grad $ Proj $ Done @
--
-- These can loosely be thought of as a list of operands
--
--    @ Grad grad : Proj : [] @
--
-- This means we are specifying explicit derivatives,
-- and using projective coordinates.
--
-- However, the fact that we are using projective coordinates
-- means that the explicit derivatives require an extra
-- component (the projective coordinate).
--
-- In this way, the type of the head of the list
-- depends on the operands appearing in the tail.
data ImageOperands
       ( props :: ImageProperties )
       ( ops   :: [OperandName]   )
     :: Type
     where
  -- | End of list.
  Done :: ImageOperands props '[ ]
  -- | Use projective coordinates.
  --
  -- Must be provided after all other operands,
  -- except possibly 'Dref'.
  Proj :: ( CanAddProj ops )
       => ImageOperands props ops
       -> ImageOperands props (ProjectiveCoords ': ops)
  -- | Provide a depth-comparison reference value.
  --
  -- Must be provided after all other operands,
  -- except possibly 'Proj'.
  Dref :: ( CanAddDref ops )
       => AST (ImageComponent props) -- ^ Reference value used to perform the depth comparison.
       -> ImageOperands props ops
       -> ImageOperands props (DepthComparison ': ops)
  -- | Add a bias to the implicit level of detail.
  Bias :: ( BasicDim "Bias" props
          , NoMS "Bias" props
          , NoDuplicate (BaseOperand ('LODOperand SPIRV.Bias)) ops
          , NoLODOps "Bias" '[SPIRV.LOD, SPIRV.Grad] ops
          )
       => AST (ImageComponent props) -- ^ Bias.
       -> ImageOperands props ops 
       -> ImageOperands props (BaseOperand ('LODOperand SPIRV.Bias) ': ops)
  -- | Provide an explicit level of detail.
  LOD  :: ( BasicDim "LOD" props
          , NoMS "LOD" props
          , NoDuplicate (BaseOperand ('LODOperand SPIRV.LOD)) ops
          , NoLODOps "LOD" '[SPIRV.Bias, SPIRV.Grad, SPIRV.MinLOD] ops
          )
       => AST (ImageComponent props) -- ^ LOD.
       -> ImageOperands props ops
       -> ImageOperands props (BaseOperand ('LODOperand SPIRV.LOD ) ': ops)
  -- | Specify the minimum level of detail to use
  -- when sampling the image.
  MinLOD :: ( BasicDim "MinLOD" props
            , NoMS "MinLOD" props
            , NoDuplicate (BaseOperand ('LODOperand SPIRV.MinLOD)) ops
            , NoLODOps "MinLOD" '[SPIRV.LOD, SPIRV.Bias] ops
            )
         => AST (ImageComponent props) -- ^ Minimum LOD.
         -> ImageOperands props ops
         -> ImageOperands props (BaseOperand ('LODOperand SPIRV.MinLOD ) ': ops)
  -- | Provide explicit derivatives.
  Grad :: ( vec ~ GradCoordinates props ops
          , NoMS "Grad" props
          , NoDuplicate (BaseOperand ('LODOperand SPIRV.Grad)) ops
          , NoLODOps "Grad" '[ SPIRV.Bias, SPIRV.LOD ] ops
          )
       => ( AST vec, AST vec ) -- ^ Gradient: ( df/dx, df/dy ).
       -> ImageOperands props ops
       -> ImageOperands props (BaseOperand ('LODOperand SPIRV.Grad) ': ops)
  -- | Add a constant offset to the coordinates.
  ConstOffsetBy
    :: ( PrimTy vec
       , vec ~ OffsetCoordinates props ops
       , NoDuplicate (BaseOperand SPIRV.ConstOffset) ops
       , NotCubeDim "ConstOffsetBy" props
       )
    => vec -- Offset (a Haskell constant).
    -> ImageOperands props ops
    -> ImageOperands props (BaseOperand SPIRV.ConstOffset ': ops)
  -- | Add an offset to the coordinates.
  OffsetBy
    :: ( PrimTy vec
       , vec ~ OffsetCoordinates props ops
       , NoDuplicate (BaseOperand SPIRV.Offset) ops
       , NotCubeDim "OffsetBy" props
       )
    => AST vec -- Offset.
    -> ImageOperands props ops
    -> ImageOperands props (BaseOperand SPIRV.Offset ': ops)
  -- | Specify which 4 offsets to use for a 'gather' operation,
  -- which reads 4 texels at a time.
  --
  -- If not performing a depth test, an additional
  -- index must be provided to specify which component to access.
  Gather
    :: ( NotCubeDim "Gather" props
       , NoDuplicate (BaseOperand SPIRV.ConstOffsets) ops
       , NoLODOps "Gather" '[ SPIRV.LOD, SPIRV.Grad, SPIRV.Bias, SPIRV.MinLOD ] ops
       , UsesAffineCoords ops
       )
    => GatherInfo (WhichGather ops)
    -> ImageOperands props ops
    -> ImageOperands props (BaseOperand SPIRV.ConstOffsets ': ops)
  -- | Specify which sample number to use in a multi-sampled image.
  SampleNo
    :: ( CanMultiSample props
       , NoDuplicate (BaseOperand SPIRV.Sample) ops
       )
    => AST Word32  -- ^ Sample number.
    -> ImageOperands props ops
    -> ImageOperands props (BaseOperand SPIRV.Sample ': ops)

-----------------------------------------------------------
-- data type for gather image operand

data Gather
  = ComponentGather
  | DrefGather
  deriving ( Show, Eq, Ord, Bounded, Enum )

-- | Information hat needs to be provided to the 'Gather' image operand.
--
-- This consists of 4 offsets, together with the information
-- of which image component (0,1,2,3) to gather
-- in the case that a depth test is /not/ being performed.
data GatherInfo :: Gather -> Type where
  ComponentWithOffsets
    :: AST Word32 -> Array 4 (V 2 Int32) -> GatherInfo ComponentGather
  DepthWithOffsets
            ::       Array 4 (V 2 Int32) -> GatherInfo DrefGather

-- Computes whether a component index needs to be provided to the
-- 'Gather' image operand.
type family WhichGather (ops :: [OperandName]) :: Gather where
  WhichGather ops
    = If
        ( DepthComparison `Elem` ops  )
        DrefGather
        ComponentGather

----------------------------------------------------------

-- | Type of coordinates used by an image
--
-- This is the type of the value to be provided to sample an image,
-- e.g. a 2-vector for a 2D texture (using affine coordinates).
type family ImageCoordinates
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
            :: Type where
  ImageCoordinates
    ( Properties a _ dim _ arr _ _ _ )
    ops
      = If
          ( ProjectiveCoords `Elem` ops )
          ( ImageCoordinatesType a dim arr Projective )
          ( ImageCoordinatesType a dim arr Affine )

-- | Texel type of an image.
--
-- This is what is returned from an image sampling operation,
-- or needs to be provided for an image write operation.
type family ImageData
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
            :: Type
            where
  ImageData ( Properties _ r _ _ _ _ _ _ ) ops
    = If
        ( DepthComparison `Elem` ops )
        r
        (V 4 r)

type family ImageCoordinatesType
              ( a    :: Type           )
              ( dim  :: Dimensionality )
              ( arr  :: Arrayness      )
              ( proj :: Projection     )
              :: Type
              where
  ImageCoordinatesType a OneD        NonArrayed Affine     =     a
  ImageCoordinatesType a TwoD        NonArrayed Affine     = V 2 a
  ImageCoordinatesType a ThreeD      NonArrayed Affine     = V 3 a
  ImageCoordinatesType a Cube        NonArrayed Affine     = V 3 a
  ImageCoordinatesType a Rect        NonArrayed Affine     = V 2 a
  ImageCoordinatesType a Buffer      NonArrayed Affine     =     a
  ImageCoordinatesType a SubpassData NonArrayed Affine     = V 2 a
  ImageCoordinatesType a OneD        NonArrayed Projective = V 2 a
  ImageCoordinatesType a TwoD        NonArrayed Projective = V 3 a
  ImageCoordinatesType a ThreeD      NonArrayed Projective = V 4 a
  ImageCoordinatesType a Cube        NonArrayed Projective = V 4 a
  ImageCoordinatesType a Rect        NonArrayed Projective = V 3 a
  ImageCoordinatesType a Buffer      NonArrayed Projective = V 2 a
  ImageCoordinatesType a SubpassData NonArrayed Projective = V 3 a
  ImageCoordinatesType a OneD        Arrayed    Affine     = V 2 a
  ImageCoordinatesType a TwoD        Arrayed    Affine     = V 3 a
  ImageCoordinatesType a Cube        Arrayed    Affine     = V 3 a -- weird stuff going on here
  ImageCoordinatesType _ dim         Arrayed    Affine
    = TypeError ( Text "Unsupported arrayed image format \
                       \with dimensionality "
                  :<>: ShowType dim
                )
  ImageCoordinatesType _ dim         Arrayed    Projective
    = TypeError ( Text "Cannot use projective coordinates with an arrayed image." )

-- which coordinates to use to provide explicit derivatives
type family GradCoordinates
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
            :: Type where
  GradCoordinates
    ( Properties a _ dim _ arr _ _ _ )
    ops
      = If
          ( ProjectiveCoords `Elem` ops )
          ( ImageCoordinatesType a dim arr Projective )
          ( ImageCoordinatesType a dim arr Affine     )

-- which coordinates to use to provide an offset
type family OffsetCoordinates
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
            :: Type where
  OffsetCoordinates
    ( Properties _ _ dim _ _ _ _ _ )
    ops
    = If
        ( ProjectiveCoords `Elem` ops )
        ( ImageCoordinatesType Int32 dim NonArrayed Projective )
        ( ImageCoordinatesType Int32 dim NonArrayed Affine     )

-----------------------------------------------------------
-- helper type families to ensure image operands are valid

-- this type family could be slightly more specific
-- and enforce that e.g. reading from an 'Unsigned Integer' image format
-- necessarily returns unsigned integers
-- the 'No instance ...' error message can also be a bit confusing
type family MatchesFormat
              ( fmt :: Maybe (ImageFormat Nat) )
              ( a   :: Type                    )
            :: Constraint where
  MatchesFormat (Just ('ImageFormat I  _)) a = Integral a 
  MatchesFormat (Just ('ImageFormat UI _)) a = Integral a
  MatchesFormat (Just ('ImageFormat _  _)) a = Floating a
  MatchesFormat _                          _ = ()

type family BasicDim (opName :: Symbol) (props :: ImageProperties) :: Constraint where
  BasicDim _      ( Properties _ _ OneD   _ _ _ _ _ ) = ()
  BasicDim _      ( Properties _ _ TwoD   _ _ _ _ _ ) = ()
  BasicDim _      ( Properties _ _ ThreeD _ _ _ _ _ ) = ()
  BasicDim _      ( Properties _ _ Cube   _ _ _ _ _ ) = ()
  BasicDim opName ( Properties _ _ dim    _ _ _ _ _ )
    = TypeError
        (     Text opName :<>: Text " operand: unexpected image dimensionality "
         :<>: ShowType dim :<>: Text "."
         :$$: Text "Image dimensionality must be 'OneD', 'TwoD', 'ThreeD' or 'Cube'. "
        )

type family NotCubeDim (opName :: Symbol) (props :: ImageProperties) :: Constraint where
  NotCubeDim opName ( Properties _ _ Cube _ _ _ _ _ )
    = TypeError
        (    Text opName :<>: Text " operand: unexpected 'Cube' image dimensionality."
        :$$: Text "Image dimensionality must not be 'Cube'."
        )
  NotCubeDim _ _ = ()

type family CanAddProj (ops :: [OperandName]) :: Constraint where
  CanAddProj '[] = ()
  CanAddProj ( ProjectiveCoords ': _ )
    = TypeError ( Text "'Proj' operand already supplied." )
  CanAddProj ( DepthComparison ': ops ) = CanAddProj ops
  CanAddProj ( op ': _ )
    = TypeError ( Text "'Proj' operand must be provided after " :<>: ShowType op )

type family CanAddDref (ops :: [OperandName]) :: Constraint where
  CanAddDref '[] = ()
  CanAddDref ( DepthComparison ': _ )
    = TypeError ( Text "'Dref' operand already supplied." )
  CanAddDref ( ProjectiveCoords ': ops ) = CanAddDref ops
  CanAddDref ( op ': _ )
    = TypeError ( Text "'Dref' operand must be provided after " :<>: ShowType op )

type family UsesAffineCoords (ops :: [OperandName]) :: Constraint where
  UsesAffineCoords '[] = ()
  UsesAffineCoords ( ProjectiveCoords ': _ )
    = TypeError ( Text "'Gather': unexpected 'Proj' operand; cannot use projective coordinates." )
  UsesAffineCoords ( _ ': ops ) = UsesAffineCoords ops

type NoDuplicate op ops = NoDuplicateFromElem op (op `Elem` ops)

type family NoDuplicateFromElem op dup :: Constraint where
  NoDuplicateFromElem _ False   = ()
  NoDuplicateFromElem op True
    = TypeError ( Text "Duplicate image operand " :<>: ShowType op :<>: Text "." )

type family NoMS (opName :: Symbol) (props :: ImageProperties) :: Constraint where
  NoMS _      (Properties _ _ _ _ _ SingleSampled _ _) = ()
  NoMS opName _
    = TypeError
        ( Text opName :<>: Text " operand: image cannot be multisampled." )

type family CanMultiSample (ms :: ImageProperties) :: Constraint where
  CanMultiSample (Properties _ _ _ _ _ SingleSampled _ _)
    = TypeError ( Text "Cannot multi-sample this single-sampled image." )
  CanMultiSample _ = ()

type family NoLODOps
              ( name  :: Symbol       )
              ( excl :: [LODOperand]  )
              ( ops  :: [OperandName] )
            :: Constraint
            where
  NoLODOps _ _ '[] = ()
  NoLODOps name excl (BaseOperand ('LODOperand op) ': ops)
    = If
        (op `Elem` excl)
        ( TypeError 
            (     Text "Cannot provide " :<>: ShowType name :<>: Text " operand:"
             :$$: ShowType op :<>: Text " operand already provided."
            )
        )
        ( NoLODOps name excl ops )
  NoLODOps name excl ( _ ': ops ) = NoLODOps name excl ops

type family SupportsDepthTest ( props :: ImageProperties ) :: Constraint where
  SupportsDepthTest (Properties _ _ _ (Just NotDepthImage) _ _ _ _)
    = TypeError ( Text "Cannot do a depth comparison: image is not a depth image." )
  SupportsDepthTest _ = ()
