{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
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

{-|
Module: FIR.Prim.Image

This module defines the 'Image' datatype,
which is a void datatype used to manipulate image handles.

This datatype is annotated at the type-level with various properties
describing the image, such as the format of the image (e.g. RGBA8, depth image, etc).
-}

module FIR.Prim.Image
  ( -- * Images
    -- ** Image properties
    ImageProperties(Properties)
  -- ** Opaque image data type
  , Image
  -- ** Type reflection of image properties
  -- $reflection
  , ImageAndCoordinate(..)
  , ImageComponent
  , knownImage, knownImageCoordinateComponent

  -- * Image operands
  , OperandName(..), ImageOperands
  -- ** Specific data type for gathering operations
  , Gather(..), GatherInfo(..)

  -- * Compute the coordinate and texel type of an image
  , ImageCoordinates, ImageData

  -- * Helper type families for checking validity of image operands
  , CanAddProj, CanAddDref, BasicDim, NotCubeDim
  , NoMS, NoDuplicate, CanMultiSample
  , NoLODOps, UsesAffineCoords
  , WhichGather, GradCoordinates, OffsetCoordinates
  )
  where

-- base
import Prelude
  hiding ( Floating, Integral )
import Data.Int
  ( Int32 )
import Data.Kind
  ( Type )
import Data.Type.Bool
  ( If )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.List
  ( Elem )
import FIR.Prim.Array
  ( Array )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( ScalarTy, scalarTy )
import FIR.Validation.Images
  ( ImageCoordinates, ImageData
  , GradCoordinates, OffsetCoordinates
  , MatchesFormat, BasicDim, NotCubeDim
  , CanAddProj, CanAddDref
  , UsesAffineCoords
  , NoDuplicate
  , NoMS, CanMultiSample
  , NoLODOps
  )
import Math.Linear
  ( V )
import SPIRV.Image
  ( Arrayness(..)
  , Dimensionality(..)
  , HasDepth(..)
  , ImageUsage(..)
  , ImageFormat(..)
  , MultiSampling(..)
  , Operand
  )
import qualified SPIRV.Image    as SPIRV
  ( Image(..) )
import qualified SPIRV.ScalarTy as SPIRV
  ( ScalarTy )

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
newtype ImageAndCoordinate
  = ImageAndCoordinate (SPIRV.Image, SPIRV.ScalarTy)

instance Demotable ImageProperties where
  type Demote ImageProperties = ImageAndCoordinate

instance ( ScalarTy                        coordComp
         , ScalarTy                        texelComp
         , Known Dimensionality            dimensionality
         , Known (Maybe HasDepth)          hasDepth
         , Known Arrayness                 arrayness
         , Known MultiSampling             multiSampling
         , Known ImageUsage                imageUsage
         , Known (Maybe (ImageFormat Nat)) imageFormat
         , MatchesFormat imageFormat texelComp
         )
  => Known ImageProperties
      ( 'Properties
           coordComp texelComp
           dimensionality hasDepth
           arrayness multiSampling
           imageUsage imageFormat
      )
  where
  known
    = ImageAndCoordinate
        ( SPIRV.Image
            { SPIRV.texelComponent   = scalarTy   @texelComp
            , SPIRV.dimensionality   = knownValue @dimensionality
            , SPIRV.hasDepth         = knownValue @hasDepth
            , SPIRV.arrayness        = knownValue @arrayness
            , SPIRV.multiSampling    = knownValue @multiSampling
            , SPIRV.imageUsage = Just (knownValue @imageUsage)
            , SPIRV.imageFormat      = knownValue @imageFormat
            }
        , scalarTy @coordComp
        )


-- $reflection
--
-- As image properties are provided at the type level,
-- we use the 'Data.Type.Known.Known' mechanism to reflect
-- this type-level information to the value level.

-- | Provided image properties at the type-level,
-- return the component type of the image's texels.
knownImageCoordinateComponent
  :: forall props. Known ImageProperties props
  => SPIRV.ScalarTy
knownImageCoordinateComponent
  = case knownValue @props of
      ImageAndCoordinate (_, comp) -> comp

-- | Return the 'SPIRV.Image.Image' type with the given properties.
knownImage :: forall props. Known ImageProperties props => SPIRV.Image
knownImage
  = case knownValue @props of
      ImageAndCoordinate (img, _) -> img

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
  deriving stock ( Show, Eq, Ord )

-- | == Image operands.
--
-- As there is no support for images in the evaluator, this datatype holds no information.
--
-- However, values of type @Code (ImageOperands props ops)@
-- can be constructed using the constructors of the AST,
-- and these contain information necessary for image sample/read/write operations.
--
-- === Overview
-- Image operands behave like a linked list of operands, with a /twist/:
--
--   - the type of an image operand in the list can depend
--     on which operands appear further down in the list.
--
-- Consider for instance the image operands:
--
--    @ Grad grad $ Proj $ NilOps @
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
  = SomeImageOperands
    deriving stock ( Eq, Ord, Show, Bounded, Enum )

-----------------------------------------------------------
-- data type for gather image operand

data Gather
  = ComponentGather
  | DrefGather
  deriving stock ( Show, Eq, Ord, Bounded, Enum )

-- | Information hat needs to be provided to the 'Gather' image operand.
--
-- This consists of 4 offsets, together with the information
-- of which image component (0,1,2,3) to gather
-- in the case that a depth test is /not/ being performed.
--
-- Instantiated with val ~ AST Word32 (left as a parameter to avoid cylic module dependencies).
data GatherInfo val (gather :: Gather) where
  ComponentWithOffsets
    :: val -> Array 4 (V 2 Int32) -> GatherInfo val ComponentGather
  DepthWithOffsets
    ::        Array 4 (V 2 Int32) -> GatherInfo val DrefGather

-- Computes whether a component index needs to be provided to the
-- 'Gather' image operand.
type family WhichGather (ops :: [OperandName]) :: Gather where
  WhichGather ops
    = If
        ( DepthComparison `Elem` ops  )
        DrefGather
        ComponentGather
