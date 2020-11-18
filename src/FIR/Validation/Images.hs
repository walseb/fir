{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Images

Validation module for stateful operations involving images,
such as 'FIR.Syntax.Program.imageRead', 'FIR.Syntax.Program.imageWrite',
using type families with custom type errors.

This checks, for instance, that one doesn't access a non-sampled storage image
using floating-point texel coordinates.

-}

module FIR.Validation.Images
  ( LookupImageProperties
  , ValidImageRead, ValidImageWrite
  , ValidImageCoordinate, ValidImageGradCoordinate, ValidImageOffsetCoordinate 
  , ImageTexelType
  , MatchesFormat, BasicDim, NotCubeDim
  , CanAddProj, CanAddDref
  , UsesAffineCoords
  , NoDuplicate
  , NoMS, CanMultiSample
  , NoLODOps, SupportsDepthTest
  )
  where

-- base
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If, type (&&), Not )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.List
  ( Elem )
import Data.Type.Map
  ( Lookup )
import FIR.Binding
  ( Binding, BindingsMap, Var )
import {-# SOURCE #-} FIR.Prim.Image
  ( ImageProperties(Properties), Image
  , OperandName(DepthComparison, ProjectiveCoords, BaseOperand)
  , ImageCoordinateKind(..)
  )
import FIR.Prim.Singletons
  ( ScalarFromTy )
import FIR.ProgramState
  ( ProgramState(ProgramState) )
import Math.Linear
  ( V )
import SPIRV.Image
  ( ImageUsage(Sampled, Storage)
  , ImageFormat(ImageFormat), I, UI
  , ImageFormatDimension
  , RequiredFormatUsage
  , Arrayness(..)
  , Dimensionality(..)
  , Normalisation(..)
  , HasDepth(..)
  , MultiSampling(..)
  , Operand(..)
  , Projection(..)
  )
import qualified SPIRV.Image    as Image
  ( Component(Integer, Floating)
  , Operand(..)
  )
import qualified SPIRV.Image    as LOD
  ( LODOperand )
import qualified SPIRV.ScalarTy as SPIRV
  ( ScalarTy(Integer, Floating) )

-------------------------------------------------
-- * Constraints for images.

-- | Retrieve the properties of an image.
--
-- Throws a type error if there is no image with given name.
type family LookupImageProperties (k :: Symbol) (i :: ProgramState) :: ImageProperties where
  LookupImageProperties k ('ProgramState i _ _ _ _)
    = ImagePropertiesFromLookup k i (Lookup k i)

type family ImagePropertiesFromLookup
              ( k      :: Symbol        )
              ( i      :: BindingsMap   )
              ( lookup :: Maybe Binding )
            :: ImageProperties
              where
  ImagePropertiesFromLookup _ _ (Just (Var _ (Image props))) = props
  ImagePropertiesFromLookup k i  Nothing
    = TypeError (     Text "Expected an image, \
                           \but nothing is bound by name " :<>: ShowType k
                )
  ImagePropertiesFromLookup k i (Just nonImage)
    = TypeError (     Text "Unexpected type " :<>: ShowType nonImage
                 :<>: Text " bound by name " :<>: ShowType k
                 :$$: Text "Expected an image."
                 )

-- | Check that we can read from an image.
--
-- Depending on the coordinate type, this is either a sampling operation
-- or a direct image read operation.
--
-- Throws a type error if the operation is incompatible with the given
-- image properties. For instance, if the image is a depth image,
-- a depth-comparison reference value must be provided.
--
-- Refer to the @SPIR-V@ specification for what operations are allowed.
type family ValidImageRead
              ( props     :: ImageProperties )
              ( ops       :: [OperandName]   )
              ( coordType :: Type            )
            :: Constraint where
  ValidImageRead
    ( Properties coordKind res dim depth arr ms usage fmt )
    ops
    coordType
      = ( AllowedIndexing coordKind ms usage
        , CheckDepthTest (DepthComparison `Elem` ops) coordKind depth
        , CheckLODOperands coordKind ops
        , CompatibleFormat (ScalarFromTy res) usage fmt
        , ValidCoordinateType "image coordinates (for reading)" (ImageCoordinatesDim dim arr ops) coordKind coordType
        )

type family ValidImageCoordinate
              ( props     :: ImageProperties )
              ( ops       :: [ OperandName ] )
              ( coordType :: Type            )
              where
  ValidImageCoordinate ( Properties coordKind _ dim _ arr _ _ _ ) ops coordType
    = ValidCoordinateType "image coordinates" (ImageCoordinatesDim dim arr ops) coordKind coordType

type family ValidImageGradCoordinate
              ( props     :: ImageProperties )
              ( ops       :: [ OperandName ] )
              ( coordType :: Type            )
              where
  ValidImageGradCoordinate ( Properties coordKind _ dim _ _ _ _ _ ) ops coordType
    = ValidCoordinateType "gradient coordinates" (GradCoordinatesDim dim ops) coordKind coordType

type family ValidImageOffsetCoordinate
              ( props     :: ImageProperties )
              ( ops       :: [ OperandName ] )
              ( coordType :: Type            )
              where
  ValidImageOffsetCoordinate ( Properties coordKind _ dim _ _ _ _ _ ) ops coordType
    = ValidCoordinateType "offset coordinates" (OffsetCoordinatesDim dim ops) coordKind coordType

type family ValidCoordinateType
              ( coordsName :: Symbol )
              ( coordDim   :: Nat )
              ( coordKind  :: ImageCoordinateKind )
              ( coordType  :: Type )
            :: Constraint where
  ValidCoordinateType _ 1 coordKind a         = CoordinateConstraint coordKind a
  ValidCoordinateType _ n coordKind ( V n a ) = CoordinateConstraint coordKind a
  ValidCoordinateType coordsName n coordKind ( V j a ) =
    TypeError 
      (    Text "Use of " :<>: Text coordsName :<>: Text " of the wrong dimension."
      :$$: Text "Expected dimension: " :<>: ShowType n
      :$$: Text "  Actual dimension: " :<>: ShowType j
      )
  ValidCoordinateType coordsName n IntegralCoordinates nonVec =
    TypeError 
      (    Text "Cannot use type " :<>: ShowType nonVec :<>: Text " as " :<>: Text coordsName :<>: Text "."
      :$$: Text "Expected a vector of integral types of dimension " :<>: ShowType n :<>: Text "."
      )
  ValidCoordinateType coordsName n FloatingPointCoordinates nonVec =
    TypeError 
      (    Text "Cannot use type " :<>: ShowType nonVec :<>: Text " as " :<>: Text coordsName :<>: Text "."
      :$$: Text "Expected a vector of floating-point types of dimension " :<>: ShowType n :<>: Text "."
      )


type family CoordinateConstraint ( coordKind :: ImageCoordinateKind ) :: ( Type -> Constraint ) where
  CoordinateConstraint IntegralCoordinates      = Integral
  CoordinateConstraint FloatingPointCoordinates = Floating

-- | Check that we can write to an image.
--
-- Throws a type error if:
--
--   * the image is not a storage image,
--   * the image is a depth image,
--   * the coordinate type is not an integral type, or is of the wrong dimension,
--   * an operand is incompatible with the given image properties,
--   * an operand doesn't make sense for write operations.
--
-- Refer to the @SPIR-V@ specification for what operations are allowed.
type family ValidImageWrite
              ( props     :: ImageProperties )
              ( ops       :: [OperandName]   )
              ( coordType :: Type            )
           :: Constraint where
  ValidImageWrite
    ( Properties _ _ _ _ _ _ Sampled _ )
    ops
    _
      = TypeError ( Text "Cannot write to a sampled image; must be a storage image." )
  ValidImageWrite
    ( Properties _ _ _ (Just DepthImage) _ _ _ _ )
    ops
    _
      = TypeError ( Text "Cannot write to a depth image." )
  ValidImageWrite
    ( Properties coordKind res dim _ arr _ usage fmt )
    ops
    coordType
      = ( IntegralIndexing coordKind
        , CompatibleFormat (ScalarFromTy res) usage fmt
        , AllowedWriteOps ops
        , ValidCoordinateType "image coordinates (for writing)" (ImageCoordinatesDim dim arr ops) coordKind coordType
        )

type family IntegralIndexing (inty :: ImageCoordinateKind) :: Constraint where
  IntegralIndexing FloatingPointCoordinates
    = TypeError ( Text "Cannot write to an image using floating-point coordinates." )
  IntegralIndexing _ = ()

-- Check whether floating-point coordinates are allowed.
type family AllowedIndexing
              ( inty  :: ImageCoordinateKind )
              ( ms    :: MultiSampling      )
              ( usage :: ImageUsage         )
            :: Constraint where
  AllowedIndexing FloatingPointCoordinates _ Storage
    = TypeError
        ( Text "Cannot use floating-point coordinates with a storage image." )
  AllowedIndexing FloatingPointCoordinates MultiSampled _
    = TypeError
        ( Text "Cannot use floating-point coordinates with multi-sampling." )
  AllowedIndexing _ _ _ = ()

-- Check that depth-testing is appropriately performed.
type family CheckDepthTest
              ( depthTesting :: Bool        )
              ( inty  :: ImageCoordinateKind )
              ( depth :: Maybe HasDepth     )
            :: Constraint where
  CheckDepthTest False _ (Just DepthImage)
    = TypeError
        ( Text "Must use a depth comparison with this depth image." )
  CheckDepthTest True _ (Just NotDepthImage)
    = TypeError
        ( Text "Cannot perform depth comparison: not a depth image." )
  CheckDepthTest True IntegralCoordinates _
    = TypeError
        ( Text "Cannot perform depth comparison using integral coordinates." )
  CheckDepthTest _ _ _ = ()

-- If using integral coordinates, LOD instructions cannot be provided.
type family CheckLODOperands
                ( inty :: ImageCoordinateKind )
                ( ops  :: [OperandName]      )
              :: Constraint where
  CheckLODOperands FloatingPointCoordinates _ = ()
  CheckLODOperands _ ( BaseOperand ('LODOperand lod) ': _ )
    = TypeError 
        ( ShowType lod :<>: Text " operand not allowed: using integral coordinates." )
  CheckLODOperands inty ( op ': ops ) = CheckLODOperands inty ops
  CheckLODOperands _ '[] = ()

type family CompatibleFormat
                ( inty  :: SPIRV.ScalarTy          )
                ( usage :: ImageUsage              )
                ( fmt   :: Maybe (ImageFormat Nat) )
              :: Constraint
              where
  CompatibleFormat (SPIRV.Integer _ _) _ (Just ('ImageFormat (Image.Integer Normalised _) _))
    = TypeError
       (    Text "Expected a floating-point type, but provided an integer type."
       :$$: Text "Image uses a normalised integer format, resulting in floating-point texel data."
       )
  CompatibleFormat (SPIRV.Integer _ _) _ (Just ('ImageFormat Image.Floating _))
    = TypeError
       (    Text "Expected a floating-point type, but provided an integer type."
       :$$: Text "Image uses a floating-point format."
       )
  CompatibleFormat (SPIRV.Floating _) _ (Just ('ImageFormat (Image.Integer Unnormalised _) _))
    = TypeError
       (    Text "Expected an integral type, but provided a floating-point type."
       :$$: Text "Image uses unnormalised integers, resulting in integral texel data."
       )
  CompatibleFormat _ Storage _
    = ()
  CompatibleFormat _ Sampled (Just fmt)
    = If
        ( RequiredFormatUsage fmt == Just Storage )
        ( TypeError
           (     Text "Image format " :<>: ShowType fmt
            :<>: Text " can only be used with storage images."
            )
        )
        ( () :: Constraint )

-- Only 'Sample' and 'Offset'/'ConstOffset' image operands allowed.
type family AllowedWriteOps (ops :: [OperandName]) :: Constraint where
  AllowedWriteOps '[] = ()
  AllowedWriteOps (BaseOperand Image.Sample      ': ops) = AllowedWriteOps ops
  AllowedWriteOps (BaseOperand Image.ConstOffset ': ops) = AllowedWriteOps ops
  AllowedWriteOps (BaseOperand Image.Offset      ': ops) = AllowedWriteOps ops
  AllowedWriteOps (op                            ': _  )
    = TypeError (     Text "Image operand " :<>: ShowType op
                 :<>: Text " cannot be used in conjunction \
                           \with an image write operation."
                )



----------------------------------------------------------
-- Computing relevant image coordinate type.

-- | Dimension of the coordinates used by an image.
--
-- For instance, a simple 2D texture requires a dimension 2 vector,
-- simple 3D textures and cube maps requires a dimension 3 vector, etc.
type family ImageCoordinatesDim
              ( dim   :: Dimensionality )
              ( arr   :: Arrayness      )
              ( ops   :: [OperandName]  )
            :: Nat where
  ImageCoordinatesDim dim arr ops =
    If ( ProjectiveCoords `Elem` ops )
      ( ComputeCoordsDim dim arr Projective )
      ( ComputeCoordsDim dim arr Affine )

-- | Texel type of an image.
--
-- This is what is returned from an image sampling operation,
-- or needs to be provided for an image write operation.
type family ImageTexelType
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
            :: Type
            where
  ImageTexelType ( Properties _ r _ _ _ _ _ fmt ) ops
    = If
        (    DepthComparison `Elem` ops
          && Not ( (BaseOperand ConstOffsets) `Elem` ops)
        )
        r
        ( MkTexelType fmt r )

type family MkTexelType ( fmt :: Maybe ( ImageFormat Nat ) ) ( texel :: Type ) :: Type where
  MkTexelType Nothing      texel = V 4 texel -- most flexible default type for unknown image format
  MkTexelType ( Just fmt ) texel = MkTexelType' ( ImageFormatDimension fmt ) texel

type family MkTexelType' ( dim :: Nat ) ( texel :: Type ) :: Type where
  MkTexelType' 1 texel = texel
  MkTexelType' n texel = V n texel

type family ComputeCoordsDim
              ( dim  :: Dimensionality )
              ( arr  :: Arrayness      )
              ( proj :: Projection     )
              :: Nat
              where
  ComputeCoordsDim OneD        NonArrayed Affine     = 1
  ComputeCoordsDim TwoD        NonArrayed Affine     = 2
  ComputeCoordsDim ThreeD      NonArrayed Affine     = 3
  ComputeCoordsDim Cube        NonArrayed Affine     = 3
  ComputeCoordsDim Rect        NonArrayed Affine     = 2
  ComputeCoordsDim Buffer      NonArrayed Affine     = 1
  ComputeCoordsDim SubpassData NonArrayed Affine     = 2
  ComputeCoordsDim OneD        NonArrayed Projective = 2
  ComputeCoordsDim TwoD        NonArrayed Projective = 3
  ComputeCoordsDim ThreeD      NonArrayed Projective = 4
  ComputeCoordsDim Cube        NonArrayed Projective = 4
  ComputeCoordsDim Rect        NonArrayed Projective = 3
  ComputeCoordsDim Buffer      NonArrayed Projective = 2
  ComputeCoordsDim SubpassData NonArrayed Projective = 3
  ComputeCoordsDim OneD        Arrayed    Affine     = 2
  ComputeCoordsDim TwoD        Arrayed    Affine     = 3
  ComputeCoordsDim Cube        Arrayed    Affine     = 3 -- weird stuff going on here
  ComputeCoordsDim dim         Arrayed    Affine
    = TypeError ( Text "Unsupported arrayed image format \
                       \with dimensionality "
                  :<>: ShowType dim
                )
  ComputeCoordsDim dim         Arrayed    Projective
    = TypeError ( Text "Cannot use projective coordinates with an arrayed image." )

-- which coordinates to use to provide explicit derivatives
type family GradCoordinatesDim
              ( props :: Dimensionality  )
              ( ops   :: [OperandName]   )
            :: Nat where
  GradCoordinatesDim dim ops =
    If ( ProjectiveCoords `Elem` ops )
      ( ComputeCoordsDim dim NonArrayed Projective )
      ( ComputeCoordsDim dim NonArrayed Affine     )

-- which coordinates to use to provide an offset
type family OffsetCoordinatesDim
              ( props :: Dimensionality  )
              ( ops   :: [OperandName]   )
            :: Nat where
  OffsetCoordinatesDim dim ops =
    If ( ProjectiveCoords `Elem` ops )
      ( ComputeCoordsDim dim NonArrayed Projective )
      ( ComputeCoordsDim dim NonArrayed Affine     )


-----------------------------------------------------------
-- Validation of image operands.

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
              ( name :: Symbol           )
              ( excl :: [LOD.LODOperand] )
              ( ops  :: [OperandName]    )
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
