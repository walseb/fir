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
  , ImageCoordinates, ImageData
  , GradCoordinates, OffsetCoordinates
  , MatchesFormat, BasicDim, NotCubeDim
  , CanAddProj, CanAddDref
  , UsesAffineCoords
  , NoDuplicate
  , NoMS, CanMultiSample
  , NoLODOps, SupportsDepthTest
  )
  where

-- base
import Data.Int
  ( Int32 )
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
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
            :: Constraint where
  ValidImageRead
    ( Properties coords res _ depth _ ms usage fmt )
    ops
      = ( AllowedIndexing (ScalarFromTy coords) ms usage
        , CheckDepthTest (DepthComparison `Elem` ops) (ScalarFromTy coords) depth
        , CheckLODOperands (ScalarFromTy coords) ops
        , CompatibleFormat (ScalarFromTy res) usage fmt
        )

-- | Check that we can write to an image.
--
-- Throws a type error if:
--
--   * the image is not a storage image,
--   * the image is a depth image,
--   * the coordinate type is not an integral type,
--   * an operand is incompatible with the given image properties,
--   * an operand doesn't make sense for write operations.
--
-- Refer to the @SPIR-V@ specification for what operations are allowed.
type family ValidImageWrite
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
           :: Constraint where
  ValidImageWrite
    ( Properties _ _ _ _ _ _ Sampled _ )
    ops
      = TypeError ( Text "Cannot write to a sampled image; must be a storage image." )
  ValidImageWrite
    ( Properties _ _ _ (Just DepthImage) _ _ _ _ )
    ops
      = TypeError ( Text "Cannot write to a depth image." )

  ValidImageWrite
    ( Properties coords res _ _ _ _ usage fmt )
    ops
      = ( IntegralIndexing (ScalarFromTy coords)
        , CompatibleFormat (ScalarFromTy res) usage fmt
        , AllowedWriteOps ops
        )

type family IntegralIndexing (inty :: SPIRV.ScalarTy) :: Constraint where
  IntegralIndexing (SPIRV.Floating _)
    = TypeError ( Text "Cannot write to an image using floating-point coordinates." )
  IntegralIndexing _ = ()

-- Check whether floating-point coordinates are allowed.
type family AllowedIndexing
              ( inty  :: SPIRV.ScalarTy )
              ( ms    :: MultiSampling  )
              ( usage :: ImageUsage     )
            :: Constraint where
  AllowedIndexing (SPIRV.Floating _) _ Storage
    = TypeError
        ( Text "Cannot use floating-point coordinates with a storage image." )
  AllowedIndexing (SPIRV.Floating _) MultiSampled _
    = TypeError
        ( Text "Cannot use floating-point coordinates with multi-sampling." )
  AllowedIndexing _ _ _ = ()

-- Check that depth-testing is appropriately performed.
type family CheckDepthTest
              ( depthTesting :: Bool    )
              ( inty  :: SPIRV.ScalarTy )
              ( depth :: Maybe HasDepth )
            :: Constraint where
  CheckDepthTest False _ (Just DepthImage)
    = TypeError
        ( Text "Must use a depth comparison with this depth image." )
  CheckDepthTest True _ (Just NotDepthImage)
    = TypeError
        ( Text "Cannot perform depth comparison: not a depth image." )
  CheckDepthTest True (SPIRV.Integer _ _ ) _
    = TypeError
        ( Text "Cannot perform depth comparison using integral coordinates." )
  CheckDepthTest _ _ _ = ()

-- If using integral coordinates, LOD instructions cannot be provided.
type family CheckLODOperands
                ( inty :: SPIRV.ScalarTy )
                ( ops  :: [OperandName]  )
              :: Constraint where
  CheckLODOperands (SPIRV.Floating _) _ = ()
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
        (    DepthComparison `Elem` ops
          && Not ( (BaseOperand ConstOffsets) `Elem` ops)
        )
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
    ( Properties a _ dim _ _ _ _ _ )
    ops
      = If
          ( ProjectiveCoords `Elem` ops )
          ( ImageCoordinatesType a dim NonArrayed Projective )
          ( ImageCoordinatesType a dim NonArrayed Affine     )

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
