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
  )
  where

-- base
import Data.Kind
  ( Constraint )
import Data.Type.Bool
  ( If )
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
import FIR.Prim.Image
  ( ImageProperties(Properties), Image
  , OperandName(DepthComparison, BaseOperand)
  )
import FIR.Prim.Singletons
  ( ScalarFromTy )
import FIR.ProgramState
  ( ProgramState(ProgramState) )
import SPIRV.Image
  ( ImageUsage(Sampled, Storage)
  , ImageFormat(ImageFormat), RequiredFormatUsage
  , Normalisation(..)
  , HasDepth(..)
  , MultiSampling(..)
  , Operand(LODOperand)
  )
import qualified SPIRV.Image    as Image
  ( Component(Integer, Floating)
  , Operand(..)
  )
import qualified SPIRV.ScalarTy as SPIRV
  ( ScalarTy(Integer, Floating) )

-------------------------------------------------
-- * Constraints for images.

-- | Retrieve the properties of an image.
--
-- Throws a type error if there is no image with given name.
type family LookupImageProperties (k :: Symbol) (i :: ProgramState) :: ImageProperties where
  LookupImageProperties k ('ProgramState i _ _ _)
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
