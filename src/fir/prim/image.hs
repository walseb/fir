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

module FIR.Prim.Image where

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
  , SamplingMethod(..)
  )
import qualified SPIRV.Image  as SPIRV
  ( Image(..), Operand(..), LODOperand(..) )
import qualified SPIRV.PrimTy as SPIRV
  ( PrimTy )

--------------------------------------------------

-- | Abstract handle to an image.
data Image (props :: ImageProperties) where

data ImageProperties where
  Properties
    :: Type -- component type for accessing image data
    -> Type -- result component type
    -> Dimensionality
    -> Maybe HasDepth
    -> Arrayness
    -> MultiSampling
    -> ImageUsage
    -> Maybe (ImageFormat Nat)
    -> ImageProperties

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

knownImageReturnComponent :: forall props. Known ImageProperties props
                          => SPIRV.PrimTy
knownImageReturnComponent
  = case knownValue @props of
      ImageAndComponent (_, comp) -> comp

knownImage :: forall props. Known ImageProperties props => SPIRV.Image
knownImage
  = case knownValue @props of
      ImageAndComponent (img, _) -> img

data OperandName
  = DepthComparison
  | ProjectiveCoords
  | BaseOperand SPIRV.Image.Operand
  deriving ( Show, Eq, Ord )

data ImageOperands
       ( props :: ImageProperties )
       ( ops   :: [OperandName]   )
     :: Type
     where
  Done :: ImageOperands props '[ ]
  Proj :: ImageOperands props '[ ProjectiveCoords ]
  Dref :: ( NoDuplicate DepthComparison ops )
       => AST a
       -> ImageOperands props ops
       -> ImageOperands props (DepthComparison ': ops)
  Bias :: ( BasicDim "Bias" props
          , NoMS "Bias" props
          , NoDuplicate (BaseOperand ('LODOperand SPIRV.Bias)) ops
          , NoLODOps "Bias" '[SPIRV.LOD, SPIRV.Grad] ops
          )
       => AST Float
       -> ImageOperands props ops 
       -> ImageOperands props (BaseOperand ('LODOperand SPIRV.Bias) ': ops)
  LOD  :: ( BasicDim "LOD" props
          , NoMS "LOD" props
          , NoDuplicate (BaseOperand ('LODOperand SPIRV.LOD)) ops
          , NoLODOps "LOD" '[SPIRV.Bias, SPIRV.Grad, SPIRV.MinLOD] ops
          )
       => AST a
       -> ImageOperands props ops
       -> ImageOperands props (BaseOperand ('LODOperand SPIRV.LOD ) ': ops)
  MinLOD :: ( BasicDim "MinLOD" props
            , NoMS "MinLOD" props
            , NoDuplicate (BaseOperand ('LODOperand SPIRV.MinLOD)) ops
            , NoLODOps "MinLOD" '[SPIRV.LOD, SPIRV.Bias] ops
            )
         => AST a
         -> ImageOperands props ops
         -> ImageOperands props (BaseOperand ('LODOperand SPIRV.MinLOD ) ': ops)
  Grad :: ( vec ~ GradCoordinates props ops
          , NoMS "Grad" props
          , NoLODOps "Grad" '[ SPIRV.Bias, SPIRV.LOD ] ops
          )
       => AST vec -- df/dx
       -> AST vec -- df/dy
       -> ImageOperands props ops
       -> ImageOperands props (BaseOperand ('LODOperand SPIRV.Grad) ': ops)
  ConstOffsetBy
    :: ( PrimTy vec
       , vec ~ OffsetCoordinates props ops
       , NotCubeDim "ConstOffsetBy" props
       )
    => vec
    -> ImageOperands props ops
    -> ImageOperands props (BaseOperand SPIRV.ConstOffset ': ops)
  OffsetBy
    :: ( PrimTy vec
       , vec ~ OffsetCoordinates props ops
       , NotCubeDim "OffsetBy" props
       )
    => AST vec
    -> ImageOperands props ops
    -> ImageOperands props (BaseOperand SPIRV.Offset ': ops)
  ConstOffsetsBy
    :: ( NotCubeDim "ConstOffsetsBy" props )
    => Array 4 (V 2 Int32)
    -> ImageOperands props ops
    -> ImageOperands props (BaseOperand SPIRV.ConstOffsets ': ops)
  SampleNo
    :: ( CanMultiSample props )
    => AST Word32
    -> ImageOperands props ops
    -> ImageOperands props (BaseOperand SPIRV.Sample ': ops)


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


type family ValidProj
              ( meth :: Maybe SamplingMethod )
              ( proj :: Projection           )
            :: Constraint
            where
  ValidProj Nothing Projective
    = TypeError ( Text "Cannot use projective coordinates without a sampler." )
  ValidProj _ _ = ()

type family SupportsDepthTest ( props :: ImageProperties ) :: Constraint where
  SupportsDepthTest (Properties _ _ _ (Just NotDepthImage) _ _ _ _)
    = TypeError ( Text "Cannot do a depth comparison: image is not a depth image." )
  SupportsDepthTest _ = ()

-----------------------------------------------------------

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
  -- TODO: can only multisample when not using a sampler!!


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
