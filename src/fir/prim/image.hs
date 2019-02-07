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
  , DepthTesting(..)
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
  | BaseOperand SPIRV.Image.Operand
  deriving ( Show, Eq, Ord )

data ImageOperand
        ( a    :: Type           )
        ( dim  :: Dimensionality )
        ( proj :: Projection     )
        ( ms   :: MultiSampling  )
        ( op   :: OperandName    )
      :: Type
      where
  Bias :: (BasicDim "Bias" dim, NoMS "Bias" ms)
       => AST Float -> ImageOperand a dim proj ms (BaseOperand ('LODOperand SPIRV.Bias))
  LOD  :: (BasicDim "LOD" dim, NoMS "LOD" ms)
       => AST a -> ImageOperand a dim proj ms (BaseOperand ('LODOperand SPIRV.LOD))
  Grad :: ( Floating a
          , vec ~ ImageCoordinatesType a dim NonArrayed proj
          , NoMS "Grad" ms
          )
       => { grad_x :: AST vec
          , grad_y :: AST vec
          } -> ImageOperand a dim proj ms (BaseOperand ('LODOperand SPIRV.Grad))
  ConstOffsetBy
    :: ( PrimTy vec
       , vec ~ ImageCoordinatesType Int32 dim NonArrayed proj
       )
    => vec
    -> ImageOperand a dim proj ms (BaseOperand SPIRV.ConstOffset)
  OffsetBy
    :: (vec ~ ImageCoordinatesType Int32 dim NonArrayed proj)
    => AST vec
    -> ImageOperand a dim proj ms (BaseOperand SPIRV.Offset)
  ConstOffsetsBy
    :: (Integral a, PrimTy a)
    => Array 4 (V 2 a)
    -> ImageOperand a dim proj ms (BaseOperand SPIRV.ConstOffsets)
  SampleNo :: (Integral a, HasMS "SampleNo" ms)
           => AST a
           -> ImageOperand a dim proj ms (BaseOperand SPIRV.Sample)
  MinLOD :: (Floating a, BasicDim "MinLOD" dim, NoMS "MinLOD" ms)
         => AST a
         -> ImageOperand a dim proj ms (BaseOperand ('LODOperand SPIRV.MinLOD))
  Dref :: AST a -> ImageOperand a dim proj ms DepthComparison

infixr 5 `Op`
data ImageOperandList
        ( a    :: Type           )
        ( dim  :: Dimensionality )
        ( proj :: Projection     )
        ( ms   :: MultiSampling  )
        ( ops  :: [OperandName]  )
        where
  Done :: ImageOperandList a dim proj ms '[]
  Op   :: ( NoDuplicate op (Elem op ops) )
       => ImageOperand     a dim proj ms op
       -> ImageOperandList a dim proj ms ops
       -> ImageOperandList a dim proj ms (op ': ops)

data ImageOperands
        ( meth  :: Maybe SamplingMethod )
        ( props :: ImageProperties      )
        where
  Operands
    :: ( ValidOps   meth ops
       , ValidProj  meth proj  -- ensure affine coordinates if no sampler
       , ValidDepth meth depth -- check for correct depth testing setting
       )
    => ImageOperandList a dim proj ms ops
    -> ImageOperands meth (Properties a b dim depth arr ms usage mbfmt )

type family ValidProj
              ( meth :: Maybe SamplingMethod )
              ( proj :: Projection           )
            :: Constraint
            where
  ValidProj Nothing Projective
    = TypeError ( Text "Cannot use projective coordinates without a sampler." )
  ValidProj _ _ = ()

type family ValidDepth
              ( meth     :: Maybe SamplingMethod )
              ( hasDepth :: Maybe HasDepth       )
            :: Constraint
            where
  ValidDepth Nothing _ = ()
  ValidDepth
    ( Just ( Method DepthTest _ ) )
    ( Just NotDepthImage            )
      = TypeError ( Text "Cannot perform a depth-test: not a depth image." )
  ValidDepth
    ( Just ( Method NoDepthTest _ ) )
    ( Just DepthImage                 )
      = TypeError ( Text "Must perform depth-comparison on this depth image." )
  ValidDepth _ _ = ()

type family ValidOps
                ( meth  :: Maybe SamplingMethod )
                ( ops   :: [OperandName]        )
              :: Constraint
              where
  ValidOps Nothing ops
    = ( NoLODOps ops, NoDepthOp (Elem DepthComparison ops) )
  ValidOps
    ( Just ( Method 'NoDepthTest _ ) )
    ops
      = ( CompatibleLODOps ops, NoDepthOp (Elem DepthComparison ops) )
  ValidOps
    ( Just ( Method 'DepthTest _ ) )
    ops
      = ( CompatibleLODOps ops, HasDepthOp (Elem DepthComparison ops) )


-----------------------------------------------------------

type family ImageCoordinates
              ( meth  :: Maybe SamplingMethod  )
              ( props :: ImageProperties       )
            :: Type where
  ImageCoordinates
    ( Just ( Method _ proj ) )
    ( Properties a _ dim _ arr _ _ _ )
      = ImageCoordinatesType a dim arr proj
  ImageCoordinates
    Nothing
    ( Properties a _ dim _ arr _ _ _ )
      = ImageCoordinatesType a dim arr Affine

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
                ( meth  :: Maybe SamplingMethod )
                ( props :: ImageProperties      )
              :: Type where
  ImageData
    ( Just ( Method DepthTest proj ) )
    ( Properties _ r _ _ _ _ _ _ )
      = r
  ImageData
    ( Just ( Method NoDepthTest proj ) )
    ( Properties _ r _ _ _ _ _ _ )
      = V 4 r
  ImageData
    Nothing
    ( Properties _ r _ _ _ _ _ _ )
      = V 4 r
      
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

type family BasicDim (opName :: Symbol) (dim :: Dimensionality) :: Constraint where
  BasicDim _      OneD   = ()
  BasicDim _      TwoD   = ()
  BasicDim _      ThreeD = ()
  BasicDim _      Cube   = ()
  BasicDim opName dim
    = TypeError
        (     Text opName :<>: Text " operand: unexpected image dimensionality "
         :<>: ShowType dim :<>: Text "."
         :$$: Text "Image dimensionality must be 'OneD', 'TwoD', 'ThreeD' or 'Cube'. "
        )

type family NotCubeDim (opName :: Symbol) (dim :: Dimensionality) :: Constraint where
  NotCubeDim opName Cube
    = TypeError
        (    Text opName :<>: Text " operand: unexpected 'Cube' image dimensionality."
        :$$: Text "Image dimensionality must not be 'Cube'."
        )
  NotCubeDim _ _ = ()

type family NotExplicitLOD (lod :: LODMethod) :: Constraint where
  NotExplicitLOD ExplicitLOD
    = TypeError (     Text "Sampling with 'ExplicitLOD', but 'MinLOD' operand provided."
                 :$$: Text "Cannot set both 'LOD' and 'MinLOD'."
                )
  NotExplicitLOD _ = ()

type family NoDuplicate op dup :: Constraint where
  NoDuplicate _ False   = ()
  NoDuplicate op True
    = TypeError ( Text "Duplicate image operand " :<>: ShowType op :<>: Text "." )

type family NoMS (opName :: Symbol) (ms :: MultiSampling) :: Constraint where
  NoMS _      SingleSampled = ()
  NoMS opName MultiSampled
    = TypeError
        ( Text opName :<>: Text " operand: image cannot be multisampled." )

type family HasMS (opName :: Symbol) (ms :: MultiSampling) :: Constraint where
  NoMS _      MultiSampled = ()
  NoMS opName SingleSampled
    = TypeError
        ( Text opName :<>: Text " operand: image must be multisampled." )

type family NoLODOps (ops :: [OperandName]) :: Constraint where
  NoLODOps '[] = ()
  NoLODOps ((BaseOperand ('LODOperand op)) ': _)
    = TypeError ( ShowType op :<>: Text " operand not allowed; no LOD necessary." )
  NoLODOps (_ ': ops) = NoLODOps ops

type family NoDepthOp (hasDepthOp :: Bool) :: Constraint where
  NoDepthOp False = ()
  NoDepthOp True
    = TypeError ( Text "Sampling without depth comparison: no 'Dref' operand required." )

type family HasDepthOp (hasDepthop :: Bool) :: Constraint where
  HasDepthOp True = ()
  HasDepthOp False
    = TypeError ( Text "Missing 'Dref' operand (depth reference value required to perform depth comparison)." )

type CompatibleLODOps (ops :: [OperandName]) = ( ValidLODMethod (GetLODMethod ops) :: Constraint )

data LODMethod
  = ImplicitLOD
  | ExplicitLOD
  deriving ( Show, Eq, Ord, Enum, Bounded )

-- evaluate the 'GetLODMethod' type family
type family ValidLODMethod (meth :: LODMethod) :: Constraint where
  ValidLODMethod ImplicitLOD = ()
  ValidLODMethod ExplicitLOD = ()
  ValidLODMethod _ = TypeError ( Text "Invalid LOD method." )

type GetLODMethod (ops :: [OperandName])
  = ( GetLODMethodWithAccum '[] ops :: LODMethod )

type family GetLODMethodWithAccum
              ( acc :: [LODOperand] )
              ( ops :: [OperandName]      )
            :: LODMethod where
  GetLODMethodWithAccum acc '[] = GetLODFromAcc acc
  GetLODMethodWithAccum acc (BaseOperand ('LODOperand lod) ': ops)
    = GetLODMethodWithAccum (LODUpdateAcc acc lod) ops
  GetLODMethodWithAccum acc (nonLODop ': ops)
    = GetLODMethodWithAccum acc ops
  
type family GetLODFromAcc (acc :: [LODOperand]) :: LODMethod where
  GetLODFromAcc '[] = ImplicitLOD
  GetLODFromAcc (SPIRV.Grad ': _) = ExplicitLOD
  GetLODFromAcc (SPIRV.LOD ': _ ) = ExplicitLOD
  GetLODFromAcc (SPIRV.Bias ': _) = ImplicitLOD
  GetLODFromAcc (SPIRV.MinLOD ': lods) = GetLODFromAcc lods

type family LODUpdateAcc
                ( acc :: [LODOperand])
                ( lod :: LODOperand  )
              :: [LODOperand] where
  LODUpdateAcc '[] lod = '[lod]
  LODUpdateAcc '[SPIRV.Bias]   SPIRV.MinLOD
    = '[SPIRV.Bias, SPIRV.MinLOD]
  LODUpdateAcc '[SPIRV.MinLOD] SPIRV.Bias
    = '[SPIRV.Bias, SPIRV.MinLOD]
  LODUpdateAcc '[SPIRV.Grad]   SPIRV.MinLOD
    = '[SPIRV.Grad, SPIRV.MinLOD]
  LODUpdateAcc '[SPIRV.MinLOD] SPIRV.Grad
    = '[SPIRV.Grad, SPIRV.MinLOD]
  LODUpdateAcc (lod ': _) lod -- can't happen as we have already checked for duplicates
  -- return a bogus value instead of throwing a second type error
    = '[]
--  = TypeError ( Text "Attempt to set " :<>: ShowType lod :<>: Text " operand more than once." )
  LODUpdateAcc (_ ': _ ': _) SPIRV.MinLOD
    = TypeError ( Text "Attempt to set MinLOD operand more than once." )
  LODUpdateAcc (op ': _) lod
    = TypeError ( Text "Incompatible image operands "
                 :<>: ShowType op
                 :<>: Text " and "
                 :<>: ShowType lod
                 :<>: Text "."
                )
