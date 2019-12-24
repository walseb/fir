{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeOperators   #-}

{-|
Module: FIR.AST.Images

Handling image operands.
-}

module FIR.AST.Images where

-- base
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )

-- containers
import Data.Tree
 ( Tree(Node) )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( pattern VF )

-- fir
import FIR.AST.Display
  ( Display(toTreeArgs) )
import FIR.AST.Type
  ( AugType(Val) )
import {-# SOURCE #-} FIR.Prim.Image
  ( ImageOperands, OperandName(..)
  , ImageComponent
  , GradCoordinates, OffsetCoordinates
  , GatherInfo(..), WhichGather
  )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( PrimTy )
import FIR.Validation.Images
  ( BasicDim, NotCubeDim
  , CanAddProj, CanAddDref
  , UsesAffineCoords
  , NoDuplicate
  , NoMS, CanMultiSample
  , NoLODOps
  )
import qualified SPIRV.Image as SPIRV

------------------------------------------------------------

infixl 5 `Dref`
infixl 5 `Bias`
infixl 5 `LOD`
infixl 5 `MinLOD`
infixl 5 `Grad`
infixl 5 `ConstOffsetBy`
infixl 5 `OffsetBy`
infixl 5 `Gather`
infixl 5 `SampleNo`

pattern NilOps                = VF NilOpsF
pattern Proj              ops = VF (ProjF              ops)
pattern Dref          ref ops = VF (DrefF          ref ops)
pattern Bias         bias ops = VF (BiasF         bias ops)
pattern LOD           lod ops = VF (LODF           lod ops)
pattern MinLOD        lod ops = VF (MinLODF        lod ops)
pattern Grad         grad ops = VF (GradF         grad ops)
pattern ConstOffsetBy off ops = VF (ConstOffsetByF off ops)
pattern OffsetBy      off ops = VF (OffsetByF      off ops)
pattern Gather       gath ops = VF (GatherF       gath ops)
pattern SampleNo      sno ops = VF (SampleNoF      sno ops)

data ImgOpsF ( ast :: AugType -> Type ) ( t :: AugType ) where

  -- | End of (snoc-)list of image operands.
  NilOpsF :: ImgOpsF ast ( Val ( ImageOperands props '[] ) )

  -- | Use projective coordinates.
  --
  -- Must be provided after all other operands,
  -- except possibly 'Dref'.
  ProjF
    :: CanAddProj ops
    => ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (ProjectiveCoords ': ops) ) )

  -- | Provide a depth-comparison reference value.
  --
  -- Must be provided after all other operands,
  -- except possibly 'Proj'.
  DrefF
    :: CanAddDref ops
    => ast ( Val ( ImageComponent props ) ) -- ^ Reference value used to perform the depth comparison.
    -> ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (DepthComparison ': ops) ) )

  -- | Add a bias to the implicit level of detail.
  BiasF
    :: ( BasicDim "Bias" props
       , NoMS "Bias" props
       , NoDuplicate (BaseOperand ('SPIRV.LODOperand SPIRV.Bias)) ops
       , NoLODOps "Bias" '[SPIRV.LOD, SPIRV.Grad] ops
       )
    => ast ( Val ( ImageComponent props ) ) -- ^ Bias.
    -> ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (BaseOperand ('SPIRV.LODOperand SPIRV.Bias) ': ops) ) )

  -- | Provide an explicit level of detail.
  LODF
    :: ( BasicDim "LOD" props
       , NoMS "LOD" props
       , NoDuplicate (BaseOperand ('SPIRV.LODOperand SPIRV.LOD)) ops
       , NoLODOps "LOD" '[SPIRV.Bias, SPIRV.Grad, SPIRV.MinLOD] ops
       )
     => ast ( Val ( ImageComponent props ) ) -- ^ LOD.
     -> ast ( Val ( ImageOperands props ops ) )
     -> ImgOpsF ast ( Val ( ImageOperands props (BaseOperand ('SPIRV.LODOperand SPIRV.LOD ) ': ops) ) )

  -- | Specify the minimum level of detail to use
  -- when sampling the image.
  MinLODF
    :: ( BasicDim "MinLOD" props
       , NoMS "MinLOD" props
       , NoDuplicate (BaseOperand ('SPIRV.LODOperand SPIRV.MinLOD)) ops
       , NoLODOps "MinLOD" '[SPIRV.LOD, SPIRV.Bias] ops
       )
    => ast ( Val ( ImageComponent props ) ) -- ^ Minimum LOD.
    -> ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (BaseOperand ('SPIRV.LODOperand SPIRV.MinLOD ) ': ops) ) )

  -- | Provide explicit derivatives.
  GradF
    :: ( vec ~ GradCoordinates props ops
       , NoMS "Grad" props
       , NoDuplicate (BaseOperand ('SPIRV.LODOperand SPIRV.Grad)) ops
       , NoLODOps "Grad" '[ SPIRV.Bias, SPIRV.LOD ] ops
       )
    => ( ast (Val vec), ast (Val vec) ) -- ^ Gradient: ( df\/dx, df\/dy ).
    -> ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (BaseOperand ('SPIRV.LODOperand SPIRV.Grad) ': ops) ) )

  -- | Add a constant offset to the coordinates.
  ConstOffsetByF
    :: ( PrimTy vec
       , vec ~ OffsetCoordinates props ops
       , NoDuplicate (BaseOperand SPIRV.ConstOffset) ops
       , NotCubeDim "ConstOffsetBy" props
       )
    => vec -- Offset (a Haskell constant).
    -> ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (BaseOperand SPIRV.ConstOffset ': ops) ) )

  -- | Add an offset to the coordinates.
  OffsetByF
    :: ( PrimTy vec
       , vec ~ OffsetCoordinates props ops
       , NoDuplicate (BaseOperand SPIRV.Offset) ops
       , NotCubeDim "OffsetBy" props
       )
    => ast ( Val vec ) -- Offset.
    -> ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (BaseOperand SPIRV.Offset ': ops) ) )

  -- | Specify an image gather operation: gather texel data from 4 specified texel coordinates.
  GatherF
    :: ( NotCubeDim "Gather" props
       , NoDuplicate (BaseOperand SPIRV.ConstOffsets) ops
       , NoLODOps "Gather" '[ SPIRV.LOD, SPIRV.Grad, SPIRV.Bias, SPIRV.MinLOD ] ops
       , UsesAffineCoords ops
       )
    => GatherInfo ( ast ( Val Word32 ) ) (WhichGather ops)
    -> ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (BaseOperand SPIRV.ConstOffsets ': ops) ) )

  -- | Specify which sample number to use in a multi-sampled image.
  SampleNoF
    :: ( CanMultiSample props
       , NoDuplicate (BaseOperand SPIRV.Sample) ops
       )
    => ast ( Val Word32 ) -- ^ Sample number.
    -> ast ( Val ( ImageOperands props ops ) )
    -> ImgOpsF ast ( Val ( ImageOperands props (BaseOperand SPIRV.Sample ': ops) ) )

------------------------------------------------------------
-- displaying

instance Display ast => Display (ImgOpsF ast) where
  toTreeArgs as NilOpsF = pure $ Node "NilOps" as
  toTreeArgs as (ProjF ops) = do
    a <- toTreeArgs [] ops
    pure $ Node "Proj" (a:as)
  toTreeArgs as (DrefF ref ops) = do
    r <- toTreeArgs [] ref
    a <- toTreeArgs [] ops
    pure $ Node "Dref" (r:a:as)
  toTreeArgs as (BiasF bias ops) = do
    b <- toTreeArgs [] bias
    a <- toTreeArgs [] ops
    pure $ Node "Bias=" (b:a:as)
  toTreeArgs as (LODF lod ops) = do
    l <- toTreeArgs [] lod
    a <- toTreeArgs [] ops
    pure $ Node "LOD" (l:a:as)
  toTreeArgs as (MinLODF lod ops) = do
    l <- toTreeArgs [] lod
    a <- toTreeArgs [] ops
    pure $ Node "MinLOD" (l:a:as)
  toTreeArgs as (GradF (dfdx, dfdy) ops) = do
    x <- toTreeArgs [] dfdx
    y <- toTreeArgs [] dfdy
    a <- toTreeArgs [] ops
    pure $ Node "Grad" (x:y:a:as)
  toTreeArgs as (ConstOffsetByF vec ops) = do
    a <- toTreeArgs [] ops
    pure $ Node "ConstOffsetBy" (pure (show vec):a:as)
  toTreeArgs as (OffsetByF vec ops) = do
    v <- toTreeArgs [] vec
    a <- toTreeArgs [] ops
    pure $ Node "OffsetBy" (v:a:as)
  toTreeArgs as (GatherF (ComponentWithOffsets comp offs) ops) = do
    c <- toTreeArgs [] comp
    a <- toTreeArgs [] ops
    pure $ Node "GatherComponentWithOffsets" (c:pure (show offs):a:as)
  toTreeArgs as (GatherF (DepthWithOffsets offs) ops) = do
    a <- toTreeArgs [] ops
    pure $ Node "GatherDepthWithOffsets" (pure (show offs):a:as)
  toTreeArgs as (SampleNoF no ops) = do
    n <- toTreeArgs [] no
    a <- toTreeArgs [] ops
    pure $ Node "SampleNo" (n:a:as)
