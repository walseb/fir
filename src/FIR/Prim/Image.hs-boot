{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module FIR.Prim.Image where

-- base
import Data.Int
  ( Int32 )
import Data.Kind
  ( Type )
import Data.Type.Bool
  ( If )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.List
  ( Elem )
import FIR.Prim.Array
  ( Array )
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

--------------------------------------------------

data ImageProperties where
  Properties
    :: Type
    -> Type
    -> Dimensionality
    -> Maybe HasDepth
    -> Arrayness
    -> MultiSampling
    -> ImageUsage
    -> Maybe (ImageFormat Nat)
    -> ImageProperties

type family ImageComponent ( props :: ImageProperties ) :: Type where
  ImageComponent ( Properties a _ _ _ _ _ _ _ ) = a

data Image (props :: ImageProperties)
type role Image phantom

data OperandName
  = DepthComparison
  | ProjectiveCoords
  | BaseOperand SPIRV.Image.Operand

data ImageOperands
        ( props :: ImageProperties )
        ( ops   :: [OperandName]   )
      :: Type
type role ImageOperands phantom phantom

data Gather
  = ComponentGather
  | DrefGather

data GatherInfo val (gather :: Gather) where
 ComponentWithOffsets
   :: val -> Array 4 (V 2 Int32) -> GatherInfo val ComponentGather
 DepthWithOffsets
   ::        Array 4 (V 2 Int32) -> GatherInfo val DrefGather

type role GatherInfo representational nominal

type family WhichGather (ops :: [OperandName]) :: Gather where
  WhichGather ops
    = If
        ( DepthComparison `Elem` ops  )
        DrefGather
        ComponentGather
