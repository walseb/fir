{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module FIR.Synonyms where

-- base
import Prelude
  hiding ( Floating, Integral )
import Data.Int
  ( Int8, Int16, Int32 )
import Data.Kind
  ( Type )
import Data.Word
  ( Word8, Word16, Word32 )
import GHC.TypeNats
  ( Nat )

-- half
import Numeric.Half
  ( Half )

-- fir
import FIR.AST
  ( AST(Ops) )
import FIR.Definition
  ( Definition(Global) )
import qualified FIR.Definition as Def
import FIR.Prim.Image
  ( ImageProperties(Properties)
  , Image, ImageOperands
  )
import qualified FIR.Prim.Image as Op -- for image operands
import SPIRV.ScalarTy
  ( Signedness(..) )
import SPIRV.Image
  ( Dimensionality(..)
  , Arrayness(..)
  , MultiSampling(..)
  , Normalisation(..)
  , ImageUsage(..)
  , ImageFormat(..)
  )
import qualified SPIRV.Image   as Image
import qualified SPIRV.Storage as Storage
import SPIRV.FunctionControl
  ( NoFunctionControl )

-- synonyms for (decorated) globals
type UniformConstant decs ty = Global Storage.UniformConstant decs ty
type Input           decs ty = Global Storage.Input           decs ty
type Output          decs ty = Global Storage.Output          decs ty
type Uniform         decs ty = Global Storage.Uniform         decs ty
type PushConstant    decs ty = Global Storage.PushConstant    decs ty
type StorageBuffer   decs ty = Global Storage.StorageBuffer   decs ty

-- synonym for function with no function control information
type Function     as b = Def.Function NoFunctionControl as b
type Function' fc as b = Def.Function fc as b

-- synonyms for images
type Texture1D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Float (FormatDefault fmt) OneD   Nothing NonArrayed SingleSampled Sampled (Just fmt))
      )
type Texture2D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Float (FormatDefault fmt) TwoD   Nothing NonArrayed SingleSampled Sampled (Just fmt))
      )
type Texture3D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Float (FormatDefault fmt) ThreeD Nothing NonArrayed SingleSampled Sampled (Just fmt))
      )

type Texture decs props
  = Global
      Storage.UniformConstant
      decs
      ( Image props )
type StorageImage decs props
  = Global
      Storage.Image
      decs
      ( Image props )

-- pattern synonyms for image operands
-- these perform wrapping/unwrapping for nicer user syntax... quite hacky
pattern NoOperands = Ops Op.Done
pattern Done       = Ops Op.Done
pattern Proj       = Ops Op.Proj
pattern Dref           dref ops = Ops (Op.Dref           dref (UnOps ops))
pattern Bias           b    ops = Ops (Op.Bias           b    (UnOps ops))
pattern LOD            lod  ops = Ops (Op.LOD            lod  (UnOps ops))
pattern MinLOD         lod  ops = Ops (Op.MinLOD         lod  (UnOps ops))
pattern Grad         gx gy  ops = Ops (Op.Grad         gx gy  (UnOps ops))
pattern ConstOffsetBy  off  ops = Ops (Op.ConstOffsetBy  off  (UnOps ops))
pattern OffsetBy       off  ops = Ops (Op.OffsetBy       off  (UnOps ops))
pattern ConstOffsetsBy offs ops = Ops (Op.ConstOffsetsBy offs (UnOps ops))
pattern SampleNo       no   ops = Ops (Op.SampleNo       no   (UnOps ops))


pattern UnOps :: AST (ImageOperands props ops) -> ImageOperands props ops
pattern UnOps astOps <- ( Ops -> astOps )
  where UnOps (Ops ops) = ops
        UnOps _ = error "weird image operands"

-----------------------------------------------------
-- helper type family for choosing image texel type

type family FormatDefault ( fmt :: ImageFormat Nat ) :: Type where
  FormatDefault ('ImageFormat Image.Floating (16 ': _)) = Half
  FormatDefault ('ImageFormat Image.Floating (_  ': _)) = Float
  FormatDefault ('ImageFormat (Image.Integer Normalised _) _ ) = Float
  FormatDefault ('ImageFormat (Image.Integer Unnormalised Signed  ) ( 8  ': _ )) = Int8
  FormatDefault ('ImageFormat (Image.Integer Unnormalised Signed  ) ( 16 ': _ )) = Int16
  FormatDefault ('ImageFormat (Image.Integer Unnormalised Signed  ) ( _  ': _ )) = Int32
  FormatDefault ('ImageFormat (Image.Integer Unnormalised Unsigned) ( 8  ': _ )) = Word8
  FormatDefault ('ImageFormat (Image.Integer Unnormalised Unsigned) ( 16 ': _ )) = Word16
  FormatDefault ('ImageFormat (Image.Integer Unnormalised Unsigned) ( _  ': _ )) = Word32

