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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

{-|
Module: FIR.Swizzle

This module provides various convenience synonyms
to mitigate verbosity in user-written programs.

-}

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
import Control.Type.Optic
  ( Optic(..), Index
  , (:.:), (:*:)
  , Joint
  , EndProd
  )
import FIR.AST
  ( AST(Ops) )
import FIR.Definition
  ( Definition(Global) )
import qualified FIR.Definition as Def
import FIR.Instances.AST
  ( )
import FIR.Instances.Optics
  ( )
import FIR.Prim.Image
  ( ImageProperties(Properties)
  , Image, ImageOperands, GatherInfo(..)
  )
import qualified FIR.Prim.Image as Op -- for image operands
  ( ImageOperands(..) )
import FIR.Prim.Struct
  ( LocationSlot(LocationSlot) )
import Math.Linear
  ( V, M )
import SPIRV.ScalarTy
  ( Signedness(..) )
import SPIRV.Image
  ( Dimensionality(..)
  , HasDepth(..)
  , Arrayness(..)
  , MultiSampling(..)
  , Normalisation(..)
  , ImageUsage(..)
  , ImageFormat(..)
  )
import SPIRV.Control
  ( NoFunctionControl )
import qualified SPIRV.Image   as Image
import qualified SPIRV.Storage as Storage

--------------------------------------------------------------------------

-- synonyms for (decorated) globals
type UniformConstant decs ty = Global Storage.UniformConstant decs ty
type Input           decs ty = Global Storage.Input           decs ty
type Output          decs ty = Global Storage.Output          decs ty
type Uniform         decs ty = Global Storage.Uniform         decs ty
type PushConstant    decs ty = Global Storage.PushConstant    decs ty
type StorageBuffer   decs ty = Global Storage.StorageBuffer   decs ty
type Private         decs ty = Global Storage.Private         decs ty

-- synonym for function with no function control information
type Function     as b = Def.Function NoFunctionControl as b
type Function' fc as b = Def.Function fc as b

-- synonym for interface location slots
type Slot (l :: Nat) (c :: Nat) = 'LocationSlot l c

-- synonyms for images
type Texture1D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Float  (FormatDefault fmt) OneD   (Just NotDepthImage) NonArrayed SingleSampled Sampled (Just fmt))
      )
type Texture2D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Float  (FormatDefault fmt) TwoD   (Just NotDepthImage) NonArrayed SingleSampled Sampled (Just fmt))
      )
type Texture3D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Float  (FormatDefault fmt) ThreeD (Just NotDepthImage) NonArrayed SingleSampled Sampled (Just fmt))
      )

type Image1D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Word32 (FormatDefault fmt) OneD   (Just NotDepthImage) NonArrayed SingleSampled Storage (Just fmt))
      )
type Image2D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Word32 (FormatDefault fmt) TwoD   (Just NotDepthImage) NonArrayed SingleSampled Storage (Just fmt))
      )
type Image3D decs fmt
  = Global Storage.UniformConstant
      decs
      ( Image
         (Properties Word32 (FormatDefault fmt) ThreeD (Just NotDepthImage) NonArrayed SingleSampled Storage (Just fmt))
      )

type Texture decs props
  = Global
      Storage.UniformConstant
      decs
      ( Image props )
type StorageImage decs props
  = Global
      Storage.UniformConstant
      decs
      ( Image props )

-- pattern synonyms for image operands
-- these perform wrapping/unwrapping for nicer user syntax... quite hacky
pattern Done                  = Ops  Op.Done
pattern Proj              ops = Ops (Op.Proj              (UnOps ops))
pattern Dref         dref ops = Ops (Op.Dref         dref (UnOps ops))
pattern Bias            b ops = Ops (Op.Bias            b (UnOps ops))
pattern LOD           lod ops = Ops (Op.LOD           lod (UnOps ops))
pattern MinLOD        lod ops = Ops (Op.MinLOD        lod (UnOps ops))
pattern Grad         grad ops = Ops (Op.Grad         grad (UnOps ops))
pattern ConstOffsetBy off ops = Ops (Op.ConstOffsetBy off (UnOps ops))
pattern OffsetBy      off ops = Ops (Op.OffsetBy      off (UnOps ops))
pattern Gather       info ops = Ops (Op.Gather       info (UnOps ops))
pattern SampleNo       no ops = Ops (Op.SampleNo       no (UnOps ops))

-- shorthands
pattern NoOperands = Done
pattern DepthTestOffsets offs ops = Gather (DepthWithOffsets offs) ops
pattern GatherComponentWithOffsets component offs ops
  = Gather (ComponentWithOffsets component offs) ops


pattern UnOps :: AST (ImageOperands props ops) -> ImageOperands props ops
pattern UnOps astOps <- ( Ops -> astOps )
  where UnOps (Ops ops) = ops
        UnOps _ = error "image operands not of the expected form"

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

----------------------------------------------------------------------
-- synonyms for optics

-- internal synonyms to help inference in subsequent definitions
type Col_  (i :: Nat) = ( Index i :: Optic '[] (M m n a) (V m a) )
type Col__ (i :: Nat) = ( Index i :: Optic '[] (AST (M m n a)) (AST (V m a)) )
type Ix_   (i :: Nat) = ( Index i :: Optic '[] (V n a) a )
type Ix__  (i :: Nat) = ( Index i :: Optic '[] (AST (V n a)) (AST a) )

type family ColRes (i :: Nat) (mat :: Type) :: Type where
  ColRes i (AST (M m n a)) = AST (V m a)
  ColRes i (M m n a) = V m a

type family Col (i :: Nat) = ( optic :: Optic '[] mat (ColRes i mat) ) | optic -> i where
  Col i = ( Col__ i :: Optic '[] (AST (M m n a)) (AST (V m a)) )
  Col i = ( Col_ i :: Optic '[] (M m n a) (V m a) )

type family RowRes (i :: Nat) (mat :: Type) :: Type where
  RowRes i (M m n a) = V n a
  RowRes i (AST (M m n a)) = AST (V n a)

type family Row (i :: Nat) = ( optic :: Optic '[] mat (RowRes i mat) ) where
  Row i = ( Prod_
              (   (Col__ 0 :.: Ix__ i)
              :*: (Col__ 1 :.: Ix__ i)
              :*: EndProd
              )
            :: Optic '[] (AST (M m 2 a)) (AST (V 2 a))
          )
  Row i = ( Prod_
              (   (Col__ 0 :.: Ix__ i)
              :*: (Col__ 1 :.: Ix__ i)
              :*: (Col__ 2 :.: Ix__ i)
              :*: EndProd
              )
            :: Optic '[] (AST (M m 3 a)) (AST (V 3 a))
          )
  Row i = ( Prod_
              (   (Col__ 0 :.: Ix__ i)
              :*: (Col__ 1 :.: Ix__ i)
              :*: (Col__ 2 :.: Ix__ i)
              :*: (Col__ 3 :.: Ix__ i)
              :*: EndProd
              )
            :: Optic '[] (AST (M m 4 a)) (AST (V 4 a))
          )
  Row i = ( Prod_
              (   (Col_ 0 :.: Ix_ i)
              :*: (Col_ 1 :.: Ix_ i)
              :*: EndProd
              )
            :: Optic '[] (M m 2 a) (V 2 a)
          )
  Row i = ( Prod_
              (   (Col_ 0 :.: Ix_ i)
              :*: (Col_ 1 :.: Ix_ i)
              :*: (Col_ 2 :.: Ix_ i)
              :*: EndProd
              )
            :: Optic '[] (M m 3 a) (V 3 a)
          )
  Row i = ( Prod_
              (   (Col_ 0 :.: Ix_ i)
              :*: (Col_ 1 :.: Ix_ i)
              :*: (Col_ 2 :.: Ix_ i)
              :*: (Col_ 3 :.: Ix_ i)
              :*: EndProd
              )
           :: Optic '[] (M m 4 a) (V 4 a)
         )

type family EntryRes (mat :: Type) :: Type where
  EntryRes (M m n a) = a
  EntryRes (AST (M m n a)) = AST a

type family Entry (i :: Nat) (j :: Nat) = ( optic :: Optic '[] mat (EntryRes mat)) where
  Entry i j = ( ( Col_  i :.: Ix_  j ) :: Optic '[] (M m n a) a )
  Entry i j = ( ( Col__ j :.: Ix__ i ) :: Optic '[] (AST (M m n a)) (AST a) )

type family DiagRes (mat :: Type) :: Type where
  DiagRes (M n n a) = V n a
  DiagRes (AST (M n n a)) = AST (V n a)

type family Diag :: Optic '[] mat (DiagRes mat) where
  Diag = ( Prod_
              (   (Col_ 0 :.: Ix_ 0)
              :*: (Col_ 1 :.: Ix_ 1)
              :*: EndProd
              )
            :: Optic '[] (M 2 2 a) (V 2 a)
         )
  Diag = ( Prod_
              (   (Col_ 0 :.: Ix_ 0)
              :*: (Col_ 1 :.: Ix_ 1)
              :*: (Col_ 2 :.: Ix_ 2)
              :*: EndProd
              )
            :: Optic '[] (M 3 3 a) (V 3 a)
         )
  Diag = ( Prod_
              (   (Col_ 0 :.: Ix_ 0)
              :*: (Col_ 1 :.: Ix_ 1)
              :*: (Col_ 2 :.: Ix_ 2)
              :*: (Col_ 3 :.: Ix_ 3)
              :*: EndProd
              )
            :: Optic '[] (M 4 4 a) (V 4 a)
         )
  Diag = ( Prod_
              (   (Col__ 0 :.: Ix__ 0)
              :*: (Col__ 1 :.: Ix__ 1)
              :*: EndProd
              )
            :: Optic '[] (AST (M 2 2 a)) (AST (V 2 a))
         )
  Diag = ( Prod_
              (   (Col__ 0 :.: Ix__ 0)
              :*: (Col__ 1 :.: Ix__ 1)
              :*: (Col__ 2 :.: Ix__ 2)
              :*: EndProd
              )
            :: Optic '[] (AST (M 3 3 a)) (AST (V 3 a))
         )
  Diag = ( Prod_
              (   (Col__ 0 :.: Ix__ 0)
              :*: (Col__ 1 :.: Ix__ 1)
              :*: (Col__ 2 :.: Ix__ 2)
              :*: (Col__ 3 :.: Ix__ 3)
              :*: EndProd
              )
            :: Optic '[] (AST (M 4 4 a)) (AST (V 4 a))
         )

type family Center :: Optic '[] mat (EntryRes mat) where
  Center = ( (     ( Diag :: Optic '[] (M n n a) (V n a) )
               :.: Joint
             ) :: Optic '[] (M n n a) a
            )
  Center = ( (     ( Diag :: Optic '[] (AST (M n n a)) (AST (V n a)) )
               :.: Joint
             ) :: Optic '[] (AST (M n n a)) (AST a)
            )
