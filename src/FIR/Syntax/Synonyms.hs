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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

{-|
Module: FIR.Syntax.Synonyms

This module provides various convenience synonyms
to mitigate verbosity in user-written programs.

-}

module FIR.Syntax.Synonyms
  (
  -- * Synonyms for floating point types of a given width
    Float16, Float32, Float64

  -- * Synonyms for entry point globals with a given storage class
  , UniformConstant, Input, Output
  , Uniform, PushConstant
  , StorageBuffer, Private
  , Workgroup, CrossWorkgroup
  , AtomicCounter

  -- * Synonyms for function with given function control information
  , Function, Function'

  -- * Synonyms for images
  -- ** Sampled images
  , Texture1D, Texture2D, Texture3D, Texture
  -- ** Storage images
  , Image1D, Image2D, Image3D, StorageImage
  -- ** Image operand shorthands
  , pattern DepthTestOffsets
  , pattern GatherComponentWithOffsets
  -- ** Helper type family for choosing image texel type
  , FormatDefault

  -- * Synonyms involved in pipeline creation
  , Slot
  , pattern StructInput

  -- * Synonyms for optics
  , Field
  , Col, Cols
  , Row, Rows
  , Entry, Elts
  , Diag, Center

  -- * Pattern synonyms for vectors/matrices
  -- ** Patterns for vectors
  , pattern Vec2, pattern Vec3, pattern Vec4

  -- ** Patterns for matrices
  , pattern Mat22, pattern Mat23, pattern Mat24
  , pattern Mat32, pattern Mat33, pattern Mat34
  , pattern Mat42, pattern Mat43, pattern Mat44
  )
  where

-- base
import Prelude
  hiding ( Floating, Integral )
import Data.Int
  ( Int8, Int16, Int32 )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word8, Word16, Word32 )
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat, KnownNat
  , type (*)
  )

-- half
import Numeric.Half
  ( Half )

-- fir
import Control.Type.Optic
  ( Optic(..), Index
  , (:.:), (:*:)
  , OfType
  , EndProd
  )
import Data.Type.Known
  ( Known )
import Data.Type.Map
  ( (:->)((:->)), InsertionSort, Value )
import FIR.AST
  ( AST((:$), MkVector, Mat, UnMat, Gather)
  , Syntactic(fromAST)
  )
import FIR.Definition
  ( Definition(Global) )
import qualified FIR.Definition as Def
import FIR.Syntax.AST
  ( )
import FIR.Syntax.Optics
  ( )
import FIR.Layout
  ( Layout(Locations)
  , Poke(SizeOf)
  )
import FIR.Pipeline
  ( PrimitiveTopology
  , BindingStrides
  , VertexLocationDescriptions
  , PipelineInfo(VertexInputInfo)
  , PipelineStages(VertexInput)
  )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Image
  ( ImageProperties(Properties)
  , Image, GatherInfo(..)
  )
import FIR.Prim.Singletons
  ( PrimTy, ScalarTy )
import FIR.Prim.Struct
  ( Struct
  , LocationSlot(LocationSlot)
  )
import FIR.Validation.Bounds
  ( StructIndexFromName )
import FIR.Validation.Formats
  ( ComputeFormats )
import Math.Linear
  ( V, M
  , pattern V2, pattern V3, pattern V4
  )
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
import qualified SPIRV.Image   as SPIRV
  ( ImageFormat )
import qualified SPIRV.Storage as Storage

--------------------------------------------------------------------------

type Float16 = Half
type Float32 = Float
type Float64 = Double

-- synonyms for (decorated) globals
type UniformConstant decs ty = Global Storage.UniformConstant decs ty
type Input           decs ty = Global Storage.Input           decs ty
type Output          decs ty = Global Storage.Output          decs ty
type Uniform         decs ty = Global Storage.Uniform         decs ty
type PushConstant    decs ty = Global Storage.PushConstant    decs ty
type StorageBuffer   decs ty = Global Storage.StorageBuffer   decs ty
type Private         decs ty = Global Storage.Private         decs ty
type Workgroup       decs ty = Global Storage.Workgroup       decs ty
type CrossWorkgroup  decs ty = Global Storage.CrossWorkgroup  decs ty
type AtomicCounter   decs ty = Global Storage.AtomicCounter   decs ty

-- synonym for function with no function control information
type Function     as b = Def.Function NoFunctionControl as b
type Function' fc as b = Def.Function fc as b

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

-- shorthand pattern synonyms for image operands
pattern DepthTestOffsets offs ops = Gather (DepthWithOffsets offs) ops
pattern GatherComponentWithOffsets component offs ops
  = Gather (ComponentWithOffsets component offs) ops

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

-----------------------------------------------------
-- pipeline synonyms

-- | Synonym for interface location slots.
type Slot (l :: Nat) (c :: Nat) = 'LocationSlot l c

-- | Utility pattern synonym for specifying a simple kind of vertex input:
-- use a single array of structs to keep track of vertex data.
pattern StructInput
  :: forall
        ( as        :: [LocationSlot Nat :-> Type] )
        ( top       :: PrimitiveTopology Nat       )
        ( descs     :: VertexLocationDescriptions  )
        ( stride    :: Nat                         )
        ( strides   :: BindingStrides              )
        ( stageData :: Type                        )
  .  ( descs   ~ StructLocationDescriptions 0 as
     , stride  ~ SizeOf Locations (Struct as)
     , strides ~ '[ 0 ':-> stride ]
     , Known (PrimitiveTopology Nat) top
     , Known VertexLocationDescriptions descs
     , KnownNat stride
     , Known BindingStrides strides
     )
  => ()
  => PipelineStages (VertexInputInfo top descs strides) stageData
pattern StructInput = VertexInput


-- internal helper type synonyms / type families used for the above pattern synonym

type StructLocationDescriptions ( bdNo :: Nat ) ( as :: [LocationSlot Nat :-> Type] )
  = ( AnnotateLocationsWithBindingAndOffset
        bdNo
        ( LocationDescriptionsOfStruct as )
    :: VertexLocationDescriptions
    )

type family AnnotateLocationsWithBindingAndOffset
              ( bdNo :: Nat )
              ( locationFormats :: [ Nat :-> SPIRV.ImageFormat Nat ] )
           :: [ Nat :-> ( Nat, Nat, SPIRV.ImageFormat Nat ) ]
           where
  AnnotateLocationsWithBindingAndOffset _ '[] = '[]
  AnnotateLocationsWithBindingAndOffset bdNo
    ( ( loc ':-> fmt ) ': locs )
      = ( loc ':-> '( bdNo, 16 * loc, fmt ) )
      ': AnnotateLocationsWithBindingAndOffset bdNo locs

type LocationDescriptionsOfStruct
      ( as :: [ LocationSlot Nat :-> Type ] )
    = ( ComputeFormats (InsertionSort as) :: [ Nat :-> SPIRV.ImageFormat Nat ] )

----------------------------------------------------------------------
-- synonyms for optics

type family FieldRes ( k :: Symbol ) ( struct :: Type ) :: Type where
  FieldRes k (Struct as)  = Value (StructIndexFromName k as)
  FieldRes k (AST (Struct as)) = AST (Value (StructIndexFromName k as))

type family Field ( k :: Symbol ) :: Optic '[] struct (FieldRes k struct) where
  Field k = ( Field_ (k :: Symbol)
              :: Optic '[] (Struct as) (FieldRes k (Struct as))
            )
  Field k = ( Field_ (k :: Symbol)
              :: Optic '[] (AST (Struct as)) (FieldRes k (AST (Struct as)))
            )

-- internal synonyms to help inference in subsequent definitions
type Col_  (i :: Nat) = ( Index i :: Optic '[] (M m n a) (V m a) )
type Col__ (i :: Nat) = ( Index i :: Optic '[] (AST (M m n a)) (AST (V m a)) )
type Ix_   (i :: Nat) = ( Index i :: Optic '[] (V n a) a )
type Ix__  (i :: Nat) = ( Index i :: Optic '[] (AST (V n a)) (AST a) )

type family EltRes (t :: Type) :: Type where
  EltRes (V m a) = a
  EltRes (M m n a) = a
  EltRes (Array n a) = a
  EltRes (AST (V m a)) = AST a
  EltRes (AST (M m n a)) = AST a
  EltRes (AST (Array n a)) = AST a

type family Elts = ( optic :: Optic '[] t (EltRes t) ) where
  Elts = ( OfType (EltRes t) :: Optic '[] t (EltRes t) )

type family ColRes (mat :: Type) :: Type where
  ColRes (AST (M m n a)) = AST (V m a)
  ColRes (M m n a) = V m a

type family Col (i :: Nat) = ( optic :: Optic '[] mat (ColRes mat) ) | optic -> i where
  Col i = ( Col__ i :: Optic '[] (AST (M m n a)) (AST (V m a)) )
  Col i = ( Col_ i :: Optic '[] (M m n a) (V m a) )

type family Cols = ( optic :: Optic '[] mat (ColRes mat) ) where
  Cols = ( OfType (ColRes mat) :: Optic '[] mat (ColRes mat) )

type family RowRes (mat :: Type) :: Type where
  RowRes (M m n a) = V n a
  RowRes (AST (M m n a)) = AST (V n a)

type family Row (i :: Nat) = ( optic :: Optic '[] mat (RowRes mat) ) where
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

type family Rows = ( optic :: Optic '[] mat (RowRes mat) ) where
  Rows = ( ( ( Prod_ ( Row 0 :*: Row 1 :*: EndProd )
                :: Optic '[] (M 2 n a) (Struct '[ "row0" ':-> V n a, "row1" ':-> V n a]) )
           :.: OfType (V n a)
           ) :: Optic '[] (M 2 n a) (V n a)
         )
  Rows = ( ( ( Prod_ ( Row 0 :*: Row 1 :*: Row 2 :*: EndProd )
                :: Optic '[] (M 3 n a) (Struct '[ "row0" ':-> V n a, "row1" ':-> V n a, "row2" ':-> V n a]) )
           :.: OfType (V n a)
           ) :: Optic '[] (M 3 n a) (V n a)
         )
  Rows = ( ( ( Prod_ ( Row 0 :*: Row 1 :*: Row 2 :*: Row 3 :*: EndProd )
                :: Optic '[] (M 4 n a) (Struct '[ "row0" ':-> V n a, "row1" ':-> V n a, "row2" ':-> V n a, "row3" ':-> V n a ]) )
           :.: OfType (V n a)
           ) :: Optic '[] (M 4 n a) (V n a)
         )
  Rows = ( ( ( Prod_ ( Row 0 :*: Row 1 :*: EndProd )
                :: Optic '[] (AST (M 2 n a)) (Struct '[ "row0" ':-> AST (V n a), "row1" ':-> AST (V n a)]) )
           :.: OfType (AST (V n a))
           ) :: Optic '[] (AST (M 2 n a)) (AST (V n a))
         )
  Rows = ( ( ( Prod_ ( Row 0 :*: Row 1 :*: Row 2 :*: EndProd )
                :: Optic '[] (AST (M 3 n a)) (Struct '[ "row0" ':-> AST (V n a), "row1" ':-> AST (V n a), "row2" ':-> AST (V n a)]) )
           :.: OfType (AST (V n a))
           ) :: Optic '[] (AST (M 3 n a)) (AST (V n a))
         )
  Rows = ( ( ( Prod_ ( Row 0 :*: Row 1 :*: Row 2 :*: Row 3 :*: EndProd )
                :: Optic '[] (AST (M 4 n a)) (Struct '[ "row0" ':-> AST (V n a), "row1" ':-> AST (V n a), "row2" ':-> AST (V n a), "row3" ':-> AST (V n a)]) )
           :.: OfType (AST (V n a))
           ) :: Optic '[] (AST (M 4 n a)) (AST (V n a))
         )

type family EntryRes (mat :: Type) :: Type where
  EntryRes (M m n a) = a
  EntryRes (AST (M m n a)) = AST a

type family Entry (i :: Nat) (j :: Nat) = ( optic :: Optic '[] mat (EntryRes mat)) where
  Entry i j = ( ( Col_  j :.: Ix_  i ) :: Optic '[] (M m n a) a )
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
  Center = ( ( ( Diag :: Optic '[] mat (DiagRes mat) )
               :.:
               ( OfType (EntryRes mat) :: Optic '[] (DiagRes mat) (EntryRes mat) )
             ) :: Optic '[] mat (EntryRes mat)
           )

----------------------------------------------------------------------
-- pattern synonyms for vectors/matrices

-- these patterns could be generalised to have types such as:
-- Vec2 :: forall a. (Syntactic a, PrimTy (Internal a))
--      => a -> a -> AST ( V 2 (Internal a) )
-- but this leads to poor type-inference

{-# COMPLETE Vec2 #-}
pattern Vec2 :: forall a. PrimTy a => AST a -> AST a -> AST ( V 2 a )
pattern Vec2 x y <- (fromAST -> V2 x y)
  where Vec2 = fromAST $ MkVector @2 @a Proxy Proxy

{-# COMPLETE Vec3 #-}
pattern Vec3 :: forall a. PrimTy a => AST a -> AST a -> AST a -> AST ( V 3 a )
pattern Vec3 x y z <- (fromAST -> V3 x y z)
  where Vec3 = fromAST $ MkVector @3 @a Proxy Proxy

{-# COMPLETE Vec4 #-}
pattern Vec4 :: forall a. PrimTy a => AST a -> AST a -> AST a -> AST a -> AST ( V 4 a )
pattern Vec4 x y z w <- (fromAST -> V4 x y z w)
  where Vec4 = fromAST $ MkVector @4 @a Proxy Proxy

{-# COMPLETE Mat22 #-}
pattern Mat22
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 2 2 a )
pattern Mat22 a11 a12
              a21 a22
  <- ( fromAST . ( UnMat :$ )
       -> V2 ( V2 a11 a21 )
             ( V2 a12 a22 )
     )
  where Mat22
            a11 a12
            a21 a22
          = Mat :$ Vec2
            ( Vec2 a11 a21 )
            ( Vec2 a12 a22 )

{-# COMPLETE Mat23 #-}
pattern Mat23
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 2 3 a )
pattern Mat23 a11 a12 a13
              a21 a22 a23
   <- ( fromAST . ( UnMat :$ )
        -> V3 ( V2 a11 a21 )
              ( V2 a12 a22 )
              ( V2 a13 a23 )
      )
  where Mat23
            a11 a12 a13
            a21 a22 a23
          = Mat :$ Vec3
              ( Vec2 a11 a21 )
              ( Vec2 a12 a22 )
              ( Vec2 a13 a23 )

{-# COMPLETE Mat24 #-}
pattern Mat24
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 2 4 a )
pattern Mat24 a11 a12 a13 a14
              a21 a22 a23 a24
   <- ( fromAST . ( UnMat :$ )
        -> V4 ( V2 a11 a21 )
              ( V2 a12 a22 )
              ( V2 a13 a23 )
              ( V2 a14 a24 )
      )
  where Mat24
            a11 a12 a13 a14
            a21 a22 a23 a24
          = Mat :$ Vec4
              ( Vec2 a11 a21 )
              ( Vec2 a12 a22 )
              ( Vec2 a13 a23 )
              ( Vec2 a14 a24 )

{-# COMPLETE Mat32 #-}
pattern Mat32
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 3 2 a )
pattern Mat32 a11 a12
              a21 a22
              a31 a32
   <- ( fromAST . ( UnMat :$ )
        -> V2 ( V3 a11 a21 a31 )
              ( V3 a12 a22 a32 )
      )
  where Mat32
            a11 a12
            a21 a22
            a31 a32
          = Mat :$ Vec2
              ( Vec3 a11 a21 a31 )
              ( Vec3 a12 a22 a32 )

{-# COMPLETE Mat33 #-}
pattern Mat33
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 3 3 a )
pattern Mat33 a11 a12 a13
              a21 a22 a23
              a31 a32 a33
   <- ( fromAST . ( UnMat :$ )
        -> V3 ( V3 a11 a21 a31 )
              ( V3 a12 a22 a32 )
              ( V3 a13 a23 a33 )
      )
  where Mat33
            a11 a12 a13
            a21 a22 a23
            a31 a32 a33
          = Mat :$ Vec3
              ( Vec3 a11 a21 a31 )
              ( Vec3 a12 a22 a32 )
              ( Vec3 a13 a23 a33 )

{-# COMPLETE Mat34 #-}
pattern Mat34
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 3 4 a )
pattern Mat34 a11 a12 a13 a14
              a21 a22 a23 a24
              a31 a32 a33 a34
   <- ( fromAST . ( UnMat :$ )
        -> V4 ( V3 a11 a21 a31 )
              ( V3 a12 a22 a32 )
              ( V3 a13 a23 a33 )
              ( V3 a14 a24 a34 )
      )
  where Mat34
            a11 a12 a13 a14
            a21 a22 a23 a24
            a31 a32 a33 a34
          = Mat :$ Vec4
              ( Vec3 a11 a21 a31 )
              ( Vec3 a12 a22 a32 )
              ( Vec3 a13 a23 a33 )
              ( Vec3 a14 a24 a34 )

{-# COMPLETE Mat42 #-}
pattern Mat42
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 4 2 a )
pattern Mat42 a11 a12
              a21 a22
              a31 a32
              a41 a42
   <- ( fromAST . ( UnMat :$ )
        -> V2 ( V4 a11 a21 a31 a41 )
              ( V4 a12 a22 a32 a42 )
      )
  where Mat42
            a11 a12
            a21 a22
            a31 a32
            a41 a42
          = Mat :$ Vec2
              ( Vec4 a11 a21 a31 a41 )
              ( Vec4 a12 a22 a32 a42 )

{-# COMPLETE Mat43 #-}
pattern Mat43
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 4 3 a )
pattern Mat43 a11 a12 a13
              a21 a22 a23
              a31 a32 a33
              a41 a42 a43
   <- ( fromAST . ( UnMat :$ )
        -> V3 ( V4 a11 a21 a31 a41 )
              ( V4 a12 a22 a32 a42 )
              ( V4 a13 a23 a33 a43 )
      )
  where Mat43
            a11 a12 a13
            a21 a22 a23
            a31 a32 a33
            a41 a42 a43
          = Mat :$ Vec3
              ( Vec4 a11 a21 a31 a41 )
              ( Vec4 a12 a22 a32 a42 )
              ( Vec4 a13 a23 a33 a43 )

{-# COMPLETE Mat44 #-}
pattern Mat44
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 4 4 a )
pattern Mat44 a11 a12 a13 a14
              a21 a22 a23 a24
              a31 a32 a33 a34
              a41 a42 a43 a44
   <- ( fromAST . ( UnMat :$ )
        -> V4 ( V4 a11 a21 a31 a41 )
              ( V4 a12 a22 a32 a42 )
              ( V4 a13 a23 a33 a43 )
              ( V4 a14 a24 a34 a44 )
      )
  where Mat44
            a11 a12 a13 a14
            a21 a22 a23 a24
            a31 a32 a33 a34
            a41 a42 a43 a44
          = Mat :$ Vec4
              ( Vec4 a11 a21 a31 a41 )
              ( Vec4 a12 a22 a32 a42 )
              ( Vec4 a13 a23 a33 a43 )
              ( Vec4 a14 a24 a34 a44 )
