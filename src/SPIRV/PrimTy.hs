{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}

{-|
Module: SPIRV.PrimTy

This module enumerates the types that exist within the SPIR-V type system.

See the SPIR-V specification, §3.22.6 __Type-Declaration instructions__.
-}

module SPIRV.PrimTy
  ( AggregateUsage(..)
  , PrimTy(..)
  , PointerTy(PointerTy, pointerTy) -- constructor (PtrTy) not exported
  , tyOp
  , scalars
  ) where

-- base
import Data.Word
  ( Word32 )
import Prelude
  hiding (Integer, Floating)

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import SPIRV.Decoration
  ( Decorations )
import SPIRV.Image
  ( Image(texelComponent) )
import SPIRV.Operation
  hiding ( Function, Image, SampledImage )
import SPIRV.ScalarTy
  ( ScalarTy(Integer, Floating) )
import SPIRV.Storage
  ( StorageClass )

--------------------------------------------------
-- SPIR-V types

data AggregateUsage
  = ForInputBuiltins
  | ForOutputBuiltins
  | NotForBuiltins
  deriving ( Show, Eq, Ord )

data PrimTy where
  Unit    :: PrimTy -- known as Void in the SPIR-V specification
  Boolean :: PrimTy
  Scalar  :: ScalarTy -> PrimTy
  Vector  ::
    { size  :: Word32
    , eltTy :: PrimTy
    } -> PrimTy
  Matrix ::
    { rows    :: Word32
    , cols    :: Word32
    , entryTy :: ScalarTy
    } -> PrimTy
  Array ::
    { size  :: Word32
    , eltTy :: PrimTy
    , decs  :: Decorations
    , usage :: AggregateUsage
    } -> PrimTy
  RuntimeArray ::
    { eltTy :: PrimTy
    , decs  :: Decorations
    , usage :: AggregateUsage
    } -> PrimTy
  Struct ::
    { eltTys :: [( Maybe ShortText, PrimTy, Decorations )]
    -- ^ the (optional) field names are only used to annotate the source code
    , decs   :: Decorations
    , usage  :: AggregateUsage
    } -> PrimTy
  Pointer  :: StorageClass -> PrimTy -> PrimTy
  Function ::
    { argumentTypes :: [PrimTy]
    , resultType    :: PrimTy
    } -> PrimTy
  Image        :: Image -> PrimTy
  Sampler      ::          PrimTy -- opaque
  SampledImage :: Image -> PrimTy
  deriving ( Show, Eq, Ord )

-- newtype to deal with types that are known to be pointers,
-- to avoid spurious error handling in code that deals with pointers
--
-- (newtype constructor not exported; pattern synonym instead)
newtype PointerTy = PtrTy { pointerTy :: PrimTy }
  deriving newtype ( Show, Eq, Ord )
{-# COMPLETE PointerTy #-}
pattern PointerTy :: StorageClass -> PrimTy -> PointerTy
pattern PointerTy storage ty = PtrTy (Pointer storage ty)


tyOp :: PrimTy -> Operation
tyOp Unit                  = TypeVoid
tyOp Boolean               = TypeBool
tyOp (Scalar Integer   {}) = TypeInt
tyOp (Scalar Floating  {}) = TypeFloat
tyOp Vector             {} = TypeVector
tyOp Matrix             {} = TypeMatrix
tyOp Array              {} = TypeArray
tyOp RuntimeArray       {} = TypeRuntimeArray
tyOp Struct             {} = TypeStruct
tyOp Function           {} = TypeFunction
tyOp Pointer            {} = TypePointer
tyOp Image              {} = TypeImage
tyOp Sampler               = TypeSampler
tyOp SampledImage       {} = TypeSampledImage

scalars :: PrimTy -> [ ScalarTy ]
scalars Unit                     = [ ]
scalars Boolean                  = [ ]
scalars (Scalar s)               = [ s ]
scalars (Vector _ a)             = scalars a
scalars (Matrix _ _ a)           = [ a ]
scalars (Array { eltTy })        = scalars eltTy
scalars (RuntimeArray { eltTy }) = scalars eltTy
scalars (Struct { eltTys })      = scalars =<< map ( \(_,ty,_) -> ty ) eltTys
scalars (Pointer _ ty)           = scalars ty
scalars (Function as b)          = scalars b ++ ( scalars =<< as )
scalars (Image img)              = [ texelComponent img ]
scalars Sampler                  = [ ]
scalars (SampledImage img)       = [ texelComponent img ]
