{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

module SPIRV.PrimTy
  ( Width(..), width
  , Signedness(..), signedness
  , ScalarTy(..)
  , PrimTy(..)
  , PointerTy(pointerTy) -- constructor not exported
  , pattern PointerTy
  , tyOp
  ) where

-- base
import Data.Word(Word32)
import Prelude hiding (Integer, Floating)

-- text-utf8
import Data.Text(Text)

-- fir
import SPIRV.Operation hiding(Function)
import SPIRV.Storage(StorageClass)

--------------------------------------------------
-- SPIR-V types

data Width
  = W8
  | W16
  | W32
  | W64
  deriving ( Show, Eq, Ord, Enum, Bounded )

width :: Width -> Word32
width W8  = 8
width W16 = 16
width W32 = 32
width W64 = 64

data Signedness
  = Unsigned
  | Signed
  deriving ( Show, Eq, Ord, Enum, Bounded )

signedness :: Signedness -> Word32
signedness Unsigned = 0
signedness Signed   = 1

data ScalarTy where
  Integer  :: Signedness -> Width -> ScalarTy
  Floating ::               Width -> ScalarTy
  deriving ( Show, Eq, Ord )

data PrimTy where
  Unit         ::                                 PrimTy -- known as Void in the SPIR-V specification
  Boolean      ::                                 PrimTy
  Scalar       ::                     ScalarTy -> PrimTy
  Vector       :: Word32           ->   PrimTy -> PrimTy
  Matrix       :: Word32 -> Word32 -> ScalarTy -> PrimTy
  Array        :: Word32 ->             PrimTy -> PrimTy
  RuntimeArray ::                       PrimTy -> PrimTy
  Struct       :: [(Text,PrimTy)]              -> PrimTy
  Pointer      :: StorageClass       -> PrimTy -> PrimTy
  Function     :: [PrimTy]           -> PrimTy -> PrimTy 
  -- todo: images, opaque types, ...
  deriving ( Show, Eq, Ord )

-- newtype to deal with types that are known to be pointers,
-- to avoids spurious error handling in code that deals with pointers
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
