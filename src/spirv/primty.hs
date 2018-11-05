{-# LANGUAGE GADTs #-}

module SPIRV.PrimTy where

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

tyOp :: PrimTy -> Operation
tyOp Unit                    = TypeVoid
tyOp Boolean                 = TypeBool
tyOp (Scalar (Integer  _ _)) = TypeInt
tyOp (Scalar (Floating   _)) = TypeFloat
tyOp (Vector         _   _ ) = TypeVector
tyOp (Matrix         _ _ _ ) = TypeMatrix
tyOp (Array          _   _ ) = TypeArray
tyOp (RuntimeArray       _ ) = TypeRuntimeArray
tyOp (Struct             _ ) = TypeStruct
tyOp (Function         _ _ ) = TypeFunction
tyOp (Pointer          _ _ ) = TypePointer

