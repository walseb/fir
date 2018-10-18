{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}

module SPIRV.PrimTy
  ( Width(..), width
  , Signedness(..), signedness
  , PrimTy(..)
  , tyAndSomeTyConArgs, ty
  ) where

-- base
import Data.Word(Word32)

-- fir
import SPIRV.Operation

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

data PrimTy where
  Unit     ::                         PrimTy -- known as Void in the SPIR-V specification
  Boolean  ::                         PrimTy
  Integer  :: Signedness -> Width  -> PrimTy
  Floating ::               Width  -> PrimTy
  Vector   :: Int        -> PrimTy -> PrimTy
  Matrix   :: Int -> Int -> PrimTy -> PrimTy
  -- todo: records, arrays, opaque types, ...
  deriving ( Show, Eq, Ord )

tyAndSomeTyConArgs :: PrimTy -> (Operation, [Word32])
tyAndSomeTyConArgs Unit           = ( TypeVoid  , [ ] )
tyAndSomeTyConArgs Boolean        = ( TypeBool  , [ ] )
tyAndSomeTyConArgs (Integer  s w) = ( TypeInt   , [ width w, signedness s ] )
tyAndSomeTyConArgs (Floating   w) = ( TypeFloat , [ width w] )
tyAndSomeTyConArgs (Vector n   _) = ( TypeVector, [ fromIntegral n ] ) -- element type is separate
tyAndSomeTyConArgs (Matrix _ m _) = ( TypeMatrix, [ fromIntegral m ] ) -- only number of columns... column type is separate

ty :: PrimTy -> Operation
ty = fst . tyAndSomeTyConArgs