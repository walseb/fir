{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module: SPIRV.ScalarTy

This module enumerates the scalar types that exist within the SPIR-V type system:

  * unsigned integers,
  * signed integers, and
  * floating point numbers.

-}

module SPIRV.ScalarTy
  ( Width(..), width, WidthToNat
  , Signedness(..), signedness
  , ScalarTy(..), scalarWidth, ScalarWidth
  ) where

-- base
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat )
import Prelude
  hiding ( Integer, Floating )

-- fir
import Data.Type.Known
  ( Demotable(Demote), Known(known) )

--------------------------------------------------
-- SPIR-V scalar types

data Width
  = W8
  | W16
  | W32
  | W64
  deriving stock ( Show, Eq, Ord, Enum, Bounded )

width :: Width -> Word32
width W8  = 8
width W16 = 16
width W32 = 32
width W64 = 64

type family WidthToNat (width :: Width) :: Nat where
  WidthToNat W8  = 8
  WidthToNat W16 = 16
  WidthToNat W32 = 32
  WidthToNat W64 = 64

data Signedness
  = Unsigned
  | Signed
  deriving stock ( Show, Eq, Ord, Enum, Bounded )

instance Demotable Signedness where
  type Demote Signedness = Signedness
instance Known Signedness Unsigned where
  known = Unsigned
instance Known Signedness Signed where
  known = Signed

signedness :: Signedness -> Word32
signedness Unsigned = 0
signedness Signed   = 1

data ScalarTy where
  Integer  :: Signedness -> Width -> ScalarTy
  Floating ::               Width -> ScalarTy
  deriving stock ( Show, Eq, Ord )

scalarWidth :: ScalarTy -> Width
scalarWidth (Integer _ w) = w
scalarWidth (Floating  w) = w

type family ScalarWidth (sTy :: ScalarTy) :: Width where
  ScalarWidth (Integer _ w) = w
  ScalarWidth (Floating  w) = w
