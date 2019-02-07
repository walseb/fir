{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module SPIRV.ScalarTy
  ( Width(..), width
  , Signedness(..), signedness
  , ScalarTy(..)
  ) where

-- base
import Data.Word
  ( Word32 )
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
  deriving ( Show, Eq, Ord )
