{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: Math.Logic.Bits

Alternative "Data.Bits" type class tailored for this library.

Justification: some of the methods of "Data.Bits" are unsuited for this library.
Consider for instance the class method

> shiftL :: a -> Int -> a

This function bakes in the type `Int`.
For this library, this would mean that only shifts
by Haskell-level constants are possible.
Instead, we want something which can instantiate to, e.g.:

> shiftL :: AST Word64 -> AST Word8 -> AST Word64

See also "Math.Algebra.Class" for further explanations
regarding the limitations of the standard type classes.

-}

module Math.Logic.Bits
  ( Bits(..)
  , BitShift(..)
  , BitCast(..)
  , zipBits2
  )
  where

-- base
import Prelude
  ( Bool(..)
  , Int, Word, Float, Double
  , ($)
  )
import qualified Prelude
import qualified Data.Bits as Base
import Data.Coerce
  ( coerce )
import Data.Int
  ( Int8,Int16,Int32,Int64 )
import Data.Kind
  ( Type )
import Data.Word
  ( Word8,Word16,Word32,Word64 )
import Foreign.C.Types
import GHC.Float
  ( castWord32ToFloat , castFloatToWord32
  , castWord64ToDouble, castDoubleToWord64
  )

-- fir
import Deriving.Base
  ( Base(..) ) -- newtype for deriving via base instances
import Math.Algebra.Class
  ( Integral )

----------------------------------------------------------------------

infixl 7 .&.
infixl 5 .|.
infixl 6 `xor`

class Bits a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  xor   :: a -> a -> a
  complement :: a -> a
  zeroBits :: a

instance Base.Bits a => Bits (Base a) where
  (.&.) = coerce ( (Base..&.) :: a -> a -> a)
  (.|.) = coerce ( (Base..&.) :: a -> a -> a)
  xor   = coerce ( Base.xor :: a -> a -> a)
  complement = coerce (Base.complement :: a -> a)
  zeroBits   = coerce (Base.zeroBits :: a)

deriving via Base Bool   instance Bits Bool
deriving via Base Word8  instance Bits Word8
deriving via Base Word16 instance Bits Word16
deriving via Base Word32 instance Bits Word32
deriving via Base Word64 instance Bits Word64
deriving via Base Word   instance Bits Word
deriving via Base Int8   instance Bits Int8
deriving via Base Int16  instance Bits Int16
deriving via Base Int32  instance Bits Int32
deriving via Base Int64  instance Bits Int64
deriving via Base Int    instance Bits Int

deriving via Base CChar   instance Bits CChar
deriving via Base CSChar  instance Bits CSChar
deriving via Base CUChar  instance Bits CUChar
deriving via Base CShort  instance Bits CShort
deriving via Base CUShort instance Bits CUShort
deriving via Base CInt    instance Bits CInt
deriving via Base CUInt   instance Bits CUInt
deriving via Base CLong   instance Bits CLong
deriving via Base CULong  instance Bits CULong
deriving via Base CLLong  instance Bits CLLong
deriving via Base CULLong instance Bits CULLong

type family BitsType (bs :: (Type,Type)) :: Type where
  BitsType '(b,_) = b
type family ShiftType (bs :: (Type,Type)) :: Type where
  ShiftType '(_,b) = b

class (bs ~ '(BitsType bs, ShiftType bs)) => BitShift (bs :: (Type,Type)) where
  -- | Shift the first argument left by the specified number of bits.
  --
  -- Ignores the sign of the shift argument (second argument).
  --
  -- This is a logical shift: least-significant bits are set to 0.
  shiftL :: BitsType bs -> ShiftType bs -> BitsType bs
  -- | Shift the first argument right by the specified number of bits.
  --
  -- Ignores the sign of the shift argument (second argument).
  --
  -- This is an arithmetic shift: most-significant bits are filled
  -- with the sign of the first argument.
  shiftR :: BitsType bs -> ShiftType bs -> BitsType bs

instance (Base.Bits a, Prelude.Integral i)
  => BitShift '(Base a,i) where
  shiftL (Base a) i = Base $ Base.shiftL a (Prelude.fromIntegral i)
  shiftR (Base a) i = Base $ Base.shiftR a (Prelude.fromIntegral i)

deriving via (forall (s :: Type). '(Base Word8 ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Word8 ,s)
deriving via (forall (s :: Type). '(Base Word16,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Word16,s)
deriving via (forall (s :: Type). '(Base Word32,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Word32,s)
deriving via (forall (s :: Type). '(Base Word64,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Word64,s)
deriving via (forall (s :: Type). '(Base Word  ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Word  ,s)
deriving via (forall (s :: Type). '(Base Int8  ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Int8  ,s)
deriving via (forall (s :: Type). '(Base Int16 ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Int16 ,s)
deriving via (forall (s :: Type). '(Base Int32 ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Int32 ,s)
deriving via (forall (s :: Type). '(Base Int64 ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Int64 ,s)
deriving via (forall (s :: Type). '(Base Int   ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(Int   ,s)

deriving via (forall (s :: Type). '(Base CChar  ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CChar  ,s)
deriving via (forall (s :: Type). '(Base CSChar ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CSChar ,s)
deriving via (forall (s :: Type). '(Base CUChar ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CUChar ,s)
deriving via (forall (s :: Type). '(Base CShort ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CShort ,s)
deriving via (forall (s :: Type). '(Base CUShort,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CUShort,s)
deriving via (forall (s :: Type). '(Base CInt   ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CInt   ,s)
deriving via (forall (s :: Type). '(Base CUInt  ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CUInt  ,s)
deriving via (forall (s :: Type). '(Base CLong  ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CLong  ,s)
deriving via (forall (s :: Type). '(Base CULong ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CULong ,s)
deriving via (forall (s :: Type). '(Base CLLong ,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CLLong ,s)
deriving via (forall (s :: Type). '(Base CULLong,s)) instance (Integral s, Prelude.Integral s) => BitShift '(CULLong,s)

class BitCast a b where
  bitcast :: a -> b

--instance BitCast Word16 Half where
instance BitCast Word32 Float where
  bitcast = castWord32ToFloat
instance BitCast Word64 Double where
  bitcast = castWord64ToDouble
--instance BitCast Half   Word16 where
instance BitCast Float  Word32 where
  bitcast = castFloatToWord32
instance BitCast Double Word64 where
  bitcast = castDoubleToWord64

zipBits2 :: Bits a
         => (Bool -> Bool -> Bool)
         -> (a    -> a    -> a   )
zipBits2 f
  = case ( f False False, f False True, f True False, f True True ) of
      ( False, False, False, False ) -> \ _ _ -> zeroBits
      ( False, False, False, True  ) ->          (.&.)
      ( False, False, True , False ) -> \ x y -> x .&. complement y
      ( False, False, True , True  ) -> \ x _ -> x
      ( False, True , False, False ) -> \ x y -> complement x .&. y
      ( False, True , False, True  ) -> \ _ y -> y
      ( False, True , True , False ) ->          xor
      ( False, True , True , True  ) ->          (.|.)
      ( True , False, False, False ) -> \ x y -> complement ( x .|. y )
      ( True , False, False, True  ) -> \ x y -> complement ( x `xor` y )
      ( True , False, True , False ) -> \ _ y -> complement y
      ( True , False, True , True  ) -> \ x y -> x .|. complement y
      ( True , True , False, False ) -> \ x _ -> complement x
      ( True , True , False, True  ) -> \ x y -> complement x .|. y
      ( True , True , True , False ) -> \ x y -> complement ( x .&. y )
      ( True , True , True , True  ) -> \ _ _ -> complement zeroBits
