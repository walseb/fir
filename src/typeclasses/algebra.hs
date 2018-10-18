{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module TypeClasses.Algebra where

import qualified Prelude
import Prelude(id, (.), Integer, Rational, Int, Float, Double, Word)
import TypeClasses.Logic(Ord, Eq((==)))
import qualified Data.Fixed


class AdditiveGroup a where
  (+) :: a -> a -> a
  zero :: a

instance AdditiveGroup Int where
  (+) = (Prelude.+)
  zero = 0
instance AdditiveGroup Float where
  (+) = (Prelude.+)
  zero = 0
instance AdditiveGroup Double where
  (+) = (Prelude.+)
  zero = 0
instance AdditiveGroup Word where
  (+) = (Prelude.+)
  zero = Prelude.fromInteger 0



class AdditiveGroup a => Semiring a where
  (*) :: a -> a -> a

instance Semiring Int where
  (*) = (Prelude.*)
instance Semiring Word where
  (*) = (Prelude.*)
instance Semiring Float where
  (*) = (Prelude.*)
instance Semiring Double where
  (*) = (Prelude.*)



class Semiring a => Ring a where
  (-) :: a -> a -> a
  negate :: a -> a
  fromInteger :: Integer -> a

instance Ring Int where
  (-) = (Prelude.-)
  negate = Prelude.negate
  fromInteger = Prelude.fromInteger
instance Ring Float where
  (-) = (Prelude.-)
  negate = Prelude.negate
  fromInteger = Prelude.fromInteger
instance Ring Double where
  (-) = (Prelude.-)
  negate = Prelude.negate
  fromInteger = Prelude.fromInteger


class Ring a => Signed a where
  abs    :: a -> a
  signum :: a -> a

instance Signed Int where
  abs    = Prelude.abs
  signum = Prelude.signum
instance Signed Float where
  abs    = Prelude.abs
  signum = Prelude.signum
instance Signed Double where
  abs    = Prelude.abs
  signum = Prelude.signum



class Ring a => DivisionRing a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

  recip a = fromInteger 1 / a

instance DivisionRing Float where
  (/) = (Prelude./)
  recip = Prelude.recip
  fromRational = Prelude.fromRational
instance DivisionRing Double where
  (/) = (Prelude./)
  recip = Prelude.recip
  fromRational = Prelude.fromRational


  
-- totally ordered archimedean groups
class (Ord a, AdditiveGroup a) => Archimedean a where
  mod :: a -> a -> a
  rem :: a -> a -> a

instance Archimedean Word where
  mod = Prelude.mod
  rem = Prelude.rem
instance Archimedean Int where
  mod = Prelude.mod
  rem = Prelude.rem
instance Archimedean Float where
  mod = Data.Fixed.mod'
  rem a m = if signum a == signum m
            then mod a m
            else negate (mod a m)
instance Archimedean Double where
  mod = Data.Fixed.mod'
  rem a m = if signum a == signum m
            then mod a m
            else negate (mod a m)



class DivisionRing a => Floating a where
  pi                  :: a
  exp, log            :: a -> a
  sqrt, invSqrt       :: a -> a
  sin, cos, tan       :: a -> a
  asin, acos, atan    :: a -> a
  sinh, cosh, tanh    :: a -> a
  asinh, acosh, atanh :: a -> a
  (**)                :: a -> a -> a
  -- no logBase

instance Floating Float where
  pi      = Prelude.pi
  exp     = Prelude.exp
  log     = Prelude.log
  sqrt    = Prelude.sqrt
  sin     = Prelude.sin
  cos     = Prelude.cos
  tan     = Prelude.tan
  asin    = Prelude.asin
  acos    = Prelude.acos
  atan    = Prelude.atan
  sinh    = Prelude.sinh
  cosh    = Prelude.cosh
  tanh    = Prelude.tanh
  asinh   = Prelude.asinh
  acosh   = Prelude.acosh
  atanh   = Prelude.atanh
  (**)    = (Prelude.**)
  invSqrt = recip . sqrt

instance Floating Double where
  pi      = Prelude.pi
  exp     = Prelude.exp
  log     = Prelude.log
  sqrt    = Prelude.sqrt
  sin     = Prelude.sin
  cos     = Prelude.cos
  tan     = Prelude.tan
  asin    = Prelude.asin
  acos    = Prelude.acos
  atan    = Prelude.atan
  sinh    = Prelude.sinh
  cosh    = Prelude.cosh
  tanh    = Prelude.tanh
  asinh   = Prelude.asinh
  acosh   = Prelude.acosh
  atanh   = Prelude.atanh
  (**)    = (Prelude.**)
  invSqrt = recip . sqrt


class Convert a b where
  convert :: a -> b

instance Convert Word Word where
  convert = id
instance Convert Int Int where
  convert = id
instance Convert Float Float where
  convert = id
instance Convert Double Double where
  convert = id

instance Convert Word Int where
  convert = Prelude.fromIntegral
instance Convert Word Float where
  convert = Prelude.fromIntegral
instance Convert Word Double where
  convert = Prelude.fromIntegral
instance Convert Int Float where
  convert = Prelude.fromIntegral
instance Convert Int Double where
  convert = Prelude.fromIntegral
instance Convert Float Double where
  convert = Prelude.realToFrac

instance Convert Int Word where
  convert = Prelude.fromIntegral
instance Convert Float Word where
  convert = Prelude.truncate
instance Convert Double Word where
  convert = Prelude.truncate
instance Convert Float Int where
  convert = Prelude.truncate
instance Convert Double Int where
  convert = Prelude.truncate
instance Convert Double Float where
  convert = Prelude.realToFrac


{-
class Round a b where
  round, truncate, ceiling, floor :: a -> b

instance Round Float Float where
  round    = fromIntegral . Prelude.round
  truncate = fromIntegral . Prelude.truncate
  ceiling  = fromIntegral . Prelude.ceiling
  floor    = fromIntegral . Prelude.floor
instance Round Double Double where
  round    = fromIntegral . Prelude.round
  truncate = fromIntegral . Prelude.truncate
  ceiling  = fromIntegral . Prelude.ceiling
  floor    = fromIntegral . Prelude.floor
instance Round Double Double where
  round    = fromIntegral . Prelude.round
  truncate = fromIntegral . Prelude.truncate
  ceiling  = fromIntegral . Prelude.ceiling
  floor    = fromIntegral . Prelude.floor
-}

