{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}


module Math.Algebra.Class where

-- base
import Prelude((.), Integer, Rational, Int, Float, Double, Word)
import qualified Prelude
import Data.Coerce(coerce)
import Data.Int(Int8, Int16, Int32, Int64)
import qualified Data.Fixed as Fixed
import Data.Word(Word8, Word16, Word32, Word64)
import qualified GHC.Float

-- half
import Numeric.Half(Half)

-- fir
import Deriving.Prelude(Prelude(..)) -- newtype for deriving Prelude instances
import Math.Logic.Class(HasBool, Eq((==)), Ord, ifThenElse)



class AdditiveGroup a where
  (+)  :: a -> a -> a
  zero :: a

instance Prelude.Num a => AdditiveGroup (Prelude a) where
  (+)  = coerce ( (Prelude.+) :: a -> a -> a )
  zero = coerce ( 0 :: a )

deriving via Prelude Word8  instance AdditiveGroup Word8
deriving via Prelude Word16 instance AdditiveGroup Word16
deriving via Prelude Word32 instance AdditiveGroup Word32
deriving via Prelude Word64 instance AdditiveGroup Word64
deriving via Prelude Word   instance AdditiveGroup Word
deriving via Prelude Int8   instance AdditiveGroup Int8
deriving via Prelude Int16  instance AdditiveGroup Int16
deriving via Prelude Int32  instance AdditiveGroup Int32
deriving via Prelude Int64  instance AdditiveGroup Int64
deriving via Prelude Int    instance AdditiveGroup Int
deriving via Prelude Half   instance AdditiveGroup Half
deriving via Prelude Float  instance AdditiveGroup Float
deriving via Prelude Double instance AdditiveGroup Double



class AdditiveGroup a => Semiring a where
  (*) :: a -> a -> a

instance Prelude.Num a => Semiring (Prelude a) where
  (*) = coerce ( (Prelude.*) :: a -> a -> a )

deriving via Prelude Word8  instance Semiring Word8
deriving via Prelude Word16 instance Semiring Word16
deriving via Prelude Word32 instance Semiring Word32
deriving via Prelude Word64 instance Semiring Word64
deriving via Prelude Word   instance Semiring Word
deriving via Prelude Int8   instance Semiring Int8
deriving via Prelude Int16  instance Semiring Int16
deriving via Prelude Int32  instance Semiring Int32
deriving via Prelude Int64  instance Semiring Int64
deriving via Prelude Int    instance Semiring Int
deriving via Prelude Half   instance Semiring Half
deriving via Prelude Float  instance Semiring Float
deriving via Prelude Double instance Semiring Double



class Semiring a => Ring a where
  (-)         :: a -> a -> a
  negate      :: a -> a
  fromInteger :: Integer -> a

instance Prelude.Num a => Ring (Prelude a) where
  (-)         = coerce ( (Prelude.-)    :: a -> a -> a )
  negate      = coerce ( Prelude.negate :: a -> a )
  fromInteger = (coerce :: a -> Prelude a) . Prelude.fromInteger

deriving via Prelude Int8   instance Ring Int8
deriving via Prelude Int16  instance Ring Int16
deriving via Prelude Int32  instance Ring Int32
deriving via Prelude Int64  instance Ring Int64
deriving via Prelude Int    instance Ring Int
deriving via Prelude Half   instance Ring Half
deriving via Prelude Float  instance Ring Float
deriving via Prelude Double instance Ring Double



class Ring a => Signed a where
  abs    :: a -> a
  signum :: a -> a

instance Prelude.Num a => Signed (Prelude a) where
  abs    = coerce ( Prelude.abs    :: a -> a )
  signum = coerce ( Prelude.signum :: a -> a )

deriving via Prelude Int8   instance Signed Int8
deriving via Prelude Int16  instance Signed Int16
deriving via Prelude Int32  instance Signed Int32
deriving via Prelude Int64  instance Signed Int64
deriving via Prelude Int    instance Signed Int
deriving via Prelude Half   instance Signed Half
deriving via Prelude Float  instance Signed Float
deriving via Prelude Double instance Signed Double



class Ring a => DivisionRing a where
  (/)   :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

  recip a = fromInteger 1 / a

instance Prelude.Fractional a => DivisionRing (Prelude a) where
  (/)   = coerce ( (Prelude./) :: a -> a -> a )
  recip = coerce ( Prelude.recip :: a -> a ) 
  fromRational = ( coerce :: a -> Prelude a ) . Prelude.fromRational

deriving via Prelude Half   instance DivisionRing Half
deriving via Prelude Float  instance DivisionRing Float
deriving via Prelude Double instance DivisionRing Double




-- totally ordered archimedean groups
class (Ord a, AdditiveGroup a) => Archimedean a where
  mod :: a -> a -> a
  rem :: a -> a -> a

instance ( Ord a, AdditiveGroup a, Prelude.Integral a ) => Archimedean (Prelude a) where
  mod = coerce ( Prelude.mod :: a -> a -> a )
  rem = coerce ( Prelude.rem :: a -> a -> a )

deriving via Prelude Word8  instance Archimedean Word8
deriving via Prelude Word16 instance Archimedean Word16
deriving via Prelude Word32 instance Archimedean Word32
deriving via Prelude Word64 instance Archimedean Word64
deriving via Prelude Word   instance Archimedean Word
deriving via Prelude Int8   instance Archimedean Int8
deriving via Prelude Int16  instance Archimedean Int16
deriving via Prelude Int32  instance Archimedean Int32
deriving via Prelude Int64  instance Archimedean Int64
deriving via Prelude Int    instance Archimedean Int

newtype Fixed a = Fixed a
deriving via a instance HasBool b     a => HasBool b     (Fixed a)
deriving via a instance Eq            a => Eq            (Fixed a)
deriving via a instance Ord           a => Ord           (Fixed a)
deriving via a instance AdditiveGroup a => AdditiveGroup (Fixed a)
deriving via a instance Semiring      a => Semiring      (Fixed a)
deriving via a instance Ring          a => Ring          (Fixed a)
deriving via a instance Signed        a => Signed        (Fixed a)

instance ( Eq a, Ord a
         , Ring a, Signed a
         , Prelude.Real a
         ) => Archimedean (Fixed a) where
  mod = coerce ( Fixed.mod' :: a -> a -> a )
  rem a m = ifThenElse (signum a == signum m)
              ( mod a m )
              ( negate ( mod a m ) )

deriving via Fixed Half   instance Archimedean Half
deriving via Fixed Float  instance Archimedean Float
deriving via Fixed Double instance Archimedean Double



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

instance Prelude.Floating a => Floating (Prelude a) where
  pi      = coerce ( Prelude.pi :: a)
  exp     = coerce ( Prelude.exp   :: a -> a )
  log     = coerce ( Prelude.log   :: a -> a )
  sqrt    = coerce ( Prelude.sqrt  :: a -> a )
  sin     = coerce ( Prelude.sin   :: a -> a )
  cos     = coerce ( Prelude.cos   :: a -> a )
  tan     = coerce ( Prelude.tan   :: a -> a )
  asin    = coerce ( Prelude.asin  :: a -> a )
  acos    = coerce ( Prelude.acos  :: a -> a )
  atan    = coerce ( Prelude.atan  :: a -> a )
  sinh    = coerce ( Prelude.sinh  :: a -> a )
  cosh    = coerce ( Prelude.cosh  :: a -> a )
  tanh    = coerce ( Prelude.tanh  :: a -> a )
  asinh   = coerce ( Prelude.asinh :: a -> a )
  acosh   = coerce ( Prelude.acosh :: a -> a )
  atanh   = coerce ( Prelude.atanh :: a -> a )
  (**)    = coerce ( (Prelude.**) :: a -> a -> a )
  invSqrt = recip . sqrt

deriving via Prelude Half   instance Floating Half
deriving via Prelude Float  instance Floating Float
deriving via Prelude Double instance Floating Double


{-
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
  convert = GHC.Float.float2Double

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
  convert = GHC.Float.double2Float


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

