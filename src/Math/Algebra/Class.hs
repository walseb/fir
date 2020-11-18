{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: Math.Algebra.Class

Alternative numeric hierarchy, tailored for this library.

The main problem with the standard numeric hierarchy consists
of class methods with monomorphic return types.

For instance, the method

> toInteger :: Integral a => a -> Integer

is not amenable to overloading when working with ASTs, as
given e.g. @AST Int@ there is no way of recovering an @Integer@.

Contrast this with a method such as

> fromInteger :: Num a => Integer -> a

There is no problem instantiating this method at a type such as @AST Int@,
as it is indeed possible to turn a Haskell-level @Integer@ into an AST for an @Int@.

-}

module Math.Algebra.Class where

-- base
import Prelude
  ( id, (.), ($)
  , Bool
  , Integer, Rational
  , Word, Int
  , Float, Double
  )
import qualified Prelude
import Data.Coerce
  ( coerce )
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.Kind
  ( Type )
import qualified Data.Fixed as Fixed
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import Foreign.C.Types
import qualified GHC.Float as Float

-- half
import Numeric.Half
  ( Half )
import qualified Numeric.Half as Half

-- fir
import Deriving.Base
  ( Base(..) ) -- newtype for deriving via base instances
import Math.Logic.Class
  ( ifThenElse, Eq(Logic,(==)), Ord )


infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /
infixr 8 **

class AdditiveMonoid a where
  -- | Addition.
  (+)  :: a -> a -> a
  -- | Identity for addition.
  zero :: a
  fromInteger :: Integer -> a
  -- technically should be a method of "Ring"
  -- but there is no rebindable syntax for "fromNatural"

instance Prelude.Num a => AdditiveMonoid (Base a) where
  (+)  = coerce ( (Prelude.+) :: a -> a -> a )
  zero = coerce ( 0 :: a )
  fromInteger = (coerce :: a -> Base a) . Prelude.fromInteger

deriving via Base Word8  instance AdditiveMonoid Word8
deriving via Base Word16 instance AdditiveMonoid Word16
deriving via Base Word32 instance AdditiveMonoid Word32
deriving via Base Word64 instance AdditiveMonoid Word64
deriving via Base Word   instance AdditiveMonoid Word
deriving via Base Int8   instance AdditiveMonoid Int8
deriving via Base Int16  instance AdditiveMonoid Int16
deriving via Base Int32  instance AdditiveMonoid Int32
deriving via Base Int64  instance AdditiveMonoid Int64
deriving via Base Int    instance AdditiveMonoid Int
deriving via Base Half   instance AdditiveMonoid Half
deriving via Base Float  instance AdditiveMonoid Float
deriving via Base Double instance AdditiveMonoid Double

deriving via Base CChar   instance AdditiveMonoid CChar
deriving via Base CSChar  instance AdditiveMonoid CSChar
deriving via Base CUChar  instance AdditiveMonoid CUChar
deriving via Base CShort  instance AdditiveMonoid CShort
deriving via Base CUShort instance AdditiveMonoid CUShort
deriving via Base CInt    instance AdditiveMonoid CInt
deriving via Base CUInt   instance AdditiveMonoid CUInt
deriving via Base CLong   instance AdditiveMonoid CLong
deriving via Base CULong  instance AdditiveMonoid CULong
deriving via Base CLLong  instance AdditiveMonoid CLLong
deriving via Base CULLong instance AdditiveMonoid CULLong
deriving via Base CFloat  instance AdditiveMonoid CFloat
deriving via Base CDouble instance AdditiveMonoid CDouble

class AdditiveMonoid a => Semiring a where
  -- | Multiplication.
  (*) :: a -> a -> a

instance Prelude.Num a => Semiring (Base a) where
  (*) = coerce ( (Prelude.*) :: a -> a -> a )

deriving via Base Word8  instance Semiring Word8
deriving via Base Word16 instance Semiring Word16
deriving via Base Word32 instance Semiring Word32
deriving via Base Word64 instance Semiring Word64
deriving via Base Word   instance Semiring Word
deriving via Base Int8   instance Semiring Int8
deriving via Base Int16  instance Semiring Int16
deriving via Base Int32  instance Semiring Int32
deriving via Base Int64  instance Semiring Int64
deriving via Base Int    instance Semiring Int
deriving via Base Half   instance Semiring Half
deriving via Base Float  instance Semiring Float
deriving via Base Double instance Semiring Double

deriving via Base CChar   instance Semiring CChar
deriving via Base CSChar  instance Semiring CSChar
deriving via Base CUChar  instance Semiring CUChar
deriving via Base CShort  instance Semiring CShort
deriving via Base CUShort instance Semiring CUShort
deriving via Base CInt    instance Semiring CInt
deriving via Base CUInt   instance Semiring CUInt
deriving via Base CLong   instance Semiring CLong
deriving via Base CULong  instance Semiring CULong
deriving via Base CLLong  instance Semiring CLLong
deriving via Base CULLong instance Semiring CULLong
deriving via Base CFloat  instance Semiring CFloat
deriving via Base CDouble instance Semiring CDouble

-- | Cancellative monoid under addition.
class AdditiveMonoid a => CancellativeAdditiveMonoid a where
  -- | Left-inverse to addition:
  --
  -- > ( \ x -> x - y ) . ( \ x -> x + y ) = id
  --
  -- Might be partially undefined or underflow,
  -- as ( \ x -> x + y ) need not be surjective for any fixed y.
  (-) :: a -> a -> a

instance Prelude.Num a => CancellativeAdditiveMonoid (Base a) where
  (-) = coerce ( (Prelude.-) :: a -> a -> a )

deriving via Base Word8  instance CancellativeAdditiveMonoid Word8
deriving via Base Word16 instance CancellativeAdditiveMonoid Word16
deriving via Base Word32 instance CancellativeAdditiveMonoid Word32
deriving via Base Word64 instance CancellativeAdditiveMonoid Word64
deriving via Base Word   instance CancellativeAdditiveMonoid Word
deriving via Base Int8   instance CancellativeAdditiveMonoid Int8
deriving via Base Int16  instance CancellativeAdditiveMonoid Int16
deriving via Base Int32  instance CancellativeAdditiveMonoid Int32
deriving via Base Int64  instance CancellativeAdditiveMonoid Int64
deriving via Base Int    instance CancellativeAdditiveMonoid Int
deriving via Base Half   instance CancellativeAdditiveMonoid Half
deriving via Base Float  instance CancellativeAdditiveMonoid Float
deriving via Base Double instance CancellativeAdditiveMonoid Double

deriving via Base CChar   instance CancellativeAdditiveMonoid CChar
deriving via Base CSChar  instance CancellativeAdditiveMonoid CSChar
deriving via Base CUChar  instance CancellativeAdditiveMonoid CUChar
deriving via Base CShort  instance CancellativeAdditiveMonoid CShort
deriving via Base CUShort instance CancellativeAdditiveMonoid CUShort
deriving via Base CInt    instance CancellativeAdditiveMonoid CInt
deriving via Base CUInt   instance CancellativeAdditiveMonoid CUInt
deriving via Base CLong   instance CancellativeAdditiveMonoid CLong
deriving via Base CULong  instance CancellativeAdditiveMonoid CULong
deriving via Base CLLong  instance CancellativeAdditiveMonoid CLLong
deriving via Base CULLong instance CancellativeAdditiveMonoid CULLong
deriving via Base CFloat  instance CancellativeAdditiveMonoid CFloat
deriving via Base CDouble instance CancellativeAdditiveMonoid CDouble

-- | Additive group.
class CancellativeAdditiveMonoid a => AdditiveGroup a where
  -- | Negation.
  negate :: a -> a

instance Prelude.Num a => AdditiveGroup (Base a) where
  negate = coerce ( Prelude.negate :: a -> a )

deriving via Base Int8   instance AdditiveGroup Int8
deriving via Base Int16  instance AdditiveGroup Int16
deriving via Base Int32  instance AdditiveGroup Int32
deriving via Base Int64  instance AdditiveGroup Int64
deriving via Base Int    instance AdditiveGroup Int
deriving via Base Half   instance AdditiveGroup Half
deriving via Base Float  instance AdditiveGroup Float
deriving via Base Double instance AdditiveGroup Double

deriving via Base CChar   instance AdditiveGroup CChar
deriving via Base CSChar  instance AdditiveGroup CSChar
deriving via Base CShort  instance AdditiveGroup CShort
deriving via Base CInt    instance AdditiveGroup CInt
deriving via Base CLong   instance AdditiveGroup CLong
deriving via Base CLLong  instance AdditiveGroup CLLong
deriving via Base CFloat  instance AdditiveGroup CFloat
deriving via Base CDouble instance AdditiveGroup CDouble

class AdditiveGroup a => Signed a where
  -- | Absolute value.
  abs    :: a -> a
  -- | Sign. 
  --
  -- > signum  5 =  1
  -- > signum -7 = -1
  -- > signum  0 =  0
  signum :: a -> a

instance Prelude.Num a => Signed (Base a) where
  abs    = coerce ( Prelude.abs    :: a -> a )
  signum = coerce ( Prelude.signum :: a -> a )

deriving via Base Int8   instance Signed Int8
deriving via Base Int16  instance Signed Int16
deriving via Base Int32  instance Signed Int32
deriving via Base Int64  instance Signed Int64
deriving via Base Int    instance Signed Int
deriving via Base Half   instance Signed Half
deriving via Base Float  instance Signed Float
deriving via Base Double instance Signed Double

deriving via Base CChar   instance Signed CChar
deriving via Base CSChar  instance Signed CSChar
deriving via Base CShort  instance Signed CShort
deriving via Base CInt    instance Signed CInt
deriving via Base CLong   instance Signed CLong
deriving via Base CLLong  instance Signed CLLong
deriving via Base CFloat  instance Signed CFloat
deriving via Base CDouble instance Signed CDouble

class    (Semiring a, AdditiveGroup a) => Ring a where
instance (Semiring a, AdditiveGroup a) => Ring a where

class Ring a => DivisionRing a where
  {-# MINIMAL ( (/) | recip ), fromRational #-}
  -- | Division.
  (/)   :: a -> a -> a
  -- | Reciprocal.
  recip :: a -> a
  fromRational :: Rational -> a

  recip a = fromInteger 1 / a
  a / b = a * recip b

instance Prelude.Fractional a => DivisionRing (Base a) where
  (/)   = coerce ( (Prelude./) :: a -> a -> a )
  recip = coerce ( Prelude.recip :: a -> a ) 
  fromRational = ( coerce :: a -> Base a ) . Prelude.fromRational

deriving via Base Half   instance DivisionRing Half
deriving via Base Float  instance DivisionRing Float
deriving via Base Double instance DivisionRing Double

deriving via Base CFloat  instance DivisionRing CFloat
deriving via Base CDouble instance DivisionRing CDouble


-- totally ordered archimedean monoids
class (Ord a, AdditiveMonoid a) => Archimedean a where
  mod    :: a -> a -> a
  rem    :: a -> a -> a
  div    :: a -> a -> a
  divMod :: a -> a -> (a,a)
  divMod a b = ( div a b, mod a b )

instance ( Ord a, CancellativeAdditiveMonoid a, Prelude.Integral a ) => Archimedean (Base a) where
  mod    = coerce ( Prelude.mod    :: a -> a -> a     )
  rem    = coerce ( Prelude.rem    :: a -> a -> a     )
  div    = coerce ( Prelude.div    :: a -> a -> a     )
  divMod = coerce ( Prelude.divMod :: a -> a -> (a,a) )

deriving via Base Word8  instance Archimedean Word8
deriving via Base Word16 instance Archimedean Word16
deriving via Base Word32 instance Archimedean Word32
deriving via Base Word64 instance Archimedean Word64
deriving via Base Word   instance Archimedean Word
deriving via Base Int8   instance Archimedean Int8
deriving via Base Int16  instance Archimedean Int16
deriving via Base Int32  instance Archimedean Int32
deriving via Base Int64  instance Archimedean Int64
deriving via Base Int    instance Archimedean Int

deriving via Base CChar   instance Archimedean CChar
deriving via Base CSChar  instance Archimedean CSChar
deriving via Base CUChar  instance Archimedean CUChar
deriving via Base CShort  instance Archimedean CShort
deriving via Base CUShort instance Archimedean CUShort
deriving via Base CInt    instance Archimedean CInt
deriving via Base CUInt   instance Archimedean CUInt
deriving via Base CLong   instance Archimedean CLong
deriving via Base CULong  instance Archimedean CULong
deriving via Base CLLong  instance Archimedean CLLong
deriving via Base CULLong instance Archimedean CULLong

newtype Fixed a = Fixed { runFixed :: a }
deriving via a instance Eq                         a
                     => Eq                         (Fixed a)
deriving via a instance Ord                        a
                     => Ord                        (Fixed a)
deriving via a instance AdditiveMonoid             a
                     => AdditiveMonoid             (Fixed a)
deriving via a instance CancellativeAdditiveMonoid a
                     => CancellativeAdditiveMonoid (Fixed a)
deriving via a instance AdditiveGroup              a
                     => AdditiveGroup              (Fixed a)
deriving via a instance Semiring                   a
                     => Semiring                   (Fixed a)
deriving via a instance Signed                     a
                     => Signed                     (Fixed a)

instance ( Eq a, Logic a ~ Bool
         , Ord a
         , Ring a, Signed a
         , Prelude.Real a
         ) => Archimedean (Fixed a) where
  mod = coerce ( Fixed.mod' :: a -> a -> a )
  rem a m = ifThenElse (signum a == signum m)
              ( mod a m )
              ( negate ( mod a m ) )
  div (Fixed a) (Fixed b)
    = Fixed $ fromInteger ( Fixed.div' a b )
  divMod (Fixed a) (Fixed b)
    = case Fixed.divMod' a b of
        (q,r) -> ( Fixed (fromInteger q), Fixed r )

deriving via Fixed Half   instance Archimedean Half
deriving via Fixed Float  instance Archimedean Float
deriving via Fixed Double instance Archimedean Double

deriving via Fixed CFloat  instance Archimedean CFloat
deriving via Fixed CDouble instance Archimedean CDouble


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

instance Prelude.Floating a => Floating (Base a) where
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

deriving via Base Half   instance Floating Half
deriving via Base Float  instance Floating Float
deriving via Base Double instance Floating Double

deriving via Base CFloat  instance Floating CFloat
deriving via Base CDouble instance Floating CDouble

class Floating a => RealFloat a where
  atan2 :: a -> a -> a

instance Prelude.RealFloat a => RealFloat (Base a) where
  atan2 = coerce ( Prelude.atan2 :: a -> a -> a )

deriving via Base Half   instance RealFloat Half
deriving via Base Float  instance RealFloat Float
deriving via Base Double instance RealFloat Double

deriving via Base CFloat  instance RealFloat CFloat
deriving via Base CDouble instance RealFloat CDouble

-- constraint for integer types,
-- no methods
class Semiring a => Integral a where
instance Integral Word8   where
instance Integral Word16  where
instance Integral Word32  where
instance Integral Word64  where
instance Integral Word    where
instance Integral Int8    where
instance Integral Int16   where
instance Integral Int32   where
instance Integral Int64   where
instance Integral Int     where
instance Integral CChar   where
instance Integral CSChar  where
instance Integral CUChar  where
instance Integral CShort  where
instance Integral CUShort where
instance Integral CInt    where
instance Integral CUInt   where
instance Integral CLong   where
instance Integral CULong  where
instance Integral CLLong  where
instance Integral CULLong where

-- constraint for unsigned integer types,
-- no methods
class Integral a => Unsigned a where
instance Unsigned Word8   where
instance Unsigned Word16  where
instance Unsigned Word32  where
instance Unsigned Word64  where
instance Unsigned Word    where
instance Unsigned CUChar  where
instance Unsigned CUShort where
instance Unsigned CUInt   where
instance Unsigned CULong  where
instance Unsigned CULLong where


type family Arr (domCod :: (Type,Type)) = (arr :: Type) | arr -> domCod where
  Arr '(dom, cod) = dom -> cod

class Convert (c :: (Type,Type)) where
  convert :: Arr c

instance Convert '(a,a) where
  convert = id

-- fromIntegral conversions
instance (Prelude.Integral a, Prelude.Num b) => Convert '(Base a, b) where
  convert (Base a) = Prelude.fromIntegral a

-- to unsigned integers
deriving via '(Base Word16, Word8 ) instance Convert '(Word16, Word8 )
deriving via '(Base Word32, Word8 ) instance Convert '(Word32, Word8 )
deriving via '(Base Word64, Word8 ) instance Convert '(Word64, Word8 )
deriving via '(Base Word  , Word8 ) instance Convert '(Word  , Word8 )
deriving via '(Base Int8  , Word8 ) instance Convert '(Int8  , Word8 )
deriving via '(Base Int16 , Word8 ) instance Convert '(Int16 , Word8 )
deriving via '(Base Int32 , Word8 ) instance Convert '(Int32 , Word8 )
deriving via '(Base Int64 , Word8 ) instance Convert '(Int64 , Word8 )
deriving via '(Base Int   , Word8 ) instance Convert '(Int   , Word8 )
deriving via '(Base Word8 , Word16) instance Convert '(Word8 , Word16)
deriving via '(Base Word32, Word16) instance Convert '(Word32, Word16)
deriving via '(Base Word64, Word16) instance Convert '(Word64, Word16)
deriving via '(Base Word  , Word16) instance Convert '(Word  , Word16)
deriving via '(Base Int8  , Word16) instance Convert '(Int8  , Word16)
deriving via '(Base Int16 , Word16) instance Convert '(Int16 , Word16)
deriving via '(Base Int32 , Word16) instance Convert '(Int32 , Word16)
deriving via '(Base Int64 , Word16) instance Convert '(Int64 , Word16)
deriving via '(Base Int   , Word16) instance Convert '(Int   , Word16)
deriving via '(Base Word8 , Word32) instance Convert '(Word8 , Word32)
deriving via '(Base Word16, Word32) instance Convert '(Word16, Word32)
deriving via '(Base Word64, Word32) instance Convert '(Word64, Word32)
deriving via '(Base Word  , Word32) instance Convert '(Word  , Word32)
deriving via '(Base Int8  , Word32) instance Convert '(Int8  , Word32)
deriving via '(Base Int16 , Word32) instance Convert '(Int16 , Word32)
deriving via '(Base Int32 , Word32) instance Convert '(Int32 , Word32)
deriving via '(Base Int64 , Word32) instance Convert '(Int64 , Word32)
deriving via '(Base Int   , Word32) instance Convert '(Int   , Word32)
deriving via '(Base Word8 , Word64) instance Convert '(Word8 , Word64)
deriving via '(Base Word16, Word64) instance Convert '(Word16, Word64)
deriving via '(Base Word32, Word64) instance Convert '(Word32, Word64)
deriving via '(Base Word  , Word64) instance Convert '(Word  , Word64)
deriving via '(Base Int8  , Word64) instance Convert '(Int8  , Word64)
deriving via '(Base Int16 , Word64) instance Convert '(Int16 , Word64)
deriving via '(Base Int32 , Word64) instance Convert '(Int32 , Word64)
deriving via '(Base Int64 , Word64) instance Convert '(Int64 , Word64)
deriving via '(Base Int   , Word64) instance Convert '(Int   , Word64)
deriving via '(Base Word8 , Word  ) instance Convert '(Word8 , Word  )
deriving via '(Base Word16, Word  ) instance Convert '(Word16, Word  )
deriving via '(Base Word32, Word  ) instance Convert '(Word32, Word  )
deriving via '(Base Word64, Word  ) instance Convert '(Word64, Word  )
deriving via '(Base Int8  , Word  ) instance Convert '(Int8  , Word  )
deriving via '(Base Int16 , Word  ) instance Convert '(Int16 , Word  )
deriving via '(Base Int32 , Word  ) instance Convert '(Int32 , Word  )
deriving via '(Base Int64 , Word  ) instance Convert '(Int64 , Word  )
deriving via '(Base Int   , Word  ) instance Convert '(Int   , Word  )
-- to signed integers
deriving via '(Base Word8 , Int8  ) instance Convert '(Word8 , Int8  )
deriving via '(Base Word16, Int8  ) instance Convert '(Word16, Int8  )
deriving via '(Base Word32, Int8  ) instance Convert '(Word32, Int8  )
deriving via '(Base Word64, Int8  ) instance Convert '(Word64, Int8  )
deriving via '(Base Word  , Int8  ) instance Convert '(Word  , Int8  )
deriving via '(Base Int16 , Int8  ) instance Convert '(Int16 , Int8  )
deriving via '(Base Int32 , Int8  ) instance Convert '(Int32 , Int8  )
deriving via '(Base Int64 , Int8  ) instance Convert '(Int64 , Int8  )
deriving via '(Base Int   , Int8  ) instance Convert '(Int   , Int8  )
deriving via '(Base Word8 , Int16 ) instance Convert '(Word8 , Int16 )
deriving via '(Base Word16, Int16 ) instance Convert '(Word16, Int16 )
deriving via '(Base Word32, Int16 ) instance Convert '(Word32, Int16 )
deriving via '(Base Word64, Int16 ) instance Convert '(Word64, Int16 )
deriving via '(Base Word  , Int16 ) instance Convert '(Word  , Int16 )
deriving via '(Base Int8  , Int16 ) instance Convert '(Int8  , Int16 )
deriving via '(Base Int32 , Int16 ) instance Convert '(Int32 , Int16 )
deriving via '(Base Int64 , Int16 ) instance Convert '(Int64 , Int16 )
deriving via '(Base Int   , Int16 ) instance Convert '(Int   , Int16 )
deriving via '(Base Word8 , Int32 ) instance Convert '(Word8 , Int32 )
deriving via '(Base Word16, Int32 ) instance Convert '(Word16, Int32 )
deriving via '(Base Word32, Int32 ) instance Convert '(Word32, Int32 )
deriving via '(Base Word64, Int32 ) instance Convert '(Word64, Int32 )
deriving via '(Base Word  , Int32 ) instance Convert '(Word  , Int32 )
deriving via '(Base Int8  , Int32 ) instance Convert '(Int8  , Int32 )
deriving via '(Base Int16 , Int32 ) instance Convert '(Int16 , Int32 )
deriving via '(Base Int64 , Int32 ) instance Convert '(Int64 , Int32 )
deriving via '(Base Int   , Int32 ) instance Convert '(Int   , Int32 )
deriving via '(Base Word8 , Int64 ) instance Convert '(Word8 , Int64 )
deriving via '(Base Word16, Int64 ) instance Convert '(Word16, Int64 )
deriving via '(Base Word32, Int64 ) instance Convert '(Word32, Int64 )
deriving via '(Base Word64, Int64 ) instance Convert '(Word64, Int64 )
deriving via '(Base Word  , Int64 ) instance Convert '(Word  , Int64 )
deriving via '(Base Int8  , Int64 ) instance Convert '(Int8  , Int64 )
deriving via '(Base Int16 , Int64 ) instance Convert '(Int16 , Int64 )
deriving via '(Base Int32 , Int64 ) instance Convert '(Int32 , Int64 )
deriving via '(Base Int   , Int64 ) instance Convert '(Int   , Int64 )
deriving via '(Base Word8 , Int   ) instance Convert '(Word8 , Int   )
deriving via '(Base Word16, Int   ) instance Convert '(Word16, Int   )
deriving via '(Base Word32, Int   ) instance Convert '(Word32, Int   )
deriving via '(Base Word64, Int   ) instance Convert '(Word64, Int   )
deriving via '(Base Word  , Int   ) instance Convert '(Word  , Int   )
deriving via '(Base Int8  , Int   ) instance Convert '(Int8  , Int   )
deriving via '(Base Int16 , Int   ) instance Convert '(Int16 , Int   )
deriving via '(Base Int32 , Int   ) instance Convert '(Int32 , Int   )
deriving via '(Base Int64 , Int   ) instance Convert '(Int64 , Int   )
-- to floating point
deriving via '(Base Word8 , Half  ) instance Convert '(Word8 , Half  )
deriving via '(Base Word16, Half  ) instance Convert '(Word16, Half  )
deriving via '(Base Word32, Half  ) instance Convert '(Word32, Half  )
deriving via '(Base Word64, Half  ) instance Convert '(Word64, Half  )
deriving via '(Base Word  , Half  ) instance Convert '(Word  , Half  )
deriving via '(Base Int8  , Half  ) instance Convert '(Int8  , Half  )
deriving via '(Base Int16 , Half  ) instance Convert '(Int16 , Half  )
deriving via '(Base Int32 , Half  ) instance Convert '(Int32 , Half  )
deriving via '(Base Int64 , Half  ) instance Convert '(Int64 , Half  )
deriving via '(Base Word8 , Float ) instance Convert '(Word8 , Float )
deriving via '(Base Word16, Float ) instance Convert '(Word16, Float )
deriving via '(Base Word32, Float ) instance Convert '(Word32, Float )
deriving via '(Base Word64, Float ) instance Convert '(Word64, Float )
deriving via '(Base Word  , Float ) instance Convert '(Word  , Float )
deriving via '(Base Int8  , Float ) instance Convert '(Int8  , Float )
deriving via '(Base Int16 , Float ) instance Convert '(Int16 , Float )
deriving via '(Base Int32 , Float ) instance Convert '(Int32 , Float )
deriving via '(Base Int64 , Float ) instance Convert '(Int64 , Float )
deriving via '(Base Word8 , Double) instance Convert '(Word8 , Double)
deriving via '(Base Word16, Double) instance Convert '(Word16, Double)
deriving via '(Base Word32, Double) instance Convert '(Word32, Double)
deriving via '(Base Word64, Double) instance Convert '(Word64, Double)
deriving via '(Base Word  , Double) instance Convert '(Word  , Double)
deriving via '(Base Int8  , Double) instance Convert '(Int8  , Double)
deriving via '(Base Int16 , Double) instance Convert '(Int16 , Double)
deriving via '(Base Int32 , Double) instance Convert '(Int32 , Double)
deriving via '(Base Int64 , Double) instance Convert '(Int64 , Double)

-- floating conversions
instance Convert '(Half  , Float ) where
  convert = Half.fromHalf
instance Convert '(Half  , Double) where
  convert = Float.float2Double . Half.fromHalf
instance Convert '(Float , Double) where
  convert = Float.float2Double
instance Convert '(Float , Half  ) where
  convert = Half.toHalf
instance Convert '(Double, Half  ) where
  convert = Half.toHalf . Float.double2Float
instance Convert '(Double, Float ) where
  convert = Float.double2Float

fromIntegral :: (Convert '(a,b), Integral a) => a -> b
fromIntegral = convert

realToFrac :: (Convert '(a,b), Floating a, DivisionRing b) => a -> b
realToFrac = convert

-- truncation/rounding/etc
class Rounding (c :: (Type,Type)) where
  -- | Truncate (towards 0).
  truncate :: Arr c
  round    :: Arr c
  floor    :: Arr c
  ceiling  :: Arr c

instance (Prelude.RealFrac a, Prelude.Integral b) => Rounding '(a, Base b) where
  truncate = Base . Prelude.truncate
  round    = Base . Prelude.round
  floor    = Base . Prelude.floor
  ceiling  = Base . Prelude.ceiling

instance (Prelude.RealFrac a, Prelude.Num b) => Rounding '(Base a, b) where
  truncate = Prelude.fromIntegral . Prelude.truncate @_ @Int64 . runBase
  round    = Prelude.fromIntegral . Prelude.round    @_ @Int64 . runBase
  floor    = Prelude.fromIntegral . Prelude.floor    @_ @Int64 . runBase
  ceiling  = Prelude.fromIntegral . Prelude.ceiling  @_ @Int64 . runBase

-- to unsigned integers
deriving via '(Half  , Base Word8 ) instance Rounding '(Half  , Word8 )
deriving via '(Half  , Base Word16) instance Rounding '(Half  , Word16)
deriving via '(Half  , Base Word32) instance Rounding '(Half  , Word32)
deriving via '(Half  , Base Word64) instance Rounding '(Half  , Word64)
deriving via '(Half  , Base Word  ) instance Rounding '(Half  , Word  )
deriving via '(Float , Base Word8 ) instance Rounding '(Float , Word8 )
deriving via '(Float , Base Word16) instance Rounding '(Float , Word16)
deriving via '(Float , Base Word32) instance Rounding '(Float , Word32)
deriving via '(Float , Base Word64) instance Rounding '(Float , Word64)
deriving via '(Float , Base Word  ) instance Rounding '(Float , Word  )
deriving via '(Double, Base Word8 ) instance Rounding '(Double, Word8 )
deriving via '(Double, Base Word16) instance Rounding '(Double, Word16)
deriving via '(Double, Base Word32) instance Rounding '(Double, Word32)
deriving via '(Double, Base Word64) instance Rounding '(Double, Word64)
deriving via '(Double, Base Word  ) instance Rounding '(Double, Word  )
-- to signed integers
deriving via '(Half  , Base Int8  ) instance Rounding '(Half  , Int8  )
deriving via '(Half  , Base Int16 ) instance Rounding '(Half  , Int16 )
deriving via '(Half  , Base Int32 ) instance Rounding '(Half  , Int32 )
deriving via '(Half  , Base Int64 ) instance Rounding '(Half  , Int64 )
deriving via '(Half  , Base Int   ) instance Rounding '(Half  , Int   )
deriving via '(Float , Base Int8  ) instance Rounding '(Float , Int8  )
deriving via '(Float , Base Int16 ) instance Rounding '(Float , Int16 )
deriving via '(Float , Base Int32 ) instance Rounding '(Float , Int32 )
deriving via '(Float , Base Int64 ) instance Rounding '(Float , Int64 )
deriving via '(Float , Base Int   ) instance Rounding '(Float , Int   )
deriving via '(Double, Base Int8  ) instance Rounding '(Double, Int8  )
deriving via '(Double, Base Int16 ) instance Rounding '(Double, Int16 )
deriving via '(Double, Base Int32 ) instance Rounding '(Double, Int32 )
deriving via '(Double, Base Int64 ) instance Rounding '(Double, Int64 )
deriving via '(Double, Base Int   ) instance Rounding '(Double, Int   )
-- to floating point
deriving via '(Base Half  , Half  ) instance Rounding '(Half  , Half  )
deriving via '(Base Half  , Float ) instance Rounding '(Half  , Float )
deriving via '(Base Half  , Double) instance Rounding '(Half  , Double)
deriving via '(Base Float , Half  ) instance Rounding '(Float , Half  )
deriving via '(Base Float , Float ) instance Rounding '(Float , Float )
deriving via '(Base Float , Double) instance Rounding '(Float , Double)
deriving via '(Base Double, Half  ) instance Rounding '(Double, Half  )
deriving via '(Base Double, Float ) instance Rounding '(Double, Float )
deriving via '(Base Double, Double) instance Rounding '(Double, Double)
