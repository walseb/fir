{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Math.Algebra.Class where

-- base
import Prelude( id, (.)
              , Bool
              , Integer, Rational
              , Word, Int
              , Float, Double
              )
import qualified Prelude
import Data.Coerce(coerce)
import Data.Int(Int8, Int16, Int32, Int64)
import Data.Kind(Type)
import qualified Data.Fixed as Fixed
import Data.Word(Word8, Word16, Word32, Word64)
import qualified GHC.Float as Float

-- half
import Numeric.Half(Half)
import qualified Numeric.Half as Half

-- fir
import Deriving.Prelude(Prelude(..)) -- newtype for deriving Prelude instances
import Math.Logic.Class(ifThenElse, Eq(Logic,(==)), Ord)



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
deriving via a instance Eq            a => Eq            (Fixed a)
deriving via a instance Ord           a => Ord           (Fixed a)
deriving via a instance AdditiveGroup a => AdditiveGroup (Fixed a)
deriving via a instance Semiring      a => Semiring      (Fixed a)
deriving via a instance Ring          a => Ring          (Fixed a)
deriving via a instance Signed        a => Signed        (Fixed a)

instance ( Eq a, Logic a ~ Bool
         , Ord a
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



type family Arr (domCod :: (Type,Type)) = (arr :: Type) | arr -> domCod where
  Arr '(dom, cod) = dom -> cod

class Convert (c :: (Type,Type)) where
  convert :: Arr c

instance Convert '(a,a) where
  convert = id


-- fromIntegral conversions
instance (Prelude.Integral a, Prelude.Num b) => Convert '((Prelude a), b) where
  convert (Prelude a) = Prelude.fromIntegral a

-- to unsigned integers
deriving via '(Prelude Word16, Word8 ) instance Convert '(Word16, Word8 )
deriving via '(Prelude Word32, Word8 ) instance Convert '(Word32, Word8 )
deriving via '(Prelude Word64, Word8 ) instance Convert '(Word64, Word8 )
deriving via '(Prelude Word  , Word8 ) instance Convert '(Word  , Word8 )
deriving via '(Prelude Int8  , Word8 ) instance Convert '(Int8  , Word8 )
deriving via '(Prelude Int16 , Word8 ) instance Convert '(Int16 , Word8 )
deriving via '(Prelude Int32 , Word8 ) instance Convert '(Int32 , Word8 )
deriving via '(Prelude Int64 , Word8 ) instance Convert '(Int64 , Word8 )
deriving via '(Prelude Int   , Word8 ) instance Convert '(Int   , Word8 )
deriving via '(Prelude Word8 , Word16) instance Convert '(Word8 , Word16)
deriving via '(Prelude Word32, Word16) instance Convert '(Word32, Word16)
deriving via '(Prelude Word64, Word16) instance Convert '(Word64, Word16)
deriving via '(Prelude Word  , Word16) instance Convert '(Word  , Word16)
deriving via '(Prelude Int8  , Word16) instance Convert '(Int8  , Word16)
deriving via '(Prelude Int16 , Word16) instance Convert '(Int16 , Word16)
deriving via '(Prelude Int32 , Word16) instance Convert '(Int32 , Word16)
deriving via '(Prelude Int64 , Word16) instance Convert '(Int64 , Word16)
deriving via '(Prelude Int   , Word16) instance Convert '(Int   , Word16)
deriving via '(Prelude Word8 , Word32) instance Convert '(Word8 , Word32)
deriving via '(Prelude Word16, Word32) instance Convert '(Word16, Word32)
deriving via '(Prelude Word64, Word32) instance Convert '(Word64, Word32)
deriving via '(Prelude Word  , Word32) instance Convert '(Word  , Word32)
deriving via '(Prelude Int8  , Word32) instance Convert '(Int8  , Word32)
deriving via '(Prelude Int16 , Word32) instance Convert '(Int16 , Word32)
deriving via '(Prelude Int32 , Word32) instance Convert '(Int32 , Word32)
deriving via '(Prelude Int64 , Word32) instance Convert '(Int64 , Word32)
deriving via '(Prelude Int   , Word32) instance Convert '(Int   , Word32)
deriving via '(Prelude Word8 , Word64) instance Convert '(Word8 , Word64)
deriving via '(Prelude Word16, Word64) instance Convert '(Word16, Word64)
deriving via '(Prelude Word32, Word64) instance Convert '(Word32, Word64)
deriving via '(Prelude Word  , Word64) instance Convert '(Word  , Word64)
deriving via '(Prelude Int8  , Word64) instance Convert '(Int8  , Word64)
deriving via '(Prelude Int16 , Word64) instance Convert '(Int16 , Word64)
deriving via '(Prelude Int32 , Word64) instance Convert '(Int32 , Word64)
deriving via '(Prelude Int64 , Word64) instance Convert '(Int64 , Word64)
deriving via '(Prelude Int   , Word64) instance Convert '(Int   , Word64)
deriving via '(Prelude Word8 , Word  ) instance Convert '(Word8 , Word  )
deriving via '(Prelude Word16, Word  ) instance Convert '(Word16, Word  )
deriving via '(Prelude Word32, Word  ) instance Convert '(Word32, Word  )
deriving via '(Prelude Word64, Word  ) instance Convert '(Word64, Word  )
deriving via '(Prelude Int8  , Word  ) instance Convert '(Int8  , Word  )
deriving via '(Prelude Int16 , Word  ) instance Convert '(Int16 , Word  )
deriving via '(Prelude Int32 , Word  ) instance Convert '(Int32 , Word  )
deriving via '(Prelude Int64 , Word  ) instance Convert '(Int64 , Word  )
deriving via '(Prelude Int   , Word  ) instance Convert '(Int   , Word  )
-- to signed integers
deriving via '(Prelude Word8 , Int8  ) instance Convert '(Word8 , Int8  )
deriving via '(Prelude Word16, Int8  ) instance Convert '(Word16, Int8  )
deriving via '(Prelude Word32, Int8  ) instance Convert '(Word32, Int8  )
deriving via '(Prelude Word64, Int8  ) instance Convert '(Word64, Int8  )
deriving via '(Prelude Word  , Int8  ) instance Convert '(Word  , Int8  )
deriving via '(Prelude Int16 , Int8  ) instance Convert '(Int16 , Int8  )
deriving via '(Prelude Int32 , Int8  ) instance Convert '(Int32 , Int8  )
deriving via '(Prelude Int64 , Int8  ) instance Convert '(Int64 , Int8  )
deriving via '(Prelude Int   , Int8  ) instance Convert '(Int   , Int8  )
deriving via '(Prelude Word8 , Int16 ) instance Convert '(Word8 , Int16 )
deriving via '(Prelude Word16, Int16 ) instance Convert '(Word16, Int16 )
deriving via '(Prelude Word32, Int16 ) instance Convert '(Word32, Int16 )
deriving via '(Prelude Word64, Int16 ) instance Convert '(Word64, Int16 )
deriving via '(Prelude Word  , Int16 ) instance Convert '(Word  , Int16 )
deriving via '(Prelude Int8  , Int16 ) instance Convert '(Int8  , Int16 )
deriving via '(Prelude Int32 , Int16 ) instance Convert '(Int32 , Int16 )
deriving via '(Prelude Int64 , Int16 ) instance Convert '(Int64 , Int16 )
deriving via '(Prelude Int   , Int16 ) instance Convert '(Int   , Int16 )
deriving via '(Prelude Word8 , Int32 ) instance Convert '(Word8 , Int32 )
deriving via '(Prelude Word16, Int32 ) instance Convert '(Word16, Int32 )
deriving via '(Prelude Word32, Int32 ) instance Convert '(Word32, Int32 )
deriving via '(Prelude Word64, Int32 ) instance Convert '(Word64, Int32 )
deriving via '(Prelude Word  , Int32 ) instance Convert '(Word  , Int32 )
deriving via '(Prelude Int8  , Int32 ) instance Convert '(Int8  , Int32 )
deriving via '(Prelude Int16 , Int32 ) instance Convert '(Int16 , Int32 )
deriving via '(Prelude Int64 , Int32 ) instance Convert '(Int64 , Int32 )
deriving via '(Prelude Int   , Int32 ) instance Convert '(Int   , Int32 )
deriving via '(Prelude Word8 , Int64 ) instance Convert '(Word8 , Int64 )
deriving via '(Prelude Word16, Int64 ) instance Convert '(Word16, Int64 )
deriving via '(Prelude Word32, Int64 ) instance Convert '(Word32, Int64 )
deriving via '(Prelude Word64, Int64 ) instance Convert '(Word64, Int64 )
deriving via '(Prelude Word  , Int64 ) instance Convert '(Word  , Int64 )
deriving via '(Prelude Int8  , Int64 ) instance Convert '(Int8  , Int64 )
deriving via '(Prelude Int16 , Int64 ) instance Convert '(Int16 , Int64 )
deriving via '(Prelude Int32 , Int64 ) instance Convert '(Int32 , Int64 )
deriving via '(Prelude Int   , Int64 ) instance Convert '(Int   , Int64 )
deriving via '(Prelude Word8 , Int   ) instance Convert '(Word8 , Int   )
deriving via '(Prelude Word16, Int   ) instance Convert '(Word16, Int   )
deriving via '(Prelude Word32, Int   ) instance Convert '(Word32, Int   )
deriving via '(Prelude Word64, Int   ) instance Convert '(Word64, Int   )
deriving via '(Prelude Word  , Int   ) instance Convert '(Word  , Int   )
deriving via '(Prelude Int8  , Int   ) instance Convert '(Int8  , Int   )
deriving via '(Prelude Int16 , Int   ) instance Convert '(Int16 , Int   )
deriving via '(Prelude Int32 , Int   ) instance Convert '(Int32 , Int   )
deriving via '(Prelude Int64 , Int   ) instance Convert '(Int64 , Int   )
-- to floating point
deriving via '(Prelude Word8 , Half  ) instance Convert '(Word8 , Half  )
deriving via '(Prelude Word16, Half  ) instance Convert '(Word16, Half  )
deriving via '(Prelude Word32, Half  ) instance Convert '(Word32, Half  )
deriving via '(Prelude Word64, Half  ) instance Convert '(Word64, Half  )
deriving via '(Prelude Word  , Half  ) instance Convert '(Word  , Half  )
deriving via '(Prelude Int8  , Half  ) instance Convert '(Int8  , Half  )
deriving via '(Prelude Int16 , Half  ) instance Convert '(Int16 , Half  )
deriving via '(Prelude Int32 , Half  ) instance Convert '(Int32 , Half  )
deriving via '(Prelude Int64 , Half  ) instance Convert '(Int64 , Half  )
deriving via '(Prelude Word8 , Float ) instance Convert '(Word8 , Float )
deriving via '(Prelude Word16, Float ) instance Convert '(Word16, Float )
deriving via '(Prelude Word32, Float ) instance Convert '(Word32, Float )
deriving via '(Prelude Word64, Float ) instance Convert '(Word64, Float )
deriving via '(Prelude Word  , Float ) instance Convert '(Word  , Float )
deriving via '(Prelude Int8  , Float ) instance Convert '(Int8  , Float )
deriving via '(Prelude Int16 , Float ) instance Convert '(Int16 , Float )
deriving via '(Prelude Int32 , Float ) instance Convert '(Int32 , Float )
deriving via '(Prelude Int64 , Float ) instance Convert '(Int64 , Float )
deriving via '(Prelude Word8 , Double) instance Convert '(Word8 , Double)
deriving via '(Prelude Word16, Double) instance Convert '(Word16, Double)
deriving via '(Prelude Word32, Double) instance Convert '(Word32, Double)
deriving via '(Prelude Word64, Double) instance Convert '(Word64, Double)
deriving via '(Prelude Word  , Double) instance Convert '(Word  , Double)
deriving via '(Prelude Int8  , Double) instance Convert '(Int8  , Double)
deriving via '(Prelude Int16 , Double) instance Convert '(Int16 , Double)
deriving via '(Prelude Int32 , Double) instance Convert '(Int32 , Double)
deriving via '(Prelude Int64 , Double) instance Convert '(Int64 , Double)


-- truncation conversions
instance (Prelude.RealFrac a, Prelude.Integral b) => Convert '(a, Prelude b) where
  convert = Prelude . Prelude.truncate

-- to unsigned integers
deriving via '(Half  , Prelude Word8 ) instance Convert '(Half  , Word8 )
deriving via '(Half  , Prelude Word16) instance Convert '(Half  , Word16)
deriving via '(Half  , Prelude Word32) instance Convert '(Half  , Word32)
deriving via '(Half  , Prelude Word64) instance Convert '(Half  , Word64)
deriving via '(Half  , Prelude Word  ) instance Convert '(Half  , Word  )
deriving via '(Float , Prelude Word8 ) instance Convert '(Float , Word8 )
deriving via '(Float , Prelude Word16) instance Convert '(Float , Word16)
deriving via '(Float , Prelude Word32) instance Convert '(Float , Word32)
deriving via '(Float , Prelude Word64) instance Convert '(Float , Word64)
deriving via '(Float , Prelude Word  ) instance Convert '(Float , Word  )
deriving via '(Double, Prelude Word8 ) instance Convert '(Double, Word8 )
deriving via '(Double, Prelude Word16) instance Convert '(Double, Word16)
deriving via '(Double, Prelude Word32) instance Convert '(Double, Word32)
deriving via '(Double, Prelude Word64) instance Convert '(Double, Word64)
deriving via '(Double, Prelude Word  ) instance Convert '(Double, Word  )
-- to signed integers
deriving via '(Half  , Prelude Int8  ) instance Convert '(Half  , Int8  )
deriving via '(Half  , Prelude Int16 ) instance Convert '(Half  , Int16 )
deriving via '(Half  , Prelude Int32 ) instance Convert '(Half  , Int32 )
deriving via '(Half  , Prelude Int64 ) instance Convert '(Half  , Int64 )
deriving via '(Half  , Prelude Int   ) instance Convert '(Half  , Int   )
deriving via '(Float , Prelude Int8  ) instance Convert '(Float , Int8  )
deriving via '(Float , Prelude Int16 ) instance Convert '(Float , Int16 )
deriving via '(Float , Prelude Int32 ) instance Convert '(Float , Int32 )
deriving via '(Float , Prelude Int64 ) instance Convert '(Float , Int64 )
deriving via '(Float , Prelude Int   ) instance Convert '(Float , Int   )
deriving via '(Double, Prelude Int8  ) instance Convert '(Double, Int8  )
deriving via '(Double, Prelude Int16 ) instance Convert '(Double, Int16 )
deriving via '(Double, Prelude Int32 ) instance Convert '(Double, Int32 )
deriving via '(Double, Prelude Int64 ) instance Convert '(Double, Int64 )
deriving via '(Double, Prelude Int   ) instance Convert '(Double, Int   )


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