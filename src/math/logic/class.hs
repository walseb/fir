{-# LANGUAGE AllowAmbiguousTypes    #-}
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
{-# LANGUAGE UndecidableInstances   #-}

module Math.Logic.Class where

-- base
import Prelude( Bool(..)
              , Int, Word, Float, Double
              , Semigroup, Monoid
              , Foldable(..)
              , flip
              )
import qualified Prelude
import Data.Coerce(Coercible, coerce)
import Data.Int(Int8,Int16,Int32,Int64)
import Data.Kind(Type)
import Data.Word(Word8,Word16,Word32,Word64)

-- half
import Numeric.Half(Half)

-- fir
import Deriving.Prelude(Prelude(..)) -- newtype for deriving Prelude instances


infixr 3 &&
infixr 2 ||

class Boolean b where
  true  :: b
  false :: b
  (&&)  :: b -> b -> b
  (||)  :: b -> b -> b
  not   :: b -> b

instance Boolean Bool where
  true  = True
  false = False
  (&&)  = (Prelude.&&)
  (||)  = (Prelude.||)
  not   = Prelude.not

infix 4 ==
infix 4 /=

type family Choosing b (t :: (Type,Type,Type)) = r | r -> b t where
  Choosing b '(x,y,z) = b -> x -> y -> z

class Boolean b => Choose b (t :: (Type,Type,Type)) where
  choose :: Choosing b t
  ifThenElse :: Choosing b t
  ifThenElse = choose

type Triple a = '(a,a,a)

instance Choose Bool (Triple a) where
  choose True  x _ = x
  choose False _ y = y

class Boolean (Logic a) => Eq a where
  type Logic a
  (==) :: a -> a -> Logic a
  (/=) :: a -> a -> Logic a
  a /= b = not (a == b)

instance Prelude.Eq a => Eq (Prelude a) where
  type Logic (Prelude a) = Bool
  (==) = coerce ( (Prelude.==) :: a -> a -> Bool )
  (/=) = coerce ( (Prelude./=) :: a -> a -> Bool )

deriving via Prelude ()     instance Eq ()
deriving via Prelude Bool   instance Eq Bool
deriving via Prelude Word8  instance Eq Word8
deriving via Prelude Word16 instance Eq Word16
deriving via Prelude Word32 instance Eq Word32
deriving via Prelude Word64 instance Eq Word64
deriving via Prelude Word   instance Eq Word
deriving via Prelude Int8   instance Eq Int8
deriving via Prelude Int16  instance Eq Int16
deriving via Prelude Int32  instance Eq Int32
deriving via Prelude Int64  instance Eq Int64
deriving via Prelude Int    instance Eq Int
deriving via Prelude Half   instance Eq Half
deriving via Prelude Float  instance Eq Float
deriving via Prelude Double instance Eq Double


newtype All b = All { getAll :: b }
newtype Any b = Any { getAny :: b }

instance Boolean b => Semigroup (All b) where
  (<>) = coerce ( (&&) @b )
instance Boolean b => Monoid (All b) where
  mempty = coerce ( true @b )

instance Boolean b => Semigroup (Any b) where
  (<>) = coerce ( (||) @b )
instance Boolean b => Monoid (Any b) where
  mempty = coerce (false @b)

-- from Data.Functor.Utils
{-# INLINE (#.) #-}
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce

and :: (Foldable t, Boolean b) => t b -> b
and = getAll #. foldMap All

or :: (Foldable t, Boolean b) => t b -> b
or = getAny #. foldMap Any

any :: (Foldable t, Boolean b) => (a -> b) -> t a -> b
any p = getAny #. foldMap (Any #. p)

all :: (Foldable t, Boolean b) => (a -> b) -> t a -> b
all p = getAll #. foldMap (All #. p)

infix 4 <=
infix 4 <
infix 4 >=
infix 4 >

class Eq a => Ord a where
  type Ordering a
  compare :: a -> a -> Ordering a
  (<=)    :: a -> a -> Logic a
  (>=)    :: a -> a -> Logic a
  (<)     :: a -> a -> Logic a
  (>)     :: a -> a -> Logic a
  max     :: a -> a -> a
  min     :: a -> a -> a

  (>=) = flip (>=)
  (>)  = flip (<)


instance Prelude.Ord a => Ord (Prelude a) where
  type Ordering (Prelude a) = Prelude.Ordering
  compare = coerce ( Prelude.compare :: a -> a -> Prelude.Ordering )
  (<=)    = coerce ( (Prelude.<=)    :: a -> a -> Bool )
  (>=)    = coerce ( (Prelude.>=)    :: a -> a -> Bool )
  (<)     = coerce ( (Prelude.<)     :: a -> a -> Bool )
  (>)     = coerce ( (Prelude.>)     :: a -> a -> Bool )
  max     = coerce ( Prelude.max     :: a -> a -> a )
  min     = coerce ( Prelude.min     :: a -> a -> a )

deriving via Prelude ()     instance Ord ()
deriving via Prelude Bool   instance Ord Bool
deriving via Prelude Word8  instance Ord Word8
deriving via Prelude Word16 instance Ord Word16
deriving via Prelude Word32 instance Ord Word32
deriving via Prelude Word64 instance Ord Word64
deriving via Prelude Word   instance Ord Word
deriving via Prelude Int8   instance Ord Int8
deriving via Prelude Int16  instance Ord Int16
deriving via Prelude Int32  instance Ord Int32
deriving via Prelude Int64  instance Ord Int64
deriving via Prelude Int    instance Ord Int
deriving via Prelude Half   instance Ord Half
deriving via Prelude Float  instance Ord Float
deriving via Prelude Double instance Ord Double
