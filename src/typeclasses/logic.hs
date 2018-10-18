{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module TypeClasses.Logic where

-- base
import Prelude hiding( Eq(..), (&&), (||), not, and, or, all, any
                     , Ord(..)
                     )
import qualified Prelude
import Data.Coerce(Coercible, coerce)


class Boolean b where
  true  :: b
  false :: b
  (&&)  :: b -> b -> b
  (||)  :: b -> b -> b
  not   :: b -> b

class Boolean b => HasBool b a where
  bool :: b -> a -> a -> a

instance Boolean Bool where
  true  = True
  false = False
  (&&)  = (Prelude.&&)
  (||)  = (Prelude.||)
  not   = Prelude.not

instance HasBool Bool a where
  bool True  x _ = x
  bool False _ y = y

class (HasBool (Logic a) a) => Eq a where
  type Logic a
  (==) :: a -> a -> Logic a
  (/=) :: a -> a -> Logic a
  a /= b = not (a == b)

instance Eq Float where
  type Logic Float = Bool
  (==) = (Prelude.==)
  (/=) = (Prelude./=)
instance Eq Double where
  type Logic Double = Bool
  (==) = (Prelude.==)
  (/=) = (Prelude./=)
instance Eq Int where
  type Logic Int = Bool
  (==) = (Prelude.==)
  (/=) = (Prelude./=)
instance Eq Bool where
  type Logic Bool = Bool
  (==) = (Prelude.==)
  (/=) = (Prelude./=)
instance Eq Word where
  type Logic Word = Bool
  (==) = (Prelude.==)
  (/=) = (Prelude./=)

ifThenElse :: HasBool b a => b -> a -> a -> a
ifThenElse = bool

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

class Eq a => Ord a where
  type Compare a
  compare :: a -> a -> Compare a
  (<=)    :: a -> a -> Logic a
  (>=)    :: a -> a -> Logic a
  (<)     :: a -> a -> Logic a
  (>)     :: a -> a -> Logic a
  max     :: a -> a -> a
  min     :: a -> a -> a

instance Ord Int where
  type Compare Int = Prelude.Ordering
  compare = Prelude.compare
  (<=) = (Prelude.<=)
  (>=) = (Prelude.>=)
  (<)  = (Prelude.<)
  (>)  = (Prelude.>)
  min  = Prelude.min
  max  = Prelude.max
instance Ord Float where
  type Compare Float = Prelude.Ordering
  compare = Prelude.compare
  (<=) = (Prelude.<=)
  (>=) = (Prelude.>=)
  (<)  = (Prelude.<)
  (>)  = (Prelude.>)
  min  = Prelude.min
  max  = Prelude.max
instance Ord Double where
  type Compare Double = Prelude.Ordering
  compare = Prelude.compare
  (<=) = (Prelude.<=)
  (>=) = (Prelude.>=)
  (<)  = (Prelude.<)
  (>)  = (Prelude.>)
  min  = Prelude.min
  max  = Prelude.max
instance Ord Word where
  type Compare Word = Prelude.Ordering
  compare = Prelude.compare
  (<=) = (Prelude.<=)
  (>=) = (Prelude.>=)
  (<)  = (Prelude.<)
  (>)  = (Prelude.>)
  min  = Prelude.min
  max  = Prelude.max
instance Ord Bool where
  type Compare Bool = Prelude.Ordering
  compare = Prelude.compare
  (<=) = (Prelude.<=)
  (>=) = (Prelude.>=)
  (<)  = (Prelude.<)
  (>)  = (Prelude.>)
  min  = Prelude.min
  max  = Prelude.max
