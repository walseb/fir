{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module TypeClasses.Equality where

-- base
import Prelude hiding(Eq(..), (&&), (||), not, and, or, all, any)
import qualified Prelude as P
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
  (&&)  = (P.&&)
  (||)  = (P.||)
  not   = P.not

instance HasBool Bool a where
  bool True  x _ = x
  bool False _ y = y

class (HasBool (Logic a) a) => Eq a where
  type Logic a
  (==) :: a -> a -> Logic a

instance Eq Float where
  type Logic Float = Bool
  (==) = (P.==)

instance Eq Int where
  type Logic Int = Bool
  (==) = (P.==)

instance Eq Bool where
 type Logic Bool = Bool
 (==) = (P.==)

instance Eq Word where
  type Logic Word = Bool
  (==) = (P.==) 

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