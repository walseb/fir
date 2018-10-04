{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module TypeClasses.Category where

-- base
import Data.Coerce(Coercible, coerce)
import Data.Kind(Type, Constraint)
import Data.Monoid(Sum(Sum,getSum))
import GHC.TypeLits(Symbol, Nat, KnownNat, TypeError, ErrorMessage(Text))
import Prelude hiding(id, (.), Functor(..), Applicative(..), Foldable(..), Traversable(..))
import qualified Prelude
import qualified Data.Foldable as Foldable

-- fir
import TypeClasses.Equality((#.))

data Void where

class Category (c :: obj -> obj -> Type) where
  id  :: c i i
  (.) :: c j k -> c i j -> c i k

instance Category (->) where
  id  = Prelude.id
  (.) = (Prelude..)

data Side
  = Internal
  | External

data V a n
data AST a
type Lol n a (k :: Symbol) = AST (V a n)



class Functor f where
  type FunctorApp f a = res | res -> f a
  fmap :: (a -> b) -> (FunctorApp f a -> FunctorApp f b)

instance Prelude.Functor f => Functor f where
  type FunctorApp f a = f a
  fmap :: (a -> b) -> (f a -> f b)
  fmap = Prelude.fmap

instance Functor (Lol n () k) where
  type FunctorApp (Lol n () k) a = Lol n a k
  fmap = error "error"


infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> (FunctorApp f a -> FunctorApp f b)
(<$>) = fmap

class Functor f => Applicative f where
  pure :: a -> FunctorApp f a
  (<*>) :: FunctorApp f (a -> b) -> (FunctorApp f a -> FunctorApp f b)


instance Prelude.Applicative f => Applicative f where
  pure = Prelude.pure
  (<*>) = (Prelude.<*>)

class Functor f => Foldable f where
  fold :: Monoid m => FunctorApp f m -> m
  fold = foldMap id

  foldMap :: Monoid m => (a -> m) -> FunctorApp f a -> m

instance (Prelude.Functor f, Foldable.Foldable f) => Foldable f where
  fold    = Foldable.fold
  foldMap = Foldable.foldMap

sum :: (Foldable f, Num a) => FunctorApp f a -> a
sum = getSum #. foldMap Sum

class Foldable f => Traversable f where
  traverse :: Prelude.Applicative g 
           => (a -> g b) -> FunctorApp f a -> g (FunctorApp f b)

instance (Prelude.Traversable f) => Traversable f where
  traverse = Prelude.traverse