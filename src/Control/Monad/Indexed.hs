{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module: Control.Monad.Indexed

Indexed functors and monads.

This module includes two versions, as described in Conor McBride's paper
/Kleisli Arrows of Outrageous Fortune/.

McBride-style indexed monads are usually written with an @ix@ /suffix/,
whereas AtKey-style indexed monads are usually written with an @ix@ /prefix/.

-}

module Control.Monad.Indexed
  ( -- * McBride-style indexed monads
    FunctorIx(fmapIx), MonadIx(returnIx, extendIx), MonadIxFail(fail)
    -- ** Identity indexed monad
  , Id(Id, runId)
    -- ** Codensity transformation
  , Codensity(Codensity, runCodensity)

    -- * Atkey-style indexed monads
    -- ** Representing Atkey indexed monads using the /At Key/ trick
  , (:=)(AtKey), atKey, withKey
    -- ** Atkey functor operations
  , ixFmap, (<<$>>), (<<$), ($>>), (<<&>>)
    -- ** Atkey applicative operations
  , IxApplicative((<<*>>))
  , (*>>), (<<*), ixLiftA2, ixLiftA3, ixLiftA4

    -- * Rebindable syntax for @do@ notation
  , (>>=), (=<<), (>>), (<<), return, pure, ixPure,

  )
  where

-- base
import Data.Kind
  ( Type )
import Prelude
  hiding ( Functor(..), Applicative(..), Monad(..), (=<<) )

------------------------------------------------
-- indexed monads (à la McBride)

class FunctorIx ( f :: (k -> Type) -> (k -> Type) ) where
  fmapIx :: ( forall ix.   p ix ->   q ix )
         -> ( forall ix. f p ix -> f q ix )

class FunctorIx m => MonadIx m where
  returnIx :: p ix -> m p ix
  -- | Demonic bind.
  extendIx :: ( forall ix.   p ix -> m q ix )
           -> ( forall ix. m p ix -> m q ix )

class MonadIx m => MonadIxFail m where
  fail :: String -> m (a := j) i

------------------------------------------------------------
-- identity indexed monad

newtype Id p i = Id { runId :: p i }

instance FunctorIx Id where
  fmapIx f (Id a) = Id (f a)

instance MonadIx Id where
  extendIx f (Id a) = f a
  returnIx = Id

------------------------------------------------------------
-- codensity transformation

-- | Demonic codensity transformation.
--
-- For any @(f :: Type -> Type)@,
-- @Codensity f@ is guaranteed to be an indexed monad.
-- 
-- This is especially useful for manipulating ASTs.
newtype Codensity f p i
  = Codensity
    { runCodensity :: forall (q :: k -> Type)
    . ( forall (j :: k). p j -> f (q j) ) -> f (q i)
    }

instance FunctorIx (Codensity f) where
  fmapIx :: ( forall ix.             p ix ->             q ix )
         -> ( forall ix. Codensity f p ix -> Codensity f q ix )
  fmapIx f (Codensity m) = Codensity ( \k -> m ( k . f ) )

instance MonadIx (Codensity f) where
  returnIx :: p i -> Codensity f p i
  returnIx a = Codensity ( \k -> k a )

  extendIx :: ( forall ix.             p ix -> Codensity f q ix )
           -> ( forall ix. Codensity f p ix -> Codensity f q ix )
  extendIx f (Codensity ma) = Codensity ( \k -> ma ( \a -> runCodensity (f a) k ) )

------------------------------------------------
-- Atkey indexing

-- | The /At Key/ datatype allows us to express Atkey-style indexed monads
-- in terms of McBride-style indexed monads.
data (:=) :: Type -> i -> (i -> Type) where
  AtKey :: a -> (a := i) i

atKey :: (a := j) i -> a
atKey (AtKey a) = a
  
instance Show a => Show ( (a := i) j ) where
  show (AtKey a) = "AtKey " ++ show a

withKey :: (a -> b) -> (a := i) j -> (b := i) j
withKey f (AtKey a) = AtKey (f a)

------------------------------------------------------------
-- useful operators & functions for AtKey-style indexing

ixFmap :: FunctorIx m => (a -> b) -> m (a := j) i -> m (b := j) i
ixFmap f = fmapIx ( withKey f )

infixl 4 <<$>>
infixl 4 <<$
infixl 4 $>>
infixl 1 <<&>>

(<<$>>) :: FunctorIx f => (a -> b) -> f (a := j) i -> f (b := j) i
(<<$>>) = ixFmap
(<<$) :: FunctorIx f => a -> f (b := j) i -> f (a := j) i
(<<$) = ixFmap . const
($>>) :: FunctorIx f => f (a := j) i -> b -> f (b := j) i
($>>) = flip (<<$)
(<<&>>) :: FunctorIx f => f (a := j) i -> (a -> b) -> f (b := j) i
(<<&>>) = flip ixFmap

infixl 4 <<*>>
class FunctorIx f => IxApplicative f where
  (<<*>>) :: f ((a -> b) := j) i -> (f (a := k) j -> f (b := k) i)

instance (FunctorIx m, MonadIx m) => IxApplicative m where
  mf <<*>> ma = mf >>= (<<$>> ma)

infixl 4 *>>
infixl 4 <<*

(*>>) :: IxApplicative f => f (a := j) i -> f (b := k) j -> f (b := k) i
fa *>> fb = (id <<$ fa) <<*>> fb
(<<*) :: IxApplicative f => f (a := j) i -> f (b := k) j -> f (a := k) i
fa <<* fb = (const <<$>> fa) <<*>> fb

ixLiftA2 :: IxApplicative f
       => (a -> b -> c)
       -> f (a := j) i -> f (b := k) j -> f (c := k) i
ixLiftA2 f a b = f <<$>> a <<*>> b

ixLiftA3 :: IxApplicative f
         => (a -> b -> c -> d)
         -> f (a := j) i -> f (b := k) j -> f (c := l) k -> f (d := l) i
ixLiftA3 f a b c = f <<$>> a <<*>> b <<*>> c

ixLiftA4 :: IxApplicative f
         => (a -> b -> c -> d -> e)
         -> f (a := j) i -> f (b := k) j -> f (c := l) k -> f (d := m) l -> f (e := m) i
ixLiftA4 f a b c d = f <<$>> a <<*>> b <<*>> c <<*>> d

------------------------------------------------
-- rebindable syntax

-- | Angelic bind.
(>>=) :: MonadIx m => m (a := j) i -> (a -> m q j) -> m q i
c >>= f = extendIx ( \ (AtKey a) -> f a ) c  

(>>) :: MonadIx m => m (a := j) i -> m q j -> m q i
ma >> mb = ma >>= const mb

(<<) :: MonadIx m => m q j -> m (a := j) i -> m q i
(<<) = flip (>>)

(=<<) :: MonadIx m => (a -> m q j) -> m (a := j) i -> m q i
(=<<) = flip (>>=)

return, pure, ixPure :: MonadIx m => a -> m (a := i) i
return = returnIx . AtKey
pure   = returnIx . AtKey
ixPure = returnIx . AtKey