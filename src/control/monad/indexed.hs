{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Indexed where

-- base
import Data.Kind(Type)
import Prelude hiding ( Applicative(..), Monad(..) )

------------------------------------------------
-- indexed monads (à la Conor McBride)

class FunctorIx ( f :: (k -> Type) -> (k -> Type) ) where
  fmapIx :: ( forall ix.   p ix ->   q ix )
         -> ( forall ix. f p ix -> f q ix )

class FunctorIx m => MonadIx m where
  returnIx :: p ix -> m p ix
  extendIx :: ( forall ix.   p ix -> m q ix )
           -> ( forall ix. m p ix -> m q ix )

------------------------------------------------
-- Atkey indexing

data (:=) :: Type -> i -> (i -> Type) where
  AtKey :: a -> (a := i) i

atKey :: (a := j) i -> a
atKey (AtKey a) = a
  
instance Show a => Show ( (a := i) j ) where
  show (AtKey a) = "AtKey " ++ show a

withKey :: (a -> b) -> (a := i) j -> (b := i) j
withKey f (AtKey a) = AtKey (f a)

------------------------------------------------
-- rebindable syntax

-- angelic bind
(>>=) :: MonadIx m => m (a := j) i -> (a -> m q j) -> m q i
c >>= f = extendIx ( \ (AtKey a) -> f a ) c  

(>>) :: MonadIx m => m (a := j) i -> m q j -> m q i
ma >> mb = ma >>= const mb

return, pure :: MonadIx m => a -> m (a := i) i
return = returnIx . AtKey
pure   = returnIx . AtKey

class MonadIx m => MonadIxFail m where
  fail :: String -> m (a := j) i