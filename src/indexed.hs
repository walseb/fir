{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Indexed where

-- base
import Data.Kind(Type)

-- fir
import AST(AST)

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
-- codensity indexed monad

-- demonic codensity
newtype Codensity m a i
  = Codensity
    { runCodensity :: forall (b :: k -> Type)
    . (forall (j :: k). a j -> AST (m b j) ) -> AST (m b i)
    }

instance FunctorIx (Codensity m) where
  fmapIx :: ( forall ix.               p ix ->               q ix )
         -> ( forall ix. (Codensity m) p ix -> (Codensity m) q ix )
  fmapIx f (Codensity m) = Codensity ( \k -> m ( k . f ) )

instance MonadIx (Codensity m) where
  returnIx :: p i -> Codensity m p i
  returnIx a = Codensity ( \k -> k a )

  extendIx :: ( forall ix.               p ix -> (Codensity m) q ix )
           -> ( forall ix. (Codensity m) p ix -> (Codensity m) q ix )
  extendIx f (Codensity ma) = Codensity ( \k -> ma ( \a -> runCodensity (f a) k ) )