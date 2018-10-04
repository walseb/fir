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
import GHC.TypeLits(Symbol)

-- fir
import Bindings(Binding)

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
  WithIx :: a -> (a := i) i

atKey :: (a := j) i -> a
atKey (WithIx a) = a
  
instance Show a => Show ( (a := i) j ) where
  show (WithIx a) = "WithIx " ++ show a

------------------------------------------------ 
-- codensity indexed monad

-- demonic codensity
data Codensity ast m a i
  = Codensity { runCodensity :: forall (b :: k -> Type). ( (forall (j :: k). a j -> ast (m b j) ) -> ast (m b i) ) }

instance FunctorIx (Codensity ast m) where
  fmapIx :: ( forall ix.                   p ix ->                   q ix ) 
         -> ( forall ix. (Codensity ast m) p ix -> (Codensity ast m) q ix )
  fmapIx f (Codensity m) = Codensity ( \k -> m ( k . f ) )

instance MonadIx (Codensity ast m) where
  returnIx :: p i -> Codensity ast m p i
  returnIx a = Codensity ( \k -> k a )

  extendIx :: ( forall ix.                   p ix -> (Codensity ast m) q ix )
           -> ( forall ix. (Codensity ast m) p ix -> (Codensity ast m) q ix )
  extendIx f (Codensity ma) = Codensity ( \k -> ma ( \a -> runCodensity (f a) k ) )