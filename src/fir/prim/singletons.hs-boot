{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeOperators     #-}

module FIR.Prim.Singletons where

-- base
import Data.Kind(Type)
import Data.Proxy(Proxy)
import Data.Typeable(Typeable)
import GHC.TypeLits(Symbol, KnownSymbol)

-- fir
import Data.Function.Variadic(ListVariadic)
import Data.Type.Map((:->)((:->)))

------------------------------------------------------------
-- singletons for primitive types

data SPrimTy :: Type -> Type where
type role SPrimTy nominal

class ( Show ty
      , Eq ty, Ord ty, Typeable ty
      , ty ~ ListVariadic '[] ty
      )
    => PrimTy ty where
  primTySing :: SPrimTy ty

data SPrimTys :: [Symbol :-> Type] -> Type where
  SNil  :: SPrimTys '[]
  SCons :: (KnownSymbol k, PrimTy a, PrimTys as)
        => Proxy k
        -> SPrimTy a
        -> SPrimTys as
        -> SPrimTys ((k ':-> a) ': as)
type role SPrimTys nominal

class PrimTys as where
  primTys :: SPrimTys as

instance PrimTys '[] where
instance (KnownSymbol k, PrimTy a, PrimTys as)
       => PrimTys ((k ':-> a) ': as) where
