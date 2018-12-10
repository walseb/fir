{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RoleAnnotations        #-}

module FIR.Prim.Singletons where

-- base
import Data.Kind(Type)
import Data.Typeable(Typeable)

-- fir
import Data.Function.Variadic(ListVariadic)

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
