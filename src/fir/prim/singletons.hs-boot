{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module FIR.Prim.Singletons where

-- base
import Data.Kind
  ( Type )
import Data.Typeable
  ( Typeable )
import GHC.TypeLits
  ( Symbol, KnownSymbol )

-- fir
import Data.Binary.Class.Put
  ( Put )
import Data.Function.Variadic
  ( ListVariadic )
import Data.Type.Map
  ( (:->)((:->)) )
import qualified SPIRV.PrimTy   as SPIRV
import qualified SPIRV.ScalarTy as SPIRV

------------------------------------------------------------
-- singletons for primitive types

data SScalarTy :: Type -> Type where
type role SScalarTy nominal

data SPrimTy :: Type -> Type where
type role SPrimTy nominal

class ( Show ty
      , Eq ty, Ord ty, Typeable ty
      , ty ~ ListVariadic '[] ty
      )
    => PrimTy ty where
  primTySing :: SPrimTy ty

class ( PrimTy ty
      , Put ty
      )
    => ScalarTy ty where
  scalarTySing :: SScalarTy ty

scalarTy :: forall ty. ScalarTy ty => SPIRV.ScalarTy
primTy   :: forall ty. PrimTy   ty => SPIRV.PrimTy

data SPrimTyMap :: [Symbol :-> Type] -> Type where
  SNil  :: SPrimTyMap '[]
  SCons :: (KnownSymbol k, PrimTy a, PrimTyMap as)
        => SPrimTyMap ((k ':-> a) ': as)
type role SPrimTyMap nominal

class PrimTyMap as where
  primTyMapSing :: SPrimTyMap as

instance PrimTyMap '[] where
instance (KnownSymbol k, PrimTy a, PrimTyMap as)
       => PrimTyMap ((k ':-> a) ': as) where
