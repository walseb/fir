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

-- fir
import Data.Binary.Class.Put
  ( Put )
import Data.Function.Variadic
  ( ListVariadic )
import Data.Type.Known
  ( Known )
import Data.Type.Map
  ( (:->)((:->)) )
import {-# SOURCE #-} FIR.Prim.Struct
  ( StructFieldKind )
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

data SPrimTyMap :: [fld :-> Type] -> Type where
  SNil  :: SPrimTyMap '[]
  SCons :: (StructFieldKind fld, Known fld k, PrimTy a, PrimTyMap as)
        => SPrimTyMap ((k ':-> a) ': as)
type role SPrimTyMap nominal

class StructFieldKind fld => PrimTyMap (as :: [fld :-> Type]) where
  primTyMapSing :: SPrimTyMap as

instance StructFieldKind fld => PrimTyMap ('[] :: [fld :-> Type]) where

instance forall ( fld :: Type           )
                ( k   :: fld            )
                ( a   :: Type           )
                ( as  :: [fld :-> Type] )
      . ( StructFieldKind fld
        , Known fld k
        , PrimTy a
        , PrimTyMap as
        )
       => PrimTyMap ((k ':-> a) ': as) where
