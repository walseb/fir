{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Prim.Array

This module defines the array type used in this library,
which simply consists in a wrapper around a 'Data.Vector.Vector'.

There are two kinds of arrays:

  * arrays, affixed with a type-level natural number denoting their length,
  * runtime arrays, whose length is not statically known.

The user cannot create new runtime arrays within a SPIR-V module
(although this is not currently enforced by this library),
as they are only meant to be used as inputs within shader storage buffer objects,
as the last object within a 'FIR.Synonyms.StorageBuffer'.
-}

module FIR.Prim.Array
  ( Array(..), mkArray
  , RuntimeArray(..)
  )
  where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import GHC.TypeNats
  ( Nat, KnownNat, natVal, type (+) )

-- vector
import qualified Data.Vector as Vector
  ( Vector, slice, replicate, (++) )

-- fir
import {-# SOURCE #-} FIR.AST
  ( Syntactic(..) )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..) )

------------------------------------------------------------
-- arrays

data Array :: Nat -> Type -> Type where
  MkArray :: forall n a. KnownNat n => Vector.Vector a -> Array n a

mkArray :: forall n a. KnownNat n => Vector.Vector a -> Array n a
mkArray arr
  = let n = fromIntegral (natVal (Proxy @n))
    in MkArray @n (Vector.slice 0 n arr)


deriving stock instance Eq   a => Eq   (Array l a)
deriving stock instance Ord  a => Ord  (Array l a)
deriving stock instance Show a => Show (Array l a)
deriving stock instance Functor     (Array n)
deriving stock instance Foldable    (Array n)
deriving stock instance Traversable (Array n)
instance KnownNat n => Applicative (Array n) where
  pure = MkArray . Vector.replicate (fromIntegral . natVal $ Proxy @n)
  (MkArray f) <*> (MkArray a) = MkArray (f <*> a)

instance Syntactic a => Syntactic (Array n a) where
  type Internal (Array n a) = Array n (Internal a)
  toAST   = error "'toAST': cannot internalise array (todo)"
  fromAST = error "'fromAST': cannot externalise array (todo)"

instance GradedSemigroup (Array 0 a) Nat where
  type Grade Nat (Array 0 a) l = Array l a
  type l1 :<!>: l2 = l1 + l2
  (<!>) :: forall l1 l2. Array l1 a -> Array l2 a -> Array (l1+l2) a
  MkArray v1 <!> MkArray v2 = MkArray (v1 Vector.++ v2)

newtype RuntimeArray a = MkRuntimeArray (Vector.Vector a)

deriving stock instance Eq   a => Eq   (RuntimeArray a)
deriving stock instance Ord  a => Ord  (RuntimeArray a)
deriving stock instance Show a => Show (RuntimeArray a)
deriving stock instance Functor     RuntimeArray
deriving stock instance Foldable    RuntimeArray
deriving stock instance Traversable RuntimeArray
