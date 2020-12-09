{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
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
  ( Array(..)
  , RuntimeArray(..)
  )
  where

-- base
import Data.Coerce
  ( coerce )
import Data.Kind
  ( Type )
import GHC.TypeNats
  ( Nat, KnownNat, type (+) )

-- vector
import qualified Data.Vector as Unsized
  ( Vector )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( Vector, (++) )

-- fir
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..) )

------------------------------------------------------------
-- arrays

newtype Array :: Nat -> Type -> Type where
  MkArray :: V.Vector n a -> Array n a

deriving stock instance Eq   a => Eq   (Array n a)
deriving stock instance Ord  a => Ord  (Array n a)
deriving stock instance Show a => Show (Array n a)
deriving via (V.Vector n) instance Functor     (Array n)
deriving via (V.Vector n) instance Foldable    (Array n)
deriving stock            instance Traversable (Array n)
deriving via (V.Vector n) instance KnownNat n => Applicative (Array n)

instance GradedSemigroup (Array 0 a) Nat where
  type Grade Nat (Array 0 a) l = Array l a
  type l1 :<!>: l2 = l1 + l2
  (<!>) = coerce (V.++)

newtype RuntimeArray (a :: Type) = MkRuntimeArray (Unsized.Vector a)

deriving via Unsized.Vector a instance Eq   a => Eq   (RuntimeArray a)
deriving via Unsized.Vector a instance Ord  a => Ord  (RuntimeArray a)
deriving via Unsized.Vector a instance Show a => Show (RuntimeArray a)
deriving via Unsized.Vector   instance Functor     RuntimeArray
deriving via Unsized.Vector   instance Foldable    RuntimeArray
deriving stock                instance Traversable RuntimeArray
