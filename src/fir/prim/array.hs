{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
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

module FIR.Prim.Array where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import GHC.TypeNats
  ( Nat, KnownNat, natVal, type (+) )
import Unsafe.Coerce
  ( unsafeCoerce )

-- vector
import qualified Data.Vector as Array

-- fir
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GeneratedGradedSemigroup(..)
  , FreeGradedSemigroup(..)
  )

------------------------------------------------------------
-- arrays

data Array :: Nat -> Type -> Type where
  MkArray :: forall n a. KnownNat n => Array.Vector a -> Array n a

mkArray :: forall n a. KnownNat n => Array.Vector a -> Array n a
mkArray arr
  = let n = fromIntegral (natVal (Proxy @n))
    in MkArray @n (Array.slice 0 n arr)


deriving instance Eq   a => Eq   (Array l a)
deriving instance Ord  a => Ord  (Array l a)
deriving instance Show a => Show (Array l a)
deriving instance Functor     (Array n)
deriving instance Foldable    (Array n)
deriving instance Traversable (Array n)
instance KnownNat n => Applicative (Array n) where
  pure = MkArray @n . Array.replicate (fromIntegral . natVal $ Proxy @n)
  (MkArray f) <*> (MkArray a) = MkArray @n (f <*> a)

instance GradedSemigroup (Array 0 a) Nat where
  type Grade Nat (Array 0 a) l = Array l a
  type l1 :<!>: l2 = l1 + l2
  (<!>) :: forall l1 l2. Array l1 a -> Array l2 a -> Array (l1+l2) a
  MkArray v1 <!> MkArray v2 = MkArray @(l1+l2) (v1 Array.++ v2)

instance GeneratedGradedSemigroup (Array 0 a) Nat () where
  type GenType    (Array 0 a) ()  _  = a
  type GenDeg Nat (Array 0 a) () '() = 1
  generator :: a -> Array (GenDeg Nat (Array 0 a) () unit) a
  generator a = unsafeCoerce (MkArray @1 (Array.singleton a))

instance FreeGradedSemigroup (Array 0 a) Nat () where
  type ValidDegree (Array 0 a) n = KnownNat n
  (>!<) :: forall n m. (KnownNat n, KnownNat m) => Array (n+m) a -> (Array n a, Array m a)
  (>!<) (MkArray arr)
    = let tk, dp :: Array.Vector a
          (tk, dp) = Array.splitAt (fromIntegral (natVal (Proxy @n))) arr
      in (MkArray @n @a tk, MkArray @m @a dp)
  generated :: Array (GenDeg Nat (Array 0 a) () unit) a -> a
  generated (MkArray arr) = Array.head arr

newtype RuntimeArray a = MkRuntimeArray (Array.Vector a)

deriving instance Eq   a => Eq   (RuntimeArray a)
deriving instance Ord  a => Ord  (RuntimeArray a)
deriving instance Show a => Show (RuntimeArray a)
deriving instance Functor     RuntimeArray
deriving instance Foldable    RuntimeArray
deriving instance Traversable RuntimeArray

instance GradedSemigroup (RuntimeArray a) () where
  type Grade () (RuntimeArray a) '() = RuntimeArray a
  type l1 :<!>: l2 = '()
  (<!>) :: Grade () (RuntimeArray a) unit1
        -> Grade () (RuntimeArray a) unit2
        -> Grade () (RuntimeArray a) unit3
  arr1 <!> arr2 = unsafeCoerce (MkRuntimeArray (v1 Array.++ v2))
    where v1, v2 :: Array.Vector a
          MkRuntimeArray v1 = unsafeCoerce arr1
          MkRuntimeArray v2 = unsafeCoerce arr2
          
instance GeneratedGradedSemigroup (RuntimeArray a) () () where
  type GenType   (RuntimeArray a) () _ = a
  type GenDeg () (RuntimeArray a) () '() = '()
  generator :: a -> Grade () (RuntimeArray a) (GenDeg () (RuntimeArray a) () unit)
  generator a = unsafeCoerce ( MkRuntimeArray (Array.singleton a) )


-- no injective graded semigroup instance for run-time arrays,
-- because knowing rtarr = rtarr1 ++ rtarr2
-- does not allow us to recover arr1 or arr2 from arr
-- (as we do not know the relevant slice index)
