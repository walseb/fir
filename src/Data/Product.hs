{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: Data.Product

This module uses heterogeneous lists to define unbiased products needed by the type-level optics of this library.
-}


module Data.Product where

-- base
import Data.Kind
  ( Type )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Data.Constraint.All
  ( All )
import Data.Type.List
  ( KnownLength
  , ZipCons
  , SameLength
  )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( PrimTy )

----------------------------------------------------------------------

infixr 3 :>

data HList (as :: [Type]) where
  HNil :: HList '[]
  (:>) :: a -> HList as -> HList (a ': as)

instance Show (HList '[]) where
  show _ = "HNil"
instance ( Show a, Show (HList as) ) => Show (HList (a ': as)) where
  show (a :> as) = show a ++ " :> " ++ show as

class KnownLength as => IsProduct (p :: Type) (as :: [Type]) {---  | p -> as  ---}
  where
  fromHList :: HList as -> p
  toHList   :: p -> HList as

instance KnownLength as => IsProduct (HList as) as where
  fromHList = id
  toHList   = id

-- TODO: refactor this using 'Data.Constraint.All'
data AreProductsDict js iss as where
  NilProducts  :: AreProductsDict '[] '[] as
  ConsProducts
    :: ( IsProduct j is
       , All PrimTy is
       , AreProducts js iss as
       , SameLength is (Distribute iss as)
       )
     => AreProductsDict (j ': js) (is ': iss) as

class SameLength js iss 
   => AreProducts (js :: [Type]) (iss :: [[Type]]) (as :: [Type])  {---  | js -> iss  ---}
   where
  productsDict :: AreProductsDict js iss as
instance AreProducts '[] '[] as where
  productsDict = NilProducts
instance (IsProduct j is, AreProducts js iss as, SameLength is (Distribute iss as), All PrimTy is)
      => AreProducts (j ': js) (is ': iss) as where
  productsDict = ConsProducts

type family MapHList (iss :: [[Type]]) = (as :: [Type]) | as -> iss where
  MapHList '[]         = '[]
  MapHList (is ': iss) = HList is ': MapHList iss

type family Distribute (iss :: [[Type]]) (as :: [Type]) = (jss :: [[Type]]) where
  Distribute '[]         '[]       = '[]
  Distribute '[]         (a ': as) = '[] ': Distribute '[] as
  Distribute (is ': iss) as        = ZipCons is (Distribute iss as)

distributeZipConsLemma1
  :: forall (a :: Type) (as :: [Type]) (ds :: [Type]) (dss :: [[Type]]) (es :: [Type]) (ess :: [[Type]])
  .  ( SameLength es ess
     , (ds ': dss) ~ Distribute (ZipCons es ess) (a ': as)
     )
  => ( ds :~: es, dss :~: Distribute ess as )
distributeZipConsLemma1 = ( unsafeCoerce Refl, unsafeCoerce Refl )

distributeZipConsLemma2
  :: forall (as :: [Type]) (is :: [Type]) (iss :: [[Type]])
  .  ( SameLength is (Distribute iss as) )
  => ( ZipCons is (Distribute iss as) :~: Distribute (is ': iss) as )
distributeZipConsLemma2 = unsafeCoerce Refl

instance IsProduct a '[a] where
  toHList x = x :> HNil
  fromHList (x :> HNil) = x

instance IsProduct (a1,a2) '[a1,a2] where
  toHList (x1,x2) = (x1 :> x2 :> HNil)
  fromHList (x1 :> x2 :> HNil) = (x1,x2)

instance IsProduct (a1,a2,a3) '[a1,a2,a3] where
  toHList (x1,x2,x3) = (x1 :> x2 :> x3 :> HNil)
  fromHList (x1 :> x2 :> x3 :> HNil) = (x1,x2,x3)

instance IsProduct (a1,a2,a3,a4) '[a1,a2,a3,a4] where
  toHList (x1,x2,x3,x4) = (x1 :> x2 :> x3 :> x4 :> HNil)
  fromHList (x1 :> x2 :> x3 :> x4 :> HNil) = (x1,x2,x3,x4)

instance IsProduct (a1,a2,a3,a4,a5) '[a1,a2,a3,a4,a5] where
  toHList (x1,x2,x3,x4,x5) = (x1 :> x2 :> x3 :> x4 :> x5 :> HNil)
  fromHList (x1 :> x2 :> x3 :> x4 :> x5 :> HNil) = (x1,x2,x3,x4,x5)

instance IsProduct (a1,a2,a3,a4,a5,a6) '[a1,a2,a3,a4,a5,a6] where
  toHList (x1,x2,x3,x4,x5,x6) = (x1 :> x2 :> x3 :> x4 :> x5 :> x6 :> HNil)
  fromHList (x1 :> x2 :> x3 :> x4 :> x5 :> x6 :> HNil) = (x1,x2,x3,x4,x5,x6)
 
instance IsProduct (a1,a2,a3,a4,a5,a6,a7) '[a1,a2,a3,a4,a5,a6,a7] where
  toHList (x1,x2,x3,x4,x5,x6,x7) = (x1 :> x2 :> x3 :> x4 :> x5 :> x6 :> x7 :> HNil)
  fromHList (x1 :> x2 :> x3 :> x4 :> x5 :> x6 :> x7 :> HNil) = (x1,x2,x3,x4,x5,x6,x7)
 
instance IsProduct (a1,a2,a3,a4,a5,a6,a7,a8) '[a1,a2,a3,a4,a5,a6,a7,a8] where
  toHList (x1,x2,x3,x4,x5,x6,x7,x8) = (x1 :> x2 :> x3 :> x4 :> x5 :> x6 :> x7 :> x8 :> HNil)
  fromHList (x1 :> x2 :> x3 :> x4 :> x5 :> x6 :> x7 :> x8 :> HNil) = (x1,x2,x3,x4,x5,x6,x7,x8)
