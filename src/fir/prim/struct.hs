{-# LANGUAGE DataKinds              #-}
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

module FIR.Prim.Struct where

-- base
import Control.Applicative(liftA2)
import Control.Arrow(first)
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import GHC.TypeLits(Symbol, KnownSymbol)
import Unsafe.Coerce(unsafeCoerce)

-- lens
import Control.Lens.Iso(Iso', iso)

-- fir
import Data.Type.Map((:->)((:->)), type (:++:))
import Math.Algebra.GradedSemigroup( GradedSemigroup(..)
                                   , GradedPresentedSemigroup(..)
                                   , GradedFreeSemigroup(..)
                                   )
import {-# SOURCE #-} FIR.Prim.Singletons(PrimTy)

------------------------------------------------------------
-- structs

infixr 4 :&
-- order *matters* for structs (memory layout!)
data Struct :: [Symbol :-> Type] -> Type where
  End  :: Struct '[]
  (:&) :: forall k a as. (KnownSymbol k, PrimTy a)
       => a -> Struct as -> Struct ((k ':-> a) ': as)

deriving instance Eq   (Struct as)
deriving instance Ord  (Struct as)
deriving instance Show (Struct as)

instance GradedSemigroup Struct [Symbol :-> Type] where
  type Apply [Symbol :-> Type] Struct as = Struct as
  type as :<!>: bs = as :++: bs
  (<!>) :: Struct as -> Struct bs -> Struct (as :++: bs)
  End <!> t = t
  (a :& s) <!> t = a :& ( s <!> t )

data KeyWithValue (kv :: (Symbol :-> Type)) where
  KeyValue :: (KnownSymbol k, PrimTy a) => a -> KeyWithValue (k ':-> a)


instance GradedPresentedSemigroup
            Struct
            [Symbol :-> Type]
            (Symbol :-> Type)
            where
  type Element                  Struct (Symbol :-> Type) kv = KeyWithValue kv
  type Degree [Symbol :-> Type] Struct (Symbol :-> Type) kv = '[ kv ]    
  homogeneous
    :: forall (kv :: (Symbol :-> Type)).
       Iso' (Struct '[ kv ]) (KeyWithValue kv)
  homogeneous
    = iso
        ( \(a :& End) -> KeyValue a )
        ( \(KeyValue a) -> a :& End )

data SLength :: [k] -> Type where
  SNil  :: SLength '[]
  SCons :: KnownLength as => Proxy as -> SLength (a ': as)

class KnownLength (as :: [k]) where
  lengthVal :: Proxy as -> SLength as

instance KnownLength '[] where
  lengthVal _ = SNil
instance KnownLength as => KnownLength (a ': as) where
  lengthVal _ = SCons Proxy

asProxyStructTypeOf :: Struct as -> Proxy as -> Struct as
asProxyStructTypeOf s _ = s

instance GradedFreeSemigroup
            Struct
            [Symbol :-> Type]
            (Symbol :-> Type)
            where
  type ValidDegree Struct as = KnownLength as
  (>!<) :: forall as bs. (KnownLength as, KnownLength bs)
        => Struct (as :++: bs) -> ( Struct as, Struct bs )
  (>!<) End = unsafeCoerce ( End, End ) -- GHC cannot deduce (as ~ '[], bs ~ '[]) from (as :++: bs) ~ '[]
  (>!<) (s :& ss)
    = case lengthVal (Proxy @as) of
        SNil -> ( End, s :& ss )
        SCons tail_as
          ->  let
                r :: Struct bs
                -- workaround to specify the type of l
                (l,r) = first (`asProxyStructTypeOf` tail_as)
                          ( (>!<) @_ @_ @_ @_ @bs ss)
              in ( s :& l, r )

foldrStruct
  :: forall as b.
  (forall a. PrimTy a => a -> b -> b)
  -> b -> Struct as -> b
foldrStruct f b = go @as
  where go :: forall xs. Struct xs -> b
        go End       = b
        go (a :& as) = f a (go as)

traverseStruct :: Applicative f =>
  ( forall a. PrimTy a => a -> f b )
  -> Struct as
  -> f [b]
traverseStruct f
   = foldrStruct ( \a bs -> liftA2 (:) (f a) bs )
       ( pure [] )