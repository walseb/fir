{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

{-|
Module: Data.Type.Known

This module provides a simple type class for reification (turning type-level data into value-level data).

This allows programs to perform type-level computations (e.g. to perform validation at compile time),
and at the end turn this type-level data back into value-level data to be used e.g. by the code generator.

This is a one way process: these are not singletons. See "FIR.Prim.Singletons" for the singletons this library uses internally.

Note that some instances are tailored to this library: type-level literals 'Symbol' are turned into 'ShortText',
and type-level natural numbers 'Nat' are turned into 'Word32'.
-}


module Data.Type.Known where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol, KnownSymbol, symbolVal )
import GHC.TypeNats
  ( Nat, KnownNat, natVal )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText

-- fir
import Data.Type.Map
  ( (:->)((:->)) )

--------------------------------------------------

class Demotable k where
  type Demote k = (d :: Type) | d -> k

class Demotable k => Known (k :: Type) (a :: k) where
  known :: Demote k

-- make the kind parameter invisible
type KindOf (a :: k) = k
knownValue :: forall a. Known (KindOf a) a => Demote (KindOf a)
knownValue = known @(KindOf a) @a

instance Demotable Symbol where
  type Demote Symbol = ShortText
instance KnownSymbol k => Known Symbol k where
  known = ShortText.pack (symbolVal @k Proxy)

instance Demotable Nat where
  type Demote Nat = Word32
instance KnownNat n => Known Nat n where
  known = fromIntegral (natVal @n Proxy)

instance Demotable Bool where
  type Demote Bool = Bool

instance Known Bool 'False where
  known = False
instance Known Bool 'True where
  known = True

instance Demotable k => Demotable (Maybe k) where
  type Demote (Maybe k) = Maybe (Demote k)
instance Demotable k => Known (Maybe k) 'Nothing where
  known = Nothing
instance Known k a => Known (Maybe k) ('Just a) where
  known = Just ( known @k @a )

instance Demotable () where
  type Demote () = ()

instance Known () '() where
  known = ()

instance (Demotable k, Demotable l) => Demotable (k,l) where
  type Demote (k,l) = (Demote k, Demote l)
instance (Known k a, Known l b) => Known (k,l) '(a,b) where
  known = ( known @k @a, known @l @b )

instance (Demotable k, Demotable l, Demotable m) => Demotable (k,l,m) where
  type Demote (k,l,m) = (Demote k, Demote l, Demote m)
instance (Known k a, Known l b, Known m c) => Known (k,l,m) '(a,b,c) where
  known = ( known @k @a, known @l @b, known @m @c )

instance Demotable k => Demotable [k] where
  type Demote [k] = [Demote k]
instance Demotable k => Known [k] '[] where
  known = []
instance ( Known k a, Known [k] as ) => Known [k] (a ': as) where
  known = known @k @a : known @[k] @as

instance ( Demotable k, Demotable v ) => Demotable (k :-> v) where
  type Demote (k :-> v) = Demote k :-> Demote v
instance ( Known k a, Known v b ) => Known (k :-> v) (a ':-> b) where
  known = known @k @a :-> known @v @b
