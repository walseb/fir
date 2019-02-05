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

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

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
  type Demote Symbol = Text
instance KnownSymbol k => Known Symbol k where
  known = Text.pack (symbolVal @k Proxy)

instance Demotable Nat where
  type Demote Nat = Word32
instance KnownNat n => Known Nat n where
  known = fromIntegral (natVal @n Proxy)

instance Demotable () where
  type Demote () = ()

instance Known () '() where
  known = ()

instance (Demotable k, Demotable l) => Demotable (k,l) where
  type Demote (k,l) = (Demote k, Demote l)
instance (Known k a, Known l b) => Known (k,l) '(a,b) where
  known = ( known @k @a, known @l @b )

instance Demotable k => Demotable (Maybe k) where
  type Demote (Maybe k) = Maybe (Demote k)
instance Demotable k => Known (Maybe k) 'Nothing where
  known = Nothing
instance Known k a => Known (Maybe k) ('Just a) where
  known = Just ( known @k @a )

instance Demotable k => Demotable [k] where
  type Demote [k] = [Demote k]
instance Demotable k => Known [k] '[] where
  known = []
instance ( Known k a, Known [k] as ) => Known [k] (a ': as) where
  known = known @k @a : known @[k] @as
