
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module FIR.Syntax.Case where

-- base
import Control.Arrow
  ( (&&&) )
import Data.Coerce
  ( coerce )
import Data.Kind
  ( Type )
import Data.List
  ( sort )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat, KnownNat, natVal )


------------------------------------------------
-- demotion of type-level data

class Demotable k where
  type Demote k :: Type

class Demotable k => Known (k :: Type) (a :: k) where
  known :: Demote k

type KindOf (a :: k) = k
knownValue :: forall a. Known (KindOf a) a => Demote (KindOf a)
knownValue = known @(KindOf a) @a

instance Demotable Nat where
  type Demote Nat = Word32
instance KnownNat n => Known Nat n where
  known = fromIntegral (natVal @n Proxy)

------------------------------------------------
-- case alternatives

newtype Case (is :: [k]) = UnsafeCase { scrutiniseCase :: Demote k }

here :: forall i is. Known (KindOf i) i => Case (i ': is)
here = UnsafeCase ( knownValue @i )

there :: forall x is. Case is -> Case (x ': is)
there (UnsafeCase i) = UnsafeCase i

class HasCase (i :: k) (is :: [k]) where
  injCase :: Case is
instance {-# OVERLAPPING #-}
  ( Known k i
  , TypeError
     ( Text "Duplicate case in pattern: Case " :<>: ShowType i :<>: Text "." )
  ) => HasCase i (i ': is) where
  injCase = here @i @is
instance ( HasCase i is )
      => HasCase i (x ': is) where
  injCase = there @x ( injCase @_ @i @is )
instance {-# INCOHERENT #-}
         ( Known k i, xs ~ (i ': ys) )
      => HasCase i xs        where
  injCase = here @i @ys

class KnownCases is where
  casesVal :: [Case is]
instance KnownCases '[] where
  casesVal = []
instance (Known k i, KnownCases is) => KnownCases (i ': is) where
  casesVal = here : map there ( casesVal @is )
instance {-# INCOHERENT #-} KnownCases is where
  casesVal = []

switch :: forall t k (is :: [k])
       .  ( Ord (Demote k), Enum (Demote k), Bounded (Demote k) )
       => ( Case is -> t )
       -> KnownCases is -- this constraint needs to be passed here so that 'is' can be correctly inferred beforehand
       => ( [ (Demote k, t) ], t )
switch f = ( switchCases &&& defaultCase ) ( casesVal @is )
  where
    switchCases :: [ Case is ] -> [ (Demote k, t) ]
    switchCases = map ( scrutiniseCase &&& f )
    defaultCase :: [ Case is ] -> t
    defaultCase = f . coerce (new @(Demote k))

new :: forall x. (Ord x, Enum x, Bounded x) => [x] -> x
new = firstGap minBound . sort
    where
      firstGap :: x -> [x] -> x
      firstGap a [] = a
      firstGap a (x:xs)
        | a < x     = a
        | otherwise = firstGap (succ x) xs

------------------------------------------------
-- testing

pattern NatCase :: Demote Nat -> Case (is :: [Nat])
pattern NatCase i = UnsafeCase i

pattern Case0 :: HasCase 0 is => Case is
pattern Case0 = NatCase 0

pattern Case1 :: HasCase 1 is => Case is
pattern Case1 = NatCase 1

pattern Case3 :: HasCase 3 is => Case is
pattern Case3 = NatCase 3

pattern Case7 :: HasCase 7 is => Case is
pattern Case7 = NatCase 7

test1 = \case
  Case7 -> "a"
  Case1 -> "b"
  Case3 -> "c"
  Case1 -> "d" -- incorrect (no warning)
  _     -> "e" -- correct (does warn if removed)

-- > :t test1
-- test1 :: Case (7 : 1 : 3 : ys) -> String

-- > switch test1
-- ([(7,"a"),(1,"b"),(3,"c")], "e")
