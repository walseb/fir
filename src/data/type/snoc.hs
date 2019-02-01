{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Type.Snoc where

-- type-level hacks for injective 'Snoc' and 'Reverse'
-- (c) Artem Chirkin

type Snoc (ns :: [k]) (n :: k) = GetSnoc (DoSnoc ns n)
type Reverse (xs :: [k]) = Reversed (DoReverse xs)

data Snocing k = SSingle k | Snocing [k]

type family DoSnoc (xs :: [k]) (z::k) = (ys :: Snocing k) | ys -> xs z where
    DoSnoc '[]       x               = 'SSingle x
    DoSnoc (x ': xs :: [k]) (y :: k) = ('Snocing (x ': GetSnoc (DoSnoc xs y) :: [k]) :: Snocing k)

type family GetSnoc (xs :: Snocing k) = (ys :: [k]) | ys -> xs where
    GetSnoc ('SSingle x)              = '[x]
    GetSnoc ('Snocing (y ': x ': xs)) = y ': x ': xs

data Reversing k = REmpty | Reversing [k]

type family Reversed (ts :: Reversing k) = (rs :: [k]) | rs -> ts where
    Reversed 'REmpty                = '[]
    Reversed ('Reversing (x ': xs)) = x ': xs

type family DoReverse (as :: [k]) = (rs :: Reversing k) | rs -> as where
    DoReverse '[]       = 'REmpty
    DoReverse (a ': as) = 'Reversing (Reversed (DoReverse as) `Snoc` a)
