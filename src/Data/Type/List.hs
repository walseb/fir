{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Type.List
  ( type (:++:)
  , Elem
  , Tail, MapTail
  , MapSingleton
  , Zip, ExactZip, ZipCons
  , Replicate
  , Length
  , SLength(SZero, SSucc)
  , KnownLength(sLength), sLengthVal
  , SameLength(sSameLength)
  , SSameLength(SSameZero,SSameSucc)
  , Postpend
    -- re-exports
  , Snoc, Reverse
  ) where

-- base 
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat, type (+), type (-) )

-- fir
import Data.Type.Snoc -- for re-export
  ( Snoc, Reverse )

------------------------------------------------
-- functionality to deal with type-level lists

infixr 6 :++:

type family (:++:) (as :: [k]) (bs :: [k]) where
  '[]       :++: bs = bs
  (a ': as) :++: bs = a ': ( as :++: bs )

infix 4 `Elem`

type family Elem (x :: k) (as :: [k]) where
  Elem _ '[]       = 'False
  Elem x (x ': _ ) = 'True
  Elem x (_ ': as) = Elem x as

type family Tail (x :: [k]) :: [k] where
  Tail (x ': xs) = xs

type family MapTail (x :: [[k]]) :: [[k]] where
  MapTail '[] = '[]
  MapTail (xs ': xss) = Tail xs ': MapTail xss

type family MapSingleton (as :: [k]) :: [[k]] where
  MapSingleton '[] = '[]
  MapSingleton (a ': as) = (a ': '[]) ': MapSingleton as

type family Zip (is :: [k]) (js :: [k]) :: [k] where
  Zip '[] js = js
  Zip is '[] = is
  Zip (i ': is) (j ': js) = (i,j) ': Zip is js

type family ExactZip (msg :: ErrorMessage) (as :: [k]) (bs :: [k]) :: [k] where
  ExactZip _  '[]        '[]       = '[]
  ExactZip msg (a ': as) (b ': bs) = (a,b) ': ExactZip msg as bs
  ExactZip msg _ _ = TypeError msg

type family ZipCons (as :: [k]) (bss :: [[k]]) = (r :: [[k]]) | r -> as bss where
  ZipCons '[] '[] = '[]
  ZipCons (a ': as) (bs ': bss) = (a ': bs) ': ZipCons as bss

type family Replicate (n :: Nat) (a :: k) :: [k] where
  Replicate 0 _ = '[]
  Replicate n a = a ': Replicate (n-1) a

type family Length (as :: [k]) :: Nat where
  Length '[]       = 0
  Length (_ ': as) = 1 + Length as

data SLength (is :: [k]) where
  SZero :: SLength '[]
  SSucc :: SLength is -> SLength (i ': is)

class KnownLength (is :: [k]) where
  sLength :: SLength is

instance KnownLength '[] where
  sLength = SZero

instance KnownLength is => KnownLength (i ': is) where
  sLength = SSucc ( sLength @_ @is )

sLengthVal :: SLength is -> Int
sLengthVal SZero     = 0
sLengthVal (SSucc i) = 1 + sLengthVal i

data SSameLength (is :: [k]) (js :: [l]) where
  SSameZero :: SSameLength '[] '[]
  SSameSucc :: SSameLength is js -> SSameLength (i ': is) (j ': js)

class SameLength (is :: [k]) (js :: [l]) where
  sSameLength :: SSameLength is js
instance SameLength '[] '[] where
  sSameLength = SSameZero
instance SameLength is js => SameLength (i ': is) (j ': js) where
  sSameLength = SSameSucc ( sSameLength @_ @_ @is @js )

infixl 4 `Postpend`
-- Non-injective type family for post-pending.
--
-- Prefer using 'Data.Type.Snoc.Snoc' to retain injectivity.
--
-- (Note that this sometimes gives GHC trouble with constraint solving...)
type family Postpend (as :: [k]) (b :: k) :: [k] where
  Postpend '[]       b = '[ b ]
  Postpend (a ': as) b = a ': Postpend as b
