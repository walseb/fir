{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Type.List
  ( type (:++:)
  , Elem
  , Zip, ExactZip
  , Length
  , SLength(SZero, SSucc)
  , KnownLength(sLength), sLengthVal
  , Postpend
    -- re-exports
  , Snoc, Reverse
  ) where

-- base 
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat, type (+) )

-- fir
import Data.Type.Snoc -- for re-export
  ( Snoc, Reverse )

------------------------------------------------
-- functionality to deal with type-level lists

type family (:++:) (as :: [k]) (bs :: [k]) where
  '[]       :++: bs = bs
  (a ': as) :++: bs = a ': ( as :++: bs )

type family Elem x as where
  Elem _ '[]       = 'False
  Elem x (x ': _ ) = 'True
  Elem x (_ ': as) = Elem x as

type family Zip (is :: [k]) (js :: [k]) :: [k] where
  Zip '[] js = js
  Zip is '[] = is
  Zip (i ': is) (j ': js) = (i,j) ': Zip is js

type family ExactZip (msg :: ErrorMessage) (as :: [k]) (bs :: [k]) :: [k] where
  ExactZip _  '[]        '[]       = '[]
  ExactZip msg (a ': as) (b ': bs) = (a,b) ': ExactZip msg as bs
  ExactZip msg _ _ = TypeError msg

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


infixl 4 `Postpend`
-- Non-injective type family for post-pending.
--
-- Prefer using 'Data.Type.Snoc.Snoc' to retain injectivity.
--
-- (Note that this sometimes gives GHC trouble with constraint solving...)
type family Postpend (as :: [k]) (b :: k) :: [k] where
  Postpend '[]       b = '[ b ]
  Postpend (a ': as) b = a ': Postpend as b
