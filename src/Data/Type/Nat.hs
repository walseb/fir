{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Nat where

-- base
import GHC.TypeNats
  ( Nat, Mod, Log2
  , type (+), type (-), type (^)
  )

--------------------------------------------------------------------------

type family RoundUp (n :: Nat) (r :: Nat) :: Nat where
  RoundUp n r
    = ( n + r ) - ( 1 + ( (n + r - 1) `Mod` r ) )

type family NextPositivePowerOf2 (n :: Nat) :: Nat where
  NextPositivePowerOf2 0 = 1
  NextPositivePowerOf2 n = NextPowerOf2 n

type family NextPowerOf2 (n :: Nat) :: Nat where
  NextPowerOf2 0 = 0
  NextPowerOf2 1 = 1
  NextPowerOf2 n = 2 ^ ( Log2 (n-1) + 1 ) -- Log2 x is actually Floor ( Log2 x )
