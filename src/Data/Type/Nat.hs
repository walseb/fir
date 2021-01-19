{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: Data.Type.Maybe

This module defines some arithmetic operations on type-level natural numbers.
-}

module Data.Type.Nat where

-- base
import GHC.TypeNats
  ( Nat, Mod, Log2
  , type (+), type (-), type (^)
  )

--------------------------------------------------------------------------

type RoundUp n r
  = ( n + r ) - ( 1 + ( (n + r - 1) `Mod` r ) )

type family NextPositivePowerOf2 (n :: Nat) :: Nat where
  NextPositivePowerOf2 0 = 1
  NextPositivePowerOf2 n = NextPowerOf2 n

type family NextPowerOf2 (n :: Nat) :: Nat where
  NextPowerOf2 0 = 0
  NextPowerOf2 1 = 1
  NextPowerOf2 n = 2 ^ ( Log2 (n-1) + 1 ) -- Log2 x is actually Floor ( Log2 x )

type family Succ ( n :: Nat ) = ( m :: Nat ) | m -> n where
  Succ  0 =  1
  Succ  1 =  2
  Succ  2 =  3
  Succ  3 =  4
  Succ  4 =  5
  Succ  5 =  6
  Succ  6 =  7
  Succ  7 =  8
  Succ  8 =  9
  Succ  9 = 10
  Succ 10 = 11
  Succ 11 = 12
  Succ 12 = 13
  Succ 13 = 14
  Succ 14 = 15
  Succ 15 = 16
  Succ 16 = 17
  Succ 17 = 18
  Succ 18 = 19
  Succ 19 = 20
  Succ 20 = 21
  Succ 21 = 22
  Succ 22 = 23
  Succ 23 = 24
  Succ 24 = 25
  Succ 25 = 26
  Succ 26 = 27
  Succ 27 = 28
  Succ 28 = 29
  Succ 29 = 30
  Succ 30 = 31
  Succ 31 = 32
  Succ 32 = 33
  Succ 33 = 34
  Succ 34 = 35
  Succ 35 = 36
  Succ 36 = 37
  Succ 37 = 38
  Succ 38 = 39
  Succ 39 = 40
  Succ 40 = 41
  Succ 41 = 42
  Succ 42 = 43
  Succ 43 = 44
  Succ 44 = 45
  Succ 45 = 46
  Succ 46 = 47
  Succ 47 = 48
  Succ 48 = 49
  Succ 49 = 50
  Succ 50 = 51
  Succ 51 = 52
  Succ 52 = 53
  Succ 53 = 54
  Succ 54 = 55
  Succ 55 = 56
  Succ 56 = 57
  Succ 57 = 58
  Succ 58 = 59
  Succ 59 = 60
  Succ 60 = 61
  Succ 61 = 62
  Succ 62 = 63
  Succ 63 = 64
  Succ 64 = 65
