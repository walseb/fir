{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: Data.Type.Ord

Promotion of the 'Ord' type class.
-}

module Data.Type.Ord where

-- base
import Data.Type.Bool
  ( If, Not )
import GHC.TypeLits
  ( Symbol, CmpSymbol )
import GHC.TypeNats
  ( Nat, CmpNat, type (+), type (-) )

-- fir
import Data.Type.LazyEquality
  ( LazyEq )

-------------------------------------------------------------------------------

infix 4 :<
infix 4 :>
infix 4 :<=
infix 4 :>=
infix 4 `Compare`

class POrd a where
  type Compare (x :: a) (y :: a) :: Ordering
  type (x :: a) :<  (y :: a) :: Bool
  type x :< y = Compare x y `LazyEq` LT
  type (x :: a) :>  (y :: a) :: Bool
  type x :> y = Compare x y `LazyEq` GT
  type (x :: a) :<= (y :: a) :: Bool
  type x :<= y = Not (Compare x y `LazyEq` GT)
  type (x :: a) :>= (y :: a) :: Bool
  type x :>= y = Not (Compare x y `LazyEq` LT)
  type Max (x :: a) (y :: a) :: a
  type Max x y = If (x :< y) y x
  type Min (x :: a) (y :: a) :: a
  type Min x y = If (x :> y) y x

instance POrd Symbol where
  type Compare x y = CmpSymbol x y

instance POrd Nat where
  type Compare x y = CmpNat x y

instance ( POrd a, POrd b ) => POrd (a,b) where
  type Compare p1 p2 = Lexicographic2 p1 p2

type family Lexicographic2 (p1 :: (a,b)) (p2 :: (a,b)) :: Ordering where
  Lexicographic2 '(x ,y1) '(x ,y2) = Compare y1 y2
  Lexicographic2 '(x1,_)  '(x2,_)  = Compare x1 x2

instance ( POrd a, POrd b, POrd c ) => POrd (a,b,c) where
  type Compare t1 t2 = Lexicographic3 t1 t2

type family Lexicographic3 (t1 :: (a,b,c)) (t2 :: (a,b,c)) :: Ordering where
  Lexicographic3 '(x ,y ,z1) '(x ,y ,z2) = Compare z1 z2
  Lexicographic3 '(x ,y1,_ ) '(x ,y2,_ ) = Compare y1 y2
  Lexicographic3 '(x1,_ ,_ ) '(x2,_ ,_ ) = Compare x1 x2


class POrd a => PEnum a where
  type Succ (x :: a) :: a
  type Pred (x :: a) :: a
  type ToEnum (i :: Nat) :: a
  type FromEnum (x :: a) :: Nat
  type EnumFromTo (x :: a) (y :: a) :: [a]
  type EnumFromTo x y
    = EnumFromToDefault x y (x :> y)
  type EnumFromOfCount (x :: a) (i :: Nat) :: [a]
  type EnumFromOfCount x i
    = EnumFromOfCountDefault x i

type family EnumFromToDefault (x :: a) (y :: a) (x_gt_y :: Bool) :: [a] where
  EnumFromToDefault _ _ 'True  = '[]
  EnumFromToDefault x y 'False = x ': EnumFromTo (Succ x) y

type family EnumFromOfCountDefault (x :: a) (i :: Nat) :: [a] where
  EnumFromOfCountDefault _ 0 = '[]
  EnumFromOfCountDefault x i = x ': EnumFromOfCountDefault (Succ x) (Pred i)

instance PEnum Nat where
  type Succ x     = x + 1
  type Pred x     = x - 1
  type ToEnum x   = x
  type FromEnum x = x
