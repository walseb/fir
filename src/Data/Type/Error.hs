{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: Data.Type.Error

Helper functions to manage custom type-errors: throwing them, combining them, etc.
-}

module Data.Type.Error where

-- base
import Data.Kind
  ( Constraint )
import GHC.TypeLits
  ( TypeError, ErrorMessage((:$$:)) )

----------------------------------------------------------------------

type family Try ( x :: Either ErrorMessage a ) :: a where
  Try (Left  err) = TypeError err
  Try (Right a  ) = a

type family IsRight ( x :: Either ErrorMessage a ) :: Constraint where
  IsRight (Left err) = TypeError err
  IsRight _          = ()

type family IfLeft ( x :: Either e a ) ( y :: Either e a ) :: Either e a where
  IfLeft (Left _) y = y
  IfLeft x        _ = x

type family And ( x :: Either ErrorMessage r ) ( y :: Either ErrorMessage r ) :: Either ErrorMessage r where
  And (Right _) y        = y
  And (Left  x) (Left y) = Left (x :$$: y)
  And (Left  x) _        = Left x

type family All ( xs :: [ Either ErrorMessage r ] ) :: Either ErrorMessage r where
  All '[]       = Right '()
  All (x ': xs) = x `And` (All xs)

data family Dummy1 :: k
data family Dummy2 :: k

type family Assert (x :: k) (y :: l) :: l where
  Assert Dummy1 _ = Dummy2
  Assert _      y = y
