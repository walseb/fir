{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.LazyEquality
  ( LazyEq )
  where

-------------------------------------------------------

infix 4 `LazyEq`

-- Lazier version of 'Data.Type.Equality.=='.
-- Ensures that @a == a@ reduces to True for any argument @a@.
type family (a :: k) `LazyEq` (b :: k) :: Bool where
  a `LazyEq` a = 'True
  _ `LazyEq` _ = 'False
