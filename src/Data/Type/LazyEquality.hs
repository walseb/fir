{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module: Data.Type.LazyEquality

This module defines an alternative version of 'Data.Type.Equality.==',
for which @a == a@ always reduces to 'True'.
-}

module Data.Type.LazyEquality
  ( LazyEq )
  where

-------------------------------------------------------

infix 4 `LazyEq`

-- Alternative version of 'Data.Type.Equality.=='.
-- Ensures that @a == a@ reduces to True for any argument @a@.
-- 
-- See "Data.Type.Equality.==", which explains the trade-off.
type family (a :: k) `LazyEq` (b :: k) :: Bool where
  a `LazyEq` a = 'True
  _ `LazyEq` _ = 'False
