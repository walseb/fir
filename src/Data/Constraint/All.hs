{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module: Data.Constraint.All

This module provides the ability to take a conjunction of a list of constraints.

Includes a reflection mechanism to enable resolution of individual constraints.
-}

module Data.Constraint.All
  ( AllDict(NilDict, ConsDict)
  , All(allDict)
  )
  where

import Data.Kind
  ( Constraint )

------------------------------------------------------------

data AllDict (c :: k -> Constraint) (as :: [k]) where
  NilDict  :: AllDict c '[]
  ConsDict :: (c a, All c as) => AllDict c (a ': as)

class All c as where
  allDict :: AllDict c as

instance All c '[] where
  allDict = NilDict

instance ( c a, All c as ) => All c (a ': as) where
  allDict = ConsDict
