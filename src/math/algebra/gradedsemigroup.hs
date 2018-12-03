{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Math.Algebra.GradedSemigroup where

-- base
import Data.Kind(Type)

infixl 6 <!>

class GradedSemigroup g k | g -> k where
  type Apply k g (i :: k) = (r :: Type) | r -> k g i
  type (i :: k) :<!>: (j :: k) :: k  
  (<!>) :: Apply k g i -> Apply k g j -> Apply k g (i :<!>: j)

class GradedSemigroup g k => GradedPresentedSemigroup g k d | g -> k d where
  type Element  g d (l :: d) :: Type
  type Degree k g d (l :: d) = (r :: k) | r -> k d l
  generator :: Element g d l -> Apply k g (Degree k g d l)

type DegreeAt k g (l :: d) = (Degree k g d (l :: d) :: k)