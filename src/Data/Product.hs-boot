{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Product where

-- base
import Data.Kind
  ( Type )

-- fir
import Data.Type.List
  ( KnownLength )

----------------------------------------------------------------------

infixr 3 :>

data HList (as :: [Type]) where
  HNil :: HList '[]
  (:>) :: a -> HList as -> HList (a ': as)

class KnownLength as => IsProduct (p :: Type) (as :: [Type]) -- | p -> as
  where
  fromHList :: HList as -> p
  toHList   :: p -> HList as
