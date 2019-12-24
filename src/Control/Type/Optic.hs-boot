{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RoleAnnotations #-}

module Control.Type.Optic
  ( Optic
  , Whole, Part, Indices
  , IndexInfo(..), IndexChain
  ) where

-- base
import Data.Kind
  ( Type )
import GHC.TypeNats
  ( Nat )

------------------------------------------------------------

data Optic (is :: [Type]) (s :: k) (a :: Type)
type role Optic phantom phantom phantom

type Whole   (optic :: Optic is s a) = s
type Part    (optic :: Optic is s a) = a
type Indices (optic :: Optic is s a) = is

data IndexInfo
  = ThisIndex Nat
  | AnyIndex

type IndexChain = [IndexInfo]
