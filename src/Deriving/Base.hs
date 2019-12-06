{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module: Deriving.Base

This module defines the newtype 'Base', used with `deriving via` to name instances coming from the Haskell prelude.

For instance (pun intended):

> deriving via Base Float  instance Floating Float
> deriving via Base Double instance Floating Double

See "Math.Algebra.Class" for illustration.

-}

module Deriving.Base
  ( Base(Base, runBase) )
  where

import qualified Prelude

newtype Base a = Base { runBase :: a }

deriving via a instance Prelude.Eq a  => Prelude.Eq  (Base a)
deriving via a instance Prelude.Ord a => Prelude.Ord (Base a)

deriving via a instance Prelude.Show a => Prelude.Show (Base a)
deriving via a instance Prelude.Read a => Prelude.Read (Base a)

deriving via a instance Prelude.Enum a    => Prelude.Enum    (Base a)
deriving via a instance Prelude.Bounded a => Prelude.Bounded (Base a)
