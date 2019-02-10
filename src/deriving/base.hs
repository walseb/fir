{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}

module Deriving.Base where

import qualified Prelude

newtype Base a = Base a

deriving via a instance Prelude.Eq a  => Prelude.Eq  (Base a)
deriving via a instance Prelude.Ord a => Prelude.Ord (Base a)

deriving via a instance Prelude.Show a => Prelude.Show (Base a)
deriving via a instance Prelude.Read a => Prelude.Read (Base a)

deriving via a instance Prelude.Enum a    => Prelude.Enum    (Base a)
deriving via a instance Prelude.Bounded a => Prelude.Bounded (Base a)
