{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}


module Deriving.Prelude where

import qualified Prelude

newtype Prelude a = Prelude a

deriving via a instance Prelude.Eq a  => Prelude.Eq  (Prelude a)
deriving via a instance Prelude.Ord a => Prelude.Ord (Prelude a)

deriving via a instance Prelude.Show a => Prelude.Show (Prelude a)
deriving via a instance Prelude.Read a => Prelude.Read (Prelude a)

deriving via a instance Prelude.Enum a    => Prelude.Enum    (Prelude a)
deriving via a instance Prelude.Bounded a => Prelude.Bounded (Prelude a)
  
