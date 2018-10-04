{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeClasses.Integral where

import Prelude(Num)
import qualified Prelude as P

class Num a => Integral a where
  quot :: a -> a -> a
  rem  :: a -> a -> a
  div  :: a -> a -> a
  mod  :: a -> a -> a
  
class FromIntegral a b where
  fromIntegral :: a -> b

instance (P.Integral a, Num b) => FromIntegral a b where
  fromIntegral = P.fromIntegral

{-
class NoToIntegral where
  toIntegral :: ...

instance TypeError blah => NoToIntegral a
-}