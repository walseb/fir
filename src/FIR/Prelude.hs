{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

-- | An internal module which re-exports some definitions needed
-- from the Prelude and related modules.
--
-- As a user of the EDSL, you should never need to import this module.
module FIR.Prelude
  ( module Prelude
  , module Data.Int
  , module Data.Word
  , module Numeric.Half
  , module Data.Complex
  , module Data.Coerce
  , module Data.String
  , module Data.Kind
  )
  where

import Prelude
  ( Bool(..), Maybe(..), Either(..)
  , Float, Double
  , Semigroup(..), Monoid(..)
  , IO
  , FilePath
#if __GLASGOW_HASKELL__ >= 903
  , type (~)
#endif
  , ($), (.)
  , otherwise, maybe, either
  , fst, snd
  , curry, uncurry
  , subtract
  , id, const
  , flip
  , until, asTypeOf
  , error, errorWithoutStackTrace
  , seq, ($!)
  )

-- NB: deliberately not re-exporting Int/Word.
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import Numeric.Half
  ( Half )

import Data.Complex
  ( Complex(..) )

import Data.Coerce
  ( Coercible, coerce )

import Data.String
  ( IsString(..) )

import Data.Kind
  ( Type, Constraint )
