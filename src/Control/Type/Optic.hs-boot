{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Control.Type.Optic
  ( IndexInfo(..), IndexChain ) where

-- base
import GHC.TypeNats
  ( Nat )

------------------------------------------------------------

data IndexInfo
  = ThisIndex Nat
  | AnyIndex

type IndexChain = [IndexInfo]
