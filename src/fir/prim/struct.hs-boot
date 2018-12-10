{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE TypeOperators          #-}

module FIR.Prim.Struct where

-- base
import Data.Kind(Type)
import GHC.TypeLits(Symbol)

-- fir
import Data.Type.Map((:->))

------------------------------------------------------------
-- structs

data Struct :: [Symbol :-> Type] -> Type where
type role Struct nominal
