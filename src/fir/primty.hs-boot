{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators   #-}

module FIR.PrimTy where

-- base
import Data.Kind(Type)
import GHC.TypeLits(Symbol)
import GHC.TypeNats(Nat)

-- vector
import qualified Data.Vector as Array

-- fir
import Data.Type.Map((:->)((:->)))

------------------------------------------------------------

data Array :: Nat -> Type -> Type
type role Array nominal representational


newtype RuntimeArray a = MkRuntimeArray (Array.Vector a)


data Struct :: [Symbol :-> Type] -> Type
type role Struct nominal





