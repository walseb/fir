{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RoleAnnotations #-}

module FIR.AST.Effs where

-- base
import Data.Kind
  ( Type )

-- fir
import FIR.AST.Type
  ( AugType )

------------------------------------------------------------

data LetF           ( ast :: AugType -> Type ) ( t :: AugType )
data DefF           ( ast :: AugType -> Type ) ( t :: AugType )
data FunDefF        ( ast :: AugType -> Type ) ( t :: AugType )
data FunCallF       ( ast :: AugType -> Type ) ( t :: AugType )
data DefEntryPointF ( ast :: AugType -> Type ) ( t :: AugType )
data StateF         ( ast :: AugType -> Type ) ( t :: AugType )

type role LetF           phantom nominal
type role DefF           phantom nominal
type role FunDefF        phantom nominal
type role FunCallF       phantom nominal
type role DefEntryPointF phantom nominal
type role StateF         phantom nominal
