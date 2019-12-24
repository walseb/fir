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

data DefF           ( ast :: AugType -> Type ) ( t :: AugType )
data FunDefF        ( ast :: AugType -> Type ) ( t :: AugType )
data FunCallF       ( ast :: AugType -> Type ) ( t :: AugType )
data DefEntryPointF ( ast :: AugType -> Type ) ( t :: AugType )
data LocallyF       ( ast :: AugType -> Type ) ( t :: AugType )
data EmbedF         ( ast :: AugType -> Type ) ( t :: AugType )

type role DefF           phantom nominal
type role FunDefF        phantom nominal
type role FunCallF       phantom nominal
type role DefEntryPointF phantom nominal
type role LocallyF       phantom nominal
type role EmbedF         phantom nominal
