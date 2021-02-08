{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RoleAnnotations     #-}

module FIR.AST.ControlFlow where

-- base
import Data.Kind
  ( Type )

-- fir
import FIR.AST.Type
  ( AugType )

------------------------------------------------------------

data SelectionF ( ast :: AugType -> Type ) ( t :: AugType )
data LoopF      ( ast :: AugType -> Type ) ( t :: AugType )

type role SelectionF representational nominal
type role LoopF      phantom          nominal
