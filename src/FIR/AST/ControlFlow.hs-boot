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

data IfF            ( ast :: AugType -> Type ) ( t :: AugType )
data IfMF           ( ast :: AugType -> Type ) ( t :: AugType )
data SwitchF        ( ast :: AugType -> Type ) ( t :: AugType )
data SwitchMF       ( ast :: AugType -> Type ) ( t :: AugType )
data WhileF         ( ast :: AugType -> Type ) ( t :: AugType )
data LoopF          ( ast :: AugType -> Type ) ( t :: AugType )
data BreakF         ( ast :: AugType -> Type ) ( t :: AugType )
data BreakContinueF ( ast :: AugType -> Type ) ( t :: AugType )

type role IfF            phantom          nominal
type role IfMF           phantom          nominal
type role SwitchF        representational nominal
type role SwitchMF       representational nominal
type role WhileF         phantom          nominal
type role LoopF          phantom          nominal
type role BreakF         phantom          nominal
type role BreakContinueF phantom          nominal
