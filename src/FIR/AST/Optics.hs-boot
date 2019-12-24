{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RoleAnnotations #-}

module FIR.AST.Optics where

-- base
import Data.Kind
  ( Type )

-- fir
import FIR.AST.Type
  ( AugType )

------------------------------------------------------------

data UseF    ( ast :: AugType -> Type ) ( t :: AugType )
data AssignF ( ast :: AugType -> Type ) ( t :: AugType )
data ViewF   ( ast :: AugType -> Type ) ( t :: AugType )
data SetF    ( ast :: AugType -> Type ) ( t :: AugType )

type role UseF    phantom nominal
type role AssignF phantom nominal
type role ViewF   phantom nominal
type role SetF    phantom nominal
