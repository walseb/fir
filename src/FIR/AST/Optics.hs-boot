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

data OpticF ( ast :: AugType -> Type ) ( t :: AugType )

type role OpticF phantom nominal
