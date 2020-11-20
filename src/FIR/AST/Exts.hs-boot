{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RoleAnnotations #-}

module FIR.AST.Exts where

-- base
import Data.Kind
  ( Type )

-- fir
import FIR.AST.Type
  ( AugType )

------------------------------------------------------------

data DebugPrintfF ( ast :: AugType -> Type ) ( t :: AugType )

type role DebugPrintfF phantom nominal
