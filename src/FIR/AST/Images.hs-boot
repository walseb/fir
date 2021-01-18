{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RoleAnnotations #-}

module FIR.AST.Images where

-- base
import Data.Kind
  ( Type )

-- fir
import FIR.AST.Type
  ( AugType )

------------------------------------------------------------

data ImgOpsF   ( ast :: AugType -> Type ) ( t :: AugType )
data ImgQueryF ( ast :: AugType -> Type ) ( t :: AugType )

type role ImgOpsF   representational nominal
type role ImgQueryF phantom          nominal
