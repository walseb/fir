{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RoleAnnotations #-}

module FIR.Syntax.Optics where

-- base
import Data.Kind
  ( Type )

-- fir
import {-# SOURCE #-} Control.Type.Optic
  ( Optic
  , Indices
  )
import Data.Type.List
  ( KnownLength )

----------------------------------------------------------------------

data SOptic (optic :: Optic i s a) :: Type
type role SOptic nominal

class KnownLength (Indices optic) => KnownOptic optic where
  opticSing :: SOptic optic

showSOptic :: SOptic (o :: Optic is s a) -> String
