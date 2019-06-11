{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Prim.Struct where

-- base
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Map
  ( (:->) )

------------------------------------------------------------
-- structs

data Struct :: [fld :-> Type] -> Type where
type role Struct nominal

data LocationSlot n where
  LocationSlot :: n -> n -> LocationSlot n

data FieldKind (fld :: Type) where
  NamedField    :: FieldKind Symbol
  LocationField :: FieldKind (LocationSlot Nat)
  OtherField    :: FieldKind fld

class StructFieldKind fld where
  fieldKind :: FieldKind fld

instance StructFieldKind Symbol where
instance StructFieldKind (LocationSlot Nat) where
instance {-# OVERLAPPABLE #-} StructFieldKind fld where
