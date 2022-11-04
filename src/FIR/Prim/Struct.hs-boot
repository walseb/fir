{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Prim.Struct where

-- base
import Data.Kind
  ( Type )
import Data.Typeable
  ( Typeable )
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Known
  ( Demote )
import Data.Type.Map
  ( (:->) )
import FIR.AST.Type
  ( AugType(Val) )

------------------------------------------------------------
-- structs

data Struct :: [fld :-> Type] -> Type where
type role Struct nominal

data LocationSlot n where
  LocationSlot :: n -> n -> LocationSlot n

instance Show n => Show (LocationSlot n) where

data FieldKind (fld :: Type) where
  NamedField    :: FieldKind Symbol
  LocationField :: FieldKind (LocationSlot Nat)
  OtherField    :: FieldKind fld

class (Show (Demote fld), Typeable fld) => StructFieldKind fld where
  fieldKind :: FieldKind fld

instance StructFieldKind Symbol where
instance StructFieldKind (LocationSlot Nat) where
instance {-# OVERLAPPABLE #-} (Show (Demote fld), Typeable fld) => StructFieldKind fld where

class Typeable bs =>
        ASTStructFields
          ( ast :: ( AugType -> Type ) )
          ( as :: [ Symbol :-> Type ] )
          ( bs :: [ Symbol :-> Type ] )
        | as -> bs, bs ast -> as where
  traverseStructASTs
    :: forall f b. Applicative f
    => ( forall a. ast (Val a) -> f b ) -> Struct as -> f [b]
