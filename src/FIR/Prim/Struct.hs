{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Prim.Struct

A __structure__ (or __struct__ for short) can be thought of as an ordered record.

Here, we simply represent a struct as a linked list, with the fields annotated at the type level.

To illustrate:

> struct :: Struct '[ "field1" ':-> Float, "field2" ':-> V 3 Float ]
> struct = 1.0 :& V3 0.0 1.0 0.0 :& End

This defines a struct with two fields:

  * first field named @"field1"@, of type @Float@,
  * second field named @"field2"@, of type @V 3 Float@.

Note that the field annotations need not be type-level literals.
For instance, when specifying memory layout of vertex input data,
one instead specifies location slots (see "FIR.Layout"):

> inputData :: Struct '[ Slot 0 0 ':-> Float, Slot 0 3 ':-> V 3 Float ]


-}

module FIR.Prim.Struct
  ( Struct(End, (:&))
  , LocationSlot(..)
  , ShowLocation, ShowLocationSlot
  , FieldKind(..)
  , StructFieldKind(fieldKind)
  , headS, tailS
  , foldrStruct
  , traverseStruct
  )
  where

-- base
import Control.Applicative
  ( liftA2 )
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol, AppendSymbol )
import GHC.TypeNats
  ( Nat, KnownNat
  , type (+), type(-), type (*)
  , Mod, Div
  )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Data.Product
  ( HList(HNil,(:>)), IsProduct(..) )
import Data.Type.List
  ( type (:++:) )
import Data.Type.Map
  ( (:->)((:->)), Key, Value )
import Data.Type.Known
  ( Demotable(Demote)
  , Known(known), knownValue
  )
import Data.Type.Ord
  ( POrd(Compare)
  , PEnum(Succ, Pred, FromEnum, ToEnum)
  )
import Data.Type.String
  ( ShowNat )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GeneratedGradedSemigroup(..)
  , FreeGradedSemigroup(..)
  )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( PrimTy
  , PrimTyMap(primTyMapSing)
  , SPrimTyMap(SNil, SCons)
  )

------------------------------------------------------------
-- structs

infixr 4 :&
-- order *matters* for structs (memory layout!)
data Struct :: [fld :-> Type] -> Type where
  End  :: Struct '[]
  (:&) :: forall k a as. a -> Struct as -> Struct ((k ':-> a) ': as)

headS :: Struct ((k ':-> a) ': as) -> a
headS (a :& _) = a

tailS :: Struct ((k ':-> a) ': as) -> Struct as
tailS (_ :& as) = as

data LocationSlot n where
  LocationSlot :: n -> n -> LocationSlot n

type family ShowLocation ( slot :: LocationSlot Nat ) :: Symbol where
  ShowLocation ('LocationSlot l _)
    = "Location " `AppendSymbol` ShowNat l

type family ShowLocationSlot ( slot :: LocationSlot Nat ) :: Symbol where
  ShowLocationSlot ('LocationSlot l c)
    = "Location " `AppendSymbol` ShowNat l `AppendSymbol` ", Component " `AppendSymbol` ShowNat c

data FieldKind (fld :: Type) where
  NamedField    :: FieldKind Symbol
  LocationField :: FieldKind (LocationSlot Nat)
  OtherField    :: FieldKind fld

class StructFieldKind fld where
  fieldKind :: FieldKind fld
instance StructFieldKind Symbol where
  fieldKind = NamedField
instance StructFieldKind (LocationSlot Nat) where
  fieldKind = LocationField
instance {-# OVERLAPPABLE #-} StructFieldKind fld where
  fieldKind = OtherField

instance PrimTyMap as => Eq (Struct as) where
  s1 == s2 = case primTyMapSing @_ @as of
    SNil
      -> True
    SCons {}
      -> case (s1, s2) of
            (a :& as, b :& bs)
              -> a == b && as == bs

instance PrimTyMap as => Ord (Struct as) where
  compare s1 s2 = case primTyMapSing @_ @as of
    SNil
      -> EQ
    SCons {}
      -> case (s1, s2) of
            (a :& as, b :& bs)
              -> case compare a b of
                    EQ -> compare as bs
                    un -> un

display :: forall as. PrimTyMap as => Struct as -> String
display s = case primTyMapSing @_ @as of
  SNil
    -> ""
  SCons {}
    -> case s of
          (a :& as)
            -> case display as of
                  "" -> show a
                  d  -> show a ++ ", " ++ d

instance PrimTyMap as => Show (Struct as) where
  show s = "{ " ++ display s ++ " }" 

instance IsProduct (Struct '[]) '[] where
  fromHList _ = End
  toHList   _ = HNil

instance IsProduct (Struct as) tys => IsProduct (Struct ((fld ':-> ty) ': as)) (ty ': tys) where
  fromHList (a :> as) = a :& fromHList as
  toHList   (a :& as) = a :> toHList   as

instance GradedSemigroup (Struct :: [ fld :-> Type ] -> Type) [fld :-> Type] where
  type Grade [fld :-> Type] (Struct :: [ fld :-> Type ] -> Type) as = Struct as
  type as :<!>: bs = as :++: bs
  (<!>) :: Struct as -> Struct bs -> Struct (as :++: bs)
  End <!> t = t
  (a :& s) <!> t = a :& ( s <!> t )

instance GeneratedGradedSemigroup
            (Struct :: [ fld :-> Type ] -> Type)
            [fld  :-> Type]
            (fld  :-> Type)
            where
  type GenType                (Struct :: [ fld :-> Type ] -> Type) (fld :-> Type) kv = Value kv
  type GenDeg [fld  :-> Type] (Struct :: [ fld :-> Type ] -> Type) (fld :-> Type) kv = '[ kv ]
  generator :: forall (kv :: fld :-> Type). Value kv -> Struct '[ kv ]
  generator a = unsafeCoerce ( (a :& End) :: Struct '[ Key kv ':-> Value kv] )
                --   ^^^^   GHC cannot deduce that kv ~ Key kv ':-> Value kv
                -- see [GHC trac #7259](https://gitlab.haskell.org/ghc/ghc/issues/7259)

instance FreeGradedSemigroup
            (Struct :: [ fld :-> Type ] -> Type)
            [fld :-> Type]
            (fld :-> Type)
            where
  type ValidDegree (Struct :: [ fld :-> Type ] -> Type) as = PrimTyMap as
  (>!<) :: forall as bs. (PrimTyMap as, PrimTyMap bs)
        => Struct (as :++: bs) -> ( Struct as, Struct bs )
  (>!<) End = unsafeCoerce ( End, End )
          --    ^^^^^^   GHC cannot deduce (as ~ '[], bs ~ '[]) from (as :++: bs) ~ '[]
  (>!<) (s :& ss)
    = case primTyMapSing @_ @as of
        SNil
          -> ( End, s :& ss )
        sCons@SCons
          -> case sCons of
                ( _ :: SPrimTyMap ((k ':-> a) ': as'))
                  -> let
                        l :: Struct as'
                        r :: Struct bs
                        (l,r) = ( (>!<) @_ @_ @_ @_ @bs ss)
                      in ( s :& l, r )
  generated :: Struct '[ kv ] -> Value kv
  generated (a :& End) = a


foldrStruct
    :: forall as b. PrimTyMap as
    => ( forall a. PrimTy a => a -> b -> b )
    -> b -> Struct as -> b
foldrStruct f b s =
  case primTyMapSing @_ @as of
    SNil
      -> b
    SCons {}
      -> case s of
          (a :& as)
            -> f a (foldrStruct f b as)

traverseStruct
    :: ( Applicative f, PrimTyMap as )
    => ( forall a. PrimTy a => a -> f b )
    -> Struct as
    -> f [b]
traverseStruct f
   = foldrStruct ( \a bs -> liftA2 (:) (f a) bs )
       ( pure [] )

-----------------------------------------------------------
-- instances for 'LocationSlot'

instance Demotable (LocationSlot Nat) where
  type Demote (LocationSlot Nat) = LocationSlot Word32

instance (KnownNat l, KnownNat c) => Known (LocationSlot Nat) ('LocationSlot l c) where
  known = LocationSlot (knownValue @l) (knownValue @c)

instance POrd (LocationSlot Nat) where
  type Compare ('LocationSlot l1 c1) ('LocationSlot l2 c2)
    = Compare '(l1, c1) '(l2, c2) -- lexicographic ordering

type family SuccLocationSlot (slot :: LocationSlot Nat) :: LocationSlot Nat where
  SuccLocationSlot ('LocationSlot l 3) = 'LocationSlot (l+1) 0
  SuccLocationSlot ('LocationSlot l c) = 'LocationSlot l (c+1)

type family PredLocationSlot (slot :: LocationSlot Nat) :: LocationSlot Nat where
  PredLocationSlot ('LocationSlot 0 0) = 'LocationSlot 0 0
  PredLocationSlot ('LocationSlot l 0) = 'LocationSlot (l-1) 3
  PredLocationSlot ('LocationSlot l c) = 'LocationSlot l (c-1)

instance PEnum (LocationSlot Nat) where
  type Succ loc = SuccLocationSlot loc
  type Pred loc = PredLocationSlot loc
  type FromEnum ('LocationSlot l c)
    = 4 * l + c
  type ToEnum n
    = 'LocationSlot (n `Div` 4) (n `Mod` 4)
