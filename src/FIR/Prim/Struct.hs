{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
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
  , ShowLocationSlot
  , FieldKind(..)
  , StructFieldKind(fieldKind)
  , HasStructField(getStructField, setStructField)
  , headS, tailS
  , foldrStruct
  , traverseStruct
  , ASTStructFields(..)
  )
  where

-- base
import Control.Applicative
  ( liftA2 )
import Data.Kind
  ( Type )
import Data.List
  ( intercalate )
import Data.Typeable
  ( Typeable )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol, KnownSymbol, AppendSymbol )
import GHC.TypeNats
  ( Nat, KnownNat
  , type (+), type(-), type (*)
  , Mod, Div
  )

-- fir
import Data.Product
  ( HList(HNil,(:>)), IsProduct(..) )
import Data.Type.List
  ( type (:++:) )
import Data.Type.Map
  ( (:->)((:->)) )
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
import {-# SOURCE #-} FIR.AST
  ( AST )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..) )
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

deriving stock instance Show n => Show (LocationSlot n)

type family ShowLocationSlot ( slot :: LocationSlot Nat ) :: Symbol where
  ShowLocationSlot ('LocationSlot l c)
    = "Location " `AppendSymbol` ShowNat l `AppendSymbol` ", Component " `AppendSymbol` ShowNat c

data FieldKind (fld :: Type) where
  NamedField    :: FieldKind Symbol
  LocationField :: FieldKind (LocationSlot Nat)
  OtherField    :: FieldKind fld

class Show (Demote fld) => StructFieldKind fld where
  fieldKind :: FieldKind fld
instance StructFieldKind Symbol where
  fieldKind = NamedField
instance StructFieldKind (LocationSlot Nat) where
  fieldKind = LocationField
instance {-# OVERLAPPABLE #-} Show (Demote fld) => StructFieldKind fld where
  fieldKind = OtherField

class HasStructField k a as | k as -> a where
  getStructField :: Struct as -> a
  setStructField :: a -> Struct as -> Struct as
instance {-# OVERLAPPING #-}
         forall fld (k :: fld) (a :: Type) (as :: [ fld :-> Type ])
         .  HasStructField k a ((k ':-> a) ': as)
         where
  getStructField = headS
  setStructField b ( _ :& as ) = b :& as
instance forall fld (k :: fld) (a :: Type) (l :: fld) (x :: Type) (as :: [ fld :-> Type ])
         .  HasStructField k a as
         => HasStructField k a ((l ':-> x) ': as)
         where
  getStructField = getStructField @k @a @as . tailS
  setStructField b ( a :& as ) = a :& setStructField @k @a @as b as
instance {-# OVERLAPPING #-}
         forall fld (k :: fld) (a :: Type) (as :: [ fld :-> Type ])
         .  HasStructField (0 :: Nat) a ((k ':-> a) ': as)
         where
  getStructField = headS
  setStructField b ( _ :& as ) = b :& as
instance forall fld (l :: fld) (i :: Nat) (a :: Type) (x :: Type) (as :: [ fld :-> Type])
         .  HasStructField (i-1) a as
         => HasStructField i a ((l ':-> x) ': as)
         where
  getStructField = getStructField @(i-1) @a @as . tailS
  setStructField b ( a :& as ) = a :& setStructField @(i-1) @a @as b as

instance PrimTyMap as => Eq (Struct as) where
  s1 == s2 = case primTyMapSing @_ @as of
    SNil -> True
    SCons ->
      case (s1, s2) of
        (a :& as, b :& bs) ->
          a == b && as == bs

instance PrimTyMap as => Ord (Struct as) where
  compare s1 s2 = case primTyMapSing @_ @as of
    SNil -> EQ
    SCons ->
      case (s1, s2) of
        (a :& as, b :& bs) ->
          case compare a b of
            EQ -> compare as bs
            un -> un

display :: forall fld (as :: [fld :-> Type])
        .  ( StructFieldKind fld, PrimTyMap as )
        => Struct as -> [String]
display s = case primTyMapSing @_ @as of
  SNil -> []
  sCons@SCons ->
    case sCons of
      ( _ :: SPrimTyMap ((k ':-> a) ': xs) ) ->
        case s of
          (a :& as) ->
            ( show (knownValue @k) ++ " :-> " ++ show a )
            : display as

instance forall fld (as :: [fld :-> Type])
         .  ( StructFieldKind fld, PrimTyMap as )
         => Show (Struct as) where
  show s = "{ " ++ intercalate ", " (display s) ++ " }"

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

class Typeable bs => ASTStructFields (as :: [ Symbol :-> Type]) (bs :: [ Symbol :-> Type])
        | as -> bs, bs -> as where
  traverseStructASTs
    :: forall f b. Applicative f
    => ( forall a. AST a -> f b ) -> Struct as -> f [b]
instance ASTStructFields '[] '[] where
  traverseStructASTs _ _ = pure []
instance (KnownSymbol k2, k1 ~ k2, a ~ AST b, Typeable b, ASTStructFields as bs)
   => ASTStructFields ( (k1 ':-> a) ': as) ( (k2 ':-> b) ': bs) where
  traverseStructASTs f (a :& as) = (:) <$> f a <*> traverseStructASTs f as

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
