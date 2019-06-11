{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Prim.Struct
  ( Struct(End, (:&))
  , LocationSlot(..)
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
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Data.Type.List
  ( type (:++:) )
import Data.Type.Map
  ( (:->)((:->)), Key, Value )
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
