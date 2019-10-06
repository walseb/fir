{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Layout

Validation module for GPU memory layout, e.g. shader location/component assignments.

See "FIR.Layout" for an overview of memory layout concerns.

-}

module FIR.Validation.Layout where

-- base
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If, type (&&), type (||) )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat
  , type (+), type (-)
  , Div, Mod
  )

-- fir
import Data.Type.List
  ( type (:++:) )
import Data.Type.Map
  ( Map, (:->)((:->))
  , Insert, ZipValue
  )
import Data.Type.Ord
  ( POrd(Max, (:<=))
  , PEnum(Pred, IncFrom)
  )
import {-# SOURCE #-} FIR.Layout
  ( Poke(SizeOf)
  , Layout(Locations)
  )
import FIR.Prim.Array
  ( Array(..), RuntimeArray )
import FIR.Prim.Singletons
  ( SScalarTy, ScalarWidth
  , SKPrimTy(..), PrimTySing
  )
import FIR.Prim.Struct
  ( Struct(..)
  , LocationSlot(LocationSlot)
  , ShowLocation
  )
import Math.Linear
  ( V, M )

-----------------------------------------------------------------------------------------------------
-- | Annotating a given slot with extra information.
--
-- Note: this data-type does not refer to the slot being annotated,
-- as we use it in a map "[ LocationSlot Nat :-> SlotProvenance ]",
-- which annotates each slot with extra information.
data SlotProvenance where
  Provenance
    :: LocationSlot Nat -- ^ Provenance (original location slot this slot is based off).
    -> SKPrimTy  ty     -- ^ Type this slot is being used for.
    -> SScalarTy scalar -- ^ Underlying scalar type.
    -> SlotProvenance

-----------------------------------------------------------------------------------------------------

type family NoOverlappingSlots (as :: [LocationSlot Nat :-> Type]) :: Constraint where
  NoOverlappingSlots as = SlotsDontOverlap (Slots as)

type family Slots
              ( as :: [LocationSlot Nat :-> Type] )
            :: Map (LocationSlot Nat) SlotProvenance
            where
  Slots '[] = '[]
  Slots ( (slot ':-> ty) ': slots)
    = AddSlotsCheckingOverlap (EnumSlots slot ty (PrimTySing ty)) (Slots slots)


-- Force evaluation of computation of all used slots, throwing a type error if any slots overlapped.
-- The overlap checking is performed in the type family 'AddSlotsCheckingOverlap',
-- which is recursively called by the type family 'Slots'.
type family SlotsDontOverlap
                (slots :: [ LocationSlot Nat :-> SlotProvenance ])
              :: Constraint
              where
  SlotsDontOverlap '[]
    = ()
  SlotsDontOverlap ( slot ': slots )
    = ( (() :: Constraint) , (() :: Constraint) ) -- disjoint from the previous equation

type family SlotDoesNotAppear
                ( slot     :: LocationSlot Nat )
                ( slotProv :: SlotProvenance   )
                ( slots    :: [ LocationSlot Nat :-> SlotProvenance ] )
              :: Bool where
  SlotDoesNotAppear _ _ '[] = 'True
  SlotDoesNotAppear slot
    ( Provenance base1 ( _ :: SKPrimTy ty1 ) _ )
    ( ( slot ':-> Provenance base2 ( _ :: SKPrimTy ty2 ) _ ) ': _ )
    = TypeError
        (    Text "Overlap at " :<>: Text (ShowLocation slot ) :<>: Text ":"
        :$$: Text "  - " :<>: ShowType ty1 :<>: Text " based at " :<>: Text (ShowLocation base1) :<>: Text ","
        :$$: Text "  - " :<>: ShowType ty2 :<>: Text " based at " :<>: Text (ShowLocation base2) :<>: Text "."
        )
  SlotDoesNotAppear slot prov ( _ ': slots)
    = SlotDoesNotAppear slot prov slots

type NbLocationsUsed ( as :: [LocationSlot Nat :-> Type] )
  = ( NbLocationsOfSlots (Slots as) :: Nat )

type family NbLocationsOfSlots
              ( slots :: Map (LocationSlot Nat) SlotProvenance )
            :: Nat
            where
  NbLocationsOfSlots '[] = 0
  NbLocationsOfSlots ( ( slot ':-> Provenance startLoc ty scalar ) ': slots )
    = NbLocationsOfSlotsAcc
        startLoc
        startLoc
        ( ( slot ':-> Provenance startLoc ty scalar ) ': slots )

type family NbLocationsOfSlotsAcc
              ( minLoc :: LocationSlot Nat )
              ( maxLoc :: LocationSlot Nat )
              ( slots :: [ LocationSlot Nat :-> SlotProvenance ] )
            :: Nat
            where
  NbLocationsOfSlotsAcc ('LocationSlot minLoc _) ('LocationSlot maxLoc _) '[]
    = 1 + maxLoc - minLoc
  NbLocationsOfSlotsAcc minLoc maxLoc ( (loc ':-> _) ': locs )
    = NbLocationsOfSlotsAcc minLoc (Max maxLoc loc) locs

type family AddSlotsCheckingOverlap
              ( locs       :: [ LocationSlot Nat :-> SlotProvenance ] )
              ( slots      :: Map (LocationSlot Nat) SlotProvenance   )
            :: Map (LocationSlot Nat) SlotProvenance
            where
  AddSlotsCheckingOverlap '[] slots = slots
  AddSlotsCheckingOverlap ( ( loc ':-> prov ) ': locs ) slots
    = If ( SlotDoesNotAppear loc prov slots )
        ( AddSlotsCheckingOverlap locs ( Insert loc prov slots ) )
        ( '[] ) -- unreachable

-- | Simple enumeration of consecutive slots,
-- as many as the size of the type.
--
-- Should not be used for compound types,
-- where the locations used are not necessarily consecutive.
type family EnumConsecutiveSlots
              ( startLoc :: LocationSlot Nat )
              ( ty       :: Type )
              :: [ LocationSlot Nat ]
              where
  EnumConsecutiveSlots loc ty
    = IncFrom loc ( Pred ( SizeOf Locations ty `Div` 4 ) )

-- | Enumerate all slots used by a type beginning in a given location.
type family EnumSlots
              ( slot :: LocationSlot Nat )
              ( ty   :: Type             )
              ( sing :: SKPrimTy ty      )
            :: Map (LocationSlot Nat) SlotProvenance
            where
  EnumSlots _ () SKUnit
    = TypeError
        ( Text "Vertex binding: unit type not supported." )
  EnumSlots _ Bool SKBool
    = TypeError
        ( Text "Vertex binding: booleans not supported. Convert to 'Word32' instead." )
  EnumSlots ('LocationSlot l c) ty (SKScalar s)
    = If ( c :<= 3 && ( c `Mod` 2 == 0 || ScalarWidth s :<= 32 ) )
        ( ( EnumConsecutiveSlots ('LocationSlot l c) ty )
          `ZipValue`
          ( Provenance ('LocationSlot l c) (SKScalar s :: SKPrimTy ty) s )
        )
        ( TypeError
          (    Text "Vertex binding: cannot position scalar of type " :<>: ShowType ty
          :$$: Text " at location " :<>: ShowType l
          :<>: Text " with component " :<>: ShowType c :<>: Text "."
          )
        )
  EnumSlots ('LocationSlot l c) (V n a) (SKVector s)
    = If ( c == 0 || ( c `Mod` 2 == 0 && c :<= 3 && n :<= 2 && ScalarWidth s :<= 32 ) )
       (  ( EnumConsecutiveSlots ('LocationSlot l c) (V n a) )
          `ZipValue`
          ( Provenance ('LocationSlot l c) (SKVector s :: SKPrimTy (V n a)) s )
       )
       ( TypeError
          (    Text "Vertex binding: cannot position vector of type "
          :<>: ShowType (V n a)
          :$$: Text "at location " :<>: ShowType l
          :<>: Text " with component " :<>: ShowType c :<>: Text "."
          )
       )
  EnumSlots ('LocationSlot l 0) (M m n a) (SKMatrix s)
    = ( EnumConsecutiveSlots ('LocationSlot l 0) (M m n a) )
      `ZipValue`
      ( Provenance ('LocationSlot l 0) (SKMatrix s :: SKPrimTy (M m n a)) s )
  EnumSlots ('LocationSlot l c) (M m n a) (SKMatrix _)
    = TypeError
        (    Text "Vertex binding: cannot position matrix at location " :<>: ShowType l
        :<>: Text " with non-zero component " :<>: ShowType c :<>: Text "."
        )
  EnumSlots loc (Array 0 a) (SKArray _) = '[]
  EnumSlots loc (Array 1 a) (SKArray elt) = EnumSlots loc a elt
  EnumSlots ('LocationSlot l c) (Array n a) (SKArray elt)
    =  If ( c :<= 3 )
        (     EnumSlots
                ('LocationSlot l c)
                a
                elt
        :++:  EnumSlots
                ( 'LocationSlot (l + 1 + ( Pred (SizeOf Locations a) `Div` 4) ) c )
                ( Array (n-1) a )
                ( SKArray elt )
        )
        ( TypeError
           (    Text "Vertex binding: cannot position array of type" :<>: ShowType (Array n a)
           :$$: Text "at location " :<>: ShowType l
           :<>: Text " with component " :<>: ShowType c :<>: Text "."
           :$$: Text "Component cannot be greater than 3."
           )
        )
  EnumSlots _ (RuntimeArray _) (SKRuntimeArray _)
    = TypeError
        ( Text "Vertex binding: runtime arrays not supported." )
  EnumSlots _ (Struct _) (SKStruct _)
    = TypeError
        ( Text "Vertex binding: nested structs not (yet?) supported." )
