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

module FIR.Validation.Layout
  ( -- * Slot provenance
    SlotProvenance(Provenance)
  , Availability(Used, Available)
    -- * Validation of memory layouts
  , Slots, ValidLayout
  , NbLocationsUsed
  )
  where

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
import Data.Type.Error
  ( Assert, MapSeq )
import Data.Type.List
  ( type (:++:) )
import Data.Type.Map
  ( Map, (:->)((:->))
  , ZipValue
  )
import Data.Type.Ord
  ( POrd(Max, (:<), (:<=))
  , PEnum(EnumFromTo, EnumFromOfCount)
  )
import {-# SOURCE #-} FIR.Layout
  ( Poke(SizeOf)
  , Layout(Locations)
  )
import FIR.Prim.Array
  ( Array(..), RuntimeArray )
import FIR.Prim.Singletons
  ( SScalarTy, ScalarWidth
  , ScalarFromSScalar
  , SKPrimTy(..), PrimTySing
  )
import FIR.Prim.Struct
  ( Struct(..)
  , LocationSlot(LocationSlot)
  , ShowLocation, ShowLocationSlot
  )
import Math.Linear
  ( V, M )
import qualified SPIRV.ScalarTy as SPIRV
  ( ScalarTy(Integer, Floating) )

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
    -> Availability
    -> SlotProvenance

data Availability
  = Available
  | Used

-----------------------------------------------------------------------------------------------------

-- Force evaluation of computation of all used slots, throwing a type error if any slots overlapped.
-- The overlap checking is performed in the type family 'AddSlotsCheckingOverlap',
-- which is recursively called by the type family 'Slots'.
type family ValidLayout (as :: [LocationSlot Nat :-> Type]) :: Constraint where
  ValidLayout as = Assert (MapSeq (Slots as)) ( () :: Constraint )

type family Slots
              ( as :: [LocationSlot Nat :-> Type] )
            :: Map (LocationSlot Nat) SlotProvenance
            where
  Slots '[] = '[]
  Slots ( (slot ':-> ty) ': slots)
    = AddSlotsCheckingOverlap (EnumSlots slot ty (PrimTySing ty)) (Slots slots)

type family AddSlotsCheckingOverlap
              ( locs       :: [ LocationSlot Nat :-> SlotProvenance ] )
              ( slots      :: Map (LocationSlot Nat) SlotProvenance   )
            :: Map (LocationSlot Nat) SlotProvenance
            where
  AddSlotsCheckingOverlap '[] slots = slots
  AddSlotsCheckingOverlap ( loc ': locs ) '[]
    = AddSlotsCheckingOverlap locs '[ loc ]
  AddSlotsCheckingOverlap
    ( ( slot ':-> Provenance base1 (_ :: SKPrimTy ty1) _ Used ) ': locs )
    ( ( slot ':-> Provenance base2 (_ :: SKPrimTy ty2) _ Used ) ': _    )
    = TypeError
      (    Text "Overlap at " :<>: Text (ShowLocationSlot slot ) :<>: Text ":"
      :$$: Text "  - " :<>: ShowType ty1 :<>: Text " with " :<>: Text (ShowLocationSlot base1) :<>: Text ","
      :$$: Text "  - " :<>: ShowType ty2 :<>: Text " with " :<>: Text (ShowLocationSlot base2) :<>: Text "."
      )
  AddSlotsCheckingOverlap
    ( ( slot ':-> Provenance base1 (sk1 :: SKPrimTy ty1) (sscalar1 :: SScalarTy scalar1) addAvail1 ) ': locs  )
    ( ( slot ':-> Provenance base2 (sk2 :: SKPrimTy ty2) (sscalar2 :: SScalarTy scalar2) addAvail2 ) ': slots )
    = If ( CompatibleSScalars sscalar1 sscalar2 )
        ( AddSlotsCheckingOverlap locs
          ( If ( addAvail1 == Used )
            ( slot ':-> Provenance base1 sk1 sscalar1 addAvail1 )
            ( slot ':-> Provenance base2 sk2 sscalar2 addAvail2 )
          ': slots
          )
        )
        ( TypeError
          (    Text "Incompatible scalars within " :<>: Text (ShowLocation slot) :<>: Text ":"
          :$$: Text "  - " :<>: ShowType ty1 :<>: Text " with " :<>: Text (ShowLocationSlot base1)
          :<>: Text " requires " :<>: ShowType scalar1 :<>: Text ","
          :$$: Text "  - " :<>: ShowType ty2 :<>: Text " with " :<>: Text (ShowLocationSlot base2)
          :<>: Text " requires " :<>: ShowType scalar2 :<>: Text "."
          )
        )
  AddSlotsCheckingOverlap
    ( (loc1 ':-> prov1) ': locs  )
    ( (loc2 ':-> prov2) ': slots )
    = AddSlotsCheckingOverlap locs
       ( If ( loc1 :< loc2 )
          ( (loc1 ':-> prov1) ': (loc2 ':-> prov2) ': slots )
          ( (loc2 ':-> prov2) ': AddSlotsCheckingOverlap '[ loc1 ':-> prov1 ] slots )
       )

type CompatibleSScalars
              ( s1 :: SScalarTy ty1 )
              ( s2 :: SScalarTy ty2 )
  = ( CompatibleScalars ( ScalarFromSScalar s1 ) ( ScalarFromSScalar s2 ) :: Bool )

type family CompatibleScalars
              ( s1 :: SPIRV.ScalarTy )
              ( s2 :: SPIRV.ScalarTy )
            :: Bool
            where
  CompatibleScalars ( SPIRV.Integer _ w ) ( SPIRV.Integer _ w ) = True
  CompatibleScalars ( SPIRV.Floating  w ) ( SPIRV.Floating  w ) = True
  CompatibleScalars _                     _                     = False

type NbLocationsUsed ( as :: [LocationSlot Nat :-> Type] )
  = ( NbLocationsOfSlots (Slots as) :: Nat )

type family NbLocationsOfSlots
              ( slots :: Map (LocationSlot Nat) SlotProvenance )
            :: Nat
            where
  NbLocationsOfSlots '[] = 0
  NbLocationsOfSlots ( ( slot ':-> Provenance startLoc ty scalar avail ) ': slots )
    = NbLocationsOfSlotsAcc
        startLoc
        startLoc
        ( ( slot ':-> Provenance startLoc ty scalar avail ) ': slots )

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
    = EnumFromOfCount loc ( SizeOf Locations ty `Div` 4 )

-- | Given a key-value map of locations and their provenances,
-- pads the initial and terminal components with a given provenance.
--
-- Should not be used for compound types for which
-- the locations used are not consecutive.
type family PadLocations
            ( slots   :: Map (LocationSlot Nat) SlotProvenance )
            ( padProv :: SlotProvenance )
          :: Map (LocationSlot Nat) SlotProvenance
          where
  PadLocations ( ( 'LocationSlot l c ':-> prov ) ': slots ) padProv
    = ( ( EnumFromOfCount ('LocationSlot l 0) c ) `ZipValue` padProv )
    :++:
    ( RightPadLocations ( ( 'LocationSlot l c ':-> prov ) ': slots ) padProv )

type family RightPadLocations
            ( slots   :: Map (LocationSlot Nat) SlotProvenance )
            ( padProv :: SlotProvenance )
          :: Map (LocationSlot Nat) SlotProvenance
          where
  RightPadLocations '[] _ = '[]
  RightPadLocations '[ 'LocationSlot l 3 ':-> prov ] _
    = '[ 'LocationSlot l 3 ':-> prov ]
  RightPadLocations '[ 'LocationSlot l c ':-> prov ] padProv
    = ( 'LocationSlot l c ':-> prov )
   ': ( EnumFromTo ( 'LocationSlot l (c+1) ) ( 'LocationSlot l 3 ) `ZipValue` padProv )
  RightPadLocations (slot ': slots) padProv
    = slot ': RightPadLocations slots padProv

-- | Enumerate all slots used by a type beginning in a given location.
type family EnumSlots
              ( slot :: LocationSlot Nat )
              ( ty   :: Type             )
              ( sing :: SKPrimTy ty      )
            :: Map (LocationSlot Nat) SlotProvenance
            where
  EnumSlots _ () SKUnit
    = TypeError
        ( Text "Unit type not supported in input/output." )
  EnumSlots _ Bool SKBool
    = TypeError
        ( Text "Input/output: booleans not supported. Convert to 'Word32' instead." )
  EnumSlots ('LocationSlot l c) ty (SKScalar s)
    = If ( c :<= 3 && ( c `Mod` 2 == 0 || ScalarWidth s :<= 32 ) )
        ( PadLocations
            ( ( EnumConsecutiveSlots ('LocationSlot l c) ty )
                `ZipValue`
                  ( Provenance ('LocationSlot l c) (SKScalar s :: SKPrimTy ty) s Used )
            )
            ( Provenance ('LocationSlot l c) (SKScalar s :: SKPrimTy ty) s Available )
        )
        ( TypeError
          (    Text "Input/output: cannot position scalar of type " :<>: ShowType ty
          :$$: Text " at location " :<>: ShowType l
          :<>: Text " with component " :<>: ShowType c :<>: Text "."
          )
        )
  EnumSlots ('LocationSlot l c) (V n a) (SKVector s)
    = If ( c == 0 || ( c `Mod` 2 == 0 && c :<= 3 && n :<= 2 && ScalarWidth s :<= 32 ) )
        ( PadLocations
            ( ( EnumConsecutiveSlots ('LocationSlot l c) (V n a) )
                `ZipValue`
                  ( Provenance ('LocationSlot l c) (SKVector s :: SKPrimTy (V n a)) s Used )
            )
            ( Provenance ('LocationSlot l c) (SKVector s :: SKPrimTy (V n a)) s Available )
        )
        ( TypeError
          (    Text "Input/output: cannot position vector of type "
          :<>: ShowType (V n a)
          :$$: Text "at location " :<>: ShowType l
          :<>: Text " with component " :<>: ShowType c :<>: Text "."
          )
        )
  EnumSlots ('LocationSlot l 0) (M m n a) (SKMatrix s)
    = ( EnumConsecutiveSlots ('LocationSlot l 0) (M m n a) )
      `ZipValue`
      ( Provenance ('LocationSlot l 0) (SKMatrix s :: SKPrimTy (M m n a)) s Used )
    -- no need to pad, as matrices automatically consume all components of each used location
  EnumSlots ('LocationSlot l c) (M m n a) (SKMatrix _)
    = TypeError
        (    Text "Input/output: cannot position matrix at location " :<>: ShowType l
        :<>: Text " with non-zero component " :<>: ShowType c :<>: Text "."
        )
  EnumSlots loc (Array 0 a) (SKArray _) = '[]
  EnumSlots loc (Array 1 a) (SKArray elt) = EnumSlots loc a elt
  EnumSlots ('LocationSlot l c) (Array n a) (SKArray elt)
    =  If ( c :<= 3 )
    -- (slight optimisation to have these in reverse lexicographic order)
        ( EnumSlots
                ( 'LocationSlot
                    ( l + 1 + (SizeOf Locations a `Div` 16) )
                    c
                )
                ( Array (n-1) a )
                ( SKArray elt :: SKPrimTy (Array (n-1) a) )
        :++: EnumSlots ('LocationSlot l c) a elt
        )
        ( TypeError
           (    Text "Input/output: cannot position array of type" :<>: ShowType (Array n a)
           :$$: Text "at location " :<>: ShowType l
           :<>: Text " with component " :<>: ShowType c :<>: Text "."
           :$$: Text "Component cannot be greater than 3."
           )
        )
  EnumSlots _ (RuntimeArray _) (SKRuntimeArray _)
    = TypeError
        ( Text "Input/output: runtime arrays not supported." )
  EnumSlots _ (Struct _) (SKStruct _)
    = TypeError
        ( Text "Input/output: structs not (yet?) supported." )
