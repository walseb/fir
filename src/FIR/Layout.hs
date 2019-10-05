{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Layout

This module is concerned with the memory layout of types for use with Vulkan.
There are three different situations one needs to account for:

  - uniform buffers must form a struct or array of structs,
  laid out according to the Extended alignment rules,
  - push constants and storage buffers must form a struct or array of structs,
  laid out according to the Base alignment rules,
  - vertex input data is specified with Location
  and Component information (see below).

This module automatically performs layout of types according to the above rules.

In the first two situations above, the shader that makes use of such structs
must provide explicit Offset and Stride decorations. These offsets
are computed by this module to avoid the user needing to manually specify them.

In the last situation, per-vertex/per-instance data is laid out in locations,
for use by the vertex shader. Each location holds four components of 4 bytes each.
The type-system checks the compliance of any user-supplied layout with the SPIR-V and Vulkan specifications,
in particular checking that the assignment of slots is non-overlapping.

For instance, suppose we want to lay out objects of the following types:

  [@A@] @Array 3 (V 2 Float)@

  [@B@] @Array 2 Float@

  [@C@] @Float@

  [@D@] @V 3 Double@

  [@E@] @Array 2 Double@

  [@F@] @M 2 3 Float@

We can lay out this data by specifying which location slots each type uses.
Consider for instance the following specification:

>  Struct
>    '[ Slot 0 0 ':-> Array 3 (V 2 Float)
>     , Slot 0 2 ':-> Array 2 Float
>     , Slot 1 3 ':-> Float
>     , Slot 3 0 ':-> V 3 Double
>     , Slot 4 2 ':-> Array 2 Double
>     , Slot 6 0 ':-> M 2 3 Float
>     ]

We can visualise this layout as follows,
each row representing a location (each consisting of 4 components):

\[
\begin{matrix}
  & 0                & 1                & 2                     & 3                     \\
0 & \color{brown}{A} & \color{brown}{A} & \color{RoyalBlue}{B}  & \circ                 \\
1 & \color{brown}{A} & \color{brown}{A} & \color{RoyalBlue}{B}  & \color{purple}{C}     \\
2 & \color{brown}{A} & \color{brown}{A} & \circ                 & \circ                 \\
3 & \color{Plum}{D}  & \color{Plum}{D}  & \color{Plum}{D}       & \color{Plum}{D}       \\
4 & \color{Plum}{D}  & \color{Plum}{D}  & \color{orange}{E}     & \color{orange}{E}     \\
5 & \circ            & \circ            & \color{orange}{E}     & \color{orange}{E}     \\
6 & \color{green}{F} & \color{green}{F} & \color{green}{\times} & \color{green}{\times} \\
7 & \color{green}{F} & \color{green}{F} & \color{green}{\times} & \color{green}{\times} \\
8 & \color{green}{F} & \color{green}{F} & \color{green}{\times} & \color{green}{\times}
\end{matrix}
\]

Here \( \circ \) denotes unused components that are still available for other data,
whereas \( \times \) denotes unused components that /cannot/ be filled with other data.

Note in particular that matrices use as many locations as they have columns, consuming each location in its entirety.

The specification guarantees that locations 0 to 15 will be available. The availability of further locations
will depend on the GPU and its implementation of Vulkan.

For further reference, see:

  - SPIR-V specification, 2.16.2 "Validation Rules for Shader Capabilities", bullet point 3,
  - SPIR-V specification, 2.18.1 "Memory Layout",
  - Vulkan specification, 14.1.4 "Location Assignment", and 14.1.5 "Component Assignment",
  - Vulkan specification, 14.5.2 "Descriptor Set Interface", and 14.5.4 "Offset and Stride Assignment".

-}

module FIR.Layout
  ( Layout(..)
  , Slots, SlotProvenance(Provenance)
  , ShowLocation
  , Poke(..), pokeArray
  , inferPointerLayout
  , roundUp, nextAligned
  )
  where

-- base
import Control.Monad
  ( foldM_ )
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If, type (&&), type (||) )
import Data.Type.Equality
  ( type (==) )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr
  ( Ptr, castPtr, plusPtr )
import qualified Foreign.Storable as Storable
  ( poke )
import GHC.TypeLits
  ( Symbol, AppendSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, KnownNat
  , type (<=), type (+), type (-), type (*)
  , Div, Mod
  )

-- containers
import qualified Data.Set as Set
  ( singleton, fromList, insert, union )

-- distributive
import Data.Distributive
  ( Distributive(distribute) )

-- half
import Numeric.Half
  ( Half, fromHalf )

-- mtl
import Control.Monad.Except
  ( MonadError, throwError )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import Data.Constraint.All
  ( AllDict(ConsDict)
  , All(allDict)
  )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.List
  ( type (:++:) )
import Data.Type.Map
  ( Map, (:->)((:->)), Value
  , Insert, ZipValue
  )
import Data.Type.Nat
  ( NextPositivePowerOf2, RoundUp )
import Data.Type.Ord
  ( POrd(Compare, Max, (:<=))
  , PEnum(Succ, Pred, FromEnum, ToEnum, IncFrom)
  )
import Data.Type.String
  ( ShowNat )
import FIR.Prim.Array
  ( Array(..), RuntimeArray )
import FIR.Prim.Singletons
  ( ScalarTy, SScalarTy, ScalarWidth
  , PrimTyMap(..), SPrimTyMap(..)
  , SPrimTy(..), SKPrimTy(..), PrimTySing
  , primTySing
  )
import qualified FIR.Prim.Singletons as Prim
  ( PrimTy )
import FIR.Prim.Struct
  ( Struct(..)
  , LocationSlot(LocationSlot)
  , FieldKind(LocationField)
  , StructFieldKind(fieldKind)
  )
import Math.Linear
  ( V, M(unM) )
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.PrimTy     as SPIRV
import           SPIRV.PrimTy
  ( PrimTy(..) ) -- field accessors for named field puns
import qualified SPIRV.ScalarTy   as SPIRV
import qualified SPIRV.Storage    as SPIRV
  ( StorageClass )
import qualified SPIRV.Storage    as Storage

--------------------------------------------------------------------------------------------

-- | Specification of alignment requirements.
data Layout
  = Base      -- ^ Layout used for storage buffers and push constants.
  | Extended  -- ^ Layout used for uniform buffers.
  | Locations -- ^ Layout used for vertex binding input data.
  deriving ( Eq, Show )

-- | Type-class for data that can be stored in memory,
-- following the specified alignment requirements.
class ( KnownNat (SizeOf    lay a)
      , KnownNat (Alignment lay a)
      ) => Poke a (lay :: Layout) where

  type SizeOf lay a :: Nat
  sizeOf :: Word32
  sizeOf = knownValue @(SizeOf lay a)

  type Alignment lay a :: Nat
  alignment :: Word32
  alignment = knownValue @(Alignment lay a)

  poke :: Ptr a -> a -> IO ()

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

type family NextAligned (size :: Nat) (ali :: Nat) :: Nat where
  NextAligned size ali = NextAlignedWithRemainder size ali (size `Mod` ali)

type family NextAlignedWithRemainder
              ( size :: Nat )
              ( ali  :: Nat )
              ( rem  :: Nat )
            :: Nat
            where
  NextAlignedWithRemainder size _   0   = size
  NextAlignedWithRemainder size ali rem = size + ali - rem

roundUp :: Integral a => a -> a -> a
roundUp n r = (n + r) - ( 1 + ( (n + r - 1) `mod` r ) )

pokeArray :: forall a lay. Poke a lay => Ptr a -> [a] -> IO ()
pokeArray ptr vals0 = go (fromIntegral $ sizeOf @a @lay) vals0 0
  where go _ [] _         = return ()
        go s (val:vals) n = do
          poke @a @lay ( ptr `plusPtr` n ) val
          go s vals (n + s)

nextAligned :: Word32 -> Word32 -> Word32
nextAligned size ali
  = case size `mod` ali of
         0 -> size
         x -> size + ali - x

instance Poke Word8 lay where
  type SizeOf    lay Word8 = 4
  type Alignment lay Word8 = 4
  poke ptr = Storable.poke (castPtr @_ @Word32 ptr) . fromIntegral

instance Poke Word16 lay where
  type SizeOf    lay Word16 = 4
  type Alignment lay Word16 = 4
  poke ptr = Storable.poke (castPtr @_ @Word32 ptr) . fromIntegral

instance Poke Word32 lay where
  type SizeOf    lay Word32 = 4
  type Alignment lay Word32 = 4
  poke = Storable.poke

instance Poke Word64 lay where
  type SizeOf    lay Word64 = 8
  type Alignment lay Word64 = 8
  poke = Storable.poke

instance Poke Int8 lay where
  type SizeOf    lay Int8 = 4
  type Alignment lay Int8 = 4
  poke ptr = Storable.poke (castPtr @_ @Int32 ptr) . fromIntegral

instance Poke Int16 lay where
  type SizeOf    lay Int16 = 4
  type Alignment lay Int16 = 4
  poke ptr = Storable.poke (castPtr @_ @Int32 ptr) . fromIntegral

instance Poke Int32 lay where
  type SizeOf    lay Int32 = 4
  type Alignment lay Int32 = 4
  poke = Storable.poke

instance Poke Int64 lay where
  type SizeOf    lay Int64 = 4
  type Alignment lay Int64 = 4
  poke = Storable.poke

instance Poke Half lay where
  type SizeOf    lay Half = 4
  type Alignment lay Half = 4
  poke ptr = Storable.poke (castPtr @_ @Float ptr) . fromHalf

instance Poke Float lay where
  type SizeOf    lay Float = 4
  type Alignment lay Float = 4
  poke = Storable.poke

instance Poke Double lay where
  type SizeOf    lay Double = 8
  type Alignment lay Double = 8
  poke = Storable.poke


instance ( Poke a lay
         , ScalarTy a
         , KnownNat n, 1 <= n
         , KnownNat (SizeOf lay (V n a))
         , KnownNat (Alignment lay (V n a))
         ) => Poke (V n a) lay where
  type SizeOf lay (V n a) = n * SizeOf lay a
  type Alignment lay (V n a)
    = SizeOf lay a * NextPositivePowerOf2 n
  poke ptr v
    = foldM_
        ( \accPtr elt -> poke @a @lay accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        v
    where
      off :: Int
      off = fromIntegral ( sizeOf @a @lay )

instance (Poke (V m a) Base, ScalarTy a, KnownNat n, KnownNat m, 1 <= n, 1 <= m)
       => Poke (M m n a) Base where
  type SizeOf Base (M m n a)
    = n * SizeOf Base (V m a)
  type Alignment Base (M m n a) = Alignment Base (V m a)
  poke ptr mat
    = foldM_
        ( \accPtr col -> poke @(V m a) @Base accPtr col *> pure (accPtr `plusPtr` colSize) )
        ( castPtr ptr )
        ( distribute . unM $ mat )
    where
      colSize = fromIntegral $ sizeOf @(V m a) @Base

instance (Poke (V m a) Extended, ScalarTy a, KnownNat n, KnownNat m, 1 <= n, 1 <= m)
       => Poke (M m n a) Extended where
  type SizeOf Extended (M m n a)
    = n * SizeOf Extended (V m a)
  type Alignment Extended (M m n a) = Alignment Extended (V m a)
  poke ptr mat
    = foldM_
        ( \accPtr col -> poke @(V m a) @Extended accPtr col *> pure (accPtr `plusPtr` colSize) )
        ( castPtr ptr )
        ( distribute . unM $ mat )
    where
      colSize = fromIntegral $ sizeOf @(V m a) @Extended

instance ( Poke (V m a) Locations
         , ScalarTy a
         , KnownNat n, KnownNat m, 1 <= n, 1 <= m
         , KnownNat (SizeOf Locations (M m n a))
         )
      => Poke (M m n a) Locations where
 type SizeOf Locations (M m n a)
   = n * ( SizeOf Locations (V m a) `RoundUp` 16 )
 type Alignment Locations (M m n a) = 16
 poke ptr mat
   = foldM_
       ( \accPtr col -> poke @(V m a) @Locations accPtr col *> pure (accPtr `plusPtr` colSize) )
       ( castPtr ptr )
       ( distribute . unM $ mat )
   where
    colSize :: Int
    colSize = fromIntegral ( sizeOf @(V m a) @Locations ) `roundUp` 16

instance (Poke a Base, Prim.PrimTy a, KnownNat n, 1 <= n)
       => Poke (Array n a) Base where
  type SizeOf Base (Array n a) = n * SizeOf Base a
  type Alignment Base (Array n a) = Alignment Base a
  poke ptr (MkArray arr)
    = foldM_
        ( \accPtr elt -> poke @a @Base accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        arr
    where
      off :: Int
      off = fromIntegral $ nextAligned (sizeOf @a @Base) (alignment @(Array n a) @Base)

instance ( Poke a Extended
         , Prim.PrimTy a
         , KnownNat n, 1 <= n
         , KnownNat (SizeOf Extended (Array n a))
         , KnownNat (Alignment Extended (Array n a))
         )
       => Poke (Array n a) Extended where
  type SizeOf Extended (Array n a)
    = n * NextAligned (SizeOf Extended a) (Alignment Extended (Array n a))
  type Alignment Extended (Array n a)
    = Alignment Extended a `RoundUp` 16
  poke ptr (MkArray arr)
    = foldM_
        ( \accPtr elt -> poke @a @Extended accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        arr
    where
      off :: Int
      off = fromIntegral $ nextAligned (sizeOf @a @Extended) (alignment @(Array n a) @Extended)

instance (Poke a Locations, Prim.PrimTy a, KnownNat n, 1 <= n)
       => Poke (Array n a) Locations where
  type SizeOf Locations (Array n a)
    = n * SizeOf Locations a
  type Alignment Locations (Array n a)
    = Alignment Locations a
  poke ptr (MkArray arr)
    = foldM_
        ( \accPtr elt -> poke @a @Locations accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        arr
    where
      off :: Int
      off = fromIntegral $ nextAligned (sizeOf @a @Locations) (alignment @(Array n a) @Locations `roundUp` 16)


class    ( Poke (Value x) lay ) => Pokeable (lay :: Layout) (x :: fld :-> Type) where
instance ( Poke (Value x) lay ) => Pokeable (lay :: Layout) (x :: fld :-> Type) where

structPoke :: forall (fld :: Type) (lay :: Layout) (as :: [fld :-> Type]).
              ( PrimTyMap as, All (Pokeable lay) as )
           => Word32 -> Ptr (Struct as) -> Struct as -> IO ()
structPoke ali ptr struct = case primTyMapSing @_ @as of
  SNil -> pure ()
  scons@SCons -> case scons of
    ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
      case (struct, allDict @(Pokeable lay) @as) of
        ( b :& bs, ConsDict ) ->
          let off = fromIntegral $ nextAligned (sizeOf @b @lay) ali
          in  poke @b @lay (castPtr ptr) b *> structPoke @fld @lay @bs ali (ptr `plusPtr` off) bs


instance ( PrimTyMap as
         , All (Pokeable Base) as
         , KnownNat (SizeOf Base (Struct as))
         , KnownNat (Alignment Base (Struct as))
         ) => Poke (Struct as) Base where
  type SizeOf Base (Struct as)
    = SumRoundedUpSizes Base (MaximumAlignment Base as) as
  type Alignment Base (Struct as)
    = MaximumAlignment Base as
  poke = structPoke @_ @Base @as (alignment @(Struct as) @Base)


instance ( PrimTyMap as
         , All (Pokeable Extended) as
         , KnownNat (SizeOf Extended (Struct as))
         , KnownNat (Alignment Extended (Struct as))
         ) => Poke (Struct as) Extended where
  type SizeOf Extended (Struct as)
    = SumRoundedUpSizes Extended (MaximumAlignment Extended as) as
  type Alignment Extended (Struct as)
    = MaximumAlignment Extended as
  poke = structPoke @_ @Extended @as (alignment @(Struct as) @Extended)

instance
  ( TypeError
      (    Text "Structure contains insufficient layout information."
      :$$: Text ""
      :$$: Text "To use the 'Locations' layout, location/component information needs to be supplied, such as:"
      :$$: Text "    Struct '[ Slot 0 0 ':-> V 2 Float, Slot 0 2 ':-> V 2 Float ],"
      :$$: Text "where 'Slot l c' refers to the interface slot in location 'l' with component 'c',"
      :$$: Text "with 0 <= l < maxVertexInputAttributes (at least 16), 0 <= b < 4."
      )
  )
  => Poke (Struct (as :: [Symbol :-> Type])) Locations
  where
    type SizeOf Locations (Struct (as :: [Symbol :-> Type]))
      = 0
    type Alignment Locations (Struct (as :: [Symbol :-> Type]))
      = 1
    poke = error "unreachable"

-- TODO: refactor this using 'Data.Constraint.All'
data PokeSlotsDict (as :: [LocationSlot Nat :-> Type]) :: Type where
  NoSlotsPoke  :: PokeSlotsDict '[]
  PokeSlotDict :: ( Poke a Locations
                  , Known (LocationSlot Nat) slot
                  , PokeSlots as
                  , NoOverlappingSlots as
                  , KnownNat (SizeOf    Locations (Struct as))
                  , KnownNat (Alignment Locations (Struct as))
                  )
               => PokeSlotsDict ( (slot ':-> a) ': as )

class PokeSlots (as :: [LocationSlot Nat :-> Type]) where
  pokeSlotsDict :: PokeSlotsDict as
instance PokeSlots '[] where
  pokeSlotsDict = NoSlotsPoke
instance ( Poke a Locations
         , Known (LocationSlot Nat) loc
         , PokeSlots as
         , NoOverlappingSlots as
         , KnownNat (SizeOf    Locations (Struct as))
         , KnownNat (Alignment Locations (Struct as))
         )
       => PokeSlots( (loc ':-> a) ': as )
       where
  pokeSlotsDict = PokeSlotDict

instance forall (as :: [LocationSlot Nat :-> Type])
       . ( PrimTyMap as
         , PokeSlots as
         , NoOverlappingSlots as
         , KnownNat (SizeOf Locations (Struct as))
         , KnownNat (Alignment Locations (Struct as))
         ) => Poke (Struct as) Locations where
  type SizeOf Locations (Struct as)
    = 16 * NbLocationsUsed as
  type Alignment Locations (Struct as)
    = 16
  poke ptr struct = case primTyMapSing @_ @as of
    SNil -> pure ()
    scons@SCons -> case scons of
      ( _ :: SPrimTyMap ((loc ':-> b) ': bs) ) ->
        case (struct, pokeSlotsDict @as) of
          ( b :& bs, PokeSlotDict ) ->
            let LocationSlot l c = knownValue @loc
                off = fromIntegral $ 16 * l + 4 * c
            in do
              -- if we are poking another struct with location information,
              -- we want to keep the pointer as is as opposed to adding an offset to it
              case primTySing @b of
                sStruct@SStruct
                  | ( _ :: SPrimTy (Struct (cs :: [fld :-> Type]) ) ) <- sStruct
                  , LocationField <- fieldKind @fld
                  -> poke @b @Locations (castPtr ptr              ) b
                _ -> poke @b @Locations (castPtr ptr `plusPtr` off) b
              poke @(Struct bs) @Locations (castPtr ptr) bs

type family NoOverlappingSlots (as :: [LocationSlot Nat :-> Type]) :: Constraint where
  NoOverlappingSlots as = SlotsDontOverlap (Slots as)

-- force evaluation of computation of all used slots, throwing a type error if any slots overlapped
-- the overlap checking is performed in the type family 'AddSlotsCheckingOverlap',
-- which is recursively called by the type family 'Slots'
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

type family ShowLocation
              ( slot :: LocationSlot Nat )
            :: Symbol
            where
  ShowLocation ('LocationSlot l c)
    = "location " `AppendSymbol` ShowNat l `AppendSymbol` ", component " `AppendSymbol` ShowNat c

type family Slots
              ( as :: [LocationSlot Nat :-> Type] )
            :: Map (LocationSlot Nat) SlotProvenance
            where
  Slots '[] = '[]
  Slots ( (slot ':-> ty) ': slots)
    = AddSlotsCheckingOverlap (EnumSlots slot ty (PrimTySing ty)) (Slots slots)

type family NbLocationsUsed
              ( as :: [LocationSlot Nat :-> Type] )
            :: Nat
            where
  NbLocationsUsed as = NbLocationsOfSlots (Slots as)

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
          (    Text "Vertex binding: cannot position value of type " :<>: ShowType ty
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
          (    Text "Vertex binding: cannot position vector of type"
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

type family SumRoundedUpSizes (lay :: Layout) (ali :: Nat) (as :: [fld :-> Type]) :: Nat where
  SumRoundedUpSizes _   _   '[]                    = 0
  SumRoundedUpSizes lay ali ( ( _ ':-> a ) ': as ) =
    NextAligned (SizeOf lay a) ali + SumRoundedUpSizes lay ali as

type family MaximumAlignment (lay :: Layout) (as :: [fld :-> Type]) :: Nat where
  MaximumAlignment lay '[] = 1
  MaximumAlignment Extended ( ( _ ':-> a ) ': as )
    = Max (Alignment Extended a) (MaximumAlignment Extended as) `RoundUp` 16
  MaximumAlignment lay ( ( _ ':-> a ) ': as )
    = Max (Alignment lay a) (MaximumAlignment lay as)

--------------------------------------------------------------------------------------------

maxMemberAlignment
   :: MonadError e m
   => ( SPIRV.PrimTy -> m Word32 ) -> [(ignore1, SPIRV.PrimTy, ignore2)] -> m Word32
maxMemberAlignment f as = foldr ( \ a b -> max <$> ( f . ( \(_,ty,_) -> ty ) ) a <*> b ) (pure 0) as

scalarAlignment :: MonadError ShortText m => SPIRV.PrimTy -> m Word32
scalarAlignment (SPIRV.Scalar (SPIRV.Integer  _ w)) = pure (SPIRV.width w `quot` 8)
scalarAlignment (SPIRV.Scalar (SPIRV.Floating   w)) = pure (SPIRV.width w `quot` 8)
scalarAlignment (SPIRV.Vector        {eltTy}) = scalarAlignment eltTy
scalarAlignment (SPIRV.Matrix      {entryTy}) = scalarAlignment (SPIRV.Scalar entryTy)
scalarAlignment (SPIRV.Array         {eltTy}) = scalarAlignment eltTy
scalarAlignment (SPIRV.RuntimeArray  {eltTy}) = scalarAlignment eltTy
scalarAlignment (SPIRV.Struct       {eltTys}) = maxMemberAlignment scalarAlignment eltTys
scalarAlignment ty
  = throwError
      ( "Layout: cannot compute scalar alignment of type " <> ShortText.pack (show ty) <> "." )

baseAlignment :: MonadError ShortText m => SPIRV.PrimTy -> m Word32
baseAlignment sc@SPIRV.Scalar {} = scalarAlignment sc
baseAlignment (SPIRV.Vector   {size, eltTy})
  | size == 0 = throwError "Layout: cannot compute base alignment of empty vector."
  | otherwise = ( 2 ^ ceiling @Float @Word32 (logBase 2 (fromIntegral size)) * ) <$> scalarAlignment eltTy
baseAlignment (SPIRV.Array          {eltTy}) = baseAlignment eltTy
baseAlignment (SPIRV.RuntimeArray   {eltTy}) = baseAlignment eltTy
baseAlignment (SPIRV.Struct        {eltTys}) = maxMemberAlignment baseAlignment eltTys
baseAlignment (SPIRV.Matrix {rows, entryTy}) = baseAlignment (SPIRV.Vector rows (SPIRV.Scalar entryTy)) -- assumed column major
baseAlignment ty
  = throwError
      ( "Layout: cannot compute base alignment of type " <> ShortText.pack (show ty) <> "." )

extendedAlignment :: MonadError ShortText m => SPIRV.PrimTy -> m Word32
extendedAlignment (SPIRV.Array         {eltTy}) = (`roundUp` 16) <$> extendedAlignment eltTy
extendedAlignment (SPIRV.RuntimeArray  {eltTy}) = (`roundUp` 16) <$> extendedAlignment eltTy
extendedAlignment (SPIRV.Struct       {eltTys}) = (`roundUp` 16) <$> maxMemberAlignment extendedAlignment eltTys
extendedAlignment ty                            = baseAlignment ty

requiredAlignment :: MonadError ShortText m => SPIRV.StorageClass -> m ( SPIRV.PrimTy -> m Word32 )
requiredAlignment Storage.Uniform       = pure extendedAlignment
requiredAlignment Storage.StorageBuffer = pure baseAlignment
requiredAlignment Storage.PushConstant  = pure baseAlignment
requiredAlignment storage
  = throwError
      ( "Layout: unsupported storage class " <> ShortText.pack (show storage) <> "." )

inferLayout :: MonadError ShortText m
            => SPIRV.AggregateUsage
            -> SPIRV.Decorations
            -> SPIRV.StorageClass
            -> SPIRV.PrimTy
            -> m SPIRV.PrimTy
inferLayout _ _ storageClass ty
  | storageClass `elem`
      [ Storage.Image    , Storage.UniformConstant
      , Storage.Workgroup, Storage.CrossWorkgroup
      , Storage.Private  , Storage.Function
      , Storage.Generic  , Storage.AtomicCounter
      ]
  = pure ty
inferLayout usage decs storageClass (SPIRV.Array lg (SPIRV.Struct as sdecs _) adecs _)
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = pure $ SPIRV.Array lg (SPIRV.Struct as (Set.insert SPIRV.Block sdecs) usage) (Set.union decs adecs) usage
  | otherwise
  = do
      f <- requiredAlignment storageClass
      ali <- f (SPIRV.Struct as sdecs usage)
      laidOutMembers <- layoutStructMembersWith f ali as
      let laidOutStruct = SPIRV.Struct laidOutMembers (Set.insert SPIRV.Block sdecs) SPIRV.NotForBuiltins
      pure ( SPIRV.Array lg laidOutStruct (Set.union decs adecs) SPIRV.NotForBuiltins )
inferLayout _ decs storageClass (SPIRV.RuntimeArray (SPIRV.Struct as sdecs _) adecs _)
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = throwError "'inferLayout': cannot use run-time arrays in 'Input'/'Output', must use a uniform or storage buffer."
  | otherwise
  = do
    f <- requiredAlignment storageClass
    ali <- f (SPIRV.Struct as sdecs SPIRV.NotForBuiltins)
    laidOutMembers <- layoutStructMembersWith f ali as
    let laidOutStruct = SPIRV.Struct laidOutMembers (Set.insert SPIRV.Block sdecs) SPIRV.NotForBuiltins
    pure ( SPIRV.RuntimeArray laidOutStruct (Set.union decs adecs) SPIRV.NotForBuiltins )
inferLayout usage decs storageClass struct@(SPIRV.Struct as sdecs _)
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = pure $ SPIRV.Struct as (Set.insert SPIRV.Block (Set.union decs sdecs)) usage
  | otherwise
  = do
      f <- requiredAlignment storageClass
      ali <- f struct
      laidOutMembers <- layoutStructMembersWith f ali as
      pure ( SPIRV.Struct laidOutMembers (Set.insert SPIRV.Block (Set.union decs sdecs)) SPIRV.NotForBuiltins )
inferLayout _ _ storageClass ty
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = pure ty
  | otherwise
  = throwError
    (  "'inferLayout': unsupported type " <> ShortText.pack (show ty)
    <> " in conjunction with storage class "
    <> ShortText.pack (show storageClass) <> "."
    )

inferPointerLayout
  :: MonadError ShortText m
  => SPIRV.AggregateUsage
  -> SPIRV.Decorations
  -> SPIRV.PointerTy
  -> m SPIRV.PointerTy
inferPointerLayout usage decs (SPIRV.PointerTy storageClass ty)
  = SPIRV.PointerTy storageClass <$> inferLayout usage decs storageClass ty


layoutWith :: MonadError ShortText m
           => ( SPIRV.PrimTy -> m Word32 ) -> SPIRV.PrimTy -> m SPIRV.PrimTy
layoutWith _ ty@(SPIRV.Scalar {}) = pure ty
layoutWith _ ty@(SPIRV.Vector {}) = pure ty
layoutWith _ mat@(SPIRV.Matrix {})
  = pure mat -- cannot decorate matrix directly, must do so indirectly using arrays and structs
layoutWith f arr@(SPIRV.Array l mat@(SPIRV.Matrix {}) decs usage) = do
  arrStride  <- f arr
  matStride  <- f mat
  pure ( SPIRV.Array l mat
            ( Set.union
                ( Set.fromList [ SPIRV.ArrayStride arrStride, SPIRV.MatrixStride matStride, SPIRV.ColMajor ] )
                decs
            )
            usage
        )
layoutWith f arr@(SPIRV.Array l elt decs usage) = do
  arrStride  <- f arr
  laidOutElt <- layoutWith f elt
  pure ( SPIRV.Array l laidOutElt (Set.insert (SPIRV.ArrayStride arrStride) decs) usage )
layoutWith f arr@(SPIRV.RuntimeArray mat@(SPIRV.Matrix {}) decs usage) = do
  arrStride  <- f arr
  matStride  <- f mat
  pure ( SPIRV.RuntimeArray mat
            ( Set.union
                ( Set.fromList [ SPIRV.ArrayStride arrStride, SPIRV.MatrixStride matStride, SPIRV.ColMajor ] )
                decs
            )
            usage
        )
layoutWith f arr@(SPIRV.RuntimeArray elt decs usage) = do
  arrStride  <- f arr
  laidOutElt <- layoutWith f elt
  pure ( SPIRV.RuntimeArray laidOutElt (Set.insert (SPIRV.ArrayStride arrStride) decs) usage )
layoutWith f struct@(SPIRV.Struct as decs structUsage) = do
  ali <- f struct
  laidOutMembers <- layoutStructMembersWith f ali as
  pure ( SPIRV.Struct laidOutMembers decs structUsage )
layoutWith _ ty
  = throwError ( "'layoutWith': unsupported type " <> ShortText.pack (show ty) <> "." )

layoutStructMembersWith
    :: forall m. MonadError ShortText m
    => ( SPIRV.PrimTy -> m Word32 )
    -> Word32
    -> [ ( Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations ) ]
    -> m [ ( Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations ) ]
layoutStructMembersWith f ali as = fst <$> go 0 as
  where
    go :: Word32
       ->     [ ( Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations ) ]
       -> m ( [ ( Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations ) ], Word32 )
    go offset []               = pure ( [], offset )
    go offset ((name, ty, decs):nxt) = do
        (newOffset, laidOutTy, newDecs)
           <- case ty of
                SPIRV.Matrix { cols } -> do
                  stride <- f ty
                  laidOutTy <- layoutWith f ty
                  pure ( nextAligned (offset + cols * stride) ali
                       , laidOutTy
                       , Set.fromList [ SPIRV.Offset offset, SPIRV.MatrixStride stride, SPIRV.ColMajor ]
                       )
                SPIRV.Array { size } -> do
                  eltAlignment <- f ty
                  laidOutTy <- layoutWith f ty
                  pure ( nextAligned (offset + size * eltAlignment) ali
                       , laidOutTy
                       , Set.singleton (SPIRV.Offset offset)
                       )
                SPIRV.RuntimeArray {} -> do
                  laidOutTy <- layoutWith f ty
                  pure ( maxBound -- should not come into play... we should be OK if this is the last struct member
                       , laidOutTy
                       , Set.singleton (SPIRV.Offset offset)
                       )
                SPIRV.Struct as' _ _ -> do
                  ( _, totalSize ) <- go 0 as'
                  laidOutTy <- layoutWith f ty
                  pure ( nextAligned (offset + totalSize) ali
                       , laidOutTy -- wasteful duplicated computation here
                       , Set.singleton (SPIRV.Offset offset)
                       )
                _ -> do
                  align     <- f ty
                  laidOutTy <- layoutWith f ty
                  pure ( nextAligned (offset + align) ali
                       , laidOutTy
                       , Set.singleton (SPIRV.Offset offset)
                       )
        ( , offset) . ( (name, laidOutTy, Set.union newDecs decs) : ) . fst <$> go newOffset nxt
