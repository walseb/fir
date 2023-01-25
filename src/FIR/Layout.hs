{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
  - vertex input data is specified with Location and Component information.

This module automatically performs layout of types according to the above rules.
See also "FIR.Validation.Layout", which performs validation of user-supplied layouts.

In the first two situations above, the shader that makes use of such structs
must provide explicit Offset and Stride decorations. These offsets
are computed by this module to avoid the user needing to manually specify them.

In the last situation, per-vertex/per-instance data is laid out in locations,
for use by the vertex shader. See "FIR.Validation.Layout".

For further reference, see:

  - SPIR-V specification, 2.16.2 "Validation Rules for Shader Capabilities", bullet point 3,
  - SPIR-V specification, 2.18.1 "Memory Layout",
  - Vulkan specification, 14.1.4 "Location Assignment", and 14.1.5 "Component Assignment",
  - Vulkan specification, 14.5.2 "Descriptor Set Interface", and 14.5.4 "Offset and Stride Assignment".

-}

module FIR.Layout
  ( Layout(..), Components
  , Poke(..), pokeArrayOff, pokeArray
  , inferPointerLayout
  , roundUp, nextAligned
  , primTySizeAndAli
  )
  where

-- base
import Control.Arrow
  ( first )
import Control.Monad
  ( foldM_ )
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(..) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr
  ( Ptr, castPtr, plusPtr )
import qualified Foreign.Storable as Storable
  ( poke )
import GHC.TypeLits
  ( Symbol, SomeSymbol(..)
  , TypeError, ErrorMessage(..)
  , someSymbolVal
  )
import GHC.TypeNats
  ( Nat, KnownNat, SomeNat(..)
  , type (<=), type (<=?), type (+), type (-), type (*)
  , Div, Mod
  , someNatVal
  )
import Numeric.Natural
  ( Natural )
import Unsafe.Coerce
  ( unsafeCoerce )

-- containers
import qualified Data.Set as Set
  ( empty, fromList, insert, singleton, union )

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
  ( Known, knownValue )
import Data.Type.Map
  ( (:->)((:->)), Value )
import Data.Type.Nat
  ( NextPositivePowerOf2, RoundUp )
import Data.Type.POrd
  ( POrd(Max) )
import FIR.Prim.Array
  ( Array(..) )
import FIR.Prim.RayTracing
  ( AccelerationStructure(..) )
import FIR.Prim.Struct
  ( Struct(..)
  , LocationSlot(LocationSlot)
  , FieldKind(..)
  , StructFieldKind(fieldKind)
  )
import FIR.Prim.Types
  ( ScalarTy, PrimTyMap(..), SPrimTyMap(..)
  , SPrimTy(..)
  , primTySing
  )
import qualified FIR.Prim.Types as Prim
  ( PrimTy )
import Math.Algebra.Class
  ( AdditiveGroup, Semiring )
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
  deriving stock ( Eq, Show )

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

type Components a = ( SizeOf Locations a `Div` 4 :: Nat )

type family NextAligned (size :: Nat) (ali :: Nat) :: Nat where
  NextAligned size ali = NextAlignedWithRemainder size ali (size `Mod` ali)

type family NextAlignedWithRemainder
              ( size :: Nat )
              ( ali  :: Nat )
              ( rem  :: Nat )
            :: Nat
            where
  NextAlignedWithRemainder size _   0   = size
  NextAlignedWithRemainder size ali rem = size + (ali - rem)

type family SumRoundedUpSizes (lay :: Layout) (as :: [fld :-> Type]) (acc :: Nat) :: Nat where
  SumRoundedUpSizes _   '[]                    acc = acc
  SumRoundedUpSizes lay ( ( _ ':-> a ) ': as ) acc = SumRoundedUpSizes lay as ( NextAligned acc ( Alignment lay a ) + SizeOf lay a )

type family MaximumAlignment (lay :: Layout) (as :: [fld :-> Type]) :: Nat where
  MaximumAlignment _   '[]                    = 1
  MaximumAlignment lay ( ( _ ':-> a ) ': as ) = Max ( Alignment lay a ) ( MaximumAlignment lay as )

roundUp :: Integral a => a -> a -> a
roundUp n r = (n + r) - ( 1 + ( (n + r - 1) `mod` r ) )

-- | Write elements to an array, whose start is given by a pointer.
pokeArray :: forall a lay. ( Poke a lay, Poke (Array 1 a) lay ) => Ptr a -> [a] -> IO ()
pokeArray ptr = pokeArrayOff @a @lay ptr 0

-- | Write elements to an array, starting from the given index offset.
pokeArrayOff :: forall a lay. ( Poke a lay, Poke (Array 1 a) lay ) => Ptr a -> Int -> [a] -> IO ()
pokeArrayOff ptr off vals0
  | off < 0   = pure ()
  | otherwise = go ( ptr `plusPtr` ( sz * off ) ) vals0
  where
    sz :: Int
    sz = fromIntegral $ nextAligned (sizeOf @a @lay) (alignment @(Array 1 a) @lay)
    go :: Ptr a -> [a] -> IO ()
    go _    []         = pure ()
    go ptr' (val:vals) = do
      poke @a @lay ptr' val
      go ( ptr' `plusPtr` sz ) vals

nextAligned :: Integral a => a -> a -> a
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

instance TypeError ( Text "Cannot store Boolean types." ) => Poke Bool lay where
  type SizeOf    lay Bool = 4
  type Alignment lay Bool = 4
  poke = error "unreachable"

-- Acceleration structures are just represented as Word64 address
instance Poke AccelerationStructure lay where
  type SizeOf    lay AccelerationStructure = 8
  type Alignment lay AccelerationStructure = 8
  poke ptr ( AccelerationStructureFromWord64 word64 ) = poke @Word64 @lay ( castPtr ptr ) word64

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
        ( unM mat )
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
        ( unM mat )
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
       ( unM mat )
   where
    colSize :: Int
    colSize = fromIntegral ( sizeOf @(V m a) @Locations ) `roundUp` 16

instance ( Poke a Base, Prim.PrimTy a, KnownNat n, 1 <= n
         , KnownNat ( SizeOf Base ( Array n a ) )
         )
       => Poke (Array n a) Base where
  type SizeOf Base (Array n a) = n * ( NextAligned ( SizeOf Base a ) ( Alignment Base (Array n a) ) )
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
         , KnownNat ( SizeOf    Extended ( Array n a ) )
         , KnownNat ( Alignment Extended ( Array n a ) )
         )
       => Poke (Array n a) Extended where
  type SizeOf Extended (Array n a)
    = n * ( NextAligned ( SizeOf Extended a ) ( Alignment Extended (Array n a) ) )
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
structPoke currOff ptr struct = case primTyMapSing @_ @as of
  SNil -> pure ()
  scons@SCons -> case scons of
    ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
      case (struct, allDict @(Pokeable lay) @as) of
        ( b :& bs, ConsDict ) -> do
          let
            off :: Word32
            off = nextAligned currOff ( alignment @b @lay )
          poke @b @lay ( castPtr ptr `plusPtr` ( fromIntegral off ) ) b
          structPoke @fld @lay @bs ( off + sizeOf @b @lay ) ( castPtr ptr ) bs

instance ( PrimTyMap as
         , All (Pokeable Base) as
         , KnownNat (SizeOf Base (Struct as))
         , KnownNat (Alignment Base (Struct as))
         ) => Poke (Struct as) Base where
  type SizeOf Base (Struct as)
    = SumRoundedUpSizes Base as 0
  type Alignment Base (Struct as)
    = MaximumAlignment Base as
  poke = structPoke @_ @Base @as 0

instance ( PrimTyMap as
         , All (Pokeable Extended) as
         , KnownNat (SizeOf Extended (Struct as))
         , KnownNat (Alignment Extended (Struct as))
         ) => Poke (Struct as) Extended where
  type SizeOf Extended (Struct as)
    = SumRoundedUpSizes Extended as 0 `RoundUp` 16
  type Alignment Extended (Struct as)
    = MaximumAlignment Extended as `RoundUp` 16
  poke = structPoke @_ @Extended @as 0

instance
  ( TypeError
      (    Text "Structure contains insufficient layout information."
      :$$: Text ""
      :$$: Text "To use the 'Locations' layout, location/component information needs to be supplied, such as:"
      :$$: Text "    Struct '[ Slot 0 0 ':-> V 2 Float, Slot 0 2 ':-> V 2 Float ],"
      :$$: Text "where 'Slot l c' refers to the interface slot in location 'l' with component 'c',"
      :$$: Text "with 0 <= l < maxVertexInputAttributes (at least 16), 0 <= c < 4."
      )
  )
  => Poke (Struct (as :: [Symbol :-> Type])) Locations
  where
    type SizeOf Locations (Struct (as :: [Symbol :-> Type]))
      = 16
    type Alignment Locations (Struct (as :: [Symbol :-> Type]))
      = 16
    poke = error "unreachable"

-- TODO: refactor this using 'Data.Constraint.All'
data PokeSlotsDict (as :: [LocationSlot Nat :-> Type]) :: Type where
  NoSlotsPoke  :: PokeSlotsDict '[]
  PokeSlotDict :: ( Poke a Locations
                  , Known (LocationSlot Nat) slot
                  , PokeSlots as
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
         , KnownNat (SizeOf    Locations (Struct as))
         , KnownNat (Alignment Locations (Struct as))
         )
       => PokeSlots ( (loc ':-> a) ': as )
       where
  pokeSlotsDict = PokeSlotDict

instance forall (as :: [LocationSlot Nat :-> Type])
       . ( PrimTyMap as
         , PokeSlots as
         , KnownNat (SizeOf Locations (Struct as))
         , KnownNat (Alignment Locations (Struct as))
         ) => Poke (Struct as) Locations where
  type SizeOf Locations (Struct as)
    = SumSizeOfLocations as
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

type family SumSizeOfLocations (as :: [LocationSlot Nat :-> Type]) :: Nat where
  SumSizeOfLocations '[] = 0
  SumSizeOfLocations ( ( _ ':-> a ) ': as )
    = ( SizeOf Locations a `RoundUp` 16 ) + SumSizeOfLocations as

--------------------------------------------------------------------------------------------
-- Computing layout decorations necessary for SPIR-V.

requiredLayout :: MonadError ShortText m => SPIRV.StorageClass -> m Layout
requiredLayout Storage.Uniform                                 = pure Extended
requiredLayout Storage.StorageBuffer                           = pure Base
requiredLayout Storage.PushConstant                            = pure Base
requiredLayout (Storage.RayStorage Storage.ShaderRecordBuffer) = pure Base
requiredLayout storage = throwError
  ( "'requiredLayout': unsupported storage class " <> ShortText.pack (show storage) <> "." )

inferLayout :: MonadError ShortText m
            => SPIRV.AggregateUsage
            -> SPIRV.Decorations
            -> SPIRV.StorageClass
            -> SPIRV.PrimTy
            -> m SPIRV.PrimTy
inferLayout _ _ storageClass ty
  | storageClass `notElem`
      [ Storage.Uniform, Storage.StorageBuffer, Storage.PushConstant
      , Storage.RayStorage Storage.ShaderRecordBuffer
      , Storage.Input, Storage.Output
      ]
  = pure ty
inferLayout usage decs storageClass (SPIRV.Array lg (SPIRV.Struct as sdecs _) adecs _)
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = pure $ SPIRV.Array lg (SPIRV.Struct as (Set.insert SPIRV.Block sdecs) usage) (Set.union decs adecs) usage
  | otherwise
  = do
      lay <- requiredLayout storageClass
      laidOutMembers <- fst <$> layoutStructMembers lay 0 as
      let laidOutStruct = SPIRV.Struct laidOutMembers (Set.insert SPIRV.Block sdecs) SPIRV.NotForBuiltins
      pure ( SPIRV.Array lg laidOutStruct (Set.union decs adecs) SPIRV.NotForBuiltins )
inferLayout usage decs storageClass (SPIRV.Struct as sdecs _)
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = pure $ SPIRV.Struct as (Set.insert SPIRV.Block (Set.union decs sdecs)) usage
  | otherwise
  = do
      lay <- requiredLayout storageClass
      laidOutMembers <- fst <$> layoutStructMembers lay 0 as
      pure ( SPIRV.Struct laidOutMembers (Set.insert SPIRV.Block (Set.union decs sdecs)) SPIRV.NotForBuiltins )
inferLayout _ _ _ SPIRV.AccelerationStructure = pure SPIRV.AccelerationStructure
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

layout :: MonadError ShortText m => Layout -> Word32 -> SPIRV.PrimTy -> m SPIRV.PrimTy
layout _ _ ty@(SPIRV.Scalar {}) = pure ty
layout _ _ ty@(SPIRV.Vector {}) = pure ty
layout _ _ mat@(SPIRV.Matrix {})
  = pure mat -- cannot decorate matrix directly, must do so indirectly using arrays and structs
layout lay _ (SPIRV.Array l mat@(SPIRV.Matrix { rows, entryTy }) decs usage) = do
  arrStride <- uncurry nextAligned <$> primTySizeAndAli lay ( SPIRV.Array 1 mat Set.empty SPIRV.NotForBuiltins )
  matStride <- uncurry nextAligned <$> primTySizeAndAli lay ( SPIRV.Vector rows ( SPIRV.Scalar entryTy ) )
  pure ( SPIRV.Array l mat
            ( Set.union
                ( Set.fromList [ SPIRV.ArrayStride arrStride, SPIRV.MatrixStride matStride, SPIRV.ColMajor ] )
                decs
            )
            usage
        )
layout lay off (SPIRV.Array l elt decs usage) = do
  arrStride  <- uncurry nextAligned <$> primTySizeAndAli lay ( SPIRV.Array 1 elt Set.empty SPIRV.NotForBuiltins )
  laidOutElt <- layout lay off elt
  pure ( SPIRV.Array l laidOutElt (Set.insert (SPIRV.ArrayStride arrStride) decs) usage )
layout lay _ (SPIRV.RuntimeArray mat@(SPIRV.Matrix { rows, entryTy }) decs usage) = do
  arrStride <- uncurry nextAligned <$> primTySizeAndAli lay ( SPIRV.Array 1 mat Set.empty SPIRV.NotForBuiltins )
  matStride <- uncurry nextAligned <$> primTySizeAndAli lay ( SPIRV.Vector rows ( SPIRV.Scalar entryTy ) )
  pure ( SPIRV.RuntimeArray mat
            ( Set.union
                ( Set.fromList [ SPIRV.ArrayStride arrStride, SPIRV.MatrixStride matStride, SPIRV.ColMajor ] )
                decs
            )
            usage
        )
layout lay off (SPIRV.RuntimeArray elt decs usage) = do
  arrStride  <- uncurry nextAligned <$> primTySizeAndAli lay ( SPIRV.Array 1 elt Set.empty SPIRV.NotForBuiltins )
  laidOutElt <- layout lay off elt
  pure ( SPIRV.RuntimeArray laidOutElt (Set.insert (SPIRV.ArrayStride arrStride) decs) usage )
layout lay off (SPIRV.Struct as decs structUsage) = do
  ali <- structAlignment lay as
  let
    off' :: Word32
    off' = nextAligned off ali
  laidOutMembers <- fst <$> layoutStructMembers lay off' as
  pure ( SPIRV.Struct laidOutMembers decs structUsage )
layout _ _ ty
  = throwError ( "'layout': unsupported type " <> ShortText.pack (show ty) <> "." )

structAlignment
  :: MonadError ShortText m
  => Layout 
  -> [ ( Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations ) ]
  -> m Word32
structAlignment lay members =
  case lay of
    Base
      | Just ( LayoutablePrimTy ( _ :: Proxy struct ) ) <- layoutableStruct @Base     adjustedMembers
      -> pure $ alignment @struct @Base
    Extended
      | Just ( LayoutablePrimTy ( _ :: Proxy struct ) ) <- layoutableStruct @Extended adjustedMembers
      -> pure $ alignment @struct @Extended
    _ -> throwError
            ( "'structAlignment': could not compute " <> ShortText.pack ( show lay ) <> " alignment for structure with members\n"
            <> ShortText.pack ( show members )
            )
  where
    adjustedMembers :: [ ( Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations ) ]
    adjustedMembers = map ( \( nm, ty, decs ) -> ( nm, adjustRuntimeArray ty, decs ) ) members
    adjustRuntimeArray :: SPIRV.PrimTy -> SPIRV.PrimTy
    adjustRuntimeArray ( SPIRV.RuntimeArray { eltTy, decs, usage } ) = SPIRV.Array { size = 1, eltTy, decs, usage }
    adjustRuntimeArray ty = ty

layoutStructMembers
    :: forall m. MonadError ShortText m
    => Layout
    -> Word32
    -> [ ( Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations ) ]
    -> m ( [ ( Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations ) ], Word32 )
layoutStructMembers _   offset []                     = pure ( [], offset )
layoutStructMembers lay offset ((name, ty, decs):nxt) = do
  (newOffset, laidOutTy, newDecs)
     <- case ty of
          SPIRV.Matrix { rows, cols, entryTy } -> do
            ( colSize, colAli ) <- primTySizeAndAli lay ( SPIRV.Vector rows ( SPIRV.Scalar entryTy ) )
            let
              matOffset :: Word32
              matOffset = nextAligned offset colAli
            laidOutTy <- layout lay matOffset ty
            pure ( matOffset + cols * colSize
                 , laidOutTy
                 , Set.fromList [ SPIRV.Offset matOffset, SPIRV.MatrixStride colSize, SPIRV.ColMajor ]
                 )
          SPIRV.Array {} -> do
            ( arrSize, arrAli ) <- primTySizeAndAli lay ty
            let
              arrOffset :: Word32
              arrOffset = nextAligned offset arrAli
            laidOutTy           <- layout lay arrOffset ty
            pure ( arrOffset + arrSize
                 , laidOutTy
                 , Set.singleton (SPIRV.Offset arrOffset)
                 )
          SPIRV.RuntimeArray { eltTy } -> do
            ( _, arrAli ) <- primTySizeAndAli lay ( SPIRV.Array 1 eltTy Set.empty SPIRV.NotForBuiltins )
            let
              arrOffset :: Word32
              arrOffset = nextAligned offset arrAli
            laidOutTy <- layout lay arrOffset ty
            pure ( error "'layoutStructMembers': offset is undefined after a run-time array"
                 , laidOutTy
                 , Set.singleton (SPIRV.Offset arrOffset)
                 )
          SPIRV.Struct as' decs' usage' -> do
            ( innerSize, innerAli ) <- primTySizeAndAli lay ty
            ( innerMembers, _ ) <- layoutStructMembers lay 0 as'
            let
              innerOffset :: Word32
              innerOffset = nextAligned offset innerAli
              laidOutTy :: SPIRV.PrimTy
              laidOutTy = SPIRV.Struct innerMembers decs' usage'
            pure ( innerOffset + innerSize
                 , laidOutTy
                 , Set.singleton (SPIRV.Offset innerOffset)
                 )
          _ -> do
            ( eltSize, eltAli ) <- primTySizeAndAli lay ty
            let
              eltOffset :: Word32
              eltOffset = nextAligned offset eltAli
            laidOutTy <- layout lay eltOffset ty
            pure ( eltOffset + eltSize
                 , laidOutTy
                 , Set.singleton (SPIRV.Offset eltOffset)
                 )
  first ( (name, laidOutTy, Set.union newDecs decs) : ) <$> layoutStructMembers lay newOffset nxt

primTySizeAndAli :: MonadError ShortText m => Layout -> SPIRV.PrimTy -> m ( Word32, Word32 )
primTySizeAndAli Base ty
  | Just ( LayoutablePrimTy ( _ :: Proxy ty ) ) <- layoutable @Base ty
  = pure $ ( sizeOf @ty @Base, alignment @ty @Base )
primTySizeAndAli Extended ty
  | Just ( LayoutablePrimTy ( _ :: Proxy ty ) ) <- layoutable @Extended ty
  = pure $ ( sizeOf @ty @Extended, alignment @ty @Extended )
primTySizeAndAli lay ty = throwError
  ( "'primTySizeAndAli': cannot compute " <> ShortText.pack (show lay) <> " size & alignment for " <> ShortText.pack (show ty) )

--------------------------------------------------------------------------------------------
-- Some functions for promoting SPIR-V primitive types that can be laid-out.
-- This allows us to re-use the type families that compute layout information
-- at the value level.

data SLayout (lay :: Layout) where
  SBase      :: SLayout Base
  SExtended  :: SLayout Extended
  SLocations :: SLayout Locations

class KnownLayout (lay :: Layout) where
  layoutSing :: SLayout lay
instance KnownLayout Base where
  layoutSing = SBase
instance KnownLayout Extended where
  layoutSing = SExtended
instance KnownLayout Locations where
  layoutSing = SLocations

data LayoutablePrimTy ( lay :: Layout ) where
  LayoutablePrimTy :: ( Prim.PrimTy a, Poke a lay ) => Proxy a -> LayoutablePrimTy lay
data LayoutableScalarTy ( lay :: Layout ) where
  LayoutableScalarTy :: ( ScalarTy a, Poke a lay ) => Proxy a -> LayoutableScalarTy lay
data LayoutableMatScalarTy ( lay :: Layout ) where
  LayoutableMatScalarTy :: ( ScalarTy a, AdditiveGroup a, Semiring a, Poke a lay ) => Proxy a -> LayoutableMatScalarTy lay
data LayoutableMembers (lay :: Layout) where
  LayoutableMembers
    :: ( PrimTyMap as, All (Pokeable lay) as
       , KnownNat (SizeOf    lay (Struct as))
       , KnownNat (Alignment lay (Struct as))
       )
    => Proxy as -> LayoutableMembers lay

layoutable :: forall ( lay :: Layout ). KnownLayout lay => SPIRV.PrimTy -> Maybe ( LayoutablePrimTy lay )
layoutable ( SPIRV.Scalar s )
  | Just ( LayoutableScalarTy ( _ :: Proxy a ) ) <- layoutableScalar @lay s
  = Just ( LayoutablePrimTy ( Proxy :: Proxy a ) )
layoutable ( SPIRV.Vector n ( SPIRV.Scalar s ) )
  | Just ( LayoutableScalarTy ( _ :: Proxy a ) ) <- layoutableScalar @lay s
  , True <- 1 <= n
  , SomeNat ( _ :: Proxy n ) <- someNatVal ( fromIntegral n )
  , Refl <- ( unsafeCoerce Refl :: ( 1 <=? n ) :~: True )
  , let
      npp2 :: Natural
      npp2
        | n == 1    = 1
        | otherwise = 2 ^ ( floor ( logBase 2 ( realToFrac ( n - 1 ) :: Float ) ) + 1 :: Int )
  , SomeNat ( _ :: Proxy npp2 ) <- someNatVal npp2
  , Refl <- ( unsafeCoerce Refl :: NextPositivePowerOf2 n :~: npp2 )
  = Just $ LayoutablePrimTy ( Proxy :: Proxy ( V n a ) )
layoutable ( SPIRV.Matrix m n a )
  | Just ( LayoutableMatScalarTy ( _ :: Proxy a ) ) <- layoutableMatScalar @lay a
  , SomeNat ( _ :: Proxy m ) <- someNatVal ( fromIntegral m )
  , SomeNat ( _ :: Proxy n ) <- someNatVal ( fromIntegral n )
  , True <- 1 <= n
  , Refl <- ( unsafeCoerce Refl :: ( 1 <=? n ) :~: True )
  , True <- 1 <= m
  , Refl <- ( unsafeCoerce Refl :: ( 1 <=? m ) :~: True )
  , let
      npp2 :: Natural
      npp2
        | n == 1    = 1
        | otherwise = 2 ^ ( floor ( logBase 2 ( realToFrac ( m - 1 ) :: Float ) ) + 1 :: Int )
  , SomeNat ( _ :: Proxy npp2 ) <- someNatVal npp2
  , Refl <- ( unsafeCoerce Refl :: NextPositivePowerOf2 m :~: npp2 )
  = case layoutSing @lay of
      SBase     -> Just $ LayoutablePrimTy ( Proxy :: Proxy ( M m n a ) )
      SExtended -> Just $ LayoutablePrimTy ( Proxy :: Proxy ( M m n a ) )
      _         -> Nothing
layoutable ( SPIRV.Array { size, eltTy } )
  | Just ( LayoutablePrimTy ( _ :: Proxy a ) ) <- layoutable @lay eltTy
  , True <- 1 <= size
  , SomeNat ( _ :: Proxy lg ) <- someNatVal ( fromIntegral size )
  , Refl <- ( unsafeCoerce Refl :: ( 1 <=? lg ) :~: True )
  = case layoutSing @lay of
      SBase
        | SomeNat ( _ :: Proxy sz ) <- someNatVal ( fromIntegral $ size * ( nextAligned ( sizeOf @a @Base ) ( alignment @a @Base ) ) )
        , Refl <- ( unsafeCoerce Refl :: sz :~: SizeOf Base ( Array lg a ) )
        -> Just $ LayoutablePrimTy ( Proxy :: Proxy ( Array lg a ) )
      SExtended
        | SomeNat ( _ :: Proxy sz ) <- someNatVal ( fromIntegral $ size * ( nextAligned ( sizeOf @a @Extended ) ( alignment @a @Extended `roundUp` 16 ) ) )
        , Refl <- ( unsafeCoerce Refl :: sz :~: SizeOf Extended ( Array lg a ) )
        , SomeNat ( _ :: Proxy ali ) <- someNatVal ( fromIntegral $ alignment @a @Extended `roundUp` 16 )
        , Refl <- ( unsafeCoerce Refl :: ali :~: (Alignment Extended a `RoundUp` 16) )
        -> Just $ LayoutablePrimTy ( Proxy :: Proxy ( Array lg a ) )
      _ -> Nothing
layoutable ( SPIRV.Struct { eltTys } ) = layoutableStruct @lay eltTys
layoutable _ = Nothing

layoutableScalar :: forall ( lay :: Layout ). SPIRV.ScalarTy -> Maybe ( LayoutableScalarTy lay )
layoutableScalar ( SPIRV.Integer SPIRV.Unsigned SPIRV.W8  ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Word8  )
layoutableScalar ( SPIRV.Integer SPIRV.Unsigned SPIRV.W16 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Word16 )
layoutableScalar ( SPIRV.Integer SPIRV.Unsigned SPIRV.W32 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Word32 )
layoutableScalar ( SPIRV.Integer SPIRV.Unsigned SPIRV.W64 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Word64 )
layoutableScalar ( SPIRV.Integer SPIRV.Signed   SPIRV.W8  ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Int8   )
layoutableScalar ( SPIRV.Integer SPIRV.Signed   SPIRV.W16 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Int16  )
layoutableScalar ( SPIRV.Integer SPIRV.Signed   SPIRV.W32 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Int32  )
layoutableScalar ( SPIRV.Integer SPIRV.Signed   SPIRV.W64 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Int64  )
layoutableScalar ( SPIRV.Floating               SPIRV.W16 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Half   )
layoutableScalar ( SPIRV.Floating               SPIRV.W32 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Float  )
layoutableScalar ( SPIRV.Floating               SPIRV.W64 ) = Just $ LayoutableScalarTy ( Proxy :: Proxy Double )
layoutableScalar _ = Nothing

layoutableMatScalar :: forall ( lay :: Layout ). SPIRV.ScalarTy -> Maybe ( LayoutableMatScalarTy lay )
layoutableMatScalar ( SPIRV.Integer SPIRV.Signed   SPIRV.W8  ) = Just $ LayoutableMatScalarTy ( Proxy :: Proxy Int8   )
layoutableMatScalar ( SPIRV.Integer SPIRV.Signed   SPIRV.W16 ) = Just $ LayoutableMatScalarTy ( Proxy :: Proxy Int16  )
layoutableMatScalar ( SPIRV.Integer SPIRV.Signed   SPIRV.W32 ) = Just $ LayoutableMatScalarTy ( Proxy :: Proxy Int32  )
layoutableMatScalar ( SPIRV.Integer SPIRV.Signed   SPIRV.W64 ) = Just $ LayoutableMatScalarTy ( Proxy :: Proxy Int64  )
layoutableMatScalar ( SPIRV.Floating               SPIRV.W16 ) = Just $ LayoutableMatScalarTy ( Proxy :: Proxy Half   )
layoutableMatScalar ( SPIRV.Floating               SPIRV.W32 ) = Just $ LayoutableMatScalarTy ( Proxy :: Proxy Float  )
layoutableMatScalar ( SPIRV.Floating               SPIRV.W64 ) = Just $ LayoutableMatScalarTy ( Proxy :: Proxy Double )
layoutableMatScalar _ = Nothing

layoutableStruct
  :: forall ( lay :: Layout ) mbName decs
  .  KnownLayout lay
  => [ ( mbName, SPIRV.PrimTy, decs ) ]
  -> Maybe ( LayoutablePrimTy lay )
layoutableStruct elts = makeLayoutableStruct =<< layoutableStructMembers 0 elts
  where
    layoutableStructMembers :: Int -> [ ( mbName, SPIRV.PrimTy, decs ) ] -> Maybe [ ( SomeSymbol, LayoutablePrimTy lay ) ]
    layoutableStructMembers _ [] = Just []
    layoutableStructMembers i ( (_,ty,_) : tys )
      | Just layoutableTy  <- layoutable @lay ty
      , Just layoutableTys <- layoutableStructMembers (i+1) tys
      , nm <- someSymbolVal ( show i )
      = Just ( ( nm, layoutableTy ) : layoutableTys )
    layoutableStructMembers _ _ = Nothing

    makeLayoutableStruct :: [ ( SomeSymbol, LayoutablePrimTy lay ) ] -> Maybe ( LayoutablePrimTy lay )
    makeLayoutableStruct members
      | Just ( _, LayoutableMembers ( _ :: Proxy as ) ) <- makeLayoutableMembers 0 members
      = case layoutSing @lay of
          SBase
            | let
                structAli :: Word32
                structAli = maximumAlignment @lay members
            , SomeNat ( _ :: Proxy structAli ) <- someNatVal ( fromIntegral structAli )
            , Refl <- ( unsafeCoerce Refl :: structAli :~: Alignment lay ( Struct as ) )
            -> Just ( LayoutablePrimTy ( Proxy :: Proxy ( Struct as ) ) )
          SExtended
            | let
                structAli :: Word32
                structAli = maximumAlignment @lay members `roundUp` 16
            , SomeNat ( _ :: Proxy structAli ) <- someNatVal ( fromIntegral structAli )
            , Refl <- ( unsafeCoerce Refl :: structAli :~: Alignment lay ( Struct as ) )
            -> Just ( LayoutablePrimTy ( Proxy :: Proxy ( Struct as ) ) )
          _ -> Nothing
    makeLayoutableStruct _ = Nothing

    makeLayoutableMembers
      :: Word32
      -> [ ( SomeSymbol, LayoutablePrimTy lay ) ]
      -> Maybe ( Word32, LayoutableMembers lay )
    makeLayoutableMembers acc [] = case layoutSing @lay of
      SBase     -> Just ( acc, LayoutableMembers ( Proxy :: Proxy ('[] :: [ Symbol :-> Type ] ) ) )
      SExtended -> Just ( acc, LayoutableMembers ( Proxy :: Proxy ('[] :: [ Symbol :-> Type ] ) ) )
      _         -> Nothing
    makeLayoutableMembers acc ( ( SomeSymbol ( _ :: Proxy nm ), LayoutablePrimTy ( _ :: Proxy a ) ) : as )
      | let
          acc' :: Word32
          acc' = nextAligned acc ( alignment @a @lay ) + sizeOf @a @lay
      = case layoutSing @lay of
          SBase
            | Just ( sz, LayoutableMembers ( _ :: Proxy ( as :: [ fld :-> Type ] ) ) ) <- makeLayoutableMembers acc' as
            , NamedField <- fieldKind @fld
            , let
                structSize :: Word32
                structSize = sz
                structAli :: Word32
                structAli = max ( alignment @a @lay ) ( alignment @(Struct as) @lay )
            , SomeNat ( _ :: Proxy structSize ) <- someNatVal ( fromIntegral structSize )
            , Refl <- ( unsafeCoerce Refl :: structSize :~: ( SizeOf lay ( Struct ( ( nm ':-> a ) ': as ) ) ) )
            , SomeNat ( _ :: Proxy structAli ) <- someNatVal ( fromIntegral structAli )
            , Refl <- ( unsafeCoerce Refl :: structAli :~: Alignment lay ( Struct ( ( nm ':-> a ) ': as ) ) )
            -> Just ( sz, LayoutableMembers ( Proxy :: Proxy ( ( nm ':-> a ) ': as ) ) )
          SExtended
            | Just ( sz, LayoutableMembers ( _ :: Proxy ( as :: [ fld :-> Type ] ) ) ) <- makeLayoutableMembers acc' as
            , NamedField <- fieldKind @fld
            , let
                structSize :: Word32
                structSize = sz `roundUp` 16
                structAli :: Word32
                structAli = ( max ( alignment @a @lay ) ( alignment @(Struct as) @lay ) ) `roundUp` 16
            , SomeNat ( _ :: Proxy structSize ) <- someNatVal ( fromIntegral structSize )
            , Refl <- ( unsafeCoerce Refl :: structSize :~: ( SizeOf lay ( Struct ( ( nm ':-> a ) ': as ) ) ) )
            , SomeNat ( _ :: Proxy structAli ) <- someNatVal ( fromIntegral structAli )
            , Refl <- ( unsafeCoerce Refl :: structAli :~: Alignment lay ( Struct ( ( nm ':-> a ) ': as ) ) )
            -> Just ( sz, LayoutableMembers ( Proxy :: Proxy ( ( nm ':-> a ) ': as ) ) )
          _ -> Nothing

maximumAlignment
  :: forall ( lay :: Layout )
  .  KnownLayout lay
  => [ ( SomeSymbol, LayoutablePrimTy lay ) ] -> Word32
maximumAlignment [] = 1
maximumAlignment ( (_, a) : as ) = case layoutSing @lay of
  SExtended -> maxAliHelper a as `roundUp` 16
  _         -> maxAliHelper a as
  where
    maxAliHelper ( LayoutablePrimTy ( _ :: Proxy a ) ) []
      = alignment @a @lay
    maxAliHelper ( LayoutablePrimTy ( _ :: Proxy a ) ) ( (_,b) : bs )
      = max ( alignment @a @lay ) ( maxAliHelper b bs )
