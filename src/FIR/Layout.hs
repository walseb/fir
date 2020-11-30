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
  )
  where

-- base
import Control.Monad
  ( foldM_ )
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.Kind
  ( Type )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr
  ( Ptr, castPtr, plusPtr )
import qualified Foreign.Storable as Storable
  ( poke )
import GHC.TypeLits
  ( Symbol
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
import Data.Type.Ord
  ( POrd(Max) )
import FIR.Prim.Array
  ( Array(..) )
import FIR.Prim.Singletons
  ( ScalarTy, PrimTyMap(..), SPrimTyMap(..)
  , SPrimTy(..)
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

type family SumRoundedUpSizes (lay :: Layout) (ali :: Nat) (as :: [fld :-> Type]) :: Nat where
  SumRoundedUpSizes _   _   '[]                    = 0
  SumRoundedUpSizes lay ali ( ( _ ':-> a ) ': as ) =
    NextAligned (SizeOf lay a) ali + SumRoundedUpSizes lay ali as

type family MaximumAlignment (lay :: Layout) (as :: [fld :-> Type]) :: Nat where
  MaximumAlignment _        '[]                    = 1
  MaximumAlignment Extended ( ( _ ':-> a ) ': as ) = MaximumAlignmentHelper Extended a as `RoundUp` 16
  MaximumAlignment lay      ( ( _ ':-> a ) ': as ) = MaximumAlignmentHelper lay      a as

type family MaximumAlignmentHelper (lay :: Layout) (a :: Type) (as :: [fld :-> Type]) :: Nat where
  MaximumAlignmentHelper lay a '[] = Alignment lay a
  MaximumAlignmentHelper lay a ( (_ ':-> b ) ': bs )
    = Max (Alignment lay a) (MaximumAlignmentHelper lay b bs)

roundUp :: Integral a => a -> a -> a
roundUp n r = (n + r) - ( 1 + ( (n + r - 1) `mod` r ) )

-- | Write elements to an array, whose start is given by a pointer.
pokeArray :: forall a lay. Poke a lay => Ptr a -> [a] -> IO ()
pokeArray ptr = pokeArrayOff @a @lay ptr 0

-- | Write elements to an array, starting from the given index offset.
pokeArrayOff :: forall a lay. Poke a lay => Ptr a -> Int -> [a] -> IO ()
pokeArrayOff ptr off vals0
  | off < 0   = pure ()
  | otherwise = go ( ptr `plusPtr` ( sz * off ) ) vals0
  where
    sz :: Int
    sz = fromIntegral ( sizeOf @a @lay )
    go :: Ptr a -> [a] -> IO ()
    go _    []         = pure ()
    go ptr' (val:vals) = do
      poke @a @lay ptr' val
      go ( ptr' `plusPtr` sz ) vals

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

instance TypeError ( Text "Cannot store Boolean types." ) => Poke Bool lay where
  type SizeOf    lay Bool = 4
  type Alignment lay Bool = 4
  poke = error "unreachable"


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
baseAlignment (SPIRV.Matrix {rows, entryTy}) = baseAlignment (SPIRV.Vector rows (SPIRV.Scalar entryTy)) -- assumed column-major
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
requiredAlignment (Storage.RayStorage Storage.ShaderRecordBuffer ) = pure baseAlignment
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
