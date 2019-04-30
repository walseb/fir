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
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module FIR.Layout where

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
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( KnownNat, type (<=) )

-- containers
import qualified Data.Set as Set

-- distributive
import Data.Distributive
  ( Distributive(distribute) )

-- half
import Numeric.Half
  ( Half )

-- mtl
import Control.Monad.Except
  ( MonadError, throwError )

-- text-utf8
import "text-utf8" Data.Text
  ( Text )
import qualified "text-utf8" Data.Text as Text

-- fir
import Data.Type.Known
  ( knownValue )
import Data.Type.Map
  ( (:->)((:->)) )
import FIR.Prim.Array
  ( Array(..) )
import FIR.Prim.Singletons
  ( ScalarTy
  , PrimTyMap(..), SPrimTyMap(..)
  )
import qualified FIR.Prim.Singletons as Prim
  ( PrimTy )
import FIR.Prim.Struct
  ( Struct(..) )
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

-- For reference, see:
--
-- - SPIR-V specification, 2.16.2 "Validation Rules for Shader Capabilities", bullet point 3,
-- - SPIR-V specification, 2.18.1 "Memory Layout",
-- - Vulkan specification, 14.5.2 "Descriptor Set Interface",
-- - Vulkan specification, 14.5.4 "Offset and Stride Assignment".

--------------------------------------------------------------------------------------------

data Alignment = Base | Extended | Packed
  deriving ( Eq, Show )

class Poke a (ali :: Alignment) where
  sizeOf    :: Word32
  alignment :: Word32
  poke :: Ptr a -> a -> IO ()

pokeArray :: forall a ali. Poke a ali => Ptr a -> [a] -> IO ()
pokeArray ptr vals0 = go vals0 0
  where go [] _         = return ()
        go (val:vals) n = do
          poke @a @ali ( ptr `plusPtr` n ) val
          go vals (n + fromIntegral (sizeOf @a @ali))

nextAligned :: Word32 -> Word32 -> Word32
nextAligned size ali
  = case size `mod` ali of
         0 -> size
         x -> size + ali - x

instance Poke Word8 ali where
  sizeOf    = 1
  alignment = 4
  poke = Storable.poke

instance Poke Word16 ali where
  sizeOf = 2
  alignment = 4
  poke = Storable.poke

instance Poke Word32 ali where
  sizeOf    = 4
  alignment = 4
  poke = Storable.poke

instance Poke Word64 ali where
  sizeOf    = 8
  alignment = 8
  poke = Storable.poke

instance Poke Int8 ali where
  sizeOf    = 1
  alignment = 4
  poke = Storable.poke

instance Poke Int16 ali where
  sizeOf    = 2
  alignment = 4
  poke = Storable.poke

instance Poke Int32 ali where
  sizeOf    = 4
  alignment = 4
  poke = Storable.poke

instance Poke Int64 ali where
  sizeOf    = 8
  alignment = 8
  poke = Storable.poke

instance Poke Half ali where
  sizeOf    = 2
  alignment = 4
  poke = Storable.poke

instance Poke Float ali where
  sizeOf    = 4
  alignment = 4
  poke = Storable.poke

instance Poke Double ali where
  sizeOf    = 8
  alignment = 8
  poke = Storable.poke

instance (Poke a Base, ScalarTy a, KnownNat n, 1 <= n) => Poke (V n a) Base where
  sizeOf    = knownValue @n * sizeOf @a @Base
  alignment = sizeOf @a @Base
            * ( 2 ^ ceiling @Float @Word32 (logBase 2 (fromIntegral (knownValue @n))) )
  poke ptr v
    = foldM_
        ( \accPtr elt -> poke @a @Base accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        v
    where
      off :: Int
      off = fromIntegral ( sizeOf @a @Base )

instance (Poke a Extended, ScalarTy a, KnownNat n, 1 <= n) => Poke (V n a) Extended where
  sizeOf    = knownValue @n * sizeOf @a @Extended
  alignment = sizeOf @a @Extended
            * ( 2 ^ ceiling @Float @Word32 (logBase 2 (fromIntegral (knownValue @n))) )
  poke ptr v
    = foldM_
        ( \accPtr elt -> poke @a @Extended accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        v
    where
      off :: Int
      off = fromIntegral ( sizeOf @a @Extended )

instance (Poke a Packed, ScalarTy a, KnownNat n, 1 <= n) => Poke (V n a) Packed where
  sizeOf    = knownValue @n * sizeOf @a @Packed
  alignment = sizeOf @a @Packed
  poke ptr v
    = foldM_
        ( \accPtr elt -> poke @a @Packed accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        v
    where
      off :: Int
      off = fromIntegral ( sizeOf @a @Packed )

instance (Poke (V m a) ali, ScalarTy a, KnownNat n, KnownNat m, 1 <= n, 1 <= m)
       => Poke (M m n a) ali where
  sizeOf    = knownValue @n * sizeOf @(V m a) @ali
  alignment = alignment @(V m a) @ali
  poke ptr mat
    = foldM_
        ( \accPtr col -> poke @(V m a) @ali accPtr col *> pure (accPtr `plusPtr` colSize) )
        ( castPtr ptr )
        ( distribute . unM $ mat )
    where
      colSize = fromIntegral $ sizeOf @(V m a) @ali

instance (Poke a Base, Prim.PrimTy a, KnownNat n, 1 <= n)
       => Poke (Array n a) Base where
  sizeOf    = knownValue @n * sizeOf @a @Base
  alignment = alignment @a @Base
  poke ptr (MkArray arr)
    = foldM_
        ( \accPtr elt -> poke @a @Base accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        arr
    where
      off :: Int
      off = fromIntegral $ nextAligned (sizeOf @a @Base) (alignment @(Array n a) @Base)

instance (Poke a Extended, Prim.PrimTy a, KnownNat n, 1 <= n)
       => Poke (Array n a) Extended where
  sizeOf    = knownValue @n
            * ( nextAligned (sizeOf @a @Extended) (alignment @(Array n a) @Extended) )
  alignment = roundUp16 $ alignment @a @Extended
  poke ptr (MkArray arr)
    = foldM_
        ( \accPtr elt -> poke @a @Extended accPtr elt *> pure (accPtr `plusPtr` off) )
        ( castPtr ptr )
        arr
    where
      off :: Int
      off = fromIntegral $ nextAligned (sizeOf @a @Extended) (alignment @(Array n a) @Extended)

data PokeDict :: [Symbol :-> Type] -> Alignment -> Type where
  VacuousPoke :: PokeDict '[] ali
  PokeDict :: (Poke a ali, PokeStruct as ali) => PokeDict ( (k ':-> a) ': as ) ali
class PokeStruct as ali where
  pokeDict :: PokeDict as ali
instance PokeStruct '[] ali where
  pokeDict = VacuousPoke
instance (Poke a ali, PokeStruct as ali) => PokeStruct ( (k ':-> a) ': as ) ali where
  pokeDict = PokeDict

instance (PrimTyMap as, PokeStruct as Base) => Poke (Struct as) Base where
  sizeOf = go @as (alignment @(Struct as) @Base)
    where
      go :: forall xs. (PrimTyMap xs, PokeStruct xs Base) => Word32 -> Word32
      go ali = case primTyMapSing @xs of
        SNil -> 0
        scons@SCons -> case scons of
          ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
            case pokeDict @xs @Base of
              PokeDict -> nextAligned (sizeOf @b @Base) ali + go @bs ali
  alignment = go @as
    where
      go :: forall xs. (PrimTyMap xs, PokeStruct xs Base) => Word32
      go = case primTyMapSing @xs of
        SNil -> 1
        scons@SCons -> case scons of
          ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
            case pokeDict @xs @Base of
              PokeDict -> max (alignment @b @Base) (alignment @(Struct bs) @Base)
  poke = go @as (alignment @(Struct as) @Base)
    where
      go :: forall xs x. (PrimTyMap xs, PokeStruct xs Base) => Word32 -> Ptr x -> Struct xs -> IO ()
      go ali ptr struct = case primTyMapSing @xs of
        SNil  -> pure ()
        scons@SCons -> case scons of
          ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
            case (struct, pokeDict @xs @Base) of
              ( b :& bs, PokeDict ) ->
                let
                  off :: Int
                  off = fromIntegral $ nextAligned (sizeOf @b @Base) ali
                in poke @b @Base (castPtr ptr) b *> poke @(Struct bs) @Base (ptr `plusPtr` off) bs

instance (PrimTyMap as, PokeStruct as Extended) => Poke (Struct as) Extended where
  sizeOf = go @as (alignment @(Struct as) @Extended)
    where
      go :: forall xs. (PrimTyMap xs, PokeStruct xs Extended) => Word32 -> Word32
      go ali = case primTyMapSing @xs of
        SNil -> 0
        scons@SCons -> case scons of
          ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
            case pokeDict @xs @Extended of
              PokeDict -> nextAligned (sizeOf @b @Extended) ali + go @bs ali
  alignment = roundUp16 ( go @as )
    where
      go :: forall xs. (PrimTyMap xs, PokeStruct xs Extended) => Word32
      go = case primTyMapSing @xs of
        SNil -> 1
        scons@SCons -> case scons of
          ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
            case pokeDict @xs @Extended of
              PokeDict -> max (alignment @b @Extended) (alignment @(Struct bs) @Extended)
  poke = go @as (alignment @(Struct as) @Extended)
    where
      go :: forall xs x. (PrimTyMap xs, PokeStruct xs Extended) => Word32 -> Ptr x -> Struct xs -> IO ()
      go ali ptr struct = case primTyMapSing @xs of
        SNil  -> pure ()
        scons@SCons -> case scons of
          ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
            case (struct, pokeDict @xs @Extended) of
              ( b :& bs, PokeDict ) ->
                let
                  off :: Int
                  off = fromIntegral $ nextAligned (sizeOf @b @Extended) ali
                in poke @b @Extended (castPtr ptr) b *> poke @(Struct bs) @Extended (ptr `plusPtr` off) bs

instance (PrimTyMap as, PokeStruct as Packed) => Poke (Struct as) Packed where
  sizeOf = go @as 0
    where
      go :: forall xs. (PrimTyMap xs, PokeStruct xs Packed) => Word32 -> Word32
      go off = case primTyMapSing @xs of
        SNil -> off
        scons@SCons -> case scons of
          ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
            case pokeDict @xs @Packed of
              PokeDict -> nextAligned off (alignment @b @Packed) + go @bs (sizeOf @b @Packed)
  alignment =
    case primTyMapSing @as of
      SNil -> 1
      scons@SCons -> case scons of
        ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
          case pokeDict @as @Packed of
            PokeDict -> alignment @b @Packed
  poke = go @as
    where
      go :: forall xs x. (PrimTyMap xs, PokeStruct xs Packed) => Ptr x -> Struct xs -> IO ()
      go ptr struct = case primTyMapSing @xs of
        SNil  -> pure ()
        scons@SCons -> case scons of
          ( _ :: SPrimTyMap ((k ':-> b) ': bs) ) ->
            case (struct, pokeDict @xs @Packed) of
              ( b :& bs, PokeDict ) ->
                let
                  off :: Int
                  off = fromIntegral $ nextAligned (sizeOf @b @Packed) (alignment @(Struct bs) @Packed)
                in poke @b @Packed (castPtr ptr) b *> poke @(Struct bs) @Packed (ptr `plusPtr` off) bs

--------------------------------------------------------------------------------------------

maxMemberAlignment
   :: MonadError e m
   => ( SPIRV.PrimTy -> m Word32 ) -> [(ignore1, SPIRV.PrimTy, ignore2)] -> m Word32
maxMemberAlignment f as = foldr ( \ a b -> max <$> (f . (\ (_,ty,_) -> ty )) a <*> b ) (pure 0) as

roundUp16 :: Word32 -> Word32
roundUp16 n = ( n `mod` 16 ) + n

scalarAlignment :: MonadError Text m => SPIRV.PrimTy -> m Word32
scalarAlignment (SPIRV.Scalar (SPIRV.Integer  _ w)) = pure (SPIRV.width w `quot` 8)
scalarAlignment (SPIRV.Scalar (SPIRV.Floating   w)) = pure (SPIRV.width w `quot` 8)
scalarAlignment (SPIRV.Vector        {eltTy}) = scalarAlignment eltTy
scalarAlignment (SPIRV.Matrix      {entryTy}) = scalarAlignment (SPIRV.Scalar entryTy)
scalarAlignment (SPIRV.Array         {eltTy}) = scalarAlignment eltTy
scalarAlignment (SPIRV.RuntimeArray  {eltTy}) = scalarAlignment eltTy
scalarAlignment (SPIRV.Struct       {eltTys}) = maxMemberAlignment scalarAlignment eltTys
scalarAlignment ty
  = throwError
      ( "Layout: cannot compute scalar alignment of type " <> Text.pack (show ty) <> "." )

baseAlignment :: MonadError Text m => SPIRV.PrimTy -> m Word32
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
      ( "Layout: cannot compute base alignment of type " <> Text.pack (show ty) <> "." )

extendedAlignment :: MonadError Text m => SPIRV.PrimTy -> m Word32
extendedAlignment (SPIRV.Array         {eltTy}) = roundUp16 <$> extendedAlignment eltTy
extendedAlignment (SPIRV.RuntimeArray  {eltTy}) = roundUp16 <$> extendedAlignment eltTy
extendedAlignment (SPIRV.Struct       {eltTys}) = roundUp16 <$> maxMemberAlignment extendedAlignment eltTys
extendedAlignment ty                            = baseAlignment ty

requiredAlignment :: MonadError Text m => SPIRV.StorageClass -> m ( SPIRV.PrimTy -> m Word32 )
requiredAlignment Storage.Uniform       = pure extendedAlignment
requiredAlignment Storage.StorageBuffer = pure baseAlignment
requiredAlignment Storage.PushConstant  = pure baseAlignment
requiredAlignment storage
  = throwError
      ( "Layout: unsupported storage class " <> Text.pack (show storage) <> "." )

inferLayout :: MonadError Text m
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
    (  "'inferLayout': unsupported type " <> Text.pack (show ty)
    <> " in conjunction with storage class "
    <> Text.pack (show storageClass) <> "."
    )

inferPointerLayout
  :: MonadError Text m
  => SPIRV.AggregateUsage
  -> SPIRV.Decorations
  -> SPIRV.PointerTy
  -> m SPIRV.PointerTy
inferPointerLayout usage decs (SPIRV.PointerTy storageClass ty)
  = SPIRV.PointerTy storageClass <$> inferLayout usage decs storageClass ty


layoutWith :: MonadError Text m
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
  = throwError ( "'layoutWith': unsupported type " <> Text.pack (show ty) <> "." )

layoutStructMembersWith
    :: forall m. MonadError Text m
    => ( SPIRV.PrimTy -> m Word32 )
    -> Word32
    -> [(Text, SPIRV.PrimTy, SPIRV.Decorations)]
    -> m [(Text, SPIRV.PrimTy, SPIRV.Decorations)]
layoutStructMembersWith f ali as = fst <$> go 0 as
  where
    go :: Word32
       ->     [ ( Text, SPIRV.PrimTy, SPIRV.Decorations ) ]
       -> m ( [ ( Text, SPIRV.PrimTy, SPIRV.Decorations ) ], Word32 )
    go offset []                    = pure ( [], offset )
    go offset ((txt, ty, decs):nxt) = do
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
        ( , offset) . ( (txt, laidOutTy, Set.union newDecs decs) : ) . fst <$> go newOffset nxt
