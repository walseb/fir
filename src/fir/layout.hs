{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module FIR.Layout where

-- base
import Data.Word
  ( Word32 )

-- containers
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except
  ( MonadError, throwError )

-- text-utf8
import "text-utf8" Data.Text
  ( Text )
import qualified "text-utf8" Data.Text as Text

-- fir
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

inferLayout :: MonadError Text m => SPIRV.StructUsage -> SPIRV.StorageClass -> SPIRV.PrimTy -> m SPIRV.PrimTy
inferLayout _ storageClass ty
  | storageClass `elem`
      [ Storage.Image    , Storage.UniformConstant
      , Storage.Workgroup, Storage.CrossWorkgroup
      , Storage.Private  , Storage.Function
      , Storage.Generic  , Storage.AtomicCounter
      ]
  = pure ty
inferLayout structUsage storageClass (SPIRV.Array lg (SPIRV.Struct as sdecs _) adecs)
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = pure $ SPIRV.Array lg (SPIRV.Struct as (Set.insert SPIRV.Block sdecs) structUsage ) adecs
  | otherwise
  =   ( \elt -> SPIRV.Array lg elt adecs ) . ( \elts -> SPIRV.Struct elts (Set.insert SPIRV.Block sdecs) SPIRV.NotForBuiltins )
  <$> ( flip layoutStructMembersWith as =<< requiredAlignment storageClass )
inferLayout _ storageClass (SPIRV.RuntimeArray (SPIRV.Struct as sdecs _) adecs)
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = throwError "'inferLayout': cannot use run-time arrays in 'Input'/'Output', must use a uniform or storage buffer."
  | otherwise
  =   ( \elt -> SPIRV.RuntimeArray elt adecs ) . ( \elt -> SPIRV.Struct elt (Set.insert SPIRV.Block sdecs) SPIRV.NotForBuiltins )
  <$> ( flip layoutStructMembersWith as =<< requiredAlignment storageClass )
inferLayout structUsage storageClass (SPIRV.Struct as sdecs _)
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = pure $ SPIRV.Struct as (Set.insert SPIRV.Block sdecs) structUsage
  | otherwise
  = ( \elts -> SPIRV.Struct elts (Set.insert SPIRV.Block sdecs) SPIRV.NotForBuiltins )
  <$> ( flip layoutStructMembersWith as =<< requiredAlignment storageClass )
inferLayout _ storageClass ty
  | storageClass `elem` [ Storage.Input, Storage.Output ]
  = pure ty
  | otherwise
  = throwError
    (  "'inferLayout': unsupported type " <> Text.pack (show ty)
    <> " in conjunction with storage class "
    <> Text.pack (show storageClass) <> "."
    )

inferPointerLayout :: MonadError Text m => SPIRV.StructUsage -> SPIRV.PointerTy -> m SPIRV.PointerTy
inferPointerLayout structUsage (SPIRV.PointerTy storageClass ty)
  = SPIRV.PointerTy storageClass <$> inferLayout structUsage storageClass ty


layoutWith :: MonadError Text m
           => ( SPIRV.PrimTy -> m Word32 ) -> SPIRV.PrimTy -> m SPIRV.PrimTy
layoutWith _ ty@(SPIRV.Scalar {}) = pure ty
layoutWith _ ty@(SPIRV.Vector {}) = pure ty
layoutWith _ mat@(SPIRV.Matrix {})
  = pure mat -- cannot decorate matrix directly, must do so indirectly using arrays and structs
layoutWith f arr@(SPIRV.Array l mat@(SPIRV.Matrix {}) decs) = do
  arrStride  <- f arr
  matStride  <- f mat
  pure ( SPIRV.Array l mat
            ( Set.union
                ( Set.fromList [ SPIRV.ArrayStride arrStride, SPIRV.MatrixStride matStride, SPIRV.ColMajor ] )
                decs
            )
        )
layoutWith f arr@(SPIRV.Array l elt decs) = do
  arrStride  <- f arr
  laidOutElt <- layoutWith f elt
  pure ( SPIRV.Array l laidOutElt (Set.insert (SPIRV.ArrayStride arrStride) decs) )
layoutWith f arr@(SPIRV.RuntimeArray mat@(SPIRV.Matrix {}) decs) = do
  arrStride  <- f arr
  matStride  <- f mat
  pure ( SPIRV.RuntimeArray mat
            ( Set.union
                ( Set.fromList [ SPIRV.ArrayStride arrStride, SPIRV.MatrixStride matStride, SPIRV.ColMajor ] )
                decs
            )
        )
layoutWith f arr@(SPIRV.RuntimeArray elt decs) = do
  arrStride  <- f arr
  laidOutElt <- layoutWith f elt
  pure ( SPIRV.RuntimeArray laidOutElt (Set.insert (SPIRV.ArrayStride arrStride) decs) )
layoutWith f (SPIRV.Struct as decs structUsage) = do
  laidOutMembers <- layoutStructMembersWith f as
  pure ( SPIRV.Struct laidOutMembers decs structUsage )
layoutWith _ ty
  = throwError ( "'layoutWith': unsupported type " <> Text.pack (show ty) <> "." )

layoutStructMembersWith
    :: forall m. MonadError Text m
    => ( SPIRV.PrimTy -> m Word32 )
    -> [(Text, SPIRV.PrimTy, SPIRV.Decorations)]
    -> m [(Text, SPIRV.PrimTy, SPIRV.Decorations)]
layoutStructMembersWith f as = fst <$> go 0 as
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
                  pure ( offset + cols * stride
                       , laidOutTy
                       , Set.fromList [ SPIRV.Offset offset, SPIRV.MatrixStride stride, SPIRV.ColMajor ]
                       )
                SPIRV.Array { size } -> do
                  eltAlignment <- f ty
                  laidOutTy <- layoutWith f ty
                  pure ( offset + size * eltAlignment
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
                  pure ( offset + totalSize
                       , laidOutTy -- wasteful duplicated computation here
                       , Set.singleton (SPIRV.Offset offset)
                       )
                _ -> do
                  alignment <- f ty
                  laidOutTy <- layoutWith f ty
                  pure ( offset + alignment
                       , laidOutTy
                       , Set.singleton (SPIRV.Offset offset)
                       )
        ( , offset) . ( (txt, laidOutTy, Set.union newDecs decs) : ) . fst <$> go newOffset nxt
