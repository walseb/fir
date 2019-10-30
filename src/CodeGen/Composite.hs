{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module CodeGen.Composite
  ( compositeConstruct
  , compositeInsert, compositeExtract
  , accessedTy
  , vectorShuffle, vectorSwizzle
  , productConstruct
  )
  where

-- base
import Data.Word
  ( Word32 )


-- mtl
import Control.Monad.Except
  ( throwError )

-- split
import Data.List.Split
  ( chunksOf )

-- text-short
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Binary
  ( instruction )
import CodeGen.IDs
  ( typeID, undefID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad, MonadFresh(fresh), note )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
import           SPIRV.PrimTy
  ( PrimTy(..) )

----------------------------------------------------------------------------
-- composite structures

compositeConstruct :: SPIRV.PrimTy -> [ ID ] -> CGMonad (ID, SPIRV.PrimTy)
compositeConstruct compositeType constituents
  = do tyID <- typeID compositeType
       v <- fresh
       instruction
         Instruction
           { operation = SPIRV.Op.CompositeConstruct
           , resTy = Just tyID
           , resID = Just v
           , args  = toArgs constituents
           }
       pure (v, compositeType)

compositeExtract
  :: (ID, SPIRV.PrimTy)
  -> [ Word32 ]
  -> CGMonad (ID, SPIRV.PrimTy)
compositeExtract (compositeID, compositeTy) indices
  = do
      constituentTy <-
          note ( "'compositeExtract': could not compute accessee type.\n\
                 \base: " <> ShortText.pack (show compositeTy) <> "\n\
                 \indices: " <> ShortText.pack (show indices) <> "."
               )
          ( accessedTy ( fmap Just indices ) compositeTy )
      constituentTyID <- typeID constituentTy
      v <- fresh
      instruction
        Instruction
          { operation = SPIRV.Op.CompositeExtract
          , resTy     = Just constituentTyID
          , resID     = Just v
          , args      = Arg compositeID
                      $ toArgs indices
          }
      pure (v, constituentTy)

compositeInsert
  :: (ID, SPIRV.PrimTy)
  -> (ID, SPIRV.PrimTy)
  -> [ Word32 ]
  -> CGMonad (ID, SPIRV.PrimTy)
compositeInsert (inserteeID, inserteeTy) (compositeID, compositeTy) indices
  = do
      compositeTyID <- typeID compositeTy
      constituentTy <-
          note ( "'compositeInsert': could not compute constituent type.\n\
                 \base: " <> ShortText.pack (show compositeTy) <> "\n\
                 \indices: " <> ShortText.pack (show indices) <> "."
               )
          ( accessedTy ( fmap Just indices ) compositeTy )
      constituentTyID <- typeID constituentTy
      correctInserteeID <-
        if constituentTy == inserteeTy
        then pure inserteeID
        else do
          u <- fresh
          instruction
            Instruction
               { operation = SPIRV.Op.BitCast
               , resTy     = Just constituentTyID
               , resID     = Just u
               , args      = Arg inserteeID EndArgs
               }
          pure u
      v <- fresh
      instruction
        Instruction
          { operation = SPIRV.Op.CompositeInsert
          , resTy     = Just compositeTyID
          , resID     = Just v
          , args      = Arg correctInserteeID
                      $ Arg compositeID
                      $ toArgs indices
          }
      pure (v, compositeTy)


accessedTy :: [ Maybe Word32 ] -> SPIRV.PrimTy -> Maybe SPIRV.PrimTy
accessedTy []           baseTy                        = Just baseTy
accessedTy ( _    :is) (SPIRV.Vector         {eltTy}) = accessedTy is eltTy
accessedTy ( _    :is) (SPIRV.Matrix {rows, entryTy}) = accessedTy is (SPIRV.Vector rows (SPIRV.Scalar entryTy))
accessedTy ( _    :is) (SPIRV.Array          {eltTy}) = accessedTy is eltTy
accessedTy ( _    :is) (SPIRV.RuntimeArray   {eltTy}) = accessedTy is eltTy
accessedTy (Just i:is) (SPIRV.Struct        {eltTys})
  | i' < length eltTys = accessedTy is ( (\ (_, ty, _) -> ty) $ eltTys !! i' )
  | otherwise          = Nothing
    where i' = fromIntegral i
accessedTy _ _
  = Nothing

----------------------------------------------------------------------------
-- vector shuffle/swizzle

vectorShuffle :: ( (ID, SPIRV.PrimTy), [Word32] )
              -> ( (ID, SPIRV.PrimTy), [Word32] )
              -> CGMonad (ID, SPIRV.PrimTy)
vectorShuffle ((u_ID, SPIRV.Vector n s), is) ((v_ID, SPIRV.Vector _ t), js)
  | s /= t = throwError "vectorShuffle: incompatible vector components"
  | otherwise =
    do
      v <- fresh
      let indices = is ++ map (+n) js
          shuffleTy = SPIRV.Vector ( fromIntegral (length is + length js) ) s
      shuffleTyID <- typeID shuffleTy
      instruction
        Instruction
          { operation = SPIRV.Op.VectorShuffle
          , resTy     = Just shuffleTyID
          , resID     = Just v
          , args      = Arg u_ID
                      $ Arg v_ID
                      $ toArgs indices
          }
      pure (v, shuffleTy)
vectorShuffle _ _
  = throwError "vectorShuffle used on non-vectors"

vectorSwizzle :: (ID, SPIRV.PrimTy) -> [Word32] -> CGMonad (ID, SPIRV.PrimTy)
vectorSwizzle v@(_, vecTy) is = do
  undef <- (,vecTy) <$> undefID vecTy
  vectorShuffle (v, is) (undef, [])
  -- using 'undefined' here prevents any code duplication that could result from
  -- 'vectorShuffle (v, is) (v, [])'
  -- (e.g. if 'v' is the result of an expensive computation, which could get inlined twice)

----------------------------------------------------------------------------
-- products

-- like 'compositeConstruct', except it checks that the types are correct
-- (for matrices, it constructs the composite in two steps when needed)
productConstruct :: SPIRV.PrimTy -> [ ( ID, SPIRV.PrimTy ) ] -> CGMonad (ID, SPIRV.PrimTy)
productConstruct p as = case p of
  SPIRV.Matrix {..}
    | all ( (== SPIRV.Scalar entryTy) . snd ) as
    && fromIntegral (length as) == rows * cols
    -> do
      columns <- traverse (compositeConstruct colTy . map fst) (chunksOf (fromIntegral rows) as)
      compositeConstruct p (map fst columns)
    | all ( (== colTy) . snd ) as
    && fromIntegral (length as) == cols
    -> compositeConstruct p (map fst as)
    | otherwise
    -> throwError $
    "'productConstruct': type mismatch for constructing matrix\n"
    <> "attempting to construct matrix with " <> ShortText.pack (show rows)
    <> " rows and " <> ShortText.pack (show cols) <> " columns of type "
    <> ShortText.pack (show entryTy) <> ",\n"
    <> "using components of types " <> ShortText.pack (show (map snd as))
      where
        colTy :: SPIRV.PrimTy
        colTy = SPIRV.Vector rows (SPIRV.Scalar entryTy)
  SPIRV.Vector {..}
    | all ( (== eltTy) . snd ) as
    && fromIntegral (length as) == size
    -> compositeConstruct p (map fst as)
    | otherwise
    -> throwError $
    "'productConstruct': type mismatch for constructing vector\n"
    <> "attempting to construct vector with " <> ShortText.pack (show size)
    <> " components of type " <> ShortText.pack (show eltTy) <> ",\n"
    <> "using components of types " <> ShortText.pack (show (map snd as))
  SPIRV.Array {..}
    | all ( (== eltTy) . snd ) as
    && fromIntegral (length as) == size
    -> compositeConstruct p (map fst as)
    | otherwise
    -> throwError $
    "'productConstruct': type mismatch for constructing array\n"
    <> "attempting to construct array of size " <> ShortText.pack (show size)
    <> " with elements of type " <> ShortText.pack (show eltTy) <> ",\n"
    <> "using components of types " <> ShortText.pack (show (map snd as))
  SPIRV.Struct {..}
    | and ( zipWith (\(_, ty1, _) (_,t2) -> ty1 == t2) eltTys as )
    && length as == length eltTys
    -> compositeConstruct p (map fst as)
    | otherwise
    -> throwError $
    "'productConstruct': type mismatch for constructing structure\n"
    <> "attempting to construct structure with elements of type "
    <> ShortText.pack (show (map (\(_, ty,_) -> ty) eltTys)) <> ",\n"
    <> "using components of types " <> ShortText.pack (show (map snd as))
  otherTy
    -> throwError $
    "'productConstruct': unexpected non-product type "
    <> ShortText.pack ( show otherTy )
