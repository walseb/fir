{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Composite
  ( compositeConstruct
  , compositeInsert, compositeExtract
  , accessedTy
  , vectorShuffle, vectorSwizzle
  )
  where

-- base
import Data.Word
  ( Word32 )

-- containers
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except
  ( throwError )

-- text-short
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad
  , MonadFresh(fresh)
  , liftPut
  , note
  )
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
       liftPut $ putInstruction Map.empty
         Instruction
           { operation = SPIRV.Op.CompositeConstruct
           , resTy = Just tyID
           , resID = Just v
           , args  = toArgs constituents
           }
       pure (v, compositeType)

compositeExtract
  :: [ Word32 ]
  -> (ID, SPIRV.PrimTy)
  -> CGMonad (ID, SPIRV.PrimTy)
compositeExtract indices (compositeID, compositeTy)
  = do
      constituentTy <-
          note ( "'compositeExtract': could not compute accessee type.\n\
                 \base: " <> ShortText.pack (show compositeTy) <> "\n\
                 \indices: " <> ShortText.pack (show indices) <> "."
               )
          ( accessedTy ( fmap Just indices ) compositeTy )
      constituentTyID <- typeID constituentTy
      v <- fresh
      liftPut $ putInstruction Map.empty
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
          liftPut $ putInstruction Map.empty
            Instruction
               { operation = SPIRV.Op.BitCast
               , resTy     = Just constituentTyID
               , resID     = Just u
               , args      = Arg inserteeID EndArgs
               }
          pure u
      v <- fresh
      liftPut $ putInstruction Map.empty
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
      liftPut $ putInstruction Map.empty
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
vectorSwizzle v is = vectorShuffle (v, is) (v, [])
