{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module: CodeGen.Composite

Code generation for composite objects (vectors, matrices, arrays and structures): construction and extraction.
-}

module CodeGen.Composite
  ( compositeConstruct
  , compositeInsert, compositeExtract
  , accessedTy
  , vectorShuffle, vectorSwizzle
  , productConstruct
  )
  where

-- base
import Data.Foldable
  ( toList )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( natVal )

-- containers
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except
  ( throwError )

-- split
import Data.List.Split
  ( chunksOf )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( toList, knownLength' )

-- fir
import CodeGen.Application
  ( Application(Applied)
  , pattern Nullary
  , ASTs(NilAST, ConsAST)
  )
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
import CodeGen.Debug
  ( whenAsserting )
import CodeGen.IDs
  ( bindingID, typeID, undefID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad, MonadFresh(fresh), note )
import Data.Type.Known
  ( knownValue )
import FIR.AST
  ( AST
  , MkVectorF(..), GradedMappendF(..)
  , MatF(..), UnMatF(..)
  , StructF(..), ArrayF(..), ArrayLengthF(..)
  )
import FIR.AST.Type
  ( AugType(Val) )
import FIR.Prim.Array
  ( Array(MkArray) )
import FIR.Prim.Struct
  ( traverseStructASTs )
import FIR.Prim.Singletons
  ( primTy )
import Math.Linear
  ( V )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
import           SPIRV.PrimTy
  ( PrimTy(..) )
import qualified SPIRV.ScalarTy as SPIRV
  ( ScalarTy(..), Signedness(..), Width(..) )

----------------------------------------------------------------------------
-- Code generation for constructing composite objects.

class    NoConstraint (a :: AugType) where
instance NoConstraint (a :: AugType) where

instance CodeGen AST => CodeGen (MkVectorF AST) where
  codeGenArgs ( Applied (MkVectorF (vec :: V n (AST (Val a))) ) NilAST ) = do
    let n = knownValue @n
    compositeTy <- case primTy @a of
      SPIRV.Scalar s ->
        pure $ SPIRV.Vector n (SPIRV.Scalar s)
      SPIRV.Vector m (SPIRV.Scalar s) ->
        pure $ SPIRV.Matrix m n s
      x ->
        throwError
          ( "codeGen: unexpected vector constituent "
          <> ShortText.pack ( show x )
          )
    ( compositeConstruct compositeTy . map fst . toList )
      =<< traverse codeGen vec

instance CodeGen AST => CodeGen (MatF AST) where
  codeGenArgs ( Applied MatF (a `ConsAST` NilAST) ) = codeGenArgs ( Nullary a )
instance CodeGen AST => CodeGen (UnMatF AST) where
  codeGenArgs ( Applied UnMatF (a `ConsAST` NilAST) ) = codeGenArgs ( Nullary a )

instance CodeGen AST => CodeGen (StructF AST) where
  codeGenArgs ( Applied structF@(StructF struct) NilAST ) = case structF of
    ( _ :: StructF AST (Val struct) ) ->
      compositeConstruct (primTy @struct)
        =<< traverseStructASTs @AST (fmap fst . codeGen) struct

instance CodeGen AST => CodeGen (ArrayF AST) where
  codeGenArgs ( Applied arrF@(ArrayF (MkArray vec)) NilAST ) = case arrF of
    ( _ :: ArrayF AST (Val (Array n a)) ) ->
      let
        n :: Word32
        n = Vector.knownLength' vec ( fromIntegral . natVal )
        vecTy :: SPIRV.PrimTy
        vecTy = SPIRV.Vector n ( primTy @a )
      in
        compositeConstruct vecTy
          =<< traverse (fmap fst . codeGen) (Vector.toList vec)

instance CodeGen (ArrayLengthF AST) where
  codeGenArgs ( Applied (ArrayLengthF (_ :: Proxy structName) (_ :: Proxy pos)) NilAST ) = do
    let
      structName :: ShortText
      structName = knownValue @structName
      arrayPos :: Word32
      arrayPos = knownValue @pos
    (structBdID, _) <- bindingID structName
    let
      word32Ty :: SPIRV.PrimTy
      word32Ty = SPIRV.Scalar ( SPIRV.Integer SPIRV.Unsigned SPIRV.W32 )
    word32TyID <- typeID word32Ty
    v <- fresh
    instruction
      Instruction
        { operation = SPIRV.Op.ArrayLength
        , resTy = Just word32TyID
        , resID = Just v
        , args  = Arg structBdID
                $ Arg arrayPos
                $ EndArgs
        }
    pure ( v, word32Ty )

instance CodeGen AST => CodeGen (GradedMappendF AST) where
  codeGenArgs ( Applied GradedMappendF ( a `ConsAST` b `ConsAST` NilAST ) ) = do
    (a_ID, a_ty) <- codeGen a
    (b_ID, b_ty) <- codeGen b
    case (a_ty, b_ty) of
      ( SPIRV.Vector i (SPIRV.Scalar s), SPIRV.Vector j (SPIRV.Scalar t)) ->
        if s /= t
        then throwError "codeGen: graded semigroup operation on incompatible vectors"
        else compositeConstruct (SPIRV.Vector (i+j) (SPIRV.Scalar s)) [a_ID, b_ID]
      ( SPIRV.Matrix m n s, SPIRV.Matrix m' n' s' ) ->
        if m /= m' || s /= s'
        then throwError "codeGen: graded semigroup operation on incompatible matrices"
        else do
          cols1 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..n -1]
          cols2 <- traverse ( \i -> fst <$> compositeExtract (b_ID, b_ty) [i] ) [0..n'-1]
          compositeConstruct (SPIRV.Matrix m (n+n') s) (cols1 ++ cols2)
      ( SPIRV.Struct as _ _, SPIRV.Struct bs _ _ ) ->
        do
          let lg  = fromIntegral (length as)
              lg' = fromIntegral (length bs)
          elts1 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..lg -1]
          elts2 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..lg'-1]
          compositeConstruct
            ( SPIRV.Struct (as ++ bs) Set.empty SPIRV.NotForBuiltins )
            ( elts1 ++ elts2 )
      ( SPIRV.Array lg eltTy _ _, SPIRV.Array lg' eltTy' _ _ ) ->
        if eltTy /= eltTy'
        then throwError "codeGen: graded semigroup operation on incompatible arrays"
        else do
          -- should probably use a loop instead of this, but nevermind
          elts1 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..lg -1]
          elts2 <- traverse ( \i -> fst <$> compositeExtract (a_ID, a_ty) [i] ) [0..lg'-1]
          compositeConstruct
            ( SPIRV.Array (lg + lg') eltTy Set.empty SPIRV.NotForBuiltins )
            ( elts1 ++ elts2 )
      _ -> throwError "codeGen: unsupported graded semigroup operation"

----------------------------------------------------------------------------
-- SPIR-V instructions.

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
  whenAsserting $
    case vecTy of
      SPIRV.Vector {} -> pure ()
      _ -> throwError ( "vectorSwizzle: ASSERT failed, swizzle on non-vector: " <> ShortText.pack ( show v ) )
  case is of
    []  -> throwError "vectorSwizzle: empty swizzle"
    [i] -> compositeExtract v [i]
    _   -> do
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
