{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module CodeGen.Optics
  ( IndexedOptic(AnIndexedOptic)
  , pattern OpticUse
  , pattern OpticAssign
  , pattern OpticView
  , pattern OpticSet
  , loadThroughAccessChain
  , storeThroughAccessChain
  , extractUsingGetter
  , insertUsingSetter
  , setUsingSetter
  , ASTs(NilAST, ConsAST)
  ) where

-- base
import Control.Arrow
  ( first )
import Control.Monad
  ( unless )
import Data.Kind
  ( Type )
import Unsafe.Coerce
  ( unsafeCoerce )

-- lens
import Control.Lens
  ( assign )

-- mtl
import Control.Monad.Except
  ( throwError )

-- text-utf8
import Data.Text
  ( Text )

-- fir
import {-# SOURCE #-} CodeGen.CodeGen
  ( codeGen )
import CodeGen.Composite
  ( compositeExtract, compositeInsert )
import CodeGen.Instruction
  ( ID )
import CodeGen.Monad
  ( CGMonad )
import CodeGen.Pointers
  ( Safeness(Unsafe)
  , Indices(RTInds, CTInds)
  , temporaryVariable, accessChain
  , loadInstruction, storeInstruction
  )
import CodeGen.State
  ( PointerState(Fresh, Modified)
  , _localBinding, _temporaryPointer
  )
import CodeGen.Untyped
  ( UAST(UAST), UASTs(NilUAST, SnocUAST), pattern Applied )
import Control.Type.Optic
  ( Optic )
import Data.Type.List
  ( type (:++:), Zip
  , SLength(SZero, SSucc)
  )
import FIR.AST
  ( AST((:$), Fst, Snd, Use, Assign, View, Set) )
import FIR.Instances.Optics
  ( SOptic(..) )
import FIR.Prim.Singletons
  ( sPrimTy )
import qualified SPIRV.PrimTy  as SPIRV
import qualified SPIRV.Storage as Storage

----------------------------------------------------------------------------
-- list of ASTs

infixr 5 `ConsAST`

data ASTs (is :: [Type]) :: Type where
  NilAST  :: ASTs '[]
  ConsAST :: AST i -> ASTs is -> ASTs (i ': is)

-- unfortunate workaround to obtain optic indices using untyped machinery
unsafeRetypeUASTs :: forall as. SLength as -> UASTs -> Maybe (ASTs as)
unsafeRetypeUASTs lg as
  | correctLength lg as = Just ( unsafeRetypeAcc as NilAST )
  | otherwise = Nothing
  where unsafeRetypeAcc :: UASTs -> ASTs bs -> ASTs as
        unsafeRetypeAcc NilUAST bs = unsafeCoerce bs
        unsafeRetypeAcc (xs `SnocUAST` x) bs = unsafeRetypeAcc xs (x `ConsAST` bs)
        correctLength :: SLength xs -> UASTs -> Bool
        correctLength SZero NilUAST = True
        correctLength (SSucc l) (xs `SnocUAST` _) = correctLength l xs
        correctLength _ _ = False

----------------------------------------------------------------------------
-- pattern synonyms for optics

-- existential type for an optic with all its run-time indices specified
data IndexedOptic where
  AnIndexedOptic :: forall k is (s :: k) a (optic :: Optic is s a).
                    SOptic optic -> ASTs is -> IndexedOptic

pattern OpticUse :: IndexedOptic -> AST t
pattern OpticUse indexedOptic <- ( used -> Just indexedOptic )

used :: AST t -> Maybe IndexedOptic
used ( Applied (Use lg sOptic) is )
  = case unsafeRetypeUASTs lg is of
      Nothing -> Nothing
      Just is' -> Just ( AnIndexedOptic sOptic is' )
used _ = Nothing

pattern OpticAssign :: IndexedOptic -> UAST -> AST t
pattern OpticAssign indexedOptic a <- ( assigned -> Just ( indexedOptic, a ) )

assigned :: AST t -> Maybe (IndexedOptic, UAST)
assigned ( Applied (Assign lg sOptic) is :$ a )
  = case unsafeRetypeUASTs lg is of
      Nothing -> Nothing
      Just is' -> Just ( AnIndexedOptic sOptic is', UAST a )
assigned _ = Nothing

pattern OpticView :: IndexedOptic -> UAST -> AST t
pattern OpticView indexedOptic s <- ( viewed -> Just ( indexedOptic, s ) )

viewed :: AST t -> Maybe (IndexedOptic, UAST)
viewed ( Applied (View lg sOptic) is :$ s )
  = case unsafeRetypeUASTs lg is of
      Nothing -> Nothing
      Just is' -> Just ( AnIndexedOptic sOptic is', UAST s )
viewed _ = Nothing

pattern OpticSet :: IndexedOptic -> UAST -> UAST -> AST t
pattern OpticSet indexedOptic a s <- ( setted -> Just ( indexedOptic, a, s ) )

setted :: AST t -> Maybe (IndexedOptic, UAST, UAST)
setted ( ( Applied (Set lg sOptic) is :$ a ) :$ s )
  = case unsafeRetypeUASTs lg is of
    Nothing -> Nothing
    Just is' -> Just ( AnIndexedOptic sOptic is', UAST a, UAST s )
setted _ = Nothing

----------------------------------------------------------------------------
-- exported optic code generation functions

loadThroughAccessChain
  :: forall is s a (optic :: Optic is s a).
     (ID, SPIRV.PointerTy) -> SOptic optic -> ASTs is -> CGMonad (ID, SPIRV.PrimTy)
loadThroughAccessChain basePtr sOptic is
  = loadThroughAccessChain' basePtr =<< operationTree is sOptic

extractUsingGetter
  :: forall is s a (optic :: Optic is s a).
     (ID, SPIRV.PrimTy) -> SOptic optic -> ASTs is -> CGMonad (ID, SPIRV.PrimTy)
extractUsingGetter base sOptic is
  = extractUsingGetter' base =<< operationTree is sOptic

storeThroughAccessChain
  :: forall is s a (optic :: Optic is s a).
     (ID, SPIRV.PointerTy) -> (ID, SPIRV.PrimTy) -> SOptic optic -> ASTs is -> CGMonad ()
storeThroughAccessChain basePtr val sOptic is
  = storeThroughAccessChain' basePtr val =<< operationTree is sOptic

insertUsingSetter
  :: forall is s a (optic :: Optic is s a).
     Text -> (ID, SPIRV.PrimTy) -> (ID, SPIRV.PrimTy) -> SOptic optic -> ASTs is -> CGMonad ()
insertUsingSetter varName base val sOptic is
  = insertUsingSetter' varName base val =<< operationTree is sOptic

setUsingSetter
  :: forall is s a (optic :: Optic is s a).
     (ID, SPIRV.PrimTy)
  -> (ID, SPIRV.PrimTy)
  -> SOptic optic
  -> ASTs is
  -> CGMonad (ID, SPIRV.PrimTy)
setUsingSetter base val sOptic is
  = setUsingSetter' base val =<< operationTree is sOptic

----------------------------------------------------------------------------
-- optical trees

data OpticalOperation where
  Access :: SPIRV.PrimTy -> Indices -> OpticalOperation
    --        ^^^-- type of 'part'
  Join   :: OpticalOperation

infixr 5 `Then`

data OpticalOperationTree where
  Done    :: OpticalOperationTree
  Then    :: OpticalOperation -> OpticalOperationTree -> OpticalOperationTree
  Combine :: [OpticalOperationTree] -> OpticalOperationTree


operationTree :: forall k is (s :: k) a (optic :: Optic is s a).
                 ASTs is -> SOptic optic -> CGMonad OpticalOperationTree
operationTree _ SId    = pure Done
operationTree _ SJoint = pure (Join `Then` Done)
operationTree (i `ConsAST` _) (SAnIndex _ a _)
  = (`Then` Done) . Access (sPrimTy a) . RTInds Unsafe . (:[]) . fst <$> codeGen i
operationTree _ (SIndex _ a n)
  = pure $ Access (sPrimTy a) ( CTInds [n] ) `Then` Done
operationTree is (SComposeO lg1 opt1 opt2)
  = do  let (is1, is2) = composedIndices lg1 is
        ops1 <- operationTree is1 opt1
        ops2 <- operationTree is2 opt2
        pure ( ops1 `continue` ops2 )
    where continue :: OpticalOperationTree -> OpticalOperationTree -> OpticalOperationTree
          continue ops1 Done = ops1
          -- recurse on first argument
          continue Done ops2 = ops2
          continue (Combine trees) ops2
            = Combine $ map (`continue` ops2) trees
          continue (Access _ (CTInds is1) `Then` Done) (Access a2 (CTInds is2) `Then` ops2)
            = Access a2 (CTInds (is1 ++ is2)) `Then` ops2
          continue (Access _ (RTInds safe1 is1) `Then` Done) (Access a2 (RTInds safe2 is2) `Then` ops2)
            = Access a2 (RTInds (safe1 <> safe2) (is1 ++ is2)) `Then` ops2
          continue (op1 `Then` ops1) ops2 = op1 `Then` continue ops1 ops2
operationTree is (SProductO lg1 lg2 o1 o2)
  = do  let (is1, is2) = combinedIndices lg1 lg2 is
        t1 <- operationTree is1 o1
        t2 <- operationTree is2 o2
        let children
              = case ( t1, t2 ) of
                  (Combine ts1, Combine ts2) -> ts1 ++ ts2
                  (Combine ts1, _          ) -> ts1 ++ [t2]
                  (_          , Combine ts2) -> t1 : ts2
                  (_          , _          ) -> [t1, t2]
        pure (Combine children)
operationTree _ (SBinding _)
  = throwError "operationTree: trying to access a binding within a binding"
operationTree _ (SImageTexel _ _)
  = throwError "operationTree: unexpected image optic"

composedIndices :: SLength is -> ASTs (is :++: js) -> (ASTs is, ASTs js)
composedIndices SZero js = ( NilAST, js )
composedIndices (SSucc tail_is) (k `ConsAST` ks)
  = first ( k `ConsAST` ) (composedIndices tail_is ks)

combinedIndices :: SLength is -> SLength js -> ASTs (Zip is js) -> (ASTs is, ASTs js)
combinedIndices SZero SZero _ = ( NilAST, NilAST )
combinedIndices SZero (SSucc _) ks = ( NilAST, ks )
combinedIndices (SSucc _) SZero ks = ( ks, NilAST )
combinedIndices (SSucc is) (SSucc js) (k1k2 `ConsAST` ks)
  = case combinedIndices is js ks of
         ( is', js' ) -> ( (Fst :$ k1k2) `ConsAST` is', (Snd :$ k1k2) `ConsAST` js' )

----------------------------------------------------------------------------
-- code generation for optics

loadThroughAccessChain'
  :: (ID, SPIRV.PointerTy) -> OpticalOperationTree -> CGMonad (ID, SPIRV.PrimTy)
loadThroughAccessChain' (basePtrID, SPIRV.PointerTy _ eltTy) Done
  = loadInstruction eltTy basePtrID
loadThroughAccessChain' basePtr ( Access a is `Then` ops )
  = do
      newBasePtr <- accessChain basePtr a is
      loadThroughAccessChain' newBasePtr ops
loadThroughAccessChain' _ ( Join `Then` _ )
  = throwError "loadThroughAccessChain': unexpected 'Joint' optic used as a getter"
loadThroughAccessChain' _ ( Combine _ )
  = throwError "loadThroughAccessChain': product getter TODO"


extractUsingGetter'
  :: (ID, SPIRV.PrimTy) -> OpticalOperationTree -> CGMonad (ID, SPIRV.PrimTy)
extractUsingGetter' base Done
  = pure base
extractUsingGetter' base ( Access a (CTInds is) `Then` ops )
  = do
      newBase <- compositeExtract a is base
      extractUsingGetter' newBase ops
extractUsingGetter' (baseID, baseTy) ops@( Access _ (RTInds _ _) `Then` _ )
  -- run-time indices: revert to loading through pointers
  = do
      let ptrTy = SPIRV.PointerTy Storage.Function baseTy
      (basePtrID, basePtrState) <- temporaryVariable baseID ptrTy
      unless
        ( basePtrState == Fresh )
        ( do storeInstruction basePtrID baseID
             -- reset temporary pointer to "fresh" as we just loaded the baseID into it
             assign ( _temporaryPointer baseID ) ( Just (basePtrID, Fresh) )
        )
      loadThroughAccessChain' (basePtrID, ptrTy) ops
extractUsingGetter' _ ( Join `Then` _)
  = throwError "extractUsingGetter': unexpected 'Joint' optic used as a getter"
extractUsingGetter' _ ( Combine _ )
  = throwError "extractUsingGetter': product getter TODO"


storeThroughAccessChain'
  :: (ID, SPIRV.PointerTy) -> (ID, SPIRV.PrimTy) -> OpticalOperationTree -> CGMonad ()
storeThroughAccessChain' (basePtrID, _) (valID, _) Done
  = storeInstruction basePtrID valID
storeThroughAccessChain' basePtr val ( Access a is `Then` ops)
  = do
      newBasePtr <- accessChain basePtr a is
      storeThroughAccessChain' newBasePtr val ops
storeThroughAccessChain' _ _ ( Join `Then` _ )
  = throwError "storeThroughAccessChain': joint setter TODO"
storeThroughAccessChain' _ _ ( Combine _ )
  = throwError "storeThroughAccessChain': product setter TODO"

insertUsingSetter'
  :: Text -> (ID, SPIRV.PrimTy) -> (ID, SPIRV.PrimTy) -> OpticalOperationTree -> CGMonad ()
-- deal with some simple cases first
insertUsingSetter' varName _ val Done
  = assign ( _localBinding varName ) (Just val)
insertUsingSetter' varName base (valID, _) ( Access _ (CTInds is) `Then` Done )
  = assign ( _localBinding varName ) . Just
      =<< compositeInsert valID base is
-- in more complex situations, revert to storing through pointers
insertUsingSetter' varName (baseID, baseTy) val ops
  = do
      let ptrTy = SPIRV.PointerTy Storage.Function baseTy
      (basePtrID, basePtrState) <- temporaryVariable baseID ptrTy
      unless
        ( basePtrState == Fresh )
        ( do storeInstruction basePtrID baseID
             -- set the temporary pointer state to "modified"
             -- as we are about to store something into it
             assign ( _temporaryPointer baseID ) ( Just (basePtrID, Modified) )
        )
      storeThroughAccessChain' (basePtrID, ptrTy) val ops
      assign ( _localBinding varName ) . Just
        =<< loadInstruction baseTy basePtrID

setUsingSetter'
  :: (ID, SPIRV.PrimTy)
  -> (ID, SPIRV.PrimTy)
  -> OpticalOperationTree
  -> CGMonad (ID, SPIRV.PrimTy)
setUsingSetter' _ val Done
  = pure val
setUsingSetter' base (valID, _) ( Access _ (CTInds is) `Then` Done )
  = compositeInsert valID base is
-- in more complex situations, revert to using load/store
setUsingSetter' (baseID, baseTy) val ops
  = do
      let ptrTy = SPIRV.PointerTy Storage.Function baseTy
      (basePtrID, basePtrState) <- temporaryVariable baseID ptrTy
      unless
        ( basePtrState == Fresh )
        ( do storeInstruction basePtrID baseID
             -- set the temporary pointer state to "modified"
             -- as we are about to store something into it
             assign ( _temporaryPointer baseID ) ( Just (basePtrID, Modified) )
        )
      storeThroughAccessChain' (basePtrID, ptrTy) val ops
      loadInstruction baseTy basePtrID
