{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

module CodeGen.Optics
  ( IndexedOptic(AnIndexedOptic)
  , IndexedOptic'(AnIndexedOptic')
  , IndexedTypedOptic(AnIndexedTypedOptic)
  , IndexedAssignment(AnIndexedAssignment)
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
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Typeable
  ( eqT )
import Data.Word
  ( Word32 )
import Unsafe.Coerce
  ( unsafeCoerce )

-- lens
import Control.Lens
  ( assign )

-- mtl
import Control.Monad.Except
  ( throwError )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import CodeGen.Application
  ( UAST(UAST)
  , ASTs(NilAST, ConsAST)
  , pattern UApplied
  , unsafeRetypeUASTs
  )
import {-# SOURCE #-} CodeGen.CodeGen
  ( codeGen )
import CodeGen.Composite
  ( compositeConstruct, compositeExtract, compositeInsert
  , vectorSwizzle
  )
import CodeGen.IDs
  ( constID )
import CodeGen.Instruction
  ( ID )
import CodeGen.Monad
  ( CGMonad )
import CodeGen.Pointers
  ( Safeness(Safe, Unsafe)
  , Indices(RTInds, CTInds)
  , temporaryVariable, accessChain
  , loadInstruction, storeInstruction
  )
import CodeGen.State
  ( PointerState(Fresh, Modified)
  , _localBinding, _temporaryPointer
  )
import Control.Type.Optic
  ( Optic
  , ProductComponents(..) )
import Data.Constraint.All
  ( All(allDict)
  , AllDict(NilDict, ConsDict)
  )
import Data.Product
  ( HList(HNil, (:>))
  , MapHList
  , IsProduct(toHList)
  , AreProducts(productsDict)
  , AreProductsDict(ConsProducts)
  , Distribute
  , distributeZipConsLemma1
  )
import Data.Type.List
  ( type (:++:), ZipCons
  , KnownLength(sLength)
  , SLength(SZero, SSucc)
  , SameLength(sSameLength)
  , SSameLength(SSameZero, SSameSucc)
  )
import FIR.AST
  ( AST
    ( (:$), Lit
    , MkID
    , Use, Assign, View, Set
    , NilHList, ConsHList
    )
  )
import FIR.Instances.Optics
  ( SOptic(..), SProductComponents(..) )
import FIR.Prim.Singletons
  ( PrimTy, primTy, SPrimTy(..) )
import qualified SPIRV.PrimTy  as SPIRV
import qualified SPIRV.Storage as Storage

----------------------------------------------------------------------------
-- pattern synonyms for optics

-- existential types for an optic with all its run-time indices specified
-- several variants keeping track of various amounts of type-level information

data IndexedOptic where
  AnIndexedOptic
    :: forall (k :: Type) (is :: [Type]) (s :: k) (a :: Type) (optic :: Optic is s a)
    .  SOptic optic -> ASTs is -> IndexedOptic

data IndexedOptic' where
  AnIndexedOptic'
    :: forall (is :: [Type]) (s :: Type) (a :: Type) (optic :: Optic is s a)
    .  SOptic optic -> ASTs is -> IndexedOptic'

data IndexedTypedOptic (s :: Type) (a :: Type) where
  AnIndexedTypedOptic
    :: forall (is :: [Type]) (s :: Type) (a :: Type) (optic :: Optic is s a)
    .  SOptic optic -> ASTs is -> IndexedTypedOptic s a

data IndexedAssignment where
  AnIndexedAssignment
    :: forall (k :: Type) (is :: [Type]) (s :: k) (a :: Type) (optic :: Optic is s a)
    .  SOptic optic -> ASTs is -> AST a -> IndexedAssignment

pattern OpticUse :: IndexedOptic -> AST t
pattern OpticUse indexedOptic <- ( used -> Just indexedOptic )

used :: AST t -> Maybe IndexedOptic
used ( UApplied (Use lg sOptic) is )
  = case unsafeRetypeUASTs lg is of
      Nothing  -> Nothing
      Just is' -> Just ( AnIndexedOptic sOptic is' )
used _ = Nothing

pattern OpticAssign :: IndexedAssignment -> AST t
pattern OpticAssign indexedAssignment <- ( assigned -> Just indexedAssignment )

assigned :: AST t -> Maybe IndexedAssignment
assigned ( UApplied (Assign lg sOptic) is :$ a )
  = case unsafeRetypeUASTs lg is of
      Nothing  -> Nothing
      Just is' -> Just ( AnIndexedAssignment sOptic is' ( unsafeCoerce a ) )
assigned _ = Nothing

pattern OpticView :: IndexedOptic' -> UAST -> AST t
pattern OpticView indexedOptic s <- ( viewed -> Just ( indexedOptic, s ) )

viewed :: AST t -> Maybe (IndexedOptic', UAST)
viewed ( UApplied ( View lg sOptic ) is :$ s )
  = case unsafeRetypeUASTs lg is of
      Nothing  -> Nothing
      Just is' -> Just ( AnIndexedOptic' sOptic is', UAST s )
viewed _ = Nothing

pattern OpticSet :: IndexedTypedOptic s a -> AST a -> AST s -> AST t
pattern OpticSet indexedOptic a s <- ( setted -> Just ( indexedOptic, a, s ) )

setted :: AST s -> Maybe (IndexedTypedOptic s a, AST a, AST s)
setted ( UApplied ( Set lg sOptic ) is :$ a :$ s )
  = case unsafeRetypeUASTs lg is of
      Nothing  -> Nothing
      Just is' -> Just ( unsafeCoerce $ AnIndexedTypedOptic sOptic is', unsafeCoerce a, unsafeCoerce s )
setted _ = Nothing

----------------------------------------------------------------------------
-- exported optic code generation functions

loadThroughAccessChain
  :: forall is s a (optic :: Optic is s a)
  .  (ID, SPIRV.PointerTy) -> SOptic optic -> ASTs is -> CGMonad (ID, SPIRV.PrimTy)
loadThroughAccessChain basePtr sOptic is
  = loadThroughAccessChain' basePtr =<< operationTree is sOptic

extractUsingGetter
  :: forall is s a (optic :: Optic is s a)
  .  (ID, SPIRV.PrimTy) -> SOptic optic -> ASTs is -> CGMonad (ID, SPIRV.PrimTy)
extractUsingGetter base sOptic is
  = extractUsingGetter' base =<< operationTree is sOptic

storeThroughAccessChain
  :: forall is s a (optic :: Optic is s a)
  .  (ID, SPIRV.PointerTy) -> AST a -> SOptic optic -> ASTs is -> CGMonad ()
storeThroughAccessChain basePtr val sOptic is
  = storeThroughAccessChain' basePtr val =<< operationTree is sOptic

insertUsingSetter
  :: forall is s a (optic :: Optic is s a)
  .  ShortText -> (ID, SPIRV.PrimTy) -> AST a -> SOptic optic -> ASTs is -> CGMonad ()
insertUsingSetter varName base val sOptic is = do
  tree   <- operationTree is sOptic
  insertUsingSetter' varName base val tree

setUsingSetter
  :: forall is s a (optic :: Optic is s a)
  .  AST s
  -> AST a
  -> SOptic optic
  -> ASTs is
  -> CGMonad (ID, SPIRV.PrimTy)
setUsingSetter base val sOptic is = do
  baseID <- codeGen base
  tree   <- operationTree is sOptic
  setUsingSetter' baseID val tree

----------------------------------------------------------------------------
-- optical trees

data OpticalOperation where
  Access :: Safeness -> Indices -> OpticalOperation
  Join   :: OpticalOperation

infixr 5 `Then`

data OpticalOperationTree where
  Done    :: OpticalOperationTree
  Then    :: OpticalOperation -> OpticalOperationTree -> OpticalOperationTree
  Combine :: forall (p :: Type) (as :: [Type])
          .  (PrimTy p, All PrimTy as, IsProduct p as)
          => Proxy p -> Proxy as -> [OpticalOperationTree] -> OpticalOperationTree


operationTree :: forall k is (s :: k) a (optic :: Optic is s a)
              .  ASTs is -> SOptic optic -> CGMonad OpticalOperationTree
operationTree _ SId    = pure Done
operationTree _ SJoint = pure (Join `Then` Done)
operationTree (i `ConsAST` _) SAnIndex {}
  = (`Then` Done) . Access Unsafe . RTInds . (:[]) . fst <$> codeGen i
operationTree _ (SIndex s _ n)
  -- if accessing a runtime array, a compile-time index may be unsafe
  | SRuntimeArray <- s
  = pure $ Access Unsafe ( CTInds [n] ) `Then` Done
  -- otherwise, a compile time index is guaranteed to be in-bounds
  | otherwise
  = pure $ Access Safe ( CTInds [n] ) `Then` Done
operationTree is (SComposeO lg1 opt1 opt2)
  = do  let (is1, is2) = composedIndices lg1 is
        ops1 <- operationTree is1 opt1
        ops2 <- operationTree is2 opt2
        ops1 `continue` ops2
    where continue :: OpticalOperationTree -> OpticalOperationTree -> CGMonad OpticalOperationTree
          continue ops1 Done = pure ops1
          -- recurse on first argument
          continue Done ops2 = pure ops2
          continue (Combine p as trees) ops2
            = Combine p as <$> traverse (`continue` ops2) trees
          -- try to combine successive accesses into a single access chain as much as possible
          -- (this helps bypass Vulkan implementation bugs in tessellation evaluation shaders)
          continue (Access safe1 (CTInds is1) `Then` Done) (Access safe2 (CTInds is2) `Then` ops2)
            = pure $ Access (safe1 <> safe2) (CTInds (is1 ++ is2)) `Then` ops2
          continue (Access safe1 (RTInds is1) `Then` Done) (Access safe2 (RTInds is2) `Then` ops2)
            = pure $ Access (safe1 <> safe2) (RTInds (is1 ++ is2)) `Then` ops2
          continue (Access safe1 (CTInds is1) `Then` Done) (Access safe2 (RTInds is2) `Then` ops2)
            = do
                js1 <- traverse constID is1
                pure $ Access (safe1 <> safe2) (RTInds (js1 ++ is2)) `Then` ops2
          continue (Access safe1 (RTInds is1) `Then` Done) (Access safe2 (CTInds is2) `Then` ops2)
            = do
                js2 <- traverse constID is2
                pure $ Access (safe1 <> safe2) (RTInds (is1 ++ js2)) `Then` ops2
          continue (op1 `Then` ops1) ops2
            = do
                cont <- ops1 `continue` ops2
                pure $ op1 `Then` cont
operationTree is
  ( SProd same (comps :: SProductComponents (os :: ProductComponents iss s as))
    :: SOptic (optic :: Optic is s a)
  ) =
    Combine ( Proxy @a ) ( Proxy @as ) <$>
      ( componentsTrees same comps =<<
          combinedIndices @iss @is @as sSameLength sLength is
      )
operationTree _ (SBinding _)
  = throwError "operationTree: trying to access a binding within a binding"
operationTree _ (SImageTexel _ _)
  = throwError "operationTree: unexpected image optic"

componentsTrees
  :: forall (k :: Type) (iss :: [[Type]]) (s :: k) (as :: [Type]) (os :: ProductComponents iss s as)
  .  SSameLength (Distribute iss as) as
  -> SProductComponents os
  -> ASTs (MapHList (Distribute iss as))
  -> CGMonad [OpticalOperationTree]
componentsTrees _                       SEndProd             _                  = pure []
componentsTrees sameSucc@(SSameSucc lg) (so `SProductO` sos) (is `ConsAST` iss) =
  case sameSucc of
    ( _ :: SSameLength (ds ': dss) (b ': bs) ) ->
      case so of
        ( _ :: SOptic ( o :: Optic es s b ) ) ->
          case sos of
           ( _ :: SProductComponents ( tail_os :: ProductComponents ess s bs ) ) ->
            case distributeZipConsLemma1 @b @bs @ds @dss @es @ess of
              (Refl, Refl) ->
                (:) <$> operationTree (astsFromHList is) so <*> componentsTrees lg sos iss

composedIndices :: SLength is -> ASTs (is :++: js) -> (ASTs is, ASTs js)
composedIndices SZero           js               = ( NilAST, js )
composedIndices (SSucc tail_is) (k `ConsAST` ks)
  = first ( k `ConsAST` ) ( composedIndices tail_is ks )

combinedIndices
  :: forall (jss :: [[Type]]) (is :: [Type]) (as :: [Type])
  .  AreProducts is jss as
  => SSameLength is jss
  -> SLength as
  -> ASTs is
  -> CGMonad (ASTs (MapHList (Distribute jss as)))
combinedIndices        SSameZero     SZero      NilAST          = pure $ NilAST
combinedIndices        SSameZero     (SSucc lg) NilAST          = (NilHList `ConsAST`) <$> combinedIndices SSameZero lg NilAST
combinedIndices sSame@(SSameSucc sm) lg        (i `ConsAST` is) =
  case sSame of
    ( _ :: SSameLength (p ': ps) (es ': ess) ) ->
      case productsDict @is @jss @as of
        ConsProducts ->
          zipIndices @es @(Distribute ess as) allDict sSameLength
            <$> deconstruct sLength i
            <*> combinedIndices sm lg is

zipIndices :: AllDict PrimTy is -> SSameLength is jss -> ASTs is -> ASTs (MapHList jss) -> ASTs (MapHList (ZipCons is jss))
zipIndices NilDict  SSameZero      NilAST           NilAST             = NilAST
zipIndices ConsDict (SSameSucc lg) (i `ConsAST` is) (js `ConsAST` jss) =
  (ConsHList :$ i :$ js) `ConsAST` zipIndices allDict lg is jss

deconstruct
  :: forall (p :: Type) (as :: [Type])
  .  (IsProduct p as, All PrimTy as)
  => SLength as -> AST p -> CGMonad (ASTs as)
deconstruct SZero      _       = pure NilAST
deconstruct _          (Lit x) = pure $ hListToASTs allDict (toHList @p @as x)
deconstruct _          p
  | Just hlist <- recogniseHList allDict p
  = pure hlist
  -- TODO: more cases to avoid spurious "compositeConstruct ---> compositeExtract"
deconstruct (SSucc lg) p = do
    composite <- codeGen p
    compositeExtractAll composite (SSucc lg) 0


recogniseHList :: AllDict PrimTy as -> AST p -> Maybe (ASTs as)
recogniseHList NilDict           _   = Just NilAST
recogniseHList consDict@ConsDict ast
  = case consDict of
      ( _ :: AllDict PrimTy (b ': bs) ) ->
        case ast of
          ( ConsHList :$ (c :: AST c) :$ cs )
            | Just Refl <- eqT @b @c
            -> (c `ConsAST`) <$> recogniseHList allDict cs
          _ -> Nothing

astsFromHList :: AST (HList as) -> ASTs as
astsFromHList NilHList
  = NilAST
astsFromHList (ConsHList :$ a :$ as)
  = a `ConsAST` astsFromHList as
astsFromHList _
  = error "'operationTree': indexing heterogenous list AST not of the expected form (TODO?)"

hListToASTs :: AllDict PrimTy as -> HList as -> ASTs as
hListToASTs _        HNil      = NilAST
hListToASTs ConsDict (a :> as) = Lit a `ConsAST` hListToASTs allDict as

compositeExtractAll :: (ID, SPIRV.PrimTy) -> SLength as -> Word32 -> CGMonad (ASTs as)
compositeExtractAll _     SZero     _ = pure NilAST
compositeExtractAll comp (SSucc lg) i = do
  extracted <- compositeExtract [i] comp
  next      <- compositeExtractAll comp lg (succ i)
  pure (MkID extracted `ConsAST` next)

----------------------------------------------------------------------------
-- code generation for optics

-- check whether a vector swizzle operation can be used
swizzleIndices :: SPIRV.PrimTy -> [OpticalOperationTree] -> Maybe [Word32]
swizzleIndices (SPIRV.Vector _ _) = traverse simpleIndex
  where
    simpleIndex :: OpticalOperationTree -> Maybe Word32
    simpleIndex (Access Safe (CTInds [i]) `Then` Done) = Just i
    simpleIndex _ = Nothing
swizzleIndices _ = const Nothing

loadThroughAccessChain'
  :: (ID, SPIRV.PointerTy) -> OpticalOperationTree -> CGMonad (ID, SPIRV.PrimTy)
loadThroughAccessChain' (basePtrID, SPIRV.PointerTy _ eltTy) Done
  = loadInstruction eltTy basePtrID
loadThroughAccessChain' basePtr ( Access safe is `Then` ops )
  = do
      newBasePtr <- accessChain basePtr safe is
      loadThroughAccessChain' newBasePtr ops
loadThroughAccessChain' basePtr@(_, SPIRV.PointerTy _ eltTy) ( Combine (_ :: Proxy p) _ trees )
  -- special case: can we use a vector swizzle operation?
  | Just is <- swizzleIndices eltTy trees
  = do
      base <- loadThroughAccessChain' basePtr Done
      vectorSwizzle base is
  | otherwise
  =  compositeConstruct (primTy @p) =<< traverse (fmap fst . loadThroughAccessChain' basePtr) trees
loadThroughAccessChain' _ ( Join `Then` _ )
  = throwError "loadThroughAccessChain': unexpected 'Joint' optic used as a getter"


extractUsingGetter'
  :: (ID, SPIRV.PrimTy) -> OpticalOperationTree -> CGMonad (ID, SPIRV.PrimTy)
extractUsingGetter' base Done
  = pure base
extractUsingGetter' base ( Access _ (CTInds is) `Then` ops )
  = do
      newBase <- compositeExtract is base
      extractUsingGetter' newBase ops
extractUsingGetter' (baseID, baseTy) ops@( Access _ (RTInds _) `Then` _ )
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
extractUsingGetter' base@(_, eltTy) ( Combine (_ :: Proxy p) _ trees )
  -- special case: can we use a vector swizzle operation?
  | Just is <- swizzleIndices eltTy trees
  = vectorSwizzle base is
  | otherwise
  = compositeConstruct (primTy @p) =<< traverse (fmap fst . extractUsingGetter' base) trees
extractUsingGetter' _ ( Join `Then` _ )
  = throwError "extractUsingGetter': unexpected 'Joint' optic used as a getter"


storeThroughAccessChain'
  :: (ID, SPIRV.PointerTy) -> AST a -> OpticalOperationTree -> CGMonad ()
storeThroughAccessChain' (basePtrID, _) val Done
  = storeInstruction basePtrID =<< fst <$> codeGen val
storeThroughAccessChain' basePtr        val ( Access safe is `Then` ops)
  = do
      newBasePtr <- accessChain basePtr safe is
      storeThroughAccessChain' newBasePtr val ops
storeThroughAccessChain' basePtr        val ( Combine (_ :: Proxy p) (_ :: Proxy as) trees )
  = successiveStores basePtr trees =<< deconstruct @p @as sLength (unsafeCoerce val)
--storeThroughAccessChain' basePtr val ( Join `Then` Combine _ trees )
--  = jointStores basePtr trees =<< deconstruct @p @as sLength (unsafeCoerce val)
storeThroughAccessChain' _              _ ( Join `Then` _ )
  = throwError "storeThroughAccessChain': joint setter TODO"

successiveStores :: (ID, SPIRV.PointerTy) -> [OpticalOperationTree] -> ASTs as -> CGMonad ()
successiveStores basePtr ( tree : trees ) (a `ConsAST` as)
  = do
      storeThroughAccessChain' basePtr a tree
      successiveStores basePtr trees as
successiveStores _       _                _
  = pure ()

{-
jointStores :: (ID, SPIRV.PointerTy) -> AST a -> [OpticalOperationTree] -> CGMonad ()
jointStores basePtr a ( tree : trees )
  = do
      storeThroughAccessChain' basePtr a tree
      jointStores basePtr a trees
jointStores _       _ []
  = pure ()
-}

insertUsingSetter'
  :: ShortText -> (ID, SPIRV.PrimTy) -> AST a -> OpticalOperationTree -> CGMonad ()
-- deal with some simple cases first
insertUsingSetter' varName _ val Done = do
  valID <- codeGen val
  assign ( _localBinding varName ) ( Just valID )
insertUsingSetter' varName base val ( Access _ (CTInds is) `Then` Done ) = do
  valID   <- codeGen val
  updated <- compositeInsert valID base is
  assign ( _localBinding varName ) ( Just updated )
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
  -> AST a
  -> OpticalOperationTree
  -> CGMonad (ID, SPIRV.PrimTy)
setUsingSetter' _ val Done = codeGen val
setUsingSetter' base val ( Access _ (CTInds is) `Then` Done ) = do
  valID <- codeGen val
  compositeInsert valID base is
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
