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
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Word
  ( Word32 )

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
import Data.Product
  ( HList
  , MapHList
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
    ( (:$)
    , Use, Assign, View, Set
    , NilHList, ConsHList
    , HeadHList, TailHList
    , ProductToHList
    )
  )
import FIR.Instances.Optics
  ( SOptic(..), SProductComponents(..) )
import FIR.Prim.Singletons
  ( SPrimTy(..), primTy )
import qualified SPIRV.PrimTy  as SPIRV
import qualified SPIRV.Storage as Storage

----------------------------------------------------------------------------
-- pattern synonyms for optics

-- existential type for an optic with all its run-time indices specified
data IndexedOptic where
  AnIndexedOptic :: forall k is (s :: k) a (optic :: Optic is s a).
                    SOptic optic -> ASTs is -> IndexedOptic

pattern OpticUse :: IndexedOptic -> AST t
pattern OpticUse indexedOptic <- ( used -> Just indexedOptic )

used :: AST t -> Maybe IndexedOptic
used ( UApplied (Use lg sOptic) is )
  = case unsafeRetypeUASTs lg is of
      Nothing  -> Nothing
      Just is' -> Just ( AnIndexedOptic sOptic is' )
used _ = Nothing

pattern OpticAssign :: IndexedOptic -> UAST -> AST t
pattern OpticAssign indexedOptic a <- ( assigned -> Just ( indexedOptic, a ) )

assigned :: AST t -> Maybe (IndexedOptic, UAST)
assigned ( UApplied (Assign lg sOptic) is :$ a )
  = case unsafeRetypeUASTs lg is of
      Nothing  -> Nothing
      Just is' -> Just ( AnIndexedOptic sOptic is', UAST a )
assigned _ = Nothing

pattern OpticView :: IndexedOptic -> UAST -> AST t
pattern OpticView indexedOptic s <- ( viewed -> Just ( indexedOptic, s ) )

viewed :: AST t -> Maybe (IndexedOptic, UAST)
viewed ( UApplied (View lg sOptic) is :$ s )
  = case unsafeRetypeUASTs lg is of
      Nothing  -> Nothing
      Just is' -> Just ( AnIndexedOptic sOptic is', UAST s )
viewed _ = Nothing

pattern OpticSet :: IndexedOptic -> UAST -> UAST -> AST t
pattern OpticSet indexedOptic a s <- ( setted -> Just ( indexedOptic, a, s ) )

setted :: AST t -> Maybe (IndexedOptic, UAST, UAST)
setted ( ( UApplied (Set lg sOptic) is :$ a ) :$ s )
  = case unsafeRetypeUASTs lg is of
    Nothing  -> Nothing
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
     ShortText -> (ID, SPIRV.PrimTy) -> (ID, SPIRV.PrimTy) -> SOptic optic -> ASTs is -> CGMonad ()
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
  Access :: Safeness -> Indices -> OpticalOperation
  Join   :: OpticalOperation

infixr 5 `Then`

data OpticalOperationTree where
  Done    :: OpticalOperationTree
  Then    :: OpticalOperation -> OpticalOperationTree -> OpticalOperationTree
  Combine :: SPIRV.PrimTy -> [OpticalOperationTree] -> OpticalOperationTree
    --        ^^^^^^ result of combining


operationTree :: forall k is (s :: k) a (optic :: Optic is s a).
                 ASTs is -> SOptic optic -> CGMonad OpticalOperationTree
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
          continue (Combine cb trees) ops2
            = Combine cb <$> traverse (`continue` ops2) trees
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
  ) =  Combine (primTy @a)
   <$> componentsTrees same (combinedIndices @iss @is @as sSameLength sLength is) comps
operationTree _ (SBinding _)
  = throwError "operationTree: trying to access a binding within a binding"
operationTree _ (SImageTexel _ _)
  = throwError "operationTree: unexpected image optic"

componentsTrees
  :: forall (k :: Type) (iss :: [[Type]]) (s :: k) (as :: [Type]) (os :: ProductComponents iss s as)
  .  SSameLength (Distribute iss as) as
  -> ASTs (MapHList (Distribute iss as))
  -> SProductComponents os
  -> CGMonad [OpticalOperationTree]
componentsTrees _                       _                  SEndProd             = pure []
componentsTrees sameSucc@(SSameSucc lg) (is `ConsAST` iss) (so `SProductO` sos) =
  case sameSucc of
    ( _ :: SSameLength (ds ': dss) (b ': bs) ) ->
      case so of
        ( _ :: SOptic ( o :: Optic es s b ) ) ->
          case sos of
           ( _ :: SProductComponents ( tail_os :: ProductComponents ess s bs ) ) ->
            case distributeZipConsLemma1 @b @bs @ds @dss @es @ess of
              (Refl, Refl) ->
                (:) <$> operationTree (astsFromHList is) so <*> componentsTrees lg iss sos

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
  -> ASTs (MapHList (Distribute jss as))
combinedIndices        SSameZero     SZero      NilAST           = NilAST
combinedIndices        SSameZero     (SSucc lg) NilAST           = NilHList `ConsAST` combinedIndices SSameZero lg NilAST
combinedIndices sSame@(SSameSucc sm) lg         (i `ConsAST` is) =
  case sSame of
    ( _ :: SSameLength (p ': ps) (es ': ess) ) ->
      case productsDict @is @jss @as of
        ConsProducts ->
          zipIndices sSameLength (toHListASTs sLength (ProductToHList :$ i)) (combinedIndices sm lg is)


toHListASTs :: SLength as -> AST (HList as) -> ASTs as
toHListASTs SZero      _ = NilAST
toHListASTs (SSucc lg) p = ( HeadHList :$ p ) `ConsAST` toHListASTs lg ( TailHList :$ p )

astsFromHList :: AST (HList as) -> ASTs as
astsFromHList NilHList = NilAST
astsFromHList (ConsHList :$ a :$ as)
  = a `ConsAST` astsFromHList as
astsFromHList _
  = error "'operationTree': indexing heterogenous list AST not of the expected form (TODO?)"

zipIndices :: SSameLength is jss -> ASTs is -> ASTs (MapHList jss) -> ASTs (MapHList (ZipCons is jss))
zipIndices SSameZero      NilAST           NilAST             = NilAST
zipIndices (SSameSucc lg) (i `ConsAST` is) (js `ConsAST` jss) =
  (ConsHList :$ i :$ js) `ConsAST` zipIndices lg is jss

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
loadThroughAccessChain' basePtr ( Combine cb trees )
  -- special case: can we use a vector swizzle operation?
  | Just is <- swizzleIndices cb trees
  = do
      base <- loadThroughAccessChain' basePtr Done
      vectorSwizzle base is
  | otherwise
  = do
      components <- traverse (fmap fst . loadThroughAccessChain' basePtr) trees
      compositeConstruct cb components
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
extractUsingGetter' base ( Combine cb trees )
  -- special case: can we use a vector swizzle operation?
  | Just is <- swizzleIndices cb trees
  = vectorSwizzle base is
  | otherwise
  = do
      components <- traverse (fmap fst . extractUsingGetter' base) trees
      compositeConstruct cb components
extractUsingGetter' _ ( Join `Then` _)
  = throwError "extractUsingGetter': unexpected 'Joint' optic used as a getter"


storeThroughAccessChain'
  :: (ID, SPIRV.PointerTy) -> (ID, SPIRV.PrimTy) -> OpticalOperationTree -> CGMonad ()
storeThroughAccessChain' (basePtrID, _) (valID, _) Done
  = storeInstruction basePtrID valID
storeThroughAccessChain' basePtr val ( Access safe is `Then` ops)
  = do
      newBasePtr <- accessChain basePtr safe is
      storeThroughAccessChain' newBasePtr val ops
storeThroughAccessChain' _ _ ( Combine _ _ )
  = throwError "storeThroughAccessChain': product setter TODO"
storeThroughAccessChain' _ _ ( Join `Then` _ )
  = throwError "storeThroughAccessChain': joint setter TODO"


insertUsingSetter'
  :: ShortText -> (ID, SPIRV.PrimTy) -> (ID, SPIRV.PrimTy) -> OpticalOperationTree -> CGMonad ()
-- deal with some simple cases first
insertUsingSetter' varName _ val Done
  = assign ( _localBinding varName ) (Just val)
insertUsingSetter' varName base val ( Access _ (CTInds is) `Then` Done )
  = assign ( _localBinding varName ) . Just
      =<< compositeInsert val base is
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
setUsingSetter' base val ( Access _ (CTInds is) `Then` Done )
  = compositeInsert val base is
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
