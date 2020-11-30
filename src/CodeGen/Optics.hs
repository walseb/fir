{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

{-|
Module: CodeGen.Optics

Code generation for operations involving the type-level optics used by this library,
such as @view@, @set@, @use@, @assign@.

-}

module CodeGen.Optics ( ) where

-- base
import Control.Arrow
  ( first )
import Control.Monad
  ( unless, forM_ )
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
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Application
  ( ASTs(NilAST, ConsAST), pattern SnocAST
  , Codes, pattern ConsCode
  , Application(..)
  )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
import CodeGen.Composite
  ( compositeConstruct
  , compositeExtract
  , compositeInsert
  , vectorSwizzle
  , productConstruct
  )
import CodeGen.Debug
  ( whenAsserting )
import CodeGen.IDs
  ( constID, bindingID, typeID )
import CodeGen.Images
  ( imageTexel, writeTexel )
import CodeGen.Instruction
  ( ID(ID) )
import CodeGen.Monad
  ( CGMonad )
import CodeGen.Pointers
  ( Safeness(Safe, Unsafe)
  , Indices(RTInds, CTInds)
  , temporaryVariable, accessChain
  , load, store
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
import Data.Type.Known
  ( knownValue )
import Data.Type.List
  ( type (:++:), ZipCons
  , KnownLength(sLength)
  , SLength(SZero, SSucc)
  , SameLength(sSameLength)
  , SSameLength(SSameZero, SSameSucc)
  , Snoc
  )
import FIR.AST
  ( AST, Code
  , pattern (:$), pattern Lit
  , pattern MkID
  , pattern NilHList, pattern ConsHList
  , UseF(..), AssignF(..), ViewF(..), SetF(..)
  )
import FIR.AST.Type
  ( AugType(Val), MapVal )
import FIR.Prim.Singletons
  ( PrimTy, primTy, primTys
  , SPrimTy(..), sPrimTy
  )
import FIR.Syntax.Optics
  ( SOptic(..), showSOptic
  , SProductComponents(..)
  )
import qualified SPIRV.PrimTy  as SPIRV
import qualified SPIRV.PrimTy
  ( almostEqual )
import qualified SPIRV.Storage as Storage

----------------------------------------------------------------------------
-- Code generation for optics.

-- View.

data OpticView where
  OpticView :: forall is s a (optic :: Optic is s a). SOptic optic -> Codes is -> Code s -> OpticView

viewed :: Application (ViewF AST) f r -> OpticView
viewed ( Applied ( ViewF _ ( sOptic :: SOptic ( optic :: Optic is s a) ) ) is_s ) =
  case unsafeCoerce is_s :: ( ASTs (MapVal is `Snoc` Val s) ) of
    ( is `SnocAST` s ) -> OpticView sOptic is s

instance CodeGen AST => CodeGen (ViewF AST) where
  codeGenArgs ( viewed -> OpticView sOptic is s ) = do
    base <- codeGen s
    extractUsingGetter base sOptic is

-- Set.

data OpticSet where
  OpticSet :: forall is s a (optic :: Optic is s a). SOptic optic -> Codes is -> Code a -> Code s -> OpticSet

setted :: Application (SetF AST) f r -> OpticSet
setted ( Applied ( SetF _ ( sOptic :: SOptic ( optic :: Optic is s a) ) ) is_a_s ) =
  case unsafeCoerce is_a_s :: ( ASTs ( MapVal is `Snoc` Val a `Snoc` Val s ) ) of
    ( is `SnocAST` a `SnocAST` s ) -> OpticSet sOptic is a s

instance CodeGen AST => CodeGen (SetF AST) where
  codeGenArgs ( setted -> OpticSet sOptic is a s ) =
    setUsingSetter s a sOptic is

-- Use.

data OpticUse where
  OpticUse :: forall is s a (optic :: Optic is s a). SOptic optic -> Codes is -> OpticUse

used :: Application (UseF AST) f r -> OpticUse
used ( Applied ( UseF _ ( sOptic :: SOptic ( optic :: Optic is s a) ) ) is ) =
  case unsafeCoerce is :: ASTs ( MapVal is ) of
    js -> OpticUse sOptic js

instance CodeGen AST => CodeGen (UseF AST) where
  codeGenArgs ( used -> OpticUse sOptic is ) = case sOptic of

    SBinding (_ :: Proxy name) ->
      do  let varName = knownValue @name
          bd@(bdID, bdTy) <- bindingID varName
          case bdTy of
            SPIRV.Pointer storage eltTy
              -> load (varName, bdID) (SPIRV.PointerTy storage eltTy)
            _ -> pure bd

    -- image
    SImageTexel ( _ :: Proxy name ) ( _ :: Proxy props ) texelTy
      -> case is of 
          ( ops `ConsAST` coords `ConsAST` NilAST )
            -> do
                  let imgName = knownValue @name
                      imgTexelTy = sPrimTy texelTy
                  bd@(bdID, bdTy) <- bindingID imgName
                  resTyID <- typeID imgTexelTy
                  img <- case bdTy of
                            SPIRV.Pointer storage imgTy
                              -> load (imgName, bdID) (SPIRV.PointerTy storage imgTy)
                            _ -> pure bd
                  (cdID, _) <- codeGen coords
                  imageTexel img ops cdID (resTyID, imgTexelTy)

    SComposeO _ (SBinding (_ :: Proxy name )) getter ->
      do  let varName = knownValue @name
          bd@(bdID, bdTy) <- bindingID varName

          case bdTy of
            SPIRV.Pointer storage eltTy
              -> loadThroughAccessChain (bdID, SPIRV.PointerTy storage eltTy) getter is
            _ -> extractUsingGetter     bd                                    getter is

    SComposeO
      (SSucc (SSucc _))
      (SImageTexel ( _ :: Proxy name ) ( _ :: Proxy props ) texelTy)
      getter
       -> case is of
            ( ops `ConsAST` coords `ConsAST` nextindices )
              ->
                do 
                  -- copy-pasted from above
                  let imgName = knownValue @name
                      imgTexelTy = sPrimTy texelTy
                  bd@(bdID, bdTy) <- bindingID imgName
                  resTyID <- typeID imgTexelTy
                  img <- case bdTy of
                            SPIRV.Pointer storage imgTy
                              -> load (imgName, bdID) (SPIRV.PointerTy storage imgTy)
                            _ -> pure bd
                  (cdID, _) <- codeGen coords
                  -- end of copy-paste
                  texel <- imageTexel img ops cdID (resTyID, imgTexelTy)
                  extractUsingGetter texel getter nextindices

    _ -> throwError (   "codeGen: cannot 'use', unsupported optic:\n"
                     <> ShortText.pack (showSOptic sOptic) <> "\n"
                     <> "Optic does not start by accessing a binding (or image texel).\n"
                    )

-- Assign.

data OpticAssign where
  OpticAssign :: forall is s a (optic :: Optic is s a). SOptic optic -> Codes is -> Code a -> OpticAssign

assigned :: Application (AssignF AST) f r -> OpticAssign
assigned ( Applied ( AssignF _ ( sOptic :: SOptic ( optic :: Optic is s a) ) ) is_a ) =
  case unsafeCoerce is_a :: ( ASTs ( MapVal is `Snoc` Val a ) ) of
    ( is `SnocAST` a ) -> OpticAssign sOptic is a

instance CodeGen AST => CodeGen (AssignF AST) where
  codeGenArgs ( assigned -> OpticAssign sOptic is a ) = case sOptic of

   SBinding (_ :: Proxy name) ->
     do  let varName = knownValue @name
         a_IDTy@(a_ID, _) <- codeGen a
         (bdID, bdTy)     <- bindingID varName

         case bdTy of
           SPIRV.Pointer storage eltTy
             -> store (varName, a_ID) bdID (SPIRV.PointerTy storage eltTy)
           _ -> assign ( _localBinding varName ) (Just a_IDTy)

         pure (ID 0, SPIRV.Unit) -- ID should never be used

   -- image
   SImageTexel ( _ :: Proxy name ) ( _ :: Proxy props ) _
     -> case is of 
         ( ops `ConsAST` coords `ConsAST` NilAST )
           -> do
                 let imgName = knownValue @name
                 bd@(bdID, bdTy) <- bindingID imgName
                 img <- case bdTy of
                           SPIRV.Pointer storage imgTy
                             -> load (imgName, bdID) (SPIRV.PointerTy storage imgTy)
                           _ -> pure bd
                 (cdID , _) <- codeGen coords
                 (texID, _) <- codeGen a
                 writeTexel img ops cdID texID
                 pure (ID 0, SPIRV.Unit) -- ID should never be used

   SComposeO _ (SBinding (_ :: Proxy name)) setter ->
     do  let varName = knownValue @name
         bd@(bdID, bdTy) <- bindingID varName

         case bdTy of
           SPIRV.Pointer storage eltTy
             -> storeThroughAccessChain   (bdID, SPIRV.PointerTy storage eltTy) a setter is
           _ -> insertUsingSetter varName bd                                    a setter is

         pure (ID 0, SPIRV.Unit) -- ID should never be used

   SComposeO _ SImageTexel {} _
     -> throwError ( "codeGen: writing to individual components \
                     \of an image texel currently unsupported"
                   )

   _ -> throwError (   "codeGen: cannot 'assign', unsupported optic:\n"
                    <> ShortText.pack (showSOptic sOptic) <> "\n"
                    <> "Optic does not start by accessing a binding."
                   )

----------------------------------------------------------------------------
-- Perform code-generation by computing associated optical trees.

-- | Load through a pointer with the supplied getter.
loadThroughAccessChain
  :: forall is s a (optic :: Optic is s a)
  .  CodeGen AST
  => (ID, SPIRV.PointerTy) -> SOptic optic -> Codes is -> CGMonad (ID, SPIRV.PrimTy)
loadThroughAccessChain basePtr sOptic is
  = loadThroughAccessChain' basePtr =<< operationTree is sOptic

-- | Use a getter to @view@ a part of the provided object.
extractUsingGetter
  :: forall is s a (optic :: Optic is s a)
  .  CodeGen AST
  => (ID, SPIRV.PrimTy) -> SOptic optic -> Codes is -> CGMonad (ID, SPIRV.PrimTy)
extractUsingGetter base sOptic is
  = extractUsingGetter' base =<< operationTree is sOptic

-- | Store into a pointer with the supplied setter.
storeThroughAccessChain
  :: forall is s a (optic :: Optic is s a)
  .  CodeGen AST
  => (ID, SPIRV.PointerTy) -> Code a -> SOptic optic -> Codes is -> CGMonad ()
storeThroughAccessChain basePtr val sOptic is
  = storeThroughAccessChain' basePtr val =<< operationTree is sOptic

-- | Set a component of an object, with focus given by the supplied setter.
--
-- Returns the updated object.
setUsingSetter
  :: forall is s a (optic :: Optic is s a)
  .  CodeGen AST
  => Code s
  -> Code a
  -> SOptic optic
  -> Codes is
  -> CGMonad (ID, SPIRV.PrimTy)
setUsingSetter base val sOptic is = do
  baseID <- codeGen base
  tree   <- operationTree is sOptic
  setUsingSetter' baseID val tree

-- | Set a component of a binding, with focus given by the supplied setter.
--
-- Doesn't return anything: updates the state of the provided binding.
insertUsingSetter
  :: forall is s a (optic :: Optic is s a)
  .  CodeGen AST
  => ShortText -> (ID, SPIRV.PrimTy) -> Code a -> SOptic optic -> Codes is -> CGMonad ()
insertUsingSetter varName base val sOptic is = do
  tree   <- operationTree is sOptic
  insertUsingSetter' varName base val tree

----------------------------------------------------------------------------
-- Optical trees.

data OpticalNode where
  Access   :: Safeness -> Indices -> OpticalNode
  OfTypeOp :: SPrimTy s -> SPrimTy a -> OpticalNode
  Combine  :: (PrimTy p, All PrimTy as, IsProduct p as)
           => Proxy p -> Proxy as -> [ OpticalOperationTree ] -> OpticalNode

instance Show OpticalNode where
  show (Access safe is) = "Access " ++ show safe ++ " " ++ show is
  show (OfTypeOp s a) = "OfTypeOp @" ++ show s ++ " @" ++ show a
  show (Combine (_ :: Proxy p) ( _ :: Proxy as) trees)
    = "Combine @" ++ show (primTy @p) ++ " @" ++ show (primTys @as)
    ++ " " ++ show trees

type OpticalOperationTree = [ OpticalNode ]

operationTree
  :: forall k is (s :: k) a (optic :: Optic is s a)
  .  CodeGen AST
  => Codes is -> SOptic optic -> CGMonad OpticalOperationTree
operationTree _ (SOfType s a) = pure [OfTypeOp s a]
operationTree (i `ConsAST` _) SAnIndex {}
  = (:[]) . Access Unsafe . RTInds . (:[]) . fst <$> codeGen i
operationTree _ (SIndex s _ n)
  -- if accessing a runtime array, a compile-time index may be unsafe
  | SRuntimeArray <- s
  = pure [ Access Unsafe (CTInds [n]) ]
  -- otherwise, a compile-time index is guaranteed to be in-bounds
  | otherwise
  = pure [ Access Safe   (CTInds [n]) ]
operationTree is (SComposeO lg1 opt1 opt2)
  = do  let (is1, is2) = composedIndices lg1 is
        ops1 <- operationTree is1 opt1
        ops2 <- operationTree is2 opt2
        ops1 `continue` ops2
operationTree is
  ( SProd same (comps :: SProductComponents (os :: ProductComponents iss s as))
    :: SOptic (optic :: Optic is s a)
  ) =
    (:[]) . Combine ( Proxy @a ) ( Proxy @as ) <$>
      ( componentsTrees same comps =<<
          combinedIndices @iss @is @as sSameLength sLength is
      )
operationTree _ (SBinding {})
  = throwError "operationTree: trying to access a binding within a binding"
operationTree _ (SImageTexel {})
  = throwError "operationTree: unexpected image optic"

continue :: OpticalOperationTree -> OpticalOperationTree -> CGMonad OpticalOperationTree
continue ops1 []   = pure ops1
continue []   ops2 = pure ops2
-- try to combine successive accesses into a single access chain as much as possible
-- (this helps bypass Vulkan implementation bugs in tessellation evaluation shaders)
continue [Access safe1 (CTInds is1)] (Access safe2 (CTInds is2) : ops2)
  = pure $ Access (safe1 <> safe2) (CTInds (is1 ++ is2)) : ops2
continue [Access safe1 (RTInds is1)] (Access safe2 (RTInds is2) : ops2)
  = pure $ Access (safe1 <> safe2) (RTInds (is1 ++ is2)) : ops2
continue [Access safe1 (CTInds is1)] (Access safe2 (RTInds is2) : ops2)
  = do
      js1 <- traverse constID is1
      pure $ ( Access (safe1 <> safe2) (RTInds (js1 ++ is2)) : ops2 )
continue [Access safe1 (RTInds is1)] (Access safe2 (CTInds is2) : ops2)
  = do
      js2 <- traverse constID is2
      pure $ Access (safe1 <> safe2) (RTInds (is1 ++ js2)) : ops2
continue (op1 : ops1) ops2
  = do
      cont <- ops1 `continue` ops2
      pure $ op1 : cont

componentsTrees
  :: forall
      (k :: Type) (iss :: [[Type]]) (s :: k)
      (as :: [Type]) (os :: ProductComponents iss s as)
  .  CodeGen AST
  => SSameLength (Distribute iss as) as
  -> SProductComponents os
  -> Codes (MapHList (Distribute iss as))
  -> CGMonad [OpticalOperationTree]
componentsTrees _ SEndProd  _
  = pure []
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

composedIndices :: SLength is -> Codes (is :++: js) -> (Codes is, Codes js)
composedIndices SZero           js               = ( NilAST, js )
composedIndices (SSucc tail_is) (k `ConsAST` ks)
  = first ( k `ConsAST` ) ( composedIndices tail_is ks )

combinedIndices
  :: forall (jss :: [[Type]]) (is :: [Type]) (as :: [Type])
  .  ( CodeGen AST, AreProducts is jss as )
  => SSameLength is jss
  -> SLength as
  -> Codes is
  -> CGMonad (Codes (MapHList (Distribute jss as)))
combinedIndices SSameZero SZero      NilAST
  = pure $ NilAST
combinedIndices SSameZero (SSucc lg) NilAST
  = (NilHList `ConsAST`) <$> combinedIndices SSameZero lg NilAST
combinedIndices sSame@(SSameSucc sm) lg (i `ConsAST` is) =
  case sSame of
    ( _ :: SSameLength (p ': ps) (es ': ess) ) ->
      case productsDict @is @jss @as of
        ConsProducts ->
          zipIndices @es @(Distribute ess as) allDict sSameLength
            <$> deconstruct allDict i
            <*> combinedIndices sm lg is

zipIndices
  :: AllDict PrimTy is
  -> SSameLength is jss
  -> Codes is
  -> Codes (MapHList jss)
  -> Codes (MapHList (ZipCons is jss))
zipIndices NilDict  SSameZero      NilAST           NilAST             = NilAST
zipIndices ConsDict (SSameSucc lg) (i `ConsAST` is) (js `ConsAST` jss) =
  (ConsHList :$ i :$ js) `ConsAST` zipIndices allDict lg is jss

deconstruct
  :: forall (p :: Type) (as :: [Type])
  .  ( CodeGen AST, IsProduct p as )
  => AllDict PrimTy as -> Code p -> CGMonad (Codes as)
deconstruct NilDict    _       = pure NilAST
deconstruct dict       (Lit x) = pure $ hListToASTs dict (toHList @p @as x)
deconstruct dict       p
  | Just hlist <- recogniseHList dict p
  = pure hlist
  -- TODO: more cases to avoid spurious "compositeConstruct ---> compositeExtract"
deconstruct ConsDict p = do
    composite <- codeGen p
    compositeExtractAll composite allDict 0


recogniseHList :: AllDict PrimTy as -> Code p -> Maybe (Codes as)
recogniseHList NilDict           _   = Just NilAST
recogniseHList consDict@ConsDict ast
  = case consDict of
      ( _ :: AllDict PrimTy (b ': bs) ) ->
        case ast of
          ( ConsHList :$ (c :: Code c) :$ cs )
            | Just Refl <- eqT @b @c
            -> (c `ConsAST`) <$> recogniseHList allDict cs
          _ -> Nothing

astsFromHList :: Code (HList as) -> Codes as
astsFromHList NilHList
  = NilAST
astsFromHList (ConsHList :$ a :$ as)
  = a `ConsAST` astsFromHList as
astsFromHList _
  = error "'operationTree': indexing heterogenous list AST not of the expected form (TODO?)"

hListToASTs :: AllDict PrimTy as -> HList as -> Codes as
hListToASTs _        HNil      = NilAST
hListToASTs ConsDict (a :> as) = Lit a `ConsAST` hListToASTs allDict as

compositeExtractAll
  :: forall (as :: [Type])
  .  CodeGen AST
  => (ID, SPIRV.PrimTy)
  -> AllDict PrimTy as
  -> Word32
  -> CGMonad (Codes as)
compositeExtractAll _                    NilDict  _ = pure NilAST
-- special case for matrix entries:
-- product for matrices can use individual components (rather than just columns)
compositeExtractAll comp@(_,compTy) dict@ConsDict i
  | SPIRV.Matrix {..} <- compTy
  , SPIRV.Scalar entryTy == expectedTy
  = do
      extracted <- traverse (compositeExtract comp) $ [ [i,j] | j <- [0..pred rows] ]
      makeIDsAndContinue (succ i) dict extracted

        where
          expectedTy :: SPIRV.PrimTy
          expectedTy = case dict of
            ( _ :: AllDict PrimTy (b ': bs) )
              -> primTy @b

          makeIDsAndContinue
            :: Word32
            -> AllDict PrimTy ts
            -> [(ID, SPIRV.PrimTy)]
            -> CGMonad (Codes ts)
          makeIDsAndContinue _        NilDict  _   = pure NilAST
          makeIDsAndContinue nxt dict'          []  = compositeExtractAll comp dict' nxt
          makeIDsAndContinue nxt dict'@ConsDict
            ( extract@(_,extractedTy) : extracts ) =
              case dict' of
                ( _ :: AllDict PrimTy (c ': cs) )
                  -> do
                        whenAsserting . unless ( expectedTy == extractedTy ) $
                          throwError $
                            "'compositeExtractAll': ASSERT failed\n"
                            <> "expected type" <> ShortText.pack (show expectedTy)
                            <> ", but composite component " <> ShortText.pack (show i)
                            <> " has type " <> ShortText.pack (show extractedTy)

                        next <- makeIDsAndContinue nxt allDict extracts
                        pure $ MkID extract `ConsAST` next
-- usual case
compositeExtractAll comp ConsDict i =
  do
    extracted <- compositeExtract comp [i]
    next      <- compositeExtractAll comp allDict (succ i)
    pure $ MkID extracted `ConsAST` next

----------------------------------------------------------------------------
-- Generate code for a tree of optical operations.

-- check whether a vector swizzle operation can be used
swizzleIndices :: SPIRV.PrimTy -> [OpticalOperationTree] -> Maybe [Word32]
swizzleIndices (SPIRV.Vector _ _) = traverse simpleIndex
  where
    simpleIndex :: OpticalOperationTree -> Maybe Word32
    simpleIndex [Access Safe (CTInds [i])] = Just i
    simpleIndex _ = Nothing
swizzleIndices _ = const Nothing

loadThroughAccessChain'
  :: CodeGen AST
  => (ID, SPIRV.PointerTy) -> OpticalOperationTree -> CGMonad (ID, SPIRV.PrimTy)
loadThroughAccessChain' (basePtrID, SPIRV.PointerTy _ eltTy) []
  = loadInstruction eltTy basePtrID
loadThroughAccessChain' basePtr ( Access safe is : ops )
  = do
      newBasePtr <- accessChain basePtr safe is
      loadThroughAccessChain' newBasePtr ops
loadThroughAccessChain' basePtr@(_, SPIRV.PointerTy _ eltTy) ( Combine (_ :: Proxy p) _ trees : ops )
  -- special case: can we use a vector swizzle operation?
  | Just is <- swizzleIndices eltTy trees
  = do
      base <- loadThroughAccessChain' basePtr []
      vec <- vectorSwizzle base is
      extractUsingGetter' vec ops
  | otherwise
  = do
      subtrees <- traverse (loadThroughAccessChain' basePtr) trees
      combined <- productConstruct (primTy @p) subtrees
      extractUsingGetter' combined ops
loadThroughAccessChain' _ ( OfTypeOp _ _ : _ )
  = throwError "loadThroughAccessChain': unexpected 'OfType' optic used as a getter"


extractUsingGetter'
  :: CodeGen AST
  => (ID, SPIRV.PrimTy) -> OpticalOperationTree -> CGMonad (ID, SPIRV.PrimTy)
extractUsingGetter' base []
  = pure base
extractUsingGetter' base tree@( Access _ (CTInds is) : ops )
  = do
      whenAsserting
        case base of
          ( _, SPIRV.Scalar _ ) ->
            throwError
              ( "extractUsingGetter': ASSERT failed, trying to extract from a scalar type\n\
                \base = " <> ShortText.pack ( show base ) <> "\n\
                \tree = " <> ShortText.pack ( show tree )
              )
          _ -> pure ()
      newBase <- compositeExtract base is
      extractUsingGetter' newBase ops
extractUsingGetter' (baseID, baseTy) ops@( Access _ (RTInds _) : _ )
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
extractUsingGetter' base@(_, eltTy) ( Combine (_ :: Proxy p) _ trees : ops )
  -- special case: can we use a vector swizzle operation?
  | Just is <- swizzleIndices eltTy trees
  = do
      vec <- vectorSwizzle base is
      extractUsingGetter' vec ops
  | otherwise
  = do
      subtrees <- traverse (extractUsingGetter' base) trees
      combined <- productConstruct (primTy @p) subtrees
      extractUsingGetter' combined ops
extractUsingGetter' _ ( OfTypeOp _ _ : _ )
  = throwError "extractUsingGetter': unexpected 'OfType' optic used as a getter"


storeThroughAccessChain'
  :: CodeGen AST
  => (ID, SPIRV.PointerTy) -> Code a -> OpticalOperationTree -> CGMonad ()
-- Base case: identity.
storeThroughAccessChain' (basePtrID, _) val []
  = storeInstruction basePtrID =<< fst <$> codeGen val
-- Access.
storeThroughAccessChain' basePtr val ( Access safe is : ops )
  = do
      newBasePtr <- accessChain basePtr safe is
      storeThroughAccessChain' newBasePtr val ops
-- OfType optic.
storeThroughAccessChain' basePtr val ( OfTypeOp s a : ops )
  = storeAtTypeThroughAccessChain basePtr val ops (sPrimTy s) (sPrimTy a)
-- Product optic.
-- This is a bit more tricky and requires a second level of recursion.
--
--   * Base case.
storeThroughAccessChain' basePtr val ( Combine (_ :: Proxy p) (_ :: Proxy as) trees : [] )
  = successiveStores basePtr trees =<< deconstruct @p @as allDict (unsafeCoerce val)
--   * Access.
storeThroughAccessChain' _ _ ( Combine {} : Access {} : _ )
  = throwError "storeThroughAccessChain': currently unsupported composition ( product optic - indexing )"
--   * OfType optic.
--     If "OfType ty" pertains to the individual components, then we can
--     perform a 'storeAtTypeThroughAccessChain' on all the components in turn.
--     We must first catch the other cases: focus onto a part that arose from the creation of the product type.
storeThroughAccessChain' basePtr val ( Combine (px_p :: Proxy p) (px_as :: Proxy as) trees : OfTypeOp _ a : ops )
  -- Start off by catching situations where we focus on a part that arose from the product.
  --   * 'Combine' followed by 'OfType combinedTy': can ignore 'OfType combinedTy' (identity lens).
  | combinedTy `SPIRV.PrimTy.almostEqual` ofTypeTy
  = storeThroughAccessChain' basePtr val ( Combine px_p px_as trees : ops )
  --   * Finicky corner case: we are combining entries (not columns) to form a matrix, but followed by `OfType colTy`.
  | SPIRV.Matrix m _ eltTy <- combinedTy
  , ( SPIRV.Scalar eltTy' : _ ) <- compTys
  , eltTy'  == eltTy
  , ( SPIRV.Vector m' (SPIRV.Scalar eltTy'') ) <- ofTypeTy
  , eltTy'' == eltTy && m' == m
  = throwError "storeThroughAccessChain': currently unsupported corner case \
      \(matrix product of entries followed by 'OfType' optic focused on column type)"
  -- General case: call 'storeAtTypeThroughAccessChain' on all the components in turn.
  | otherwise
  = forM_ ( zip trees compTys ) \( tree, compTy ) -> do
      tree_ops <- tree `continue` ops
      storeAtTypeThroughAccessChain basePtr val tree_ops compTy ofTypeTy
  where
    combinedTy, ofTypeTy :: SPIRV.PrimTy
    compTys :: [SPIRV.PrimTy]
    combinedTy = primTy @p
    ofTypeTy   = sPrimTy a
    compTys    = primTys @as
--  * Second product optic.
storeThroughAccessChain' _ _ ( Combine {} : Combine {} : _ )
  = throwError "storeThroughAccessChain': currently unsupported composition ( product optic - product optic )"


storeAtTypeThroughAccessChain
  :: CodeGen AST
  => (ID, SPIRV.PointerTy)
  -> Code a
  -> OpticalOperationTree
  -> SPIRV.PrimTy
  -> SPIRV.PrimTy
  -> CGMonad ()
storeAtTypeThroughAccessChain _       _   _   SPIRV.Unit    _
  = pure ()
storeAtTypeThroughAccessChain basePtr val ops SPIRV.Boolean a
  | a == SPIRV.Boolean
  = storeThroughAccessChain' basePtr val ops
  | otherwise
  = pure ()
storeAtTypeThroughAccessChain basePtr val ops s@(SPIRV.Scalar _) a
  | a == s
  = storeThroughAccessChain' basePtr val ops
  | otherwise
  = pure ()
storeAtTypeThroughAccessChain basePtr val ops vecTy@(SPIRV.Vector n elt) a
  | SPIRV.PrimTy.almostEqual vecTy a
  = storeThroughAccessChain' basePtr val ops
  | SPIRV.PrimTy.almostEqual elt a
  , [] <- ops
    = do
        valID <- fst <$> codeGen val
        vecID <- fst <$> compositeConstruct vecTy ( replicate (fromIntegral n) valID )
        storeInstruction (fst basePtr) vecID
  | otherwise
    = forM_ [0..n-1] \i -> do
        i_ops <- [Access Safe (CTInds [i])] `continue` ops
        storeAtTypeThroughAccessChain basePtr val i_ops elt a
storeAtTypeThroughAccessChain basePtr val ops matTy@(SPIRV.Matrix m n elt) a
  | a == matTy
  = storeThroughAccessChain' basePtr val ops
  | a == SPIRV.Vector m (SPIRV.Scalar elt)
  = if
    | [] <- ops
    ->
      do
        colID <- fst <$> codeGen val
        matID <- fst <$> compositeConstruct matTy ( replicate (fromIntegral n) colID )
        storeInstruction (fst basePtr) matID
    | otherwise
    ->
      forM_ [0..n-1] \i -> do
        i_ops <- [Access Safe (CTInds [i])] `continue` ops
        storeThroughAccessChain' basePtr val i_ops
  | a == SPIRV.Scalar elt
  , [] <- ops
  = do
      valID <- fst <$> codeGen val
      colID <- fst <$> compositeConstruct (SPIRV.Vector m a) ( replicate (fromIntegral m) valID )
      matID <- fst <$> compositeConstruct matTy              ( replicate (fromIntegral n) colID )
      storeInstruction (fst basePtr) matID
  | otherwise
  = forM_ [ (i,j) | i <- [0..n-1], j <- [0..m-1] ] \(i,j) -> do
      ij_ops <- [Access Safe (CTInds [i,j])] `continue` ops
      storeAtTypeThroughAccessChain basePtr val ij_ops (SPIRV.Scalar elt) a
storeAtTypeThroughAccessChain basePtr val ops arrayTy@(SPIRV.Array l elt _ _) a
  | SPIRV.PrimTy.almostEqual arrayTy a
  = storeThroughAccessChain' basePtr val ops
  | otherwise
  -- TODO: should be a loop as opposed to being fully unrolled,
  -- but creating loops is unfortunately quite cumbersome
  = forM_ ( if l == 0 then [] else [0..l-1] ) \i -> do
      i_ops <- [Access Safe (CTInds [i])] `continue` ops
      storeAtTypeThroughAccessChain basePtr val i_ops elt a
storeAtTypeThroughAccessChain basePtr val ops structTy@(SPIRV.Struct eltTys _ _) a
  | SPIRV.PrimTy.almostEqual structTy a
  = storeThroughAccessChain' basePtr val ops
  | otherwise
  = forM_ (zip eltTys [0..]) \((_, eltTy, _), i) -> do
      field_ops <- [Access Safe (CTInds [i])] `continue` ops
      storeAtTypeThroughAccessChain basePtr val field_ops eltTy a
storeAtTypeThroughAccessChain _ _ _ (SPIRV.RuntimeArray {}) _
  = throwError "storeAtTypeThroughAccessChain: cannot store into runtime array"
storeAtTypeThroughAccessChain _ _ _ (SPIRV.Pointer {})      _
  = throwError "storeAtTypeThroughAccessChain: unexpected pointer type"
storeAtTypeThroughAccessChain _ _ _ (SPIRV.Function {})     _
  = throwError "storeAtTypeThroughAccessChain: unexpected function type"
storeAtTypeThroughAccessChain _ _ _ (SPIRV.Image {})        _
  = throwError "storeAtTypeThroughAccessChain: unexpected image type"
storeAtTypeThroughAccessChain _ _ _ SPIRV.Sampler           _
  = throwError "storeAtTypeThroughAccessChain: unexpected sampler type"
storeAtTypeThroughAccessChain _ _ _ (SPIRV.SampledImage {}) _
  = throwError "storeAtTypeThroughAccessChain: unexpected sampled image type"
storeAtTypeThroughAccessChain _ _ _ (SPIRV.AccelerationStructure) _
  = throwError "storeAtTypeThroughAccessChain: unexpected acceleration structure type"
storeAtTypeThroughAccessChain _ _ _ (SPIRV.RayQuery) _
  = throwError "storeAtTypeThroughAccessChain: unexpected ray query type"

successiveStores
  :: CodeGen AST
  => (ID, SPIRV.PointerTy)
  -> [OpticalOperationTree]
  -> Codes as
  -> CGMonad ()
successiveStores basePtr ( tree : trees ) (ConsCode a as) = do
  storeThroughAccessChain' basePtr a  tree
  successiveStores basePtr trees   as
successiveStores _       _                _
  = pure ()

setUsingSetter'
  :: CodeGen AST
  => (ID, SPIRV.PrimTy)
  -> Code a
  -> OpticalOperationTree
  -> CGMonad (ID, SPIRV.PrimTy)
setUsingSetter' _ val [] = codeGen val
setUsingSetter' base val [Access _ (CTInds is)]
  = do
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

insertUsingSetter'
  :: CodeGen AST
  => ShortText
  -> (ID, SPIRV.PrimTy)
  -> Code a
  -> OpticalOperationTree
  -> CGMonad ()
insertUsingSetter' varName base val ops =
  assign ( _localBinding varName ) . Just =<< setUsingSetter' base val ops
