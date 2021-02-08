{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

{-|
Module: CodeGen.Applicative

Code generation for (functor and) applicative operations:

  * vectors use vectorised instructions (whenever possible),
  * matrices use vectorised operations on their columns (whenever possible),
  * arrays use loops.

-}

module CodeGen.Applicative ( ) where

-- base
import Prelude
  hiding ( Num(..), Integral(..)
         , Fractional(..), Floating(..), RealFloat(..)
         )
import qualified Prelude
import Control.Arrow
  ( second )
import Data.Foldable
  ( toList )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat, KnownNat )
import Unsafe.Coerce
  ( unsafeCoerce )

-- containers
import qualified Data.Set as Set

-- haskus-utils-variant
import Haskus.Utils.VariantF
  ( VariantF, ApplyAll, BottomUp(toBottomUp) )
import Haskus.Utils.EGADT
  ( EGADT(..), HVariantF(..) )

-- lens
import Control.Lens
  ( use, assign )

-- mtl
import Control.Monad.Except
  ( throwError )

-- text-short
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Application
  ( Application(Applied)
  , ASTs(NilAST, ConsAST)
  , pattern SnocAST
  , pattern Nullary
  )
import CodeGen.Array
  ( createArray )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
import CodeGen.Composite
  ( compositeConstruct )
import CodeGen.Instruction
  ( ID )
import CodeGen.Monad
  ( CGMonad )
import CodeGen.Pointers
  ( temporaryVariable )
import CodeGen.State
  ( _localBinding )
import Control.Type.Optic
  ( AnIndex
  , ReifiedGetter(view)
  )
import Data.Constraint.All
  ( All(allDict), AllDict(NilDict, ConsDict) )
import Data.Type.Known
  ( knownValue )
import Data.Type.List
  ( Snoc )
import FIR.AST
  ( Code, AST
  , Syntactic(fromAST)
  , primOp
  , AppF(..), LitF(..)
  , ApplicativeF(..)
  , PrimOpF(..), MkVectorF(..)
  , UnsafeCoerceF(..)
  , pattern (:$), pattern Lam, pattern Pure, pattern MkID
  , pattern MkVector, pattern UnMat
  , pattern UnsafeCoerce, pattern Lit
  )
import FIR.AST.Type
  ( AugType(Val,(:-->)), UnderlyingType
  , FunArgs, FunRes, ApplyFun
  , ApplyFAug, MapApplyFAug
  )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Op
  ( PrimOp(..), SPrimOp(SMul)
  , Vectorise(Vectorise), VecPrimOpType(VecPrimOpType)
  , PrimTyVal
  )
import FIR.Prim.Types
  ( PrimTy, primTy
  , IntegralTy
  , PrimFunc(primFuncSing, distDict), DistDict(DistDict)
  , SPrimFunc(..)
  )
import FIR.Syntax.AST
  ( ) -- 'PrimFunc' instances, optic instances
import Math.Algebra.Class
  ( Semiring((*)), Integral )
import Math.Linear
  ( V, M, Semimodule((*^)) )
import qualified SPIRV.PrimTy  as SPIRV
import qualified SPIRV.Storage as Storage

----------------------------------------------------------------------------
-- Dispatch code-generation on AST constructors.

instance CodeGen AST => CodeGen (ApplicativeF AST) where

  codeGenArgs ( Applied ( ApF pxf (Pure _ f :: AST (Val (f a) :--> ApplyFAug f b )) (Pure _ a :: AST (Val (f a))) ) as )
    | ( _ :: AST t ) <- f
    , ( _ :: AST v ) <- a
    , Refl <- ( unsafeCoerce Refl :: t :~: ( Val a :--> b ) )
    , Refl <- ( unsafeCoerce Refl :: v :~: Val a )
    , ConsDict <- allDict @PrimTyVal @(Val a ': FunArgs b)
    = codeGenArgs ( Applied ( PureF pxf (f :$ a) ) as )
  codeGenArgs ( Applied ( ApF ( _ :: Proxy f) f a ) as ) =
    codeGenArgs ( Applied f (a `ConsAST` as) )

  codeGenArgs ( Applied (PureF ( _ :: Proxy f) (a :: AST a)) (as :: ASTs fas) )
    | Refl <- ( unsafeCoerce Refl :: ApplyFun a fas :~: Val (UnderlyingType (FunRes a)) )
    , Refl <- ( unsafeCoerce Refl :: fas :~: MapApplyFAug f (FunArgs a) )
    = idiom (makeIdiom @f @a @(FunArgs a) a as)


makeIdiom
  :: forall f a as fas
  .  ( fas ~ MapApplyFAug f as, All PrimTyVal as )
  => AST a -> ASTs fas -> Idiom f as (ApplyFun a fas)
makeIdiom v NilAST
  | Refl <- ( unsafeCoerce Refl :: ApplyFun a fas :~: a )
  , Refl <- ( unsafeCoerce Refl :: as :~: '[] )
  = PureIdiom v
makeIdiom h as@(ConsAST _ _) = case snocDict ( allDict @PrimTyVal @as ) of
  sDict@SnocDict
    | ( _ :: SnocDict PrimTyVal (init_as `Snoc` last_as) ) <- sDict
    , ( _ :: Proxy init_fas ) <- Proxy @( MapApplyFAug f init_as )
    , Refl <- ( unsafeCoerce Refl :: fas :~: ( init_fas `Snoc` (ApplyFAug f last_as) ) )
    , ( bs `SnocAST` b ) <- as
    , ( nxt :: Idiom f init_as (ApplyFun a init_fas ) ) <- makeIdiom h bs
    , Refl <- ( unsafeCoerce Refl :: ApplyFun a init_fas :~: ( Val (UnderlyingType last_as) :--> ApplyFun a fas ) )
    -> ApIdiom nxt b

data Idiom (f :: Type -> Type) (as :: [AugType]) (b :: AugType) where
  PureIdiom :: forall f b. AST b -> Idiom f '[] b
  ApIdiom   :: PrimTy a => Idiom f as (Val a :--> b) -> AST (Val (f a)) -> Idiom f (as `Snoc` (Val a)) b
deriving stock instance Show (Idiom f as b)

data SnocDict c as where
  NilSnocDict :: SnocDict c '[]
  SnocDict    :: (All c as, c a) => SnocDict c (as `Snoc` a)

snocDict :: forall c as. AllDict c as -> SnocDict c as
snocDict NilDict = NilSnocDict
snocDict consDict@ConsDict = case consDict of
  ( _ :: AllDict c (b ': bs) ) -> case snocDict (allDict @c @bs) of
    NilSnocDict -> SnocDict
    sDict@SnocDict -> case sDict of
      ( _ :: SnocDict c (xs `Snoc` x ) )
        | Refl <- ( unsafeCoerce Refl :: as :~: ( (b ': xs) `Snoc` x) )
        -> SnocDict

----------------------------------------------------------------------------
-- Code-generation of an idiomatic expression.

idiom
  :: forall f bs r
  .  ( CodeGen AST, PrimFunc f, PrimTy r )
  => Idiom f bs (Val r) -> CGMonad ( ID, SPIRV.PrimTy )
idiom (PureIdiom b) = do
  elt@(eltID, eltTy) <- codeGen b
  case primFuncSing @f of
    sFuncVector@SFuncVector ->
      case sFuncVector of
        ( _ :: SPrimFunc (V n) ) ->
          let n :: Prelude.Num t => t
              n = Prelude.fromIntegral (knownValue @n)
          in compositeConstruct
               ( SPIRV.Vector n eltTy )
               ( replicate n eltID )
    sFuncMatrix@SFuncMatrix ->
      case sFuncMatrix of
        ( _ :: SPrimFunc (M m n) ) ->
          do
            constituentTy <-
              case eltTy of
                SPIRV.Scalar ty -> pure ty
                ty -> throwError 
                        ( "codeGen: matrix contains non-scalars of type "
                         <> ShortText.pack (show ty)
                        )
            let colDim :: Word32
                colDim = knownValue @m
                rowDim :: Prelude.Num t => t
                rowDim = Prelude.fromIntegral (knownValue @n)
            col <- fst <$> idiom ( PureIdiom @(V m) (MkID elt :: Code r) )
            compositeConstruct
              ( SPIRV.Matrix colDim rowDim constituentTy )
              ( replicate rowDim col )
    sFuncArray@SFuncArray ->
      case sFuncArray of
          ( _ :: SPrimFunc (Array n) ) ->
            let n :: Prelude.Num t => t
                n = Prelude.fromIntegral (knownValue @n)
            in compositeConstruct
                 ( SPIRV.Array n eltTy Set.empty SPIRV.NotForBuiltins )
                 ( replicate n eltID )
idiom apIdiom@(ApIdiom idi a) =
  case primFuncSing @f of
    sFuncVector@SFuncVector ->
      case sFuncVector of
        ( _ :: SPrimFunc (V n) ) ->
          case applyIdiomV @n apIdiom of
            -- attempt to directly vectorise operation if possible
            Just res -> codeGen res
            -- otherwise, revert to doing it component by component
            _ -> do 
              ids <- toList <$> traverse codeGen (applyIdiom idi a)
              eltTy <- case ids of
                  ((_, ty) : _) -> pure ty
                  _             -> throwError "codeGen: empty vector"
              compositeConstruct
                ( SPIRV.Vector (knownValue @n) eltTy )
                ( map fst ids )
    sFuncMatrix@SFuncMatrix ->
      case sFuncMatrix of
        ( _ :: SPrimFunc (M m n) ) ->
          let idiom_cols :: V n (Idiom (V m) bs (Val r))
              idiom_cols = distributeMatrixIdiom apIdiom
          in do
               cols <- toList <$> traverse idiom idiom_cols
               eltTy <- case cols of
                 ((_, SPIRV.Vector _ (SPIRV.Scalar ty)) : _)
                     -> pure ty
                 _   -> throwError "codeGen: invalid matrix type, columns are not vectors of scalars"
               compositeConstruct
                 ( SPIRV.Matrix (knownValue @m) (knownValue @n) eltTy )
                 ( map fst cols )
    sFuncArray@SFuncArray ->
      case sFuncArray of
        ( _ :: SPrimFunc (Array n) ) ->
          let arrayFunction = applyIdiomArray @n @Word32 idi a
              arrayLg = knownValue @n
              eltTy = primTy @r
          in do
            let resArrPtrTy =
                  SPIRV.PointerTy Storage.Function
                    ( SPIRV.Array arrayLg eltTy Set.empty SPIRV.NotForBuiltins )
            arrID <- fst <$> codeGen a
            resArrPtrID <- fst <$> temporaryVariable arrID resArrPtrTy
            let resArr = (resArrPtrID, resArrPtrTy)

            oldTmpArray      <- use ( _localBinding "__tmp_array"      )
            oldTmpArrayIndex <- use ( _localBinding "__tmp_arrayIndex" )

            assign ( _localBinding "__tmp_array"      ) ( Just $ second SPIRV.pointerTy resArr )
            assign ( _localBinding "__tmp_arrayIndex" ) Nothing

            res <- codeGen ( createArray @n @"__tmp_array" @"__tmp_arrayIndex" @r arrayFunction )

            assign ( _localBinding "__tmp_array"      ) oldTmpArray
            assign ( _localBinding "__tmp_arrayIndex" ) oldTmpArrayIndex

            pure res


applyIdiom
  :: forall f as a b
  .  ( PrimFunc f, PrimTy a )
  => Idiom f as (Val a :--> b) -> AST (Val (f a)) -> f (AST b)
applyIdiom i a
  | DistDict <- distDict @f @(AST (Val a))
  = case i of
    PureIdiom f    ->   fromAST f                     <$> ( fromAST a :: f (AST (Val a)) )
    ApIdiom   f a' -> ( fromAST <$> applyIdiom f a' ) <*> ( fromAST a :: f (AST (Val a)) )

distributeMatrixIdiom
  :: forall m n as b.
     ( KnownNat m, KnownNat n )
  => Idiom (M m n) as b -> V n (Idiom (V m) as b)
distributeMatrixIdiom (PureIdiom f)
  = fmap PureIdiom . pure @(V n) $ f
distributeMatrixIdiom (ApIdiom f a)
  = ApIdiom <$> distributeMatrixIdiom f <*> fromAST ( UnMat :$ a )


applyIdiomArray
  :: forall n ix as a b
  .  ( KnownNat n
     , Integral ix, IntegralTy ix
     , PrimTy a
     )
  => Idiom (Array n) as (Val a :--> b) -> Code (Array n a) -> ( Code ix -> AST b )
applyIdiomArray (PureIdiom f) arr i =
  fromAST f (view @(AnIndex (Code ix)) i arr)
applyIdiomArray (ApIdiom f arr') arr i =
  let f' :: Code a -> AST b
      f' = fromAST ( applyIdiomArray f arr' i )
  in f' (view @(AnIndex (Code ix)) i arr)

---------------------------------------------------------------------------------------
-- awful hacks to use native SPIR-V vectorised instructions if possible

newtype FakeScalar n a = FakeScalar a
  deriving stock Show

type family Vectorisation (n :: Nat) (a :: AugType) = (r :: AugType) | r -> a where
  Vectorisation n (Val a :--> b) = Val (FakeScalar n a) :--> Vectorisation n b
  Vectorisation n (Val b)        = Val b

applyIdiomV
  :: forall n as b
  .  ( KnownNat n, PrimTy b )
  => Idiom (V n) as (Val b) -> Maybe (Code (V n b))
applyIdiomV idi = sanitiseVectorisation @n ( unsafeVectorise idi )

unsafeVectorise :: forall n as b. Idiom (V n) as b -> AST (Vectorisation n b)
unsafeVectorise (PureIdiom b) = unsafeCoerce b
unsafeVectorise (ApIdiom f (v :: Code (V n a)))
  = fromAST
      ( unsafeVectorise f )
      ( ( UnsafeCoerce :$ v ) :: AST (Val (FakeScalar n a)) )

class KnownNat n => SanitiseVectorisation (n :: Nat) (ast :: AugType -> Type) where
  sanitiseVectorisationArgs :: PrimTy r => Application ast f (Val r) -> Maybe (Code (V n r))

sanitiseVectorisation
  :: forall n r ast
  .  ( SanitiseVectorisation n ast, PrimTy r )
  => ast (Val r) -> Maybe (Code (V n r))
sanitiseVectorisation r = sanitiseVectorisationArgs (Nullary r)

instance ( KnownNat n, BottomUp (SanitiseVectorisation n) vs ) => SanitiseVectorisation n (VariantF vs) where
  sanitiseVectorisationArgs (Applied f args) =
    toBottomUp @(SanitiseVectorisation n) ( \x -> sanitiseVectorisationArgs $ Applied x args ) f

deriving via VariantF ( ApplyAll (EGADT vs :: AugType -> Type) vs )
  instance ( KnownNat n, BottomUp (SanitiseVectorisation n) ( ApplyAll (EGADT vs) vs ) )
        => SanitiseVectorisation n (EGADT vs)

instance SanitiseVectorisation n AST => SanitiseVectorisation n (AppF AST) where
  sanitiseVectorisationArgs (Applied (AppF (Lam f) a) as) = sanitiseVectorisationArgs (Applied (f a) as)
  sanitiseVectorisationArgs (Applied (AppF f a) as) = sanitiseVectorisationArgs (Applied f (a `ConsAST` as))

instance {-# OVERLAPPABLE #-} KnownNat n => SanitiseVectorisation n ast where
  sanitiseVectorisationArgs = const Nothing

instance KnownNat n => SanitiseVectorisation n (MkVectorF AST) where
  sanitiseVectorisationArgs ( Applied (MkVectorF vec) NilAST )
    = Just ( unsafeCoerce $ MkVector vec )

instance KnownNat n => SanitiseVectorisation n (UnsafeCoerceF AST) where
  sanitiseVectorisationArgs ( Applied UnsafeCoerceF (vec `ConsAST` NilAST) )
    = Just ( unsafeCoerce vec )

instance KnownNat n => SanitiseVectorisation n (LitF AST) where
  sanitiseVectorisationArgs ( Applied (LitF a) NilAST )
    = Just ( Pure (Proxy @(V n)) (Lit ( unsafeCoerce a)) )

instance SanitiseVectorisation n AST => SanitiseVectorisation n (PrimOpF AST) where
  sanitiseVectorisationArgs ( Applied (PrimOpF ( _ :: Proxy a ) ( _ :: Proxy op )) as )
    -- special case for 'vector times scalar' optimisation
    |  Just smul@SMul <- opSing @_ @_ @op @a
    , ( _ :: SPrimOp a mul_op ) <- smul
    , ( v1 `ConsAST` v2 `ConsAST` NilAST ) <- as
    = case ( sanitiseVectorisation @n v1, sanitiseVectorisation @n v2 ) of
      ( Just (Pure _ (b1 :: AST vb1)), Just (Pure _ (b2 :: AST vb2)) )
        | Refl <- ( unsafeCoerce Refl :: vb1 :~: Val (UnderlyingType vb1) )
        , Refl <- ( unsafeCoerce Refl :: vb2 :~: Val (UnderlyingType vb2) )
        , ( _ :: Proxy b1 ) <- Proxy @(UnderlyingType vb1)
        , ( _ :: Proxy b2 ) <- Proxy @(UnderlyingType vb2)
        -> let b1b2 :: Code a
               b1b2 = unsafeCoerce b1 * unsafeCoerce b2
               b1b2' :: Code b1
               b1b2' = unsafeCoerce b1b2
           in Just $ Pure ( Proxy @(V n) ) b1b2'
      ( Just (Pure _ b1), Just sv2 )
        -> let b1' :: Code a
               b1' = unsafeCoerce b1
               sv2' :: Code (V n a)
               sv2' = unsafeCoerce sv2
           in Just $ b1' *^ sv2'
      ( Just sv1, Just (Pure _ b2) )
        -> let sv1' :: Code (V n a)
               sv1' = unsafeCoerce sv1
               b2' :: Code a
               b2' = unsafeCoerce b2
           in Just $ b2' *^ sv1'
      ( Just sv1, Just sv2 )
        -> let
              b1, b2 :: Code a
              b1 = unsafeCoerce sv1
              b2 = unsafeCoerce sv2
           in applyvPrimOp
                ( primOp @(V n a) @('Vectorise mul_op) )
                ( b1 `ConsAST` b2 `ConsAST` NilAST )
      _ -> Nothing
    | Just ( VecPrimOpType ( _ :: Proxy v_a) ) <- vectorisation @_ @_ @op @a @n
    , ( _ :: Proxy f  ) <- Proxy @(PrimOpAugType op a)
    , ( _ :: Proxy vf ) <- Proxy @(PrimOpAugType ('Vectorise op) v_a)
    , Refl <- ( unsafeCoerce Refl :: FunRes f :~: Val (UnderlyingType (FunRes f)) )
    , Refl <- ( unsafeCoerce Refl :: ApplyFAug (V n) f :~: vf )
    = applyvPrimOp @n @f @(UnderlyingType (FunRes f)) ( primOp @v_a @('Vectorise op) ) as
    | otherwise
    = Nothing

applyvPrimOp
  :: forall (n :: Nat) f r
  .  ( KnownNat n, FunRes f ~ Val r, All PrimTyVal (FunArgs f) )
  => AST (ApplyFAug (V n) f) -> ASTs (FunArgs f) -> Maybe (Code (V n r))
applyvPrimOp vPrimOp as = case allDict @PrimTyVal @(FunArgs f) of
  NilDict
    | Refl <- ( unsafeCoerce Refl :: f :~: Val (UnderlyingType f) )
    -> Just vPrimOp
  ConsDict -> case as of
    ( ( x :: AST (Val x) ) `ConsAST` xs )
      | Refl <- ( unsafeCoerce Refl :: f :~: ( Val x :--> SkipFirstArg f ) )
      , Just v <- sanitiseVectorisation @n x
      -> applyvPrimOp ( vPrimOp :$ v ) xs
    _ -> Nothing

type family SkipFirstArg (x :: AugType) :: AugType where
  SkipFirstArg (_ :--> b) = b
