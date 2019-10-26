{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

module CodeGen.Applicative
  ( idiomatic
  , pattern Idiomatic
  )
  where

-- base
import Prelude
  hiding ( Num(..), Integral(..)
         , Fractional(..), Floating(..), RealFloat(..)
         )
import qualified Prelude
import Control.Applicative
  ( liftA2 )
import Control.Arrow
  ( second )
import Data.Foldable
  ( toList )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat, KnownNat )
import Unsafe.Coerce
  ( unsafeCoerce )

-- containers
import qualified Data.Set as Set

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
  ( pattern UApplied
  , UAST(..), UASTs(..)
  )
import CodeGen.Array
  ( createArray )
import {-# SOURCE #-} CodeGen.CodeGen
  ( codeGen )
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
import Data.Type.Known
  ( knownValue )
import Data.Type.List
  ( Snoc )
import FIR.AST
  ( AST(..)
  , Syntactic(fromAST)
  , primOp
  )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Op
  ( PrimOp(..), SPrimOp(SMul)
  , Vectorise(Vectorise), VecPrimOpType(VecPrimOpType)
  )
import FIR.Prim.Singletons
  ( PrimTy, primTy
  , IntegralTy
  , PrimFunc(primFuncSing, distDict), DistDict(DistDict)
  , SPrimFunc(..)
  , Arity(ZeroArity,SuccArity), KnownArity(arity)
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
-- code generation for functor and applicative operations

data Idiom (f :: Type -> Type) (as :: [Type]) (b :: Type) where
  Val       :: KnownArity b => AST (f b) -> Idiom f '[] b -- 'b' should never be a function type in this case
  PureIdiom :: forall f b. AST b -> Idiom f '[] b
  ApIdiom   :: (PrimTy a, KnownArity b) => Idiom f as (a -> b) -> AST (f a) -> Idiom f (as `Snoc` a) b
deriving stock instance Show (Idiom f as b)

data AnIdiom (f :: Type -> Type) (b :: Type) where
  AnIdiom :: Idiom f as b -> AnIdiom f b
deriving stock instance Show (AnIdiom f b)

data AnyIdiom :: Type where
  AnyIdiom :: (Applicative f, PrimFunc f) => AnIdiom f b -> AnyIdiom
deriving stock instance Show AnyIdiom

anIdiom :: forall f b. (Applicative f, PrimFunc f, KnownArity b)
        => AST (f b) -> Maybe (AnIdiom f b)
anIdiom ( Pure :$ a )
  = Just $ AnIdiom $ PureIdiom a
anIdiom ( Fmap :$ f :$ ( Pure :$ a ) )
  = Just $ AnIdiom $ PureIdiom (fromAST f a)
anIdiom ( Ap :$ ( Pure :$ f ) :$ ( Pure :$ a ) )
  = Just $ AnIdiom $ PureIdiom (fromAST f a)
anIdiom ( Fmap :$ f :$ a )
  = Just $ AnIdiom $ ApIdiom (PureIdiom f) a
anIdiom ( Ap :$ ( Pure :$ f ) :$ a )
  = Just $ AnIdiom $ ApIdiom (PureIdiom f) a
anIdiom ( Ap :$ f :$ a )
  = case anIdiom f of
      Just (AnIdiom i) -> Just $ AnIdiom (ApIdiom i a)
      _                -> Nothing
anIdiom fb
  = case arity @b of
    ZeroArity -> Just (AnIdiom $ Val fb)
    _         -> Nothing

recogniseIdiom :: AST ast -> Maybe AnyIdiom
recogniseIdiom ast@(Pure :$ _ )     = AnyIdiom <$> anIdiom ast
recogniseIdiom ast@(Fmap :$ _ :$ _) = AnyIdiom <$> anIdiom ast
recogniseIdiom ast@(Ap   :$ _ :$ _) = AnyIdiom <$> anIdiom ast
recogniseIdiom _                    = Nothing

pattern Idiomatic :: AnyIdiom -> AST ast
pattern Idiomatic idi <- ( recogniseIdiom -> Just idi )

-- codeGen for an existentially wrapped idiom
idiomatic :: AnyIdiom -> CGMonad (ID, SPIRV.PrimTy)
idiomatic ( AnyIdiom (AnIdiom idi) ) = idiom idi

-- codeGen for an idiom
idiom :: forall f as b. PrimFunc f
      => Idiom f as b
      -> CGMonad (ID, SPIRV.PrimTy)
idiom (Val fb) = codeGen fb
idiom (PureIdiom b)
  = do
      elt@(eltID, eltTy) <- codeGen b
      case primFuncSing @f of
        sFuncVector@SFuncVector
          -> case sFuncVector of
              ( _ :: SPrimFunc (V n) )
                -> let n :: Prelude.Num a => a
                       n = Prelude.fromIntegral (knownValue @n)
                   in compositeConstruct
                        ( SPIRV.Vector n eltTy )
                        ( replicate n eltID )
        sFuncMatrix@SFuncMatrix
          -> case sFuncMatrix of
              ( _ :: SPrimFunc (M m n) )
                -> do constituentTy
                        <- case eltTy of
                              SPIRV.Scalar ty
                                 -> pure ty
                              ty -> throwError 
                                      ( "codeGen: matrix contains non-scalars of type "
                                       <> ShortText.pack (show ty)
                                      )
                      let colDim :: Word32
                          colDim = knownValue @m
                          rowDim :: Prelude.Num a => a
                          rowDim = Prelude.fromIntegral (knownValue @n)
                      col <- fst <$> idiom (PureIdiom @(V m) (MkID elt) )
                      compositeConstruct
                        ( SPIRV.Matrix colDim rowDim constituentTy )
                        ( replicate rowDim col )
        sFuncArray@SFuncArray
          -> case sFuncArray of
              ( _ :: SPrimFunc (Array n) )
                -> let n :: Prelude.Num a => a
                       n = Prelude.fromIntegral (knownValue @n)
                   in compositeConstruct
                        ( SPIRV.Array n eltTy Set.empty SPIRV.NotForBuiltins )
                        ( replicate n eltID )
idiom apIdiom@(ApIdiom idi (a :: AST (f a))) =
  case primFuncSing @f of
    sFuncVector@SFuncVector
      -> case sFuncVector of
          ( _ :: SPrimFunc (V n) )
            -> case applyIdiomV @n (ApIdiom idi a) of
                  -- attempt to directly vectorise operation if possible
                  Just (UAST uast) -> codeGen uast
                  -- otherwise, revert to doing it component by component
                  _ ->
                    do 
                      ids <- toList <$> traverse codeGen (applyIdiom idi a)
                      eltTy <- case ids of
                          ((_, ty) : _) -> pure ty
                          _             -> throwError "codeGen: empty vector"
                      compositeConstruct
                        ( SPIRV.Vector (knownValue @n) eltTy )
                        ( map fst ids )
    sFuncMatrix@SFuncMatrix
      -> case sFuncMatrix of
          ( _ :: SPrimFunc (M m n) )
            -> let idiom_cols :: V n (Idiom (V m) as b)
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
    sFuncArray@SFuncArray
      -> case sFuncArray of
          ( _ :: SPrimFunc (Array n) )
            -> case arity @b of
                  SuccArity _ -> throwError "codeGen: partially applied applicative operation"
                  ZeroArity ->
                    let arrayFunction = applyIdiomArray @n @Word32 idi a
                        arrayLg = knownValue @n
                        eltTy = primTy @b
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

                      res <- codeGen ( createArray @n @"__tmp_array" @"__tmp_arrayIndex" @b arrayFunction )

                      assign ( _localBinding "__tmp_array"      ) oldTmpArray
                      assign ( _localBinding "__tmp_arrayIndex" ) oldTmpArrayIndex

                      pure res

applyIdiom
  :: forall f as a b
  .  ( PrimFunc f
     , PrimTy a
     )
  => Idiom f as (a -> b) -> AST (f a) -> f (AST b)
applyIdiom i a = case distDict @f @(AST a) of
  DistDict ->
    case i of
      PureIdiom f    ->   fromAST f                     <$> ( fromAST a :: f (AST a) )
      ApIdiom   f a' -> ( fromAST <$> applyIdiom f a' ) <*> ( fromAST a :: f (AST a) )
      Val _           -> error "applyIdiom: unexpected value used as a function"

distributeMatrixIdiom
  :: forall m n as b.
     ( KnownNat m, KnownNat n, KnownArity b )
  => Idiom (M m n) as b -> V n (Idiom (V m) as b)
distributeMatrixIdiom (Val v)
  = case arity @b of
      ZeroArity   -> fmap Val . fromAST $ ( UnMat :$ v )
      SuccArity _ -> error "distributeMatrixIdiom: unexpected value used as a function"
distributeMatrixIdiom (PureIdiom f)
  = fmap PureIdiom . pure @(V n) $ f
distributeMatrixIdiom (ApIdiom f a)
  = ApIdiom <$> distributeMatrixIdiom f <*> fromAST ( UnMat :$ a )

applyIdiomArray :: forall n ix as a b.
                  ( KnownNat n
                  , Integral ix, IntegralTy ix
                  , PrimTy a
                  )
                => Idiom (Array n) as (a -> b) -> AST (Array n a) -> (AST ix -> AST b)
applyIdiomArray (Val _) _ _
  = error "applyIdiomArray: unexpected value used as a function"
applyIdiomArray (PureIdiom f) arr i
  = fromAST f (view @(AnIndex (AST ix)) i arr)
applyIdiomArray (ApIdiom f arr') arr i
  = let f' :: AST a -> AST b
        f' = fromAST ( applyIdiomArray f arr' i )
    in f' (view @(AnIndex (AST ix)) i arr)

---------------------------------------------------------------------------------------
-- awful hacks to use native SPIR-V vectorised instructions if possible

newtype FakeVector n a = FakeVector (V n a)
  deriving stock Show
newtype FakeScalar n a = FakeScalar a
  deriving stock Show

type family Vectorisation (n :: Nat) (a :: Type) = (r :: Type) | r -> n a where
  Vectorisation n (a -> b) = FakeScalar n a -> Vectorisation n b
  Vectorisation n b        = FakeVector n b

applyIdiomV :: forall n as b. KnownNat n
            => Idiom (V n) as b -> Maybe UAST
applyIdiomV = sanitiseVectorisation @n @b . unsafeVectorise

sanitiseVectorisation :: forall n b. KnownNat n
                      => AST (Vectorisation n b) -> Maybe UAST
sanitiseVectorisation v = sanitiseVectorisation' @n (unsafeCoerce v :: AST b)

sanitiseVectorisation' :: forall n ast. KnownNat n
                       => AST ast -> Maybe UAST
sanitiseVectorisation'
-- special case for 'vector times scalar' optimisation
  ( PrimOp (_ :: Proxy a) (_ :: Proxy op) :$ v1 :$ v2 )
    | Just SMul <- opSing @_ @_ @op @a
    = case ( sanitiseVectorisation' @n v1, sanitiseVectorisation' @n v2 ) of
        ( Just (UAST (Pure :$ (b1 :: AST b1))), Just (UAST (Pure :$ (b2 :: AST b2))) )
          -> let b1b2 :: AST a
                 b1b2 = unsafeCoerce b1 * unsafeCoerce b2
                 b1b2' :: AST b1
                 b1b2' = unsafeCoerce b1b2
             in Just . UAST $ unsafeCoerce @(AST (V n b1)) @(AST (V n a))
                                ( Pure @(V n) :$ b1b2' )
        ( Just (UAST (Pure :$ b1)), Just (UAST sv2) )
          -> let b1' :: AST a
                 b1' = unsafeCoerce b1
                 sv2' :: AST (V n a)
                 sv2' = unsafeCoerce sv2
             in Just . UAST $ b1' *^ sv2'
        ( Just (UAST sv1), Just (UAST (Pure :$ b2)) )
          -> let sv1' :: AST (V n a)
                 sv1' = unsafeCoerce sv1
                 b2' :: AST a
                 b2' = unsafeCoerce b2
             in Just . UAST $ b2' *^ sv1'
        (Just (UAST sv1), Just (UAST sv2) )
           -> liftA2 applyvPrimOp
                ( vectorisePrimOp @n @a @op )
                ( Just ( NilUAST `SnocUAST` sv1 `SnocUAST` sv2 ) )
        _ -> Nothing
sanitiseVectorisation'
  ( UApplied ( PrimOp (_ :: Proxy a) (_ :: Proxy op) ) as )
    = liftA2 applyvPrimOp
        ( vectorisePrimOp @n @a @op )
        ( sanitiseUASTs   @n as     )
sanitiseVectorisation'
  ( UApplied ( MkID _ ) (_ `SnocUAST` _ ) )
    = Nothing -- cannot vectorise opaque functions (e.g. native function calls)
sanitiseVectorisation' ast@(UApplied (MkVector _ _) _) = Just . UAST $ ast
sanitiseVectorisation' (Coerce :$ v) = Just . UAST $ v
sanitiseVectorisation' (Lit a) = Just . UAST $ Pure @(V n) :$ Lit a
sanitiseVectorisation' _ = Nothing -- Just . UAST $ Pure @(V n) :$ ast

sanitiseUASTs :: forall n. KnownNat n => UASTs -> Maybe UASTs
sanitiseUASTs NilUAST = Just NilUAST
sanitiseUASTs (as `SnocUAST` a)
  = liftA2 (\xs (UAST y) -> SnocUAST xs y)
      ( sanitiseUASTs @n as )
      ( sanitiseVectorisation' @n a )

applyvPrimOp :: UAST -> UASTs -> UAST
applyvPrimOp vPrimOp NilUAST = vPrimOp
applyvPrimOp vPrimOp (vs `SnocUAST` v)
  = case applyvPrimOp vPrimOp vs of
      UAST r -> UAST (unsafeCoerce r :$ v) -- should be able to eliminate this 'unsafeCoerce'

unsafeVectorise :: forall n as b. Idiom (V n) as b -> AST (Vectorisation n b)
unsafeVectorise (Val       b) = unsafeCoerce b
unsafeVectorise (PureIdiom b) = unsafeCoerce b
unsafeVectorise (ApIdiom f (v :: AST (V n a)))
  = fromAST
      ( unsafeVectorise f )
      ( Coerce @(V n a) @(FakeScalar n a) :$ v )

vectorisePrimOp :: forall n a op. (KnownNat n, PrimOp op a)
                => Maybe UAST
vectorisePrimOp = case vectorisation @_ @_ @op @a @n of
  Just ( VecPrimOpType (_ :: Proxy v_a) )
    -> Just . UAST $ primOp @v_a @('Vectorise op)
  _ -> Nothing
