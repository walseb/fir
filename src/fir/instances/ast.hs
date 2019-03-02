{-# OPTIONS_HADDOCK ignore-exports #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-|
Module: FIR.Instances.AST

This module, together with "FIR.Instances.Codensity",
provides most of the user-facing syntax for constructing
and manipulating values in the EDSL.

This is done through type class overloading, here in the form of
orphan instances for types of the form @AST a@
(representing pure values in the EDSL).

-}

module FIR.Instances.AST
  ( -- functor/applicative for AST values
    ASTFunctor(fmapAST)
  , ASTApplicative(pureAST, (<**>)), (<$$>)

    -- patterns for vectors
  , pattern Vec2, pattern Vec3, pattern Vec4

    -- patterns for matrices
  , pattern Mat22, pattern Mat23, pattern Mat24
  , pattern Mat32, pattern Mat33, pattern Mat34
  , pattern Mat42, pattern Mat43, pattern Mat44

    -- + orphan instances
  )
  where

-- base
import Prelude hiding
  ( Eq(..), (&&), (||), not
  , Ord(..)
  , Num(..), Floating(..)
  , Integral(..)
  , Fractional(..), fromRational
  , Floating(..)
  , RealFloat(..)
  , Functor(..), (<$>)
  , Applicative(..)
  )
import qualified Prelude
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( (:~:)(Refl), testEquality )
import Data.Word
  ( Word16 )
import GHC.TypeLits
  ( KnownSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( KnownNat
  , type (+), type (-)
  , type (<=), CmpNat
  )
import Type.Reflection
  ( typeRep )

-- fir
import Control.Type.Optic
  ( Optic(..), Index
  , Gettable, ReifiedGetter(view)
  , Settable, ReifiedSetter(set)
  )
import Data.Function.Variadic
  ( NatVariadic, ListVariadic )
import Data.Type.List
  ( KnownLength(sLength) )
import FIR.AST
  ( AST(..)
  , Syntactic(Internal, toAST, fromAST)
  , primOp
  )
import FIR.Instances.Optics
  ( KnownOptic(opticSing) )
import FIR.Prim.Op
  ( Vectorise(Vectorise) )
import FIR.Prim.Singletons
  ( PrimTy
  , ScalarTy
  , SPrimFunc(..), PrimFunc(..)
  )
import Math.Algebra.Class
  ( AdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), Ring
  , DivisionRing(..)
  , Signed(..), Archimedean(..)
  , Floating(..), RealFloat(..)
  , Integral
  , Convert(..)
  )
import Math.Linear
  ( Semimodule(..), Module(..)
  , Inner(..), Cross(..)
  , Matrix(..)
  , V, M(..)
  , dfoldrV, buildV
  , pattern V2, pattern V3, pattern V4
  )
import Math.Logic.Bits
  ( Bits(..), BitShift(..) )
import Math.Logic.Class
  ( Eq(..), Boolean(..)
  , Choose(..), Triple
  , Ord(..)
  )
import qualified SPIRV.PrimOp as SPIRV

--------------------------------------------------------------------------------------
-- Instances for AST type

-- * Logical operations
--
-- $logical
-- Instances for:
--
-- 'Boolean', 'Choose',
--
-- 'Eq', 'Ord' (note: not the "Prelude" type classes).

instance Boolean (AST Bool) where
  true  = Lit True
  false = Lit False
  (&&)  = primOp @Bool @SPIRV.BoolAnd
  (||)  = primOp @Bool @SPIRV.BoolOr
  not   = primOp @Bool @SPIRV.BoolNot

instance PrimTy a => Choose (AST Bool) (Triple (AST a)) where
  choose = fromAST If

instance ( PrimTy a, Eq a, Logic a ~ Bool )
  => Eq (AST a) where
  type Logic (AST a) = AST Bool
  (==) = primOp @a @SPIRV.Equal
  (/=) = primOp @a @SPIRV.NotEqual

instance ( ScalarTy a, Ord a, Logic a ~ Bool ) 
  => Ord (AST a) where
  type Ordering (AST a) = AST Word16
  compare = error "todo"
  (<=) = primOp @a @SPIRV.LTE
  (>=) = primOp @a @SPIRV.GTE
  (<)  = primOp @a @SPIRV.LT
  (>)  = primOp @a @SPIRV.GT
  min  = primOp @a @SPIRV.Min
  max  = primOp @a @SPIRV.Max

-- * Bitwise operations
--
-- $bitwise
-- Instances for:
--
-- 'Bits', 'BitShift' (note: not 'Data.Bits.Bits').

instance (ScalarTy a, Bits a) => Bits (AST a) where
  (.&.)      = primOp @a @SPIRV.BitAnd
  (.|.)      = primOp @a @SPIRV.BitOr
  xor        = primOp @a @SPIRV.BitXor
  complement = primOp @a @SPIRV.BitNot
  zeroBits   = Lit ( zeroBits @a )

instance (ScalarTy a, ScalarTy s, BitShift '(a,s))
  => BitShift '(AST a, AST s) where
  shiftL = primOp @'(a,s) @SPIRV.BitShiftLeft
  shiftR = primOp @'(a,s) @SPIRV.BitShiftRightArithmetic

-- * Numeric operations
-- 
-- $numeric
-- Instances for:
--
-- 'AdditiveMonoid', 'AdditiveGroup', 'Signed',
--
-- 'Semiring', 'Ring', 
--
-- 'DivisionRing', 'Archimedean' (Archimedean ordered group),
--
-- 'Floating', 'RealFloat' (note: not the "Prelude" type classes).

instance (ScalarTy a, AdditiveMonoid a) => AdditiveMonoid (AST a) where
  (+)    = primOp @a @SPIRV.Add
  zero   = Lit (zero :: a)
  fromInteger = Lit . fromInteger
instance (ScalarTy a, Semiring a) => Semiring (AST a) where
  (*)    = primOp @a @SPIRV.Mul
instance (ScalarTy a, AdditiveGroup a) => AdditiveGroup (AST a) where
  (-)    = primOp @a @SPIRV.Sub
  negate = primOp @a @SPIRV.Neg
instance (ScalarTy a, Signed a) => Signed (AST a) where
  abs    = primOp @a @SPIRV.Abs
  signum = primOp @a @SPIRV.Sign
instance (ScalarTy a, DivisionRing a) => DivisionRing (AST a) where
  (/)    = primOp @a @SPIRV.Div
  fromRational = Lit . fromRational
instance ( ScalarTy a
         , Archimedean a
         , Logic a ~ Bool
         ) => Archimedean (AST a) where
  mod    = primOp @a @SPIRV.Mod
  rem    = primOp @a @SPIRV.Rem

instance (ScalarTy a, Floating a) => Floating (AST a) where
  pi      = Lit pi
  exp     = primOp @a @SPIRV.FExp
  log     = primOp @a @SPIRV.FLog
  sqrt    = primOp @a @SPIRV.FSqrt
  invSqrt = primOp @a @SPIRV.FInvsqrt
  sin     = primOp @a @SPIRV.FSin
  cos     = primOp @a @SPIRV.FCos
  tan     = primOp @a @SPIRV.FTan
  asin    = primOp @a @SPIRV.FAsin
  acos    = primOp @a @SPIRV.FAcos
  atan    = primOp @a @SPIRV.FAtan
  sinh    = primOp @a @SPIRV.FSinh
  cosh    = primOp @a @SPIRV.FCosh
  tanh    = primOp @a @SPIRV.FTanh
  asinh   = primOp @a @SPIRV.FAsinh
  acosh   = primOp @a @SPIRV.FAcosh
  atanh   = primOp @a @SPIRV.FAtanh
  (**)    = primOp @a @SPIRV.FPow

instance (ScalarTy a, RealFloat a) => RealFloat (AST a) where
  atan2   = primOp @a @SPIRV.FAtan2


-- * Numeric conversions
--
-- $conversions
-- Instance for 'Convert'.
instance (ScalarTy a, ScalarTy b, Convert '(a,b))
         => Convert '(AST a, AST b) where
  convert = case testEquality (typeRep @a) (typeRep @b) of
    Just Refl -> id
    _         -> primOp @'(a,b) @SPIRV.Convert

-- TODO: there should be a way to do this more efficiently,
-- without using type reflection machinery.
-- However this at least avoids writing out
-- a large amount of instances by hand (one for each pair of types).

-----------------------------------------------
-- * Optics


-- ** Getters
--
-- $getters
-- Instances for getters.

instance ( KnownSymbol k
         , empty ~ '[]
         , Gettable (Name_ k :: Optic '[] s a)
         , r ~ AST a
         )
      => Gettable (Name_ k :: Optic empty (AST s) r)
      where
instance ( KnownSymbol k
         , empty ~ '[]
         , Gettable (Name_ k :: Optic '[] s a)
         , ReifiedGetter (Name_ k :: Optic '[] s a)
         , ListVariadic '[] a ~ a
         , KnownOptic (Name_ k :: Optic '[] s a)
         , r ~ AST a
         )
      => ReifiedGetter (Name_ k :: Optic empty (AST s) r)
      where
  view = fromAST
       $ View
            sLength
            ( opticSing @(Name_ k :: Optic '[] s a) )

instance ( Integral ty
         , ix ~ '[AST ty]
         , Gettable (RTOptic_ :: Optic '[ty] s a)
         , r ~ AST a
         )
      => Gettable (RTOptic_ :: Optic ix (AST s) r)
      where
instance ( Integral ty
         , ix ~ '[AST ty]
         , Gettable (RTOptic_ :: Optic '[ty] s a)
         , ReifiedGetter (RTOptic_ :: Optic '[ty] s a)
         , ListVariadic '[] a ~ a
         , KnownOptic (RTOptic_ :: Optic '[ty] s a)
         , r ~ AST a
         )
      => ReifiedGetter (RTOptic_ :: Optic ix (AST s) r)
      where
  view = fromAST
       $ View
            sLength
            ( opticSing @(RTOptic_ :: Optic '[ty] s a) )
instance ( KnownNat i
         , empty ~ '[]
         , Gettable (Index_ i :: Optic '[] s a)
         , r ~ AST a
         )
      => Gettable (Index_ i :: Optic empty (AST s) r)
      where
instance ( KnownNat i
         , empty ~ '[]
         , Gettable (Index_ i :: Optic '[] s a)
         , ReifiedGetter (Index_ i :: Optic '[] s a)
         , ListVariadic '[] a ~ a
         , KnownOptic (Index_ i :: Optic '[] s a)
         , r ~ AST a
         )
      => ReifiedGetter (Index_ i :: Optic empty (AST s) r)
      where
  view = fromAST
       $ View
            sLength
            ( opticSing @(Index_ i :: Optic '[] s a) )

-- ** Setters
--
-- $setters
-- Instances for setters.

instance ( KnownSymbol k
         , empty ~ '[]
         , Settable (Name_ k :: Optic '[] s a)
         , r ~ AST a
         )
      => Settable (Name_ k :: Optic empty (AST s) r)
      where
instance ( KnownSymbol k
         , empty ~ '[]
         , Settable (Name_ k :: Optic '[] s a)
         , ReifiedSetter (Name_ k :: Optic '[] s a)
         , ListVariadic '[] s ~ s
         , KnownOptic (Name_ k :: Optic '[] s a)
         , r ~ AST a
         )
      => ReifiedSetter (Name_ k :: Optic empty (AST s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(Name_ k :: Optic '[] s a) )

instance ( Integral ty
         , ix ~ '[AST ty]
         , Settable (RTOptic_ :: Optic '[ty] s a)
         , r ~ AST a
         )
      => Settable (RTOptic_ :: Optic ix (AST s) r)
      where
instance ( Integral ty
         , ix ~ '[AST ty]
         , Settable (RTOptic_ :: Optic '[ty] s a)
         , ReifiedSetter (RTOptic_ :: Optic '[ty] s a)
         , ListVariadic '[] s ~ s
         , KnownOptic (RTOptic_ :: Optic '[ty] s a)
         , r ~ AST a
         )
      => ReifiedSetter (RTOptic_ :: Optic ix (AST s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(RTOptic_ :: Optic '[ty] s a) )
instance ( KnownNat i
         , empty ~ '[]
         , Settable (Index_ i :: Optic '[] s a)
         , r ~ AST a
         )
      => Settable (Index_ i :: Optic empty (AST s) r)
      where
instance ( KnownNat i
         , empty ~ '[]
         , Settable (Index_ i :: Optic '[] s a)
         , ReifiedSetter (Index_ i :: Optic '[] s a)
         , ListVariadic '[] s ~ s
         , KnownOptic (Index_ i :: Optic '[] s a)
         , r ~ AST a
         )
      => ReifiedSetter (Index_ i :: Optic empty (AST s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(Index_ i :: Optic '[] s a) )

-----------------------------------------------
-- * Functor functionality

infixl 4 <$$>
infixl 4 <**>

class ASTFunctor f where
  fmapAST :: ( Syntactic x
             , Internal x ~ (a -> b)
             , Syntactic r
             , Internal r ~ ( (f a) -> (f b) )
             , PrimTy a
             )
          => x -> r

class ASTApplicative f where
  pureAST :: AST a -> AST (f a)
  (<**>)  :: ( Syntactic r
             , Internal r ~ ( (f a) -> (f b) )
             , PrimTy a
             )
           => AST ( f (a -> b) ) -> r

(<$$>) :: ( ASTFunctor f
          , Syntactic x
          , Internal x ~ (a -> b)
          , Syntactic r
          , Internal r ~ ( (f a) -> (f b) )
          , PrimTy a
          )
        => x -> r
(<$$>) = fmapAST

instance KnownNat n => ASTFunctor (V n) where
  fmapAST = fromAST $ Fmap @(V n)

instance (KnownNat m, KnownNat n) => ASTFunctor (M m n) where
  fmapAST = fromAST $ Fmap @(M m n)

instance KnownNat n => ASTApplicative (V n) where
  pureAST = fromAST $ Pure @(V n)
  (<**>)  = fromAST $ Ap   @(V n)

instance (KnownNat m, KnownNat n) => ASTApplicative (M m n) where
  pureAST = fromAST $ Pure @(M m n)
  (<**>)  = fromAST $ Ap   @(M m n)

instance 
  TypeError (     Text "The AST datatype does not have a Functor instance:"
             :$$: Text "    cannot map Haskell functions over internal types."
             :$$: Text "To map an internal function over an internal type, use 'fmapAST'/'<$$>'."
            ) => Prelude.Functor AST where
  fmap = error "unreachable"

-----------------------------------------------
-- * Internal functors

instance KnownNat n => PrimFunc (V n) where
  primFuncSing = SFuncVector @n
  distributeAST = fromAST
instance ( KnownNat m, KnownNat n ) => PrimFunc (M m n) where
  primFuncSing = SFuncMatrix @m @n
  distributeAST = error "distributeAST: todo for matrices"

-----------------------------------------------
-- * Syntactic instances
--
-- $syntactic
-- Instances for 'Syntactic'.

instance Syntactic () where
  type Internal () = ()
  toAST   = const ( Lit () )
  fromAST = const ()

{-
instance (Syntactic a, Syntactic b) => Syntactic (a,b) where
  type Internal (a,b) = Struct '[ "_0" ':-> a
                                , "_1" ':-> b
                                ]
  toAST (a,b) = a :& b :& End
  fromAST (a :& b :& End) = (a,b)
-}

-- utility type for the following instance declaration
newtype B n a b i = B { unB :: AST (NatVariadic (n-i) a b) }

instance  ( KnownNat n
          , Syntactic a
          , PrimTy (Internal a)
          )
        => Syntactic (V n a)
        where
  type Internal (V n a) = V n (Internal a)

  toAST :: V n a -> AST (V n (Internal a))
  toAST v = res'
    where f :: forall i.
              ( KnownNat i
              , (1 <= (n-i))
              , (n-(i+1)) ~ ((n-i)-1) -- help inference along
              )
            => a
            -> B n (Internal a) (V n (Internal a)) i
            -> B n (Internal a) (V n (Internal a)) (i+1)
          f a (B b) = B ( b :$ toAST a )
          a0 :: B n (Internal a) (V n (Internal a)) 0
          a0 = B ( MkVector (Proxy @n) (Proxy @(Internal a)) )
          res :: B n (Internal a) (V n (Internal a)) n
          res = dfoldrV f a0 v
          res' :: ((n-n) ~ 0) -- ditto
               => AST (NatVariadic 0 (Internal a) (V n (Internal a)))
          res' = unB res

  fromAST :: AST (V n (Internal a)) -> V n a
  fromAST = buildV builder
    where builder :: forall i. (KnownNat i, CmpNat i n ~ Prelude.LT)
                  => Proxy i -> AST (V n (Internal a)) -> a
          builder _ v = fromAST ( View sLength (opticSing @(Index i)) :$ v )

deriving instance
    ( KnownNat m, KnownNat n
    , Syntactic a
    , ScalarTy (Internal a)
    )
  => Syntactic (M m n a)

-----------------------------------------------
-- * Vectors and matrices

-- ** Vectors
--
-- $vectors
-- Instances for:
--
-- 'Semimodule', 'Module', 'Inner', 'Cross'.
instance (ScalarTy a, Semiring a) => Semimodule (AST (V 0 a)) where
  type Scalar (AST (V 0 a))  = AST      a
  type AST (V 0 a) `OfDim` n = AST (V n a)

  (^+^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST (V n a)
  (^+^) = primOp @(V n a) @('Vectorise SPIRV.Add)

  (^*) :: forall n. KnownNat n
        => AST (V n a) -> AST a -> AST (V n a)
  (^*)  = primOp @(V n a) @SPIRV.VMulK

instance (ScalarTy a, Ring a) => Module (AST (V 0 a)) where
  (^-^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST (V n a)
  (^-^) = primOp @(V n a) @('Vectorise SPIRV.Sub)

  (-^) :: forall n. KnownNat n => AST (V n a) -> AST (V n a)
  (-^) = primOp @(V n a) @('Vectorise SPIRV.Neg)

instance (ScalarTy a, Floating a) => Inner (AST (V 0 a)) where
  (^.^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST a
  (^.^) = primOp @(V n a) @SPIRV.DotV

  normalise :: forall n. KnownNat n => AST (V n a) -> AST (V n a)
  normalise = primOp @(V n a) @SPIRV.NormaliseV

instance (ScalarTy a, Floating a) => Cross (AST (V 0 a)) where
  cross = primOp @(V 3 a) @SPIRV.CrossV

-- *** Bidirectional pattern synonyms

-- these patterns could be generalised to have types such as:
-- Vec2 :: forall a. (Syntactic a, PrimTy (Internal a))
--      => a -> a -> AST ( V 2 (Internal a) )
-- but this leads to poor type-inference

{-# COMPLETE Vec2 #-}
pattern Vec2 :: forall a. PrimTy a => AST a -> AST a -> AST ( V 2 a )
pattern Vec2 x y <- (fromAST -> V2 x y)
  where Vec2 = fromAST $ MkVector @2 @a Proxy Proxy

{-# COMPLETE Vec3 #-}
pattern Vec3 :: forall a. PrimTy a => AST a -> AST a -> AST a -> AST ( V 3 a )
pattern Vec3 x y z <- (fromAST -> V3 x y z)
  where Vec3 = fromAST $ MkVector @3 @a Proxy Proxy

{-# COMPLETE Vec4 #-}
pattern Vec4 :: forall a. PrimTy a => AST a -> AST a -> AST a -> AST a -> AST ( V 4 a )
pattern Vec4 x y z w <- (fromAST -> V4 x y z w)
  where Vec4 = fromAST $ MkVector @4 @a Proxy Proxy

-- ** Matrices
--
-- $matrices
-- Instance for 'Matrix'.

instance (ScalarTy a, Floating a) => Matrix (AST (M 0 0 a)) where
  type Vector (AST (M 0 0 a))        = AST (V 0   a)
  type AST (M 0 0 a) `OfDims` '(m,n) = AST (M m n a)

  diag    = error "todo"
  konst a = Mat :$ pureAST (pureAST a)

  transpose :: forall n m. (KnownNat n, KnownNat m)
            => AST (M n m a) -> AST (M m n a)
  transpose = primOp @'(a,n,m) @SPIRV.Transp

  inverse :: forall n. KnownNat n
          => AST (M n n a) -> AST (M n n a)
  inverse = primOp @'(a,n) @SPIRV.Inv
  
  determinant :: forall n. KnownNat n
              => AST (M n n a) -> AST a
  determinant = primOp @'(a,n) @SPIRV.Det

  -- no built-in matrix addition and subtraction, so we use the vector operations
  (!+!) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST (M i j a) -> AST (M i j a)
  x !+! y = Mat :$ ( vecAdd <$$> (UnMat :$ x) <**> (UnMat :$ y) )
    where vecAdd :: AST (V j a) -> AST (V j a) -> AST (V j a)
          vecAdd = primOp @(V j a) @('Vectorise SPIRV.Add)
  (!-!) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST (M i j a) -> AST (M i j a)
  x !-! y = Mat :$ ( vecSub <$$> (UnMat :$ x) <**> (UnMat :$ y) )
    where vecSub :: AST (V j a) -> AST (V j a) -> AST (V j a)
          vecSub = primOp @(V j a) @('Vectorise SPIRV.Sub)

  (!*!) :: forall i j k. (KnownNat i, KnownNat j, KnownNat k)
        => AST (M i j a) -> AST (M j k a) -> AST (M i k a)
  (!*!) = primOp @'(a,i,j,k) @SPIRV.MMulM

  (^*!) :: forall i j. (KnownNat i, KnownNat j)
        => AST (V i a) -> AST (M i j a) -> AST (V j a)
  (^*!) = primOp @'(a,i,j) @SPIRV.VMulM

  (!*^) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST (V j a) -> AST (V i a)
  (!*^) = primOp @'(a,i,j) @SPIRV.MMulV

  (!*) :: forall i j. (KnownNat i, KnownNat j)
       => AST (M i j a) -> AST a -> AST (M i j a)
  (!*) = primOp @'(a,i,j) @SPIRV.MMulK


-- *** Bidirectional pattern synonyms

{-# COMPLETE Mat22 #-}
pattern Mat22
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 2 2 a )
pattern Mat22 a11 a12
              a21 a22
  <- ( fromAST . ( UnMat :$ )
       -> V2 ( V2 a11 a12 )
             ( V2 a21 a22 )
     )
  where Mat22
            a11 a12
            a21 a22
          = Mat :$ Vec2
            ( Vec2 a11 a12 )
            ( Vec2 a21 a22 )

{-# COMPLETE Mat23 #-}
pattern Mat23
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 2 3 a )
pattern Mat23 a11 a12 a13
              a21 a22 a23
   <- ( fromAST . ( UnMat :$ )
        -> V2 ( V3 a11 a12 a13 )
              ( V3 a21 a22 a23 )
      )
  where Mat23
            a11 a12 a13
            a21 a22 a23
          = Mat :$ Vec2
            ( Vec3 a11 a12 a13 )
            ( Vec3 a21 a22 a23 )

{-# COMPLETE Mat24 #-}
pattern Mat24
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 2 4 a )
pattern Mat24 a11 a12 a13 a14
              a21 a22 a23 a24
   <- ( fromAST . ( UnMat :$ )
        -> V2 ( V4 a11 a12 a13 a14 )
              ( V4 a21 a22 a23 a24 )
      )
  where Mat24
            a11 a12 a13 a14
            a21 a22 a23 a24
          = Mat :$ Vec2
              ( Vec4 a11 a12 a13 a14 )
              ( Vec4 a21 a22 a23 a24 )

{-# COMPLETE Mat32 #-}
pattern Mat32
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 3 2 a )
pattern Mat32 a11 a12
              a21 a22
              a31 a32
   <- ( fromAST . ( UnMat :$ )
        -> V3 ( V2 a11 a12 )
              ( V2 a21 a22 )
              ( V2 a31 a32 )
      )
  where Mat32
            a11 a12
            a21 a22
            a31 a32
          = Mat :$ Vec3
            ( Vec2 a11 a12 )
            ( Vec2 a21 a22 )
            ( Vec2 a31 a32 )


{-# COMPLETE Mat33 #-}
pattern Mat33
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 3 3 a )
pattern Mat33 a11 a12 a13
              a21 a22 a23
              a31 a32 a33
   <- ( fromAST . ( UnMat :$ )
        -> V3 ( V3 a11 a12 a13 )
              ( V3 a21 a22 a23 )
              ( V3 a31 a32 a33 )
      )
  where Mat33
            a11 a12 a13
            a21 a22 a23
            a31 a32 a33
          = Mat :$ Vec3
              ( Vec3 a11 a12 a13 )
              ( Vec3 a21 a22 a23 )
              ( Vec3 a31 a32 a33 )

{-# COMPLETE Mat34 #-}
pattern Mat34
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 3 4 a )
pattern Mat34 a11 a12 a13 a14
              a21 a22 a23 a24
              a31 a32 a33 a34
   <- ( fromAST . ( UnMat :$ )
        -> V3 ( V4 a11 a12 a13 a14 )
              ( V4 a21 a22 a23 a24 )
              ( V4 a31 a32 a33 a34 )
      )
  where Mat34
            a11 a12 a13 a14
            a21 a22 a23 a24
            a31 a32 a33 a34
          = Mat :$ Vec3
              ( Vec4 a11 a12 a13 a14 )
              ( Vec4 a21 a22 a23 a24 )
              ( Vec4 a31 a32 a33 a34 )

{-# COMPLETE Mat42 #-}
pattern Mat42
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 4 2 a )
pattern Mat42 a11 a12
              a21 a22
              a31 a32
              a41 a42
   <- ( fromAST . ( UnMat :$ )
        -> V4 ( V2 a11 a12 )
              ( V2 a21 a22 )
              ( V2 a31 a32 )
              ( V2 a41 a42 )
      )
  where Mat42
            a11 a12
            a21 a22
            a31 a32
            a41 a42
          = Mat :$ Vec4
              ( Vec2 a11 a12 )
              ( Vec2 a21 a22 )
              ( Vec2 a31 a32 )
              ( Vec2 a41 a42 )

{-# COMPLETE Mat43 #-}
pattern Mat43
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 4 3 a )
pattern Mat43 a11 a12 a13
              a21 a22 a23
              a31 a32 a33
              a41 a42 a43
   <- ( fromAST . ( UnMat :$ )
        -> V4 ( V3 a11 a12 a13 )
              ( V3 a21 a22 a23 )
              ( V3 a31 a32 a33 )
              ( V3 a41 a42 a43 )
      )
  where Mat43
            a11 a12 a13
            a21 a22 a23
            a31 a32 a33
            a41 a42 a43
          = Mat :$ Vec4
              ( Vec3 a11 a12 a13 )
              ( Vec3 a21 a22 a23 )
              ( Vec3 a31 a32 a33 )
              ( Vec3 a41 a42 a43 )

{-# COMPLETE Mat44 #-}
pattern Mat44
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 4 4 a )
pattern Mat44 a11 a12 a13 a14
              a21 a22 a23 a24
              a31 a32 a33 a34
              a41 a42 a43 a44
   <- ( fromAST . ( UnMat :$ )
        -> V4 ( V4 a11 a12 a13 a14 )
              ( V4 a21 a22 a23 a24 )
              ( V4 a31 a32 a33 a34 )
              ( V4 a41 a42 a43 a44 )
      )
  where Mat44
            a11 a12 a13 a14
            a21 a22 a23 a24
            a31 a32 a33 a34
            a41 a42 a43 a44
          = Mat :$ Vec4
              ( Vec4 a11 a12 a13 a14 )
              ( Vec4 a21 a22 a23 a24 )
              ( Vec4 a31 a32 a33 a34 )
              ( Vec4 a41 a42 a43 a44 )
