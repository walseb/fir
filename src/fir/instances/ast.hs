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

module FIR.Instances.AST where

-- base
import Prelude hiding( Eq(..), (&&), (||), not
                     , Ord(..)
                     , Num(..), Floating(..)
                     , Integral(..)
                     , Fractional(..), fromRational
                     , Functor(..)
                     , Applicative(..)
                     )
import qualified Prelude
import Data.Proxy(Proxy(Proxy))
import Data.Type.Equality((:~:)(Refl), testEquality)
import Data.Word(Word16, Word32)
import GHC.TypeLits(TypeError, ErrorMessage(..))
import GHC.TypeNats( KnownNat, natVal
                   , type (+), type (-), type (<=?)
                   )
import Type.Reflection(typeRep)

-- fir
import Control.Type.Optic( Optic(..)
                         , Gettable, ReifiedGetter(view)
                         , Settable, ReifiedSetter(set)
                         )
import Data.Function.Variadic(NatVariadic)
import FIR.AST(AST(..), Syntactic(Internal,toAST,fromAST))
import FIR.PrimTy( PrimTy, primTy, ScalarTy, scalarTy
                 , SPrimFunc(SFuncVector, SFuncMatrix)
                 )
import Math.Algebra.Class ( AdditiveGroup(..)
                          , Semiring(..), Ring(..)
                          , DivisionRing(..)
                          , Signed(..), Archimedean(..)
                          , Convert(..)
                          )
import Math.Linear( Semimodule(..), Module(..)
                  , Inner(..)
                  , Matrix(..)
                  , V, M(..)
                  , dfoldrV, buildV
                  , pattern V2, pattern V3, pattern V4
                  )
import Math.Logic.Class ( Eq(..), Boolean(..)
                        , Choose(..), Triple
                        , Ord(..)
                        )
import qualified SPIRV.PrimOp as SPIRV

--------------------------------------------------------------------------------------
-- instances for AST

lit :: forall a. PrimTy a => a -> AST a
lit = Lit (Proxy @a)

-- logical operations
instance Boolean (AST Bool) where
  true  = lit True
  false = lit False
  (&&)  = fromAST $ PrimOp (SPIRV.BoolOp SPIRV.And) (&&)
  (||)  = fromAST $ PrimOp (SPIRV.BoolOp SPIRV.Or ) (||)
  not   = fromAST $ PrimOp (SPIRV.BoolOp SPIRV.Not) not

instance PrimTy a => Choose (AST Bool) (Triple (AST a)) where
  choose = fromAST If

instance ( PrimTy a, Eq a, Logic a ~ Bool )
  => Eq (AST a) where
  type Logic (AST a) = AST Bool
  (==) = fromAST $ PrimOp (SPIRV.EqOp SPIRV.Equal    (primTy @a)) ((==) @a)
  (/=) = fromAST $ PrimOp (SPIRV.EqOp SPIRV.NotEqual (primTy @a)) ((/=) @a)

instance ( PrimTy a, Ord a, Logic a ~ Bool ) 
  => Ord (AST a) where
  type Ordering (AST a) = AST Word16
  compare = error "todo"
  (<=) = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.LTE (primTy @a)) ((<=) @a)
  (>=) = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.GTE (primTy @a)) ((>=) @a)
  (<)  = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.LT  (primTy @a)) ((<)  @a)
  (>)  = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.GT  (primTy @a)) ((>)  @a)
  min  = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.Min (primTy @a)) min
  max  = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.Max (primTy @a)) max

-- numeric operations
instance (ScalarTy a, AdditiveGroup a) => AdditiveGroup (AST a) where
  (+)    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Add  (scalarTy @a)) (+)
  zero   = lit (zero :: a)
  fromInteger = lit . fromInteger
instance (ScalarTy a, Semiring a) => Semiring (AST a) where
  (*)    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Mul  (scalarTy @a)) (*)
instance (ScalarTy a, Ring a) => Ring (AST a) where
  (-)    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Sub  (scalarTy @a)) (-)
  negate = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Neg  (scalarTy @a)) negate  
instance (ScalarTy a, Signed a) => Signed (AST a) where
  abs    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Abs  (scalarTy @a)) abs
  signum = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Sign (scalarTy @a)) signum
instance (ScalarTy a, DivisionRing a) => DivisionRing (AST a) where
  (/)    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Div  (scalarTy @a)) (/)
  fromRational = lit . fromRational
instance ( ScalarTy a
         , Archimedean a
         , Logic a ~ Bool
         ) => Archimedean (AST a) where
  mod    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Mod  (scalarTy @a)) mod
  rem    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Rem  (scalarTy @a)) rem


-- numeric conversions
instance (ScalarTy a, ScalarTy b, Convert '(a,b))
         => Convert '(AST a, AST b) where
  convert = case testEquality (typeRep @a) (typeRep @b) of
    Just Refl -> id
    _         -> fromAST $ PrimOp (SPIRV.ConvOp SPIRV.Convert (scalarTy @a) (scalarTy @b)) convert



val :: forall n. KnownNat n => Word32
val = fromIntegral ( natVal (Proxy @n))

-- vectors
instance (ScalarTy a, Semiring a) => Semimodule (AST (V 0 a)) where
  type Scalar (AST (V 0 a))   = AST      a
  type OfDim  (AST (V 0 a)) n = AST (V n a)

  (^+^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST (V n a)
  (^+^) = fromAST $ PrimOp (SPIRV.VecOp SPIRV.AddV  (val @n) (scalarTy @a)) (^+^)

  (^*) :: forall n. KnownNat n
        => AST (V n a) -> AST a -> AST (V n a)
  (^*)  = fromAST $ PrimOp (SPIRV.VecOp SPIRV.VMulK (val @n) (scalarTy @a)) (^*)

instance (ScalarTy a, Ring a) => Module (AST (V 0 a)) where
  (^-^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST (V n a)
  (^-^) = fromAST $ PrimOp (SPIRV.VecOp SPIRV.SubV  (val @n) (scalarTy @a)) (^-^)

instance (ScalarTy a, Semiring a) => Inner (AST (V 0 a)) where
  (^.^) = fromAST $ PrimOp (SPIRV.VecOp SPIRV.DotV  (val @0) (scalarTy @a)) (^.^)


-- matrices

instance (ScalarTy a, Ring a) => Matrix (AST (M 0 0 a)) where
  type Vector (AST (M 0 0 a))     = AST (V 0   a)
  type OfDims (AST (M 0 0 a)) m n = AST (M m n a)

  diag    = error "todo"
  konst a = Mat :$ pureAST (pureAST a)

  transpose :: forall n m. (KnownNat n, KnownNat m)
            => AST (M n m a) -> AST (M m n a)
  transpose   = fromAST $ PrimOp (SPIRV.MatOp SPIRV.Transp (val @m) (val @n) (scalarTy @a)) transpose

  inverse :: forall n. KnownNat n
            => AST (M n n a) -> AST (M n n a)
  inverse     = fromAST $ PrimOp (SPIRV.MatOp SPIRV.Inv    (val @n) (val @n) (scalarTy @a)) inverse
  
  determinant :: forall n. KnownNat n
              => AST (M n n a) -> AST a
  determinant = fromAST $ PrimOp (SPIRV.MatOp SPIRV.Det    (val @0) (val @0) (scalarTy @a)) determinant

  -- no built-in matrix addition and subtraction, so we use the vector operations
  (!+!) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST (M i j a) -> AST (M i j a)
  x !+! y = Mat :$ ( vecAdd <$$> (UnMat :$ x) <**> (UnMat :$ y) )
    where vecAdd :: AST (V j a) -> AST(V j a -> V j a)
          vecAdd = fromAST $ PrimOp (SPIRV.VecOp SPIRV.AddV (val @i) (scalarTy @a)) (^+^)

  (!-!) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST (M i j a) -> AST (M i j a)
  x !-! y = Mat :$ ( vecSub <$$> (UnMat :$ x) <**> (UnMat :$ y) )
    where vecSub :: AST (V j a) -> AST(V j a -> V j a)
          vecSub = fromAST $ PrimOp (SPIRV.VecOp SPIRV.SubV (val @i) (scalarTy @a)) (^-^)

  (!*!) :: forall i j k. (KnownNat i, KnownNat j, KnownNat k)
        => AST (M i j a) -> AST (M j k a) -> AST (M i k a)
  (!*!) = fromAST $ PrimOp (SPIRV.MatOp SPIRV.MMulM (val @i) (val @k) (scalarTy @a)) (!*!)

  (^*!) :: forall i j. (KnownNat i, KnownNat j)
        => AST (V i a) -> AST (M i j a) -> AST (V j a)
  (^*!) = fromAST $ PrimOp (SPIRV.MatOp SPIRV.VMulM (val @j) (val @0) (scalarTy @a)) (^*!)

  (!*^) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST (V j a) -> AST (V i a)
  (!*^) = fromAST $ PrimOp (SPIRV.MatOp SPIRV.MMulV (val @i) (val @0) (scalarTy @a)) (!*^)

  (!*) :: forall i j. (KnownNat i, KnownNat j)
       => AST (M i j a) -> AST a -> AST (M i j a)
  (!*) = fromAST $ PrimOp (SPIRV.MatOp SPIRV.MMulK (val @i) (val @j) (scalarTy @a)) (!*)


infixl 4 <$$>
infixl 4 <**>

-- functor functionality
class ASTFunctor f where
  fmapAST :: PrimTy a => (AST a -> AST b) -> AST (f a) -> AST (f b) --  AST (a -> b) -> AST (f a) -> AST (f b)

class ASTApplicative f where
  pureAST :: AST a -> AST (f a)
  (<**>)  :: PrimTy a => AST ( f (a -> b) ) -> AST ( f a ) -> AST ( f b )

(<$$>) :: (ASTFunctor f, PrimTy a) => (AST a -> AST b) -> AST (f a) -> AST (f b) -- AST (a -> b) -> AST (f a) -> AST (f b)
(<$$>) = fmapAST


instance KnownNat n => ASTFunctor (V n) where
  fmapAST = fromAST $ Fmap (SFuncVector (Proxy @n))

instance (KnownNat m, KnownNat n) => ASTFunctor (M m n) where
  fmapAST = fromAST $ Fmap (SFuncMatrix (Proxy @m) (Proxy @n))

instance KnownNat n => ASTApplicative (V n) where
  pureAST = fromAST $ Pure (SFuncVector (Proxy @n))
  (<**>)  = fromAST $ Ap   (SFuncVector (Proxy @n)) Proxy

instance (KnownNat m, KnownNat n) => ASTApplicative (M m n) where
  pureAST = fromAST $ Pure (SFuncMatrix (Proxy @m) (Proxy @n))
  (<**>)  = fromAST $ Ap   (SFuncMatrix (Proxy @m) (Proxy @n)) Proxy


instance 
  TypeError (     Text "The AST datatype does not have a Functor instance:"
             :$$: Text "    cannot map Haskell functions over internal types."
             :$$: Text "To map an internal function over an internal type, use 'fmapAST'."
            ) => Prelude.Functor AST where
  fmap = error "unreachable"

-----------------------------------------------
-- syntactic instances

instance Syntactic (AST a) where
  type Internal (AST a) = a
  toAST   = id
  fromAST = id

instance Syntactic () where
  type Internal () = ()
  toAST   = lit
  fromAST = const ()

instance (Syntactic a, Syntactic b) => Syntactic (a -> b) where
  type Internal (a -> b) = Internal a -> Internal b
  toAST   f = Lam ( toAST . f . fromAST )
  fromAST f = \a -> fromAST ( f :$ toAST a )

-- utility type for the following instance declaration
newtype B n a b i = B { unB :: AST (NatVariadic (n-i) a b) }

instance (KnownNat n, Syntactic a, PrimTy (Internal a)) => Syntactic (V n a) where
  type Internal (V n a) = V n (Internal a)

  toAST :: V n a -> AST (V n (Internal a))
  toAST v = res'
    where f :: forall i. (KnownNat i, (n-(i+1)) ~ ((n-i)-1), (1 <=? (n-i)) ~ 'True)
            => a
            -> B n (Internal a) (V n (Internal a)) i
            -> B n (Internal a) (V n (Internal a)) (i+1)
          f a (B b) = B ( b :$ toAST a )
          a0 :: B n (Internal a) (V n (Internal a)) 0
          a0 = B ( MkVector (Proxy @n) (Proxy @(Internal a)) )
          res :: B n (Internal a) (V n (Internal a)) n
          res = dfoldrV f a0 v
          res' :: ((n-n) ~ 0) => AST (NatVariadic 0 (Internal a) (V n (Internal a)))
          res' = unB res

  fromAST :: AST (V n (Internal a)) -> V n a
  fromAST = buildV ( \i v -> fromAST ( VectorAt (Proxy @(Internal a)) i :$ v) )

deriving instance (KnownNat m, KnownNat n, Syntactic a, ScalarTy (Internal a))
  => Syntactic (M m n a)

-- these patterns and constructors could be generalised to have types such as:
-- Vec2 :: (Syntactic a, PrimTy (Internal a)) => a -> a -> AST ( V 2 (Internal a) )
-- but this leads to poor type-inference

{-# COMPLETE Vec2 #-}
pattern Vec2 :: PrimTy a => AST a -> AST a -> AST ( V 2 a )
pattern Vec2 x y <- (fromAST -> V2 x y)

{-# COMPLETE Vec3 #-}
pattern Vec3 :: PrimTy a => AST a -> AST a -> AST a -> AST ( V 3 a )
pattern Vec3 x y z <- (fromAST -> V3 x y z)

{-# COMPLETE Vec4 #-}
pattern Vec4 :: PrimTy a => AST a -> AST a -> AST a -> AST a -> AST ( V 4 a )
pattern Vec4 x y z w <- (fromAST -> V4 x y z w)

vec2 :: forall a. PrimTy a => AST a -> AST a -> AST ( V 2 a )
vec2 = fromAST $ MkVector @2 @a Proxy Proxy

vec3 :: forall a. PrimTy a => AST a -> AST a -> AST a -> AST ( V 3 a )
vec3 = fromAST $ MkVector @3 @a Proxy Proxy

vec4 :: forall a. PrimTy a => AST a -> AST a -> AST a -> AST a -> AST ( V 4 a )
vec4 = fromAST $ MkVector @4 @a Proxy Proxy


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

mat22
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 2 2 a )
mat22 a11 a12
      a21 a22
  = Mat :$ vec2
      ( vec2 a11 a12 )
      ( vec2 a21 a22 )

mat23
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 2 3 a )
mat23 a11 a12 a13
      a21 a22 a23
  = Mat :$ vec2
      ( vec3 a11 a12 a13 )
      ( vec3 a21 a22 a23 )

mat24
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 2 4 a )
mat24 a11 a12 a13 a14
      a21 a22 a23 a24
  = Mat :$ vec2
      ( vec4 a11 a12 a13 a14 )
      ( vec4 a21 a22 a23 a24 )

mat32
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 3 2 a )
mat32 a11 a12
      a21 a22
      a31 a32
  = Mat :$ vec3
      ( vec2 a11 a12 )
      ( vec2 a21 a22 )
      ( vec2 a31 a32 )

mat33
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 3 3 a )
mat33 a11 a12 a13
      a21 a22 a23
      a31 a32 a33
  = Mat :$ vec3
      ( vec3 a11 a12 a13 )
      ( vec3 a21 a22 a23 )
      ( vec3 a31 a32 a33 )

mat34
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 3 4 a )
mat34 a11 a12 a13 a14
      a21 a22 a23 a24
      a31 a32 a33 a34
  = Mat :$ vec3
      ( vec4 a11 a12 a13 a14 )
      ( vec4 a21 a22 a23 a24 )
      ( vec4 a31 a32 a33 a34 )

mat42
  :: ScalarTy a
  => AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 4 2 a )
mat42 a11 a12
      a21 a22
      a31 a32
      a41 a42
  = Mat :$ vec4
      ( vec2 a11 a12 )
      ( vec2 a21 a22 )
      ( vec2 a31 a32 )
      ( vec2 a41 a42 )

mat43
  :: ScalarTy a
  => AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 4 3 a )
mat43 a11 a12 a13
      a21 a22 a23
      a31 a32 a33
      a41 a42 a43
  = Mat :$ vec4
      ( vec3 a11 a12 a13 )
      ( vec3 a21 a22 a23 )
      ( vec3 a31 a32 a33 )
      ( vec3 a41 a42 a43 )

mat44
  :: ScalarTy a
  => AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 4 4 a )
mat44 a11 a12 a13 a14
      a21 a22 a23 a24
      a31 a32 a33 a34
      a41 a42 a43 a44
  = Mat :$ vec4
      ( vec4 a11 a12 a13 a14 )
      ( vec4 a21 a22 a23 a24 )
      ( vec4 a31 a32 a33 a34 )
      ( vec4 a41 a42 a43 a44 )
