{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module FIR.Instances where

-- base
import Prelude hiding( Eq(..), (&&), (||), not
                     , Ord(..)
                     , Num(..), Floating(..)
                     , Integral(..)
                     , Fractional(..), fromRational
                     )
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import GHC.TypeLits  ( KnownNat, type (+), type (-)
                     , KnownSymbol
                     , TypeError, ErrorMessage(..)
                     )
import GHC.TypeNats(type (<=?))

-- fir  
import FIR.AST(AST(..), Codensity(..), PrimTy, PrimScalarTy, scalar, S)
import qualified FIR.AST as AST
import Control.Monad.Indexed ( FunctorIx(fmapIx)
                             , MonadIx(..), MonadIxFail(..)
                             , (:=)(..), withKey
                             )
import Data.Type.Bindings( BindingType, Var, Fun
                         , CanDef, CanFunDef
                         , Put, Get
                         , Union, Insert
                         , Variadic
                         )
import Math.Logic.Class ( Eq(..), Boolean(..), HasBool(..)
                        , Ord(..)
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
                  , V, M
                  , dim
                  , dfoldrV, buildV
                  , pattern V2, pattern V3, pattern V4
                  )
import qualified SPIRV.PrimOp as SPIRV

---------------------------------------------------

class MonadIx m => ScopeIx m where

  def' :: forall k perms a i. 
          ( KnownSymbol k
          , CanDef k i ~ 'True
          , PrimTy a
          )
       => Proxy k
       -> Proxy perms
       -> AST a
       -> m (AST a := Insert k (Var perms a) i) i

  defun' :: forall k as b l i. 
            ( KnownSymbol k
            , CanFunDef k as i l ~ 'True
            , PrimTy b
            )
         => Proxy k
         -> Proxy as
         -> Proxy l
         -> AST (S (b := l) (Union i as))
         -> m (AST (BindingType (Fun as b)) := Insert k (Fun as b) i) i

  get' :: forall k a i. (KnownSymbol k, Get k i ~ a)
       => Proxy k
       -> m (AST a := i) i

  put' :: forall k a i. (KnownSymbol k, Put k i ~ a, PrimTy a)
       => Proxy k
       -> AST a
       -> m (AST () := i) i
  
def :: forall k perms a i m. (ScopeIx m, KnownSymbol k, CanDef k i ~ 'True, PrimTy a)
    => AST a
    -> m (AST a := Insert k (Var perms a) i) i 
def = def' @m @k @perms @a @i Proxy Proxy

defun :: forall k as b l i m. (ScopeIx m, KnownSymbol k, CanFunDef k as i l ~ 'True, PrimTy b)
        => AST (S (b := l) (Union i as))
        -> m (AST (BindingType (Fun as b)) := Insert k (Fun as b) i) i
defun = defun' @m @k @as @b @l @i Proxy Proxy Proxy

get :: forall k a i m. (ScopeIx m, KnownSymbol k, Get k i ~ a)
     => m (AST a := i) i
get = get' @m @k @a @i Proxy

put :: forall k a i m. (ScopeIx m, KnownSymbol k, Put k i ~ a, PrimTy a)
     => AST a -> m (AST () := i) i
put = put' @m @k @a @i Proxy

---------------------------------------------------
-- specialise function definition to work with the codensity representation
-- generalise the return type, to allow user-defined function to yield types of either forms:
-- AST a_1 -> ... -> AST a_n -> AST b
-- AST ( a_1 -> ... -> a_n -> b)

fundef :: forall k as b f l i. 
          ( KnownSymbol k, PrimTy b
          , CanFunDef k as i l ~ 'True          
          , Syntactic f, Internal f ~ Variadic as b
          )
       => Codensity S (AST b := l) (Union i as) 
       -> Codensity S (f := Insert k (Fun as b) i) i
fundef = fmapIx (fromAST `withKey`) . defun @k @as . toAST

--------------------------------------------------------------------------

-- force uniqueness of ScopeIx instance to improve type inference
instance (m ~ Codensity S) => ScopeIx m where
  def'   k b   a = Codensity ( \h -> Bind :$ (Def    k b   :$ a) :$ (Lam $ h . AtKey) )
  defun' k j l f = Codensity ( \h -> Bind :$ (FunDef k j l :$ f) :$ (Lam $ h . AtKey) )
  get'   k       = Codensity ( \h -> Bind :$  Get    k           :$ (Lam $ h . AtKey) )
  put'   k     a = Codensity ( \h -> Bind :$ (Put    k     :$ a) :$ (Lam $ h . AtKey) )

instance TypeError (     Text "Failable pattern detected in AST construction."
                    :$$: Text "Only irrefutable patterns are supported."
                    :$$: Text "As inference of pattern refutability is sometimes patchy,"
                    :$$: Text "consider using a lazy pattern match instead:"
                    :$$: Text "'~pat <- ...' instead of 'pat <- ...'."
                    ) => MonadIxFail (Codensity S) where
  fail = error "'fail': irrefutable pattern failed to match, during AST construction."

--------------------------------------------------------------------------------------
-- instances for AST

-- logical operations
instance Boolean (AST Bool) where
  true  = Lit True
  false = Lit False
  (&&)  = fromAST $ PrimOp (SPIRV.BoolOp SPIRV.And) (&&)
  (||)  = fromAST $ PrimOp (SPIRV.BoolOp SPIRV.Or ) (||)
  not   = fromAST $ PrimOp (SPIRV.BoolOp SPIRV.Not) not

instance PrimTy a => HasBool (AST Bool) (AST a) where
  bool = fromAST If

instance (PrimScalarTy a, Eq a , Logic a ~ Bool) => Eq (AST a) where
  type Logic (AST a) = AST (Logic a)
  (==) = fromAST $ PrimOp (SPIRV.EqOp SPIRV.Equal    (scalar @a)) (==)
  (/=) = fromAST $ PrimOp (SPIRV.EqOp SPIRV.NotEqual (scalar @a)) (/=)

instance (PrimScalarTy a, Ord a, Logic a ~ Bool) => Ord (AST a) where
  type Compare (AST a) = AST Int
  compare = error "todo"
  (<=) = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.LTE (scalar @a)) (<=)
  (>=) = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.GTE (scalar @a)) (>=)
  (<)  = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.LT  (scalar @a)) (<)
  (>)  = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.GT  (scalar @a)) (>)
  min  = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.Min (scalar @a)) min
  max  = fromAST $ PrimOp (SPIRV.OrdOp SPIRV.Max (scalar @a)) max

-- numeric operations
instance (PrimScalarTy a, AdditiveGroup a) => AdditiveGroup (AST a) where
  (+)    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Add  (scalar @a)) (+)
  zero   = Lit (zero :: a)
instance (PrimScalarTy a, Semiring a) => Semiring (AST a) where
  (*)    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Mul  (scalar @a)) (*)  
instance (PrimScalarTy a, Ring a) => Ring (AST a) where
  (-)    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Sub  (scalar @a)) (-)
  negate = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Neg  (scalar @a)) negate
  fromInteger = Lit . fromInteger
instance (PrimScalarTy a, Signed a) => Signed (AST a) where
  abs    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Abs  (scalar @a)) abs
  signum = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Sign (scalar @a)) signum
instance (PrimScalarTy a, DivisionRing a) => DivisionRing (AST a) where
  (/)    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Div  (scalar @a)) (/)
  fromRational = Lit . fromRational
instance (PrimScalarTy a, Archimedean a, Logic a ~ Bool) => Archimedean (AST a) where
  mod    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Mod  (scalar @a)) mod
  rem    = fromAST $ PrimOp (SPIRV.NumOp SPIRV.Rem  (scalar @a)) rem


-- numeric conversions

-- too lazy to define all instances by hand, so using this hack
type family DisEq a b where
  DisEq a a = 'False
  DisEq a b = 'True

instance (PrimScalarTy a, PrimScalarTy b, Convert a b, DisEq a b ~ 'True)
         => Convert (AST a) (AST b) where
  convert = fromAST $ PrimOp (SPIRV.ConvOp SPIRV.Convert (scalar @a) (scalar @b)) convert

-- vectors
instance (PrimScalarTy a, Semiring a) => Semimodule (AST (V 0 a)) where
  type Scalar (AST (V 0 a))   = AST a
  type OfDim  (AST (V 0 a)) n = AST (V n a)

  (^+^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST (V n a)
  (^+^) = fromAST $ PrimOp (SPIRV.VecOp SPIRV.AddV  (dim @n) (scalar @a)) (^+^)

  (^*) :: forall n. KnownNat n
        => AST (V n a) -> AST a -> AST (V n a)
  (^*)  = fromAST $ PrimOp (SPIRV.VecOp SPIRV.VMulK (dim @n) (scalar @a)) (^*)

instance (PrimScalarTy a, Ring a) => Module (AST (V 0 a)) where
  (^-^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST (V n a)
  (^-^) = fromAST $ PrimOp (SPIRV.VecOp SPIRV.SubV  (dim @n) (scalar @a)) (^-^)

instance (PrimScalarTy a, Semiring a) => Inner (AST (V 0 a)) where
  (^.^) = fromAST $ PrimOp (SPIRV.VecOp SPIRV.DotV  (dim @0) (scalar @a)) (^.^)


-- matrices
instance (PrimScalarTy a, Ring a) => Matrix (AST (M 0 0 a)) where
  type Vector (AST (M 0 0 a)) = AST (V 0 a)
  type OfDims (AST (M 0 0 a)) m n = AST (M m n a)

  diag        = error "todo"
  konst       = error "todo"

  transpose :: forall n m. (KnownNat n, KnownNat m)
            => AST (M n m a) -> AST (M m n a)
  transpose   = fromAST $ PrimOp (SPIRV.MatOp SPIRV.Transp (dim @m) (dim @n) (scalar @a)) transpose

  inverse :: forall n. KnownNat n
            => AST (M n n a) -> AST (M n n a)
  inverse     = fromAST $ PrimOp (SPIRV.MatOp SPIRV.Inv    (dim @n) (dim @n) (scalar @a)) inverse
  
  determinant :: forall n. KnownNat n
              => AST (M n n a) -> AST a
  determinant = fromAST $ PrimOp (SPIRV.MatOp SPIRV.Det    (dim @0) (dim @0) (scalar @a)) determinant

  (!+!) = error "todo"
  (!-!) = error "todo"

  (!*!) :: forall i j k. (KnownNat i, KnownNat j, KnownNat k)
        => AST (M i j a) -> AST (M j k a) -> AST (M i k a)
  (!*!) = fromAST $ PrimOp (SPIRV.MatOp SPIRV.MMulM (dim @i) (dim @k) (scalar @a)) (!*!)

  (^*!) :: forall i j. (KnownNat i, KnownNat j)
        => AST (V i a) -> AST (M i j a) -> AST (V j a)
  (^*!) = fromAST $ PrimOp (SPIRV.MatOp SPIRV.VMulM (dim @j) (dim @0) (scalar @a)) (^*!)

  (!*^) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST (V j a) -> AST (V i a)
  (!*^) = fromAST $ PrimOp (SPIRV.MatOp SPIRV.MMulV (dim @i) (dim @0) (scalar @a)) (!*^)

  (!*) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST a -> AST (M i j a)
  (!* ) = fromAST $ PrimOp (SPIRV.MatOp SPIRV.MMulK (dim @i) (dim @j) (scalar @a)) (!*)

-- functor functionality

class FunctorAST f where
  fmapAST :: (Syntactic ret, Internal ret ~ (f a -> f b))
          => AST (a -> b) -> ret

instance KnownNat n => FunctorAST (V n) where
  fmapAST = fromAST $ FmapVector (Proxy @n)

instance 
  TypeError (     Text "The AST datatype does not have a Functor instance:"
             :$$: Text "    cannot map Haskell functions over internal types."
             :$$: Text "To map an internal function over an internal type, use 'fmapAST'."
            ) => Functor AST where
  fmap = error "unreachable"

------------------------------------------------
-- syntactic

class Syntactic a where
  type Internal a
  toAST :: a -> AST (Internal a)
  fromAST :: AST (Internal a) -> a


instance Syntactic (AST a) where
  type Internal (AST a) = a
  toAST   = id
  fromAST = id

instance Syntactic () where
  type Internal () = ()
  toAST   = Lit
  fromAST = const ()

instance (Syntactic a, Syntactic b) => Syntactic (a -> b) where
  type Internal (a -> b) = Internal a -> Internal b
  toAST   f = Lam ( toAST . f . fromAST )
  fromAST f = \a -> fromAST ( f :$ toAST a )


instance Syntactic a => Syntactic ( (a := j) i ) where
  type Internal ( (a := j) i ) = (Internal a := j) i
  toAST :: (a := j) i -> AST ( (Internal a := j) i)
  toAST (AtKey a) = Ix :$ toAST a
  fromAST :: AST ( (Internal a := j) i) -> (a := j) i
  fromAST (Ix :$ a) = AtKey (fromAST a)
  fromAST _ = error "help needed"


instance Syntactic a => Syntactic (Codensity (m :: (k -> Type) -> (k -> Type)) (a := j) i) where
  type Internal (Codensity m (a := j) i) = m (Internal a := j) i

  toAST :: Codensity m (a := j) i -> AST (m (Internal a := j) i)
  toAST (Codensity k) = k ( fromAST Pure )

  fromAST :: AST (m (Internal a := j) i) -> Codensity m (a := j) i
  fromAST a = Codensity ( \k -> fromAST Bind a (k . AtKey) )



-- utility type for the following instance declaration
newtype B n a b i = B { unB :: AST (AST.Variadic (n-i) a b) }

instance (KnownNat n, Syntactic a) => Syntactic (V n a) where
  type Internal (V n a) = V n (Internal a)

  toAST :: V n a -> AST (V n (Internal a))
  toAST v = res'
    where f :: forall i. (KnownNat i, (n-(i+1)) ~ ((n-i)-1), (1 <=? (n-i)) ~ 'True)
            => a
            -> B n (Internal a) (V n (Internal a)) i
            -> B n (Internal a) (V n (Internal a)) (i+1)
          f a (B b) = B ( b :$ toAST a )
          a0 :: B n (Internal a) (V n (Internal a)) 0
          a0 = B ( MkVector @n @(Internal a) Proxy )
          res :: B n (Internal a) (V n (Internal a)) n
          res = dfoldrV f a0 v
          res' :: ((n-n) ~ 0) => AST (AST.Variadic 0 (Internal a) (V n (Internal a)))
          res' = unB res

  fromAST :: AST (V n (Internal a)) -> V n a
  fromAST = buildV ( \i v -> fromAST ( VectorAt i :$ v) )

-- these patterns and constructors could be generalised to have types such as:
-- Vec2 :: Syntactic a => a -> a -> AST ( V 2 (Internal a) )
-- but this leads to poor type-inference

{-# COMPLETE Vec2 #-}
pattern Vec2 :: AST a -> AST a -> AST ( V 2 a )
pattern Vec2 x y <- (fromAST -> V2 x y)

{-# COMPLETE Vec3 #-}
pattern Vec3 :: AST a -> AST a -> AST a -> AST ( V 3 a )
pattern Vec3 x y z <- (fromAST -> V3 x y z)

{-# COMPLETE Vec4 #-}
pattern Vec4 :: AST a -> AST a -> AST a -> AST a -> AST ( V 4 a )
pattern Vec4 x y z w <- (fromAST -> V4 x y z w)

vec2 :: AST a -> AST a -> AST ( V 2 a )
vec2 = fromAST $ MkVector (Proxy @2)

vec3 :: AST a -> AST a -> AST a -> AST ( V 3 a )
vec3 = fromAST $ MkVector (Proxy @3)

vec4 :: AST a -> AST a -> AST a -> AST a -> AST ( V 4 a )
vec4 = fromAST $ MkVector (Proxy @4)

{-# COMPLETE Mat22 #-}
pattern Mat22
  :: AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 2 2 a )
pattern Mat22 a11 a12
              a21 a22
  <- ( fromAST
       -> V2 ( V2 a11 a12 )
             ( V2 a21 a22 )
     )

{-# COMPLETE Mat23 #-}
pattern Mat23
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 2 3 a )
pattern Mat23 a11 a12 a13
              a21 a22 a23
   <- ( fromAST
        -> V2 ( V3 a11 a12 a13 )
              ( V3 a21 a22 a23 )
      )

{-# COMPLETE Mat24 #-}
pattern Mat24
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 2 4 a )
pattern Mat24 a11 a12 a13 a14
              a21 a22 a23 a24
   <- ( fromAST
        -> V2 ( V4 a11 a12 a13 a14 )
              ( V4 a21 a22 a23 a24 )
      )

{-# COMPLETE Mat32 #-}
pattern Mat32
  :: AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 3 2 a )
pattern Mat32 a11 a12
              a21 a22
              a31 a32
   <- ( fromAST
        -> V3 ( V2 a11 a12 )
              ( V2 a21 a22 )
              ( V2 a31 a32 )
      )

{-# COMPLETE Mat33 #-}
pattern Mat33
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 3 3 a )
pattern Mat33 a11 a12 a13
              a21 a22 a23
              a31 a32 a33
   <- ( fromAST
        -> V3 ( V3 a11 a12 a13 )
              ( V3 a21 a22 a23 )
              ( V3 a31 a32 a33 )
      )

{-# COMPLETE Mat34 #-}
pattern Mat34
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 3 4 a )
pattern Mat34 a11 a12 a13 a14
              a21 a22 a23 a24
              a31 a32 a33 a34
   <- ( fromAST
        -> V3 ( V4 a11 a12 a13 a14 )
              ( V4 a21 a22 a23 a24 )
              ( V4 a31 a32 a33 a34 )
      )

{-# COMPLETE Mat42 #-}
pattern Mat42
  :: AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 4 2 a )
pattern Mat42 a11 a12
              a21 a22
              a31 a32
              a41 a42
   <- ( fromAST
        -> V4 ( V2 a11 a12 )
              ( V2 a21 a22 )
              ( V2 a31 a32 )
              ( V2 a41 a42 )
      )

{-# COMPLETE Mat43 #-}
pattern Mat43
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 4 3 a )
pattern Mat43 a11 a12 a13
              a21 a22 a23
              a31 a32 a33
              a41 a42 a43
   <- ( fromAST
        -> V4 ( V3 a11 a12 a13 )
              ( V3 a21 a22 a23 )
              ( V3 a31 a32 a33 )
              ( V3 a41 a42 a43 )
      )

{-# COMPLETE Mat44 #-}
pattern Mat44
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 4 4 a )
pattern Mat44 a11 a12 a13 a14
              a21 a22 a23 a24
              a31 a32 a33 a34
              a41 a42 a43 a44
   <- ( fromAST
        -> V4 ( V4 a11 a12 a13 a14 )
              ( V4 a21 a22 a23 a24 )
              ( V4 a31 a32 a33 a34 )
              ( V4 a41 a42 a43 a44 )
      )

mat22
  :: AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 2 2 a )
mat22 a11 a12
      a21 a22
  = vec2
      ( vec2 a11 a12 )
      ( vec2 a21 a22 )

mat23
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 2 3 a )
mat23 a11 a12 a13
      a21 a22 a23
  = vec2
      ( vec3 a11 a12 a13 )
      ( vec3 a21 a22 a23 )

mat24
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 2 4 a )
mat24 a11 a12 a13 a14
      a21 a22 a23 a24
  = vec2
      ( vec4 a11 a12 a13 a14 )
      ( vec4 a21 a22 a23 a24 )

mat32
  :: AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 3 2 a )
mat32 a11 a12
      a21 a22
      a31 a32
  = vec3
      ( vec2 a11 a12 )
      ( vec2 a21 a22 )
      ( vec2 a31 a32 )

mat33
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 3 3 a )
mat33 a11 a12 a13
      a21 a22 a23
      a31 a32 a33
  = vec3
      ( vec3 a11 a12 a13 )
      ( vec3 a21 a22 a23 )
      ( vec3 a31 a32 a33 )

mat34
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 3 4 a )
mat34 a11 a12 a13 a14
      a21 a22 a23 a24
      a31 a32 a33 a34
  = vec3
      ( vec4 a11 a12 a13 a14 )
      ( vec4 a21 a22 a23 a24 )
      ( vec4 a31 a32 a33 a34 )

mat42
  :: AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 4 2 a )
mat42 a11 a12
      a21 a22
      a31 a32
      a41 a42
  = vec4
      ( vec2 a11 a12 )
      ( vec2 a21 a22 )
      ( vec2 a31 a32 )
      ( vec2 a41 a42 )

mat43
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 4 3 a )
mat43 a11 a12 a13
      a21 a22 a23
      a31 a32 a33
      a41 a42 a43
  = vec4
      ( vec3 a11 a12 a13 )
      ( vec3 a21 a22 a23 )
      ( vec3 a31 a32 a33 )
      ( vec3 a41 a42 a43 )

mat44
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 4 4 a )
mat44 a11 a12 a13 a14
      a21 a22 a23 a24
      a31 a32 a33 a34
      a41 a42 a43 a44
  = vec4
      ( vec4 a11 a12 a13 a14 )
      ( vec4 a21 a22 a23 a24 )
      ( vec4 a31 a32 a33 a34 )
      ( vec4 a41 a42 a43 a44 )