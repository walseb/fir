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

module FIR where

-- base
import Prelude hiding(Functor(..), Eq(..), (&&), (||), not, Integral(..), fromIntegral)
import qualified Prelude as P

import Data.Coerce(Coercible, coerce)
import Unsafe.Coerce(unsafeCoerce)
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import GHC.TypeLits(Nat, KnownNat, type (+), type (-), Symbol, KnownSymbol, TypeError, ErrorMessage(Text,(:$$:),(:<>:)))
import GHC.TypeNats(type (<=?))

-- fir  
import AST(AST(..), PrimTy, ScalarTy(scalar), S)
import qualified AST
import Bindings(Assignment((:->)), CanDef, CanFunDef, Put, Get, Union, Variadic, Binding(Var,Fun), BindingType, Insert, Permission(Read,Write))
import TypeClasses.Equality(Eq(..), Boolean(..), HasBool(..))
import TypeClasses.Category(Functor(..))
import Indexed((:=)(WithIx), Codensity(Codensity), MonadIx, FunctorIx(fmapIx))
import Linear(Module(..), Inner(..), Matrix(..), V, M, dfoldrV, buildV, pattern V2, pattern V3, pattern V4)
import qualified SPIRV

---------------------------------------------------

class MonadIx m => ScopeIx m where

  def' :: forall k perms ty i. (KnownSymbol k, CanDef k i ~ 'True, PrimTy ty)
       => Proxy k -> Proxy perms -> AST ty -> m (AST () := Insert k ('Var perms ty) i) i

  defun' :: forall k j ty l i. (KnownSymbol k, CanFunDef k i j l ~ 'True, PrimTy ty)
         => Proxy k -> Proxy j -> Proxy l -> AST (S (ty := l) (Union i j)) -> m (AST (BindingType ('Fun j ty)) := Insert k ('Fun j ty) i) i

  get' :: forall k ty i. (KnownSymbol k, Get k i ~ ty)
       => Proxy k -> m (AST ty := i) i

  put' :: forall k ty i. (KnownSymbol k, Put k i ~ ty, PrimTy ty)
       => Proxy k -> AST ty -> m (AST () := i) i
  
def :: forall k perms ty i m. (ScopeIx m, KnownSymbol k, CanDef k i ~ 'True, PrimTy ty)
    => AST ty -> m (AST () := Insert k ('Var perms ty) i) i 
def = def' @m @k @perms @ty @i Proxy Proxy

defun :: forall k j ty l i m. (ScopeIx m, KnownSymbol k, CanFunDef k i j l ~ 'True, PrimTy ty)
        => AST (S (ty := l) (Union i j)) -> m (AST (BindingType ('Fun j ty)) := Insert k ('Fun j ty) i) i
defun = defun' @m @k @j @ty @l @i Proxy Proxy Proxy

get :: forall k ty i m. (ScopeIx m, KnownSymbol k, Get k i ~ ty)
     => m (AST ty := i) i
get = get' @m @k @ty @i Proxy

put :: forall k ty i m. (ScopeIx m, KnownSymbol k, Put k i ~ ty, PrimTy ty)
     => AST ty -> m (AST () := i) i
put = put' @m @k @ty @i Proxy

---------------------------------------------------
-- specialise function definition code
-- to work with the codensity representation,
-- and modify it so that the return type of 'fundef'
-- provides the user with a bona-fide function,
-- of type AST a1 -> AST a2 -> ... -> AST b,
-- instead of AST (a1 -> a2 -> ... -> b)


fundef :: forall k j ty l i. (KnownSymbol k, CanFunDef k i j l ~ 'True, PrimTy ty, Convert j ty)
       => Codensity AST S (AST ty := l) (Union i j) 
       -> Codensity AST S (VariadicAST j ty := Insert k ('Fun j ty) i) i
fundef f =
  let f' :: Codensity AST S (AST ty := l) (Union i j) 
      f' = coerce f
      f'' :: AST (S (ty := l) (Union i j))
      f'' = toAST f'
      f''' :: Codensity AST S (AST (BindingType ('Fun j ty)) := Insert k ('Fun j ty) i) i
      f''' = defun @k @j @ty @l @i @(Codensity AST S) f''
  in functionalise @k @j @ty @i f'''

-- variadic mischievery
functionalise :: forall k j ty i. (KnownSymbol k, PrimTy ty, Convert j ty)
     => Codensity AST S (AST (Variadic j ty) := Insert k ('Fun j ty) i) i
     -> Codensity AST S (VariadicAST j ty  := Insert k ('Fun j ty) i) i
functionalise = fmapIx ( throughIx (convert @j @ty) )

throughIx :: (a -> b) -> (a := i) j -> (b := i) j
throughIx f (WithIx a) = WithIx (f a)

type family VariadicAST as b where
  VariadicAST '[]                 b = AST b
  VariadicAST (( _ ':-> a) ': as) b = AST (BindingType a) -> VariadicAST as b

class Convert as b where
  convert :: AST (Variadic as b) -> VariadicAST as b

instance Convert '[] b where
  convert = id

instance (Convert as b) => Convert ((k ':-> v) ': as) b where
  convert :: AST (Variadic ((k ':-> v) ': as) b) -> VariadicAST ((k ':-> v) ': as) b
  convert f a = convert @as @b (f :$ a)
  -- everything happens here ---^^^^^^

--------------------------------------------------------------------------

instance ScopeIx (Codensity AST S) where
  def'   k b   a = Codensity ( \h -> Bind :$ (Def    k b   :$ coerce a) :$ (Lam $ h . WithIx         ) )
  defun' k j l f = Codensity ( \h -> Bind :$ (FunDef k j l :$        f) :$ (Lam $ h . WithIx         ) )
  get'   k       = Codensity ( \h -> Bind :$  Get    k                  :$ (Lam $ h . WithIx . coerce) )
  put'   k     a = Codensity ( \h -> Bind :$ (Put    k     :$ coerce a) :$ (Lam $ h . WithIx         ) )

--------------------------------------------------------------------------------------
-- instances for AST

instance ( ScalarTy a
         , Num a
         ) => Num (AST a) where
  (+)    = fromAST $ PrimOp (SPIRV.Numeric (scalar @a)) (SPIRV.NumOp SPIRV.Add ) (+)
  (-)    = fromAST $ PrimOp (SPIRV.Numeric (scalar @a)) (SPIRV.NumOp SPIRV.Sub ) (-)
  (*)    = fromAST $ PrimOp (SPIRV.Numeric (scalar @a)) (SPIRV.NumOp SPIRV.Mul ) (*)
  abs    = fromAST $ PrimOp (SPIRV.Numeric (scalar @a)) (SPIRV.NumOp SPIRV.Abs ) abs
  signum = fromAST $ PrimOp (SPIRV.Numeric (scalar @a)) (SPIRV.NumOp SPIRV.Sign) signum
  negate = fromAST $ PrimOp (SPIRV.Numeric (scalar @a)) (SPIRV.NumOp SPIRV.Neg ) negate
  fromInteger = Lit . fromInteger

instance Boolean (AST Bool) where
  true  = Lit True
  false = Lit False
  (&&)  = fromAST $ PrimOp SPIRV.Boolean (SPIRV.BoolOp SPIRV.And) (&&)
  (||)  = fromAST $ PrimOp SPIRV.Boolean (SPIRV.BoolOp SPIRV.Or ) (||)
  not   = fromAST $ PrimOp SPIRV.Boolean (SPIRV.BoolOp SPIRV.Not) not

instance PrimTy a => HasBool (AST Bool) (AST a) where
  bool = fromAST $ If

instance (PrimTy a, Eq a , Logic a ~ Bool) => Eq (AST a) where
  type Logic (AST a) = AST (Logic a)
  (==) = error "todo" --fromAST $ PrimOp "==" (==)

instance ( ScalarTy a
         , Fractional a
         ) => Fractional (AST a) where
  (/)   = fromAST $ PrimOp (SPIRV.Numeric (scalar @a)) (SPIRV.NumOp SPIRV.Div) (/)
  fromRational = Lit . fromRational


instance ( ScalarTy a
         , Floating a
         ) => Floating (AST a) where
  (**)  = error "todo" --fromAST $ PrimOp "**"    (**)
  sqrt  = error "todo" --fromAST $ PrimOp "sqrt"  sqrt
  acos  = error "todo" --fromAST $ PrimOp "acos"  acos
  asin  = error "todo" --fromAST $ PrimOp "asin"  asin
  atan  = error "todo" --fromAST $ PrimOp "atan"  atan
  cos   = error "todo" --fromAST $ PrimOp "cos"   cos
  sin   = error "todo" --fromAST $ PrimOp "sin"   sin
  tan   = error "todo" --fromAST $ PrimOp "tan"   tan
  acosh = error "todo" --fromAST $ PrimOp "acosh" acosh
  asinh = error "todo" --fromAST $ PrimOp "asinh" asinh
  atanh = error "todo" --fromAST $ PrimOp "atanh" atanh
  cosh  = error "todo" --fromAST $ PrimOp "cosh"  cosh
  sinh  = error "todo" --fromAST $ PrimOp "sinh"  sinh
  tanh  = error "todo" --fromAST $ PrimOp "tanh"  tanh
  exp   = error "todo" --fromAST $ PrimOp "exp"   exp
  log   = error "todo" --fromAST $ PrimOp "log"   log
  pi    = Lit pi

instance ( ScalarTy a
         , Num a
         ) => Module (AST (V 0 a)) where
  type Scalar (AST (V 0 a))   = AST a
  type OfDim  (AST (V 0 a)) n = AST (V n a)
  v ^+^ w = fromAST (PrimOp (SPIRV.Vector (scalar @a)) (SPIRV.VecOp SPIRV.AddV ) (^+^)) v w
  v ^-^ w = fromAST (PrimOp (SPIRV.Vector (scalar @a)) (SPIRV.VecOp SPIRV.SubV ) (^-^)) v w
  v ^*  k = fromAST (PrimOp (SPIRV.Vector (scalar @a)) (SPIRV.VecOp SPIRV.VMulK) (^*) ) v k


instance Functor (AST (V n ())) where
  type FunctorApp (AST (V n ())) a = AST (V n a)
  fmap = undefined


instance ( ScalarTy a
         , Num a
         ) => Inner (AST (V 0 a)) where
  v ^.^ w = fromAST ( PrimOp (SPIRV.Vector (scalar @a) ) (SPIRV.VecOp SPIRV.DotV) (^.^)) v w

instance ( ScalarTy a
         , Num a
         ) => Matrix (AST (M 0 0 a)) where
  type Vector (AST (M 0 0 a)) = AST (V 0 a)
  type OfDims (AST (M 0 0 a)) m n = AST (M m n a)

  diag        t  = error "todo" --ASTMat ( fromAST (PrimOp "diag"        diag       ) t )
  konst       t  = error "todo" --ASTMat ( fromAST (PrimOp "konst"       konst      ) t )
  transpose   m = fromAST (PrimOp (SPIRV.Matrix (scalar @a)) (SPIRV.MatOp SPIRV.Transp) transpose  ) m
  inverse     m = fromAST (PrimOp (SPIRV.Matrix (scalar @a)) (SPIRV.MatOp SPIRV.Inv   ) inverse    ) m
  determinant m = fromAST (PrimOp (SPIRV.Matrix (scalar @a)) (SPIRV.MatOp SPIRV.Det   ) determinant) m

  m !+! n = error "todo" --ASTMat ( fromAST (PrimOp "!+!" (!+!)) m n )
  m !-! n = error "todo" --ASTMat ( fromAST (PrimOp "!-!" (!-!)) m n )
  m !*! n = fromAST (PrimOp (SPIRV.Matrix (scalar @a)) (SPIRV.MatOp SPIRV.MMulM) (!*!)) m n
  v ^*! n = fromAST (PrimOp (SPIRV.Matrix (scalar @a)) (SPIRV.MatOp SPIRV.VMulM) (^*!)) v n
  m !*^ w = fromAST (PrimOp (SPIRV.Matrix (scalar @a)) (SPIRV.MatOp SPIRV.MMulV) (!*^)) m w
  m !*  k = fromAST (PrimOp (SPIRV.Matrix (scalar @a)) (SPIRV.MatOp SPIRV.MMulK) (!*) ) m k

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

instance (Syntactic a, Syntactic b) => Syntactic (a -> b) where
  type Internal (a -> b) = Internal a -> Internal b
  toAST   f = Lam ( toAST . f . fromAST )
  fromAST f = \a -> fromAST ( f :$ toAST a )

instance Syntactic a => Syntactic ( (a := j) i ) where
  type Internal ( (a := j) i ) = (Internal a := j) i
  toAST :: (a := j) i -> AST ( (Internal a := j) i)
  toAST (WithIx a) = Ix :$ toAST a
  fromAST :: AST ( (Internal a := j) i) -> (a := j) i
  fromAST (Ix :$ a) = WithIx (fromAST a)
  fromAST _ = error "help needed"

instance (Syntactic a) => Syntactic (Codensity AST (m :: (k -> Type) -> (k -> Type)) (a := j) i) where
  type Internal (Codensity AST m (a := j) i) = m (Internal a := j) i
  toAST :: Codensity AST m (a := j) i -> AST (m (Internal a := j) i)
  toAST (Codensity k) = k ( fromAST Pure )
  fromAST :: AST (m (Internal a := j) i) -> Codensity AST m (a := j) i
  fromAST a = Codensity ( \k -> fromAST Bind a (k . WithIx) )



-- utility type for the following instance declaration
newtype B n a b i = B { unB :: AST (AST.Variadic (n-i) a b) }

instance KnownNat n => Syntactic (V n (AST a)) where
  type Internal (V n (AST a)) = V n a

  toAST :: V n (AST a) -> AST (V n a)
  toAST v = res'
    where f :: forall i. (KnownNat i, (n-(i+1)) ~ ((n-i)-1), (1 <=? (n-i)) ~ 'True)
            => AST a
            -> B n a (V n a) i
            -> B n a (V n a) (i+1)
          f a (B b) = B ( b :$ a )
          a0 :: B n a (V n a) 0
          a0 = B ( MkVector @n @a Proxy )
          res :: B n a (V n a) n
          res = dfoldrV f a0 v
          res' :: ((n-n) ~ 0) => AST (AST.Variadic 0 a (V n a))
          res' = unB res

  fromAST :: AST (V n a) -> V n (AST a)
  fromAST = buildV (\i v -> VectorAt i :$ v)

instance (KnownNat n, KnownNat m) => Syntactic (M n m (AST a)) where
  type Internal (M n m (AST a)) = M n m a

  toAST :: M n m (AST a) -> AST (M n m a)
  toAST = error "todo todo todo"

  fromAST :: AST (M n m a) -> M n m (AST a)
  fromAST =  buildV ( \i -> buildV ( \j y -> MatrixAt i j :$ y ) )


pattern Vec2 :: AST a -> AST a -> AST ( V 2 a )
pattern Vec2 x y <- (fromAST -> V2 x y)

pattern Vec3 :: AST a -> AST a -> AST a -> AST ( V 3 a )
pattern Vec3 x y z <- (fromAST -> V3 x y z)

pattern Vec4 :: AST a -> AST a -> AST a -> AST a -> AST ( V 4 a )
pattern Vec4 x y z w <- (fromAST -> V4 x y z w)

vec2 :: AST a -> AST a -> AST ( V 2 a )
vec2 x y = fromAST (MkVector (Proxy @2) ) x y

vec3 :: AST a -> AST a -> AST a -> AST ( V 3 a )
vec3 x y z = fromAST (MkVector (Proxy @3) ) x y z

vec4 :: AST a -> AST a -> AST a -> AST a -> AST ( V 4 a )
vec4 x y z w = fromAST (MkVector (Proxy @4) ) x y z w

pattern Mat22
  :: AST a -> AST a
  -> AST a -> AST a 
  -> AST ( M 2 2 a )
pattern Mat22 a11 a12 
              a21 a22 
  <- ( fromAST 
       -> V2 (V2 a11 a12)
             (V2 a21 a22)
     )

pattern Mat23
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 2 3 a )
pattern Mat23 a11 a12 a13
              a21 a22 a23
   <- ( fromAST 
        -> V2 (V3 a11 a12 a13)
              (V3 a21 a22 a23)
      )

pattern Mat24
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 2 4 a )
pattern Mat24 a11 a12 a13 a14
              a21 a22 a23 a24
   <- ( fromAST 
        -> V2 (V4 a11 a12 a13 a14)
              (V4 a21 a22 a23 a24)
      )

pattern Mat32
  :: AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 3 2 a )
pattern Mat32 a11 a12
              a21 a22
              a31 a32
   <- ( fromAST 
        -> V3 (V2 a11 a12)
              (V2 a21 a22)
              (V2 a31 a32)
      )

pattern Mat33
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 3 3 a )
pattern Mat33 a11 a12 a13
              a21 a22 a23
              a31 a32 a33
   <- ( fromAST 
        -> V3 (V3 a11 a12 a13)
              (V3 a21 a22 a23)
              (V3 a31 a32 a33)
      )

pattern Mat34
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 3 4 a )
pattern Mat34 a11 a12 a13 a14
              a21 a22 a23 a24
              a31 a32 a33 a34
   <- ( fromAST
        -> V3 (V4 a11 a12 a13 a14)
              (V4 a21 a22 a23 a24)
              (V4 a31 a32 a33 a34)
      )

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
        -> V4 (V2 a11 a12)
              (V2 a21 a22)
              (V2 a31 a32)
              (V2 a41 a42)
      )

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
        -> V4 (V3 a11 a12 a13)
              (V3 a21 a22 a23)
              (V3 a31 a32 a33)
              (V3 a41 a42 a43)
      )

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
        -> V4 (V4 a11 a12 a13 a14)
              (V4 a21 a22 a23 a24)
              (V4 a31 a32 a33 a34)
              (V4 a41 a42 a43 a44)
      )

mat22 
  :: AST a -> AST a
  -> AST a -> AST a 
  -> AST ( M 2 2 a )
mat22 a11 a12
      a21 a22
  = fromAST ( MkMatrix (Proxy @2) (Proxy @2) ) 
      a11 a12
      a21 a22

mat23
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 2 3 a )
mat23 a11 a12 a13
      a21 a22 a23
  = fromAST (MkMatrix (Proxy @2) (Proxy @3) ) 
      a11 a12 a13
      a21 a22 a23

mat24
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 2 4 a )
mat24 a11 a12 a13 a14
      a21 a22 a23 a24
  = fromAST (MkMatrix (Proxy @2) (Proxy @4) ) 
      a11 a12 a13 a14
      a21 a22 a23 a24

mat32
  :: AST a -> AST a
  -> AST a -> AST a
  -> AST a -> AST a
  -> AST ( M 3 2 a )
mat32 a11 a12
      a21 a22
      a31 a32
  = fromAST (MkMatrix (Proxy @3) (Proxy @2) ) 
      a11 a12
      a21 a22
      a31 a32

mat33
  :: AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a
  -> AST ( M 3 3 a )
mat33 a11 a12 a13
      a21 a22 a23
      a31 a32 a33
  = fromAST (MkMatrix (Proxy @3) (Proxy @3) ) 
      a11 a12 a13
      a21 a22 a23
      a31 a32 a33

mat34
  :: AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST a -> AST a -> AST a -> AST a
  -> AST ( M 3 4 a )
mat34 a11 a12 a13 a14
      a21 a22 a23 a24
      a31 a32 a33 a34
  = fromAST (MkMatrix (Proxy @3) (Proxy @4) ) 
      a11 a12 a13 a14
      a21 a22 a23 a24
      a31 a32 a33 a34

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
  = fromAST (MkMatrix (Proxy @4) (Proxy @2) ) 
      a11 a12
      a21 a22
      a31 a32
      a41 a42

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
  = fromAST (MkMatrix (Proxy @4) (Proxy @3) ) 
      a11 a12 a13
      a21 a22 a23
      a31 a32 a33
      a41 a42 a43

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
  = fromAST (MkMatrix (Proxy @4) (Proxy @4) ) 
      a11 a12 a13 a14 
      a21 a22 a23 a24
      a31 a32 a33 a34
      a41 a42 a43 a44