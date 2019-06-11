{-# OPTIONS_HADDOCK ignore-exports #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-} -- helping SCC computations for type families
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

  -- helper typeclass to avoid conversions when 'id' suffices
  , WhichConversion(conversion)

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
import Data.Int
  ( Int32 )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, KnownNat
  , type (+), type (-)
  , type (<=), CmpNat
  )

-- fir
import Control.Type.Optic
  ( Optic(..), Index
  , Product, ProductIfDisjoint
  , Gettable, ReifiedGetter(view)
  , Settable, ReifiedSetter(set)
  , Contained(..), ContainerKind, DegreeKind, LabelKind
  , MonoContained(..)
  )
import Data.Function.Variadic
  ( NatVariadic, ListVariadic )
import Data.Type.List
  ( KnownLength(sLength)
  , type (:++:), Zip, Postpend
  )
import Data.Type.Map
  ( (:->)((:->)), Key )
import FIR.AST
  ( AST(..)
  , Syntactic(Internal, toAST, fromAST)
  , primOp
  )
import FIR.Instances.Optics
  ( KnownOptic(opticSing)
  , (%:*:), (%:.:)
  , ValidAnIndexOptic
  , StructElemFromIndex
  )
import FIR.Prim.Array
  ( Array, RuntimeArray )
import FIR.Prim.Op
  ( Vectorise(Vectorise) )
import FIR.Prim.Singletons
  ( PrimTy
  , ScalarTy, IntegralTy
  , SPrimFunc(..), PrimFunc(..)
  , KnownArity
  )
import FIR.Prim.Struct
  ( Struct )
import Math.Algebra.Class
  ( AdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), Ring
  , DivisionRing(..)
  , Signed(..), Archimedean(..)
  , Floating(..), RealFloat(..)
  , Integral, Unsigned
  , Convert(..), Rounding(..)
  )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GeneratedGradedSemigroup(..)
  )
import Math.Linear
  ( Semimodule(..), Module(..)
  , Inner(..), Cross(..)
  , Matrix(..), VectorOf
  , V, M(..)
  , dfoldrV, buildV
  , pattern V2, pattern V3, pattern V4
  )
import Math.Logic.Bits
  ( Bits(..), BitShift(..) )
import Math.Logic.Class
  ( Eq(..), Boolean(..)
  , Choose(..)
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

instance ( PrimTy a
         , x ~ AST a
         , y ~ AST a
         )
       => Choose (AST Bool) '(x, y, AST a) where
  choose = fromAST If

instance ( PrimTy a, Eq a, Logic a ~ Bool )
  => Eq (AST a) where
  type Logic (AST a) = AST Bool
  (==) = primOp @a @SPIRV.Equal
  (/=) = primOp @a @SPIRV.NotEqual

instance ( ScalarTy a, Ord a, Logic a ~ Bool ) 
  => Ord (AST a) where
  type Ordering (AST a) = AST Int32
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
-- 'Floating', 'RealFloat' (note: not the "Prelude" type classes)
--
-- 'Integral', 'Unsigned'.

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

instance (ScalarTy a, Integral a) => Integral (AST a) where
instance (ScalarTy a, Unsigned a) => Unsigned (AST a) where

-- * Numeric conversions
--
-- $conversions
-- Instance for 'Convert', 'Rounding'.

-- Helper type class to choose which conversion function to use,
-- depending on whether @a ~ b@.
class WhichConversion a b (useIdentity :: Bool) where
  conversion :: AST a -> AST b

instance (ScalarTy a, ScalarTy b, Convert '(a,b)) => WhichConversion a b 'False where
  conversion = primOp @'(a,b) @SPIRV.Convert
instance ScalarTy a => WhichConversion a a 'True where
  conversion = id

instance ( ScalarTy a, ScalarTy b, WhichConversion a b (a == b) )
       => Convert '(AST a, AST b) where
  convert = conversion @a @b @(a==b)

instance {-# OVERLAPPING #-}
         ( ScalarTy a, Rounding '(a,a) )
      => Rounding '(AST a, AST a) where
  truncate = primOp @'(a,a) @SPIRV.CTruncate
  round    = primOp @'(a,a) @SPIRV.CRound
  floor    = primOp @'(a,a) @SPIRV.CFloor
  ceiling  = primOp @'(a,a) @SPIRV.CCeiling

instance ( ScalarTy a, ScalarTy b
         , Floating a, Integral b
         , Rounding '(a,a), Rounding '(a,b)
         )
       => Rounding '(AST a, AST b) where
  truncate = primOp @'(a,b) @SPIRV.CTruncate
  round    = primOp @'(a,b) @SPIRV.CTruncate @(AST a -> AST b)
           . primOp @'(a,a) @SPIRV.CRound
  floor    = primOp @'(a,b) @SPIRV.CTruncate @(AST a -> AST b)
           . primOp @'(a,a) @SPIRV.CFloor
  ceiling  = primOp @'(a,b) @SPIRV.CTruncate @(AST a -> AST b)
           . primOp @'(a,a) @SPIRV.CCeiling


-----------------------------------------------
-- * Optics

class KnownOptic optic => KnownASTOptic astoptic optic | astoptic -> optic where

-- ** Simple instances

-- *** Name

instance ( empty ~ '[]
         , KnownSymbol k
         , r ~ AST a
         , KnownOptic (Field_ k :: Optic empty s a)
         )
       => KnownASTOptic (Field_ k :: Optic empty (AST s) r) (Field_ k :: Optic '[] s a)
       where
instance ( KnownSymbol k
         , empty ~ '[]
         , Gettable (Field_ k :: Optic '[] s a)
         , r ~ AST a
         )
      => Gettable (Field_ k :: Optic empty (AST s) r)
      where
instance ( KnownSymbol k
         , empty ~ '[]
         , Gettable (Field_ k :: Optic '[] s a)
         , ReifiedGetter (Field_ k :: Optic '[] s a)
         , ListVariadic '[] a ~ a
         , KnownOptic (Field_ k :: Optic '[] s a)
         , r ~ AST a
         )
      => ReifiedGetter (Field_ k :: Optic empty (AST s) r)
      where
  view = fromAST
       $ View
            sLength
            ( opticSing @(Field_ k :: Optic '[] s a) )
instance ( KnownSymbol k
         , empty ~ '[]
         , Settable (Field_ k :: Optic '[] s a)
         , r ~ AST a
         )
      => Settable (Field_ k :: Optic empty (AST s) r)
      where
instance ( KnownSymbol k
         , empty ~ '[]
         , Settable (Field_ k :: Optic '[] s a)
         , ReifiedSetter (Field_ k :: Optic '[] s a)
         , ListVariadic '[] s ~ s
         , KnownOptic (Field_ k :: Optic '[] s a)
         , r ~ AST a
         )
      => ReifiedSetter (Field_ k :: Optic empty (AST s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(Field_ k :: Optic '[] s a) )

-- *** Run-time index

instance ( Integral ty, IntegralTy ty
         , ix' ~ '[ AST ty ]
         , r ~ AST a
         , ValidAnIndexOptic '[ty] s a
         , PrimTy s, PrimTy a
         )
       => KnownASTOptic (RTOptic_ :: Optic ix' (AST s) r) (RTOptic_ :: Optic '[ty] s a)
       where
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

-- *** Compile-time index

instance ( empty ~ '[]
         , KnownNat i
         , r ~ AST a
         , KnownOptic (Field_ i :: Optic empty s a)
         )
       => KnownASTOptic (Field_ i :: Optic empty (AST s) r) (Field_ i :: Optic '[] s a)
       where
instance ( KnownNat i
         , empty ~ '[]
         , Gettable (Field_ i :: Optic '[] s a)
         , r ~ AST a
         )
      => Gettable (Field_ i :: Optic empty (AST s) r)
      where
instance ( KnownNat i
         , empty ~ '[]
         , Gettable (Field_ i :: Optic '[] s a)
         , ReifiedGetter (Field_ i :: Optic '[] s a)
         , ListVariadic '[] a ~ a
         , KnownOptic (Field_ i :: Optic '[] s a)
         , r ~ AST a
         )
      => ReifiedGetter (Field_ i :: Optic empty (AST s) r)
      where
  view = fromAST
       $ View
            sLength
            ( opticSing @(Field_ i :: Optic '[] s a) )
instance ( KnownNat i
         , empty ~ '[]
         , Settable (Field_ i :: Optic '[] s a)
         , r ~ AST a
         )
      => Settable (Field_ i :: Optic empty (AST s) r)
      where
instance ( KnownNat i
         , empty ~ '[]
         , Settable (Field_ i :: Optic '[] s a)
         , ReifiedSetter (Field_ i :: Optic '[] s a)
         , ListVariadic '[] s ~ s
         , KnownOptic (Field_ i :: Optic '[] s a)
         , r ~ AST a
         )
      => ReifiedSetter (Field_ i :: Optic empty (AST s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(Field_ i :: Optic '[] s a) )


-- ** Composite instances

-- *** Containers
--
-- $containers
-- Container instances, for product optics

type instance ContainerKind (AST (V n a)) = Type
type instance DegreeKind    (AST (V n a)) = Nat
type instance LabelKind     (AST (V n a)) = ()

type instance ContainerKind (AST (M m n a)) = Type
type instance DegreeKind    (AST (M m n a)) = Nat
type instance LabelKind     (AST (M m n a)) = ()

type instance ContainerKind (AST (Struct as)) = Type
type instance DegreeKind    (AST (Struct as)) = [Symbol :-> Type]
type instance LabelKind     (AST (Struct as)) = (Symbol :-> Type)

type instance ContainerKind (AST (Array n a)) = Type
type instance DegreeKind    (AST (Array n a)) = Nat
type instance LabelKind     (AST (Array n a)) = ()

$(Prelude.pure [])

instance Contained (AST (V n a)) where
  type Container  (AST (V n a))   = AST (V 0 a)
  type DegreeOf   (AST (V n a))   = n
  type LabelOf    (AST (V n a)) _ = '()
  type Overlapping  (AST (V n a)) k _
    = TypeError (    Text "optic: attempt to index a vector component with name " :<>: ShowType k
                :$$: Text "Maybe you intended to use a swizzle?"
                )

instance PrimTy a => GradedSemigroup (AST (V 0 a)) Nat where
  type Grade Nat (AST (V 0 a)) i = AST (V i a)
  type i :<!>: j = i + j
  (<!>) :: AST (V i a) -> AST (V j a) -> AST (V (i+j) a)
  (<!>) = fromAST GradedMappend

instance PrimTy a => GeneratedGradedSemigroup (AST (V 0 a)) Nat () where
  type GenType (AST (V 0 a)) () _
    = TypeError ( Text "Cannot construct AST for internal length 1 vector." )
  type GenDeg Nat (AST (V 0 a)) () '() = 1
  generator = error "unreachable"

instance KnownNat m => Contained (AST (M m n a)) where
  type Container (AST (M m n a))   = AST (M m 0 a)
  type DegreeOf  (AST (M m n a))   = n
  type LabelOf   (AST (M m n a)) _ = '()
  type Overlapping (AST (M m n a)) k _
    = TypeError ( Text "optic: attempt to index a matrix component with name " :<>: ShowType k )

instance KnownNat m => GradedSemigroup (AST (M m 0 a)) Nat where
  type Grade Nat (AST (M m 0 a)) i = AST (M m i a)
  type i :<!>: j = i + j
  (<!>) :: AST (M m i a) -> AST (M m j a) -> AST (M m (i+j) a)
  (<!>) = fromAST GradedMappend

instance KnownNat m => GeneratedGradedSemigroup (AST (M m 0 a)) Nat () where
  type GenType (AST (M m 0 a)) () _
    = TypeError ( Text "Cannot construct AST for internal matrix with a single column." )
  type GenDeg Nat (AST (M m 0 a)) () '() = 1
  generator = error "unreachable"

instance Contained (AST (Struct (as :: [Symbol :-> Type]))) where
  type Container (AST (Struct (as :: [Symbol :-> Type]))) = (AST (Struct ('[] :: [Symbol :-> Type])))
  type DegreeOf  (AST (Struct (as :: [Symbol :-> Type]))) = as
  type LabelOf   (AST (Struct (as :: [Symbol :-> Type]))) (Field_ (k :: Symbol) :: Optic _ (AST (Struct as)) (AST a))
    = k ':-> a
  type LabelOf   (AST (Struct (as :: [Symbol :-> Type]))) (Field_ (i :: Nat)    :: Optic _ (AST (Struct as)) (AST a))
    = Key ( StructElemFromIndex
              (Text "key: ")
              i as i as
          )
      ':-> a
  type Overlapping (AST (Struct (as :: [Symbol :-> Type]))) k i
    = k == Key (StructElemFromIndex (Text "key: ") i as i as)

instance GradedSemigroup (AST (Struct ('[] :: [Symbol :-> Type]))) [Symbol :-> Type] where
  type Grade [Symbol :-> Type] (AST (Struct ('[] :: [Symbol :-> Type]))) as = AST (Struct as)
  type as :<!>: bs = as :++: bs
  (<!>) :: AST (Struct as) -> AST (Struct bs) -> AST (Struct (as :++: bs))
  (<!>) = fromAST GradedMappend

instance GeneratedGradedSemigroup
            (AST (Struct ('[] :: [Symbol :-> Type])))
            [Symbol :-> Type]
            (Symbol :-> Type)
            where
  type GenType (AST (Struct ('[] :: [Symbol :-> Type]))) (Symbol :-> Type) kv
    = TypeError ( Text "Internal error: superfluous construction of size 1 struct." )
  type GenDeg [Symbol :-> Type] (AST (Struct ('[] :: [Symbol :-> Type]))) (Symbol :-> Type) kv = '[ kv ]
  generator = error "unreachable"

instance Contained (AST (Array n a)) where
  type Container (AST (Array n a))   = AST (Array 0 a)
  type DegreeOf  (AST (Array n a))   = n
  type LabelOf   (AST (Array n a)) _ = '()
  type Overlapping (AST (Array n a)) k _
        = TypeError ( Text "optic: attempt to index an array using name " :<>: ShowType k )

instance GradedSemigroup (AST (Array 0 a)) Nat where
  type Grade Nat (AST (Array 0 a)) l = AST (Array l a)
  type l1 :<!>: l2 = l1 + l2
  (<!>) :: forall l1 l2. AST (Array l1 a) -> AST (Array l2 a) -> AST (Array (l1+l2) a)
  (<!>) = fromAST GradedMappend

instance GeneratedGradedSemigroup (AST (Array 0 a)) Nat () where
  type GenType (AST (Array 0 a)) () _
    = TypeError ( Text "Internal error: superfluous construction of size 1 array." )
  type GenDeg Nat (AST (Array 0 a)) () '() = 1
  generator = error "unreachable"

instance
  ( TypeError ( Text "Cannot recombine runtime arrays.") )
  => Contained (AST (RuntimeArray a)) where
    type Container (AST (RuntimeArray a)) = TypeError ( Text "Cannot recombine runtime arrays.")
    type DegreeOf  (AST (RuntimeArray a)) = TypeError ( Text "Cannot recombine runtime arrays.")
    type LabelOf (AST (RuntimeArray a)) _ = TypeError ( Text "Cannot recombine runtime arrays.")
    type Overlapping (AST (RuntimeArray a)) _ _ = TypeError ( Text "Cannot recombine runtime arrays.")


-- *** Equalisers
--
-- $equalisers
-- Monomorphic containers, for equalisers.

instance (PrimTy a, KnownNat n) => MonoContained (AST (V n a)) where
  type MonoType (AST (V n a)) = AST a
  setAll a _ = pureAST a
instance (PrimTy a, KnownNat n, KnownNat m) => MonoContained (AST (M m n a)) where
  type MonoType (AST (M m n a)) = AST a
  setAll a _ = pureAST a
instance MonoContained (Struct as) => MonoContained (AST (Struct (as :: [Symbol :-> Type]))) where
  type MonoType (AST (Struct (as :: [Symbol :-> Type]))) = AST (MonoType (Struct as))
  setAll = error "TODO: structure 'setAll'"

-- *** Compound instance resolution

-- **** Composites

instance {-# OVERLAPPING #-}
         forall is js ks s x y
                (o1 :: Optic is (AST s) x) (o2 :: Optic js x y)
                is' js' ks' a b
                (o1' :: Optic is' s a) (o2' :: Optic js' a b)
                .
         ( KnownASTOptic o1 o1'
         , KnownASTOptic o2 o2'
         , ks  ~ ( is  :++: js  )
         , ks' ~ ( is' :++: js' )
         , KnownLength ks'
         , x ~ AST a
         , y ~ AST b
         )
         => KnownASTOptic
             ( (o1  `ComposeO` o2 ) :: Optic ks (AST s) y )
             ( (o1' `ComposeO` o2') :: Optic ks'     s  b )
         where
instance {-# OVERLAPPING #-}
         forall is js ks s x y
                (o1 :: Optic is (AST s) x) (o2 :: Optic js x y)
                is' js' ks' a b
                (o1' :: Optic is' s a) (o2' :: Optic js' a b)
                .
         ( Gettable o1
         , Gettable o2
         , Gettable o1'
         , Gettable o2'
         , KnownASTOptic o1 o1'
         , KnownASTOptic o2 o2'
         , ks  ~ ( is  :++: js  )
         , ks' ~ ( is' :++: js' )
         , KnownLength ks'
         , x ~ AST a
         , y ~ AST b
         , Syntactic (ListVariadic (ks `Postpend` AST s) y)
         , Internal (ListVariadic (ks `Postpend` AST s) y)
            ~ (ListVariadic (ks' `Postpend` s) b)
         )
         => ReifiedGetter ( (o1 `ComposeO` o2) :: Optic ks (AST s) y )
         where
  view = fromAST
       $ View sLength ( opticSing @o1' %:.: opticSing @o2' )
instance {-# OVERLAPPING #-}
         forall is js ks s x y
                (o1 :: Optic is (AST s) x) (o2 :: Optic js x y)
                is' js' ks' a b
                (o1' :: Optic is' s a) (o2' :: Optic js' a b)
                .
         ( Settable o1
         , Settable o2
         , Settable o1'
         , Settable o2'
         , KnownASTOptic o1 o1'
         , KnownASTOptic o2 o2'
         , ks  ~ ( is  :++: js  )
         , ks' ~ ( is' :++: js' )
         , KnownLength ks'
         , x ~ AST a
         , y ~ AST b
         , Syntactic (ListVariadic (ks `Postpend` y `Postpend` AST s) (AST s))
         , Internal (ListVariadic (ks `Postpend` y `Postpend` AST s) (AST s))
            ~ (ListVariadic (ks' `Postpend` b `Postpend` s) s)
         )
         => ReifiedSetter ( (o1 `ComposeO` o2) :: Optic ks (AST s) y )
         where
  set = fromAST
      $ Set sLength ( opticSing @o1' %:.: opticSing @o2' )

-- **** Products

instance {-# OVERLAPPING #-}
         forall is js ks s x y z
                (o1 :: Optic is (AST s) x) (o2 :: Optic js (AST s) y)
                is' js' ks' a b c
                (o1' :: Optic is' s a) (o2' :: Optic js' s b)
                .
         ( KnownASTOptic o1 o1'
         , KnownASTOptic o2 o2'
         , ks  ~ Zip is  js
         , ks' ~ Zip is' js'
         , KnownLength ks'
         , x ~ AST a
         , y ~ AST b
         , z ~ Product o1  o2
         , c ~ Product o1' o2'
         , PrimTy c
         )
         => KnownASTOptic
              ( (o1  `ProductO` o2 ) :: Optic ks (AST s) z )
              ( (o1' `ProductO` o2') :: Optic ks'     s  c )
         where
instance {-# OVERLAPPING #-}
         forall is js ks s x y z
                (o1 :: Optic is (AST s) x) (o2 :: Optic js (AST s) y)
                is' js' ks' a b c
                (o1' :: Optic is' s a) (o2' :: Optic js' s b)
                .
         ( Gettable o1
         , Gettable o2
         , Gettable o1'
         , Gettable o2'
         , KnownASTOptic o1 o1'
         , KnownASTOptic o2 o2'
         , ks  ~ Zip is  js
         , ks' ~ Zip is' js'
         , KnownLength ks'
         , x ~ AST a
         , y ~ AST b
         , z ~ Product o1  o2
         , c ~ Product o1' o2'
         , PrimTy c
         , Syntactic (ListVariadic (ks `Postpend` AST s) z)
         , Internal (ListVariadic (ks `Postpend` AST s) z)
            ~ (ListVariadic (ks' `Postpend` s) c)
         )
         => ReifiedGetter ( (o1 `ProductO` o2) :: Optic ks (AST s) z )
         where
  view = fromAST
       $ View sLength ( opticSing @o1' %:*: opticSing @o2' )
instance {-# OVERLAPPING #-}
         forall is js ks s x y z
                (o1 :: Optic is (AST s) x) (o2 :: Optic js (AST s) y)
                is' js' ks' a b c
                (o1' :: Optic is' s a) (o2' :: Optic js' s b)
                .
         ( Settable o1
         , Settable o2
         , Settable o1'
         , Settable o2'
         , KnownASTOptic o1 o1'
         , KnownASTOptic o2 o2'
         , ks  ~ Zip is  js
         , ks' ~ Zip is' js'
         , KnownLength ks'
         , x ~ AST a
         , y ~ AST b
         , z ~ Product o1  o2
         , c ~ Product o1' o2'
         , z ~ ProductIfDisjoint o1  o2
         , c ~ ProductIfDisjoint o1' o2'
         , PrimTy c
         , Syntactic (ListVariadic (ks `Postpend` z `Postpend` AST s) (AST s))
         , Internal (ListVariadic (ks `Postpend` z `Postpend` AST s) (AST s))
            ~ (ListVariadic (ks' `Postpend` c `Postpend` s) s)
         )
         => ReifiedSetter ( (o1 `ProductO` o2) :: Optic ks (AST s) z )
         where
  set = fromAST
      $ Set sLength ( opticSing @o1' %:*: opticSing @o2' )

-----------------------------------------------
-- * Functor functionality

infixl 4 <$$>
infixl 4 <**>

class ASTFunctor f where
  fmapAST :: ( Syntactic x
             , Internal x ~ (a -> b)
             , Syntactic r
             , Internal r ~ ( f a -> f b )
             , PrimTy a
             , KnownArity b
             )
          => x -> r

class ASTApplicative f where
  pureAST :: KnownArity a
          => AST a
          -> AST (f a)
  (<**>)  :: ( Syntactic r
             , Internal r ~ ( f a -> f b )
             , PrimTy a
             , KnownArity b
             )
           => AST ( f (a -> b) ) -> r

(<$$>) :: ( ASTFunctor f
          , Syntactic x
          , Internal x ~ (a -> b)
          , Syntactic r
          , Internal r ~ ( f a -> f b )
          , PrimTy a
          , KnownArity b
          )
        => x -> r
(<$$>) = fmapAST

instance KnownNat n => ASTFunctor (V n) where
  fmapAST = fromAST $ Fmap @(V n)

instance (KnownNat m, KnownNat n) => ASTFunctor (M m n) where
  fmapAST = fromAST $ Fmap @(M m n)

instance KnownNat n => ASTFunctor (Array n) where
  fmapAST = fromAST $ Fmap @(Array n)

instance KnownNat n => ASTApplicative (V n) where
  pureAST = fromAST $ Pure @(V n)
  (<**>)  = fromAST $ Ap   @(V n)

instance (KnownNat m, KnownNat n) => ASTApplicative (M m n) where
  pureAST = fromAST $ Pure @(M m n)
  (<**>)  = fromAST $ Ap   @(M m n)

instance KnownNat n => ASTApplicative (Array n) where
  pureAST = fromAST $ Pure @(Array n)
  (<**>)  = fromAST $ Ap   @(Array n)

instance 
  TypeError (     Text "The AST datatype does not have a Functor instance:"
             :$$: Text "    cannot map Haskell functions over internal types."
             :$$: Text "To map an internal function over an internal type, use 'fmapAST'/'<$$>'."
            ) => Prelude.Functor AST where
  fmap = error "unreachable"

-----------------------------------------------
-- * Internal functors
--
-- $internalfunctors
-- Instances for vectors, matrices and arrays.

instance KnownNat n => PrimFunc (V n) where
  primFuncSing = SFuncVector @n
  distributeAST = fromAST
instance ( KnownNat m, KnownNat n ) => PrimFunc (M m n) where
  primFuncSing = SFuncMatrix @m @n
  distributeAST = error "distributeAST: todo for matrices"
instance KnownNat n => PrimFunc (Array n) where
  primFuncSing = SFuncArray @n
  distributeAST = error "distributeAST: no distribute for arrays"

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
instance (ScalarTy a, Semiring a) => Semimodule Nat (AST (V 0 a)) where
  type Scalar   (AST (V 0 a))       = AST      a
  type OfDim    (AST (V 0 a)) Nat n = AST (V n a)
  type ValidDim (AST (V 0 a)) Nat n = KnownNat n

  (^+^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST (V n a)
  (^+^) = primOp @(V n a) @('Vectorise SPIRV.Add)

  (^*) :: forall n. KnownNat n
       => AST (V n a) -> AST a -> AST (V n a)
  (^*)  = primOp @(V n a) @SPIRV.VMulK

instance (ScalarTy a, Ring a) => Module Nat (AST (V 0 a)) where
  (^-^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST (V n a)
  (^-^) = primOp @(V n a) @('Vectorise SPIRV.Sub)

  (-^) :: forall n. KnownNat n => AST (V n a) -> AST (V n a)
  (-^) = primOp @(V n a) @('Vectorise SPIRV.Neg)

instance (ScalarTy a, Floating a) => Inner Nat (AST (V 0 a)) where
  (^.^) :: forall n. KnownNat n
        => AST (V n a) -> AST (V n a) -> AST a
  (^.^) = primOp @(V n a) @SPIRV.DotV

  normalise :: forall n. KnownNat n => AST (V n a) -> AST (V n a)
  normalise = primOp @(V n a) @SPIRV.NormaliseV

instance (ScalarTy a, Floating a) => Cross Nat (AST (V 0 a)) where
  type CrossDim (AST (V 0 a)) Nat n = ( n ~ 3 )

  cross :: AST (V 3 a) -> AST (V 3 a) -> AST (V 3 a)
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

type instance VectorOf (AST (M 0 0 a)) = AST (V 0 a)

instance (ScalarTy a, Floating a) => Matrix Nat (AST (M 0 0 a)) where
  type OfDims (AST (M 0 0 a)) Nat '(m,n) = AST (M m n a)

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
