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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-|
Module: FIR.Syntax.AST

This module, together with "FIR.Syntax.Codensity",
provides most of the user-facing syntax for constructing
and manipulating values in the EDSL.

This is done through type class overloading, here in the form of
orphan instances for types of the form @AST a@
(representing pure values in the EDSL).

-}

module FIR.Syntax.AST
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
  , Integral(..), floor
  , Fractional(..), fromRational
  , Floating(..)
  , RealFloat(..)
  , Functor(..), (<$>)
  , Applicative(..)
  )
import qualified Prelude
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( type (==) )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, KnownNat
  , type (+), type (-)
  , type (<=), CmpNat
  , sameNat
  )

-- half
import Numeric.Half
  ( Half )

-- fir
import Control.Type.Optic
  ( Optic(..), Index
  , Gettable, ReifiedGetter(view)
  , Settable, ReifiedSetter(set)
  , ProductComponents(ProductO, EndProd_)
  , ComponentsGettable
  , ComponentsSettable
  , ArePairwiseDisjoint
  , Container(Overlapping)
  , MonoContainer(MonoType, setAll)
  )
import Data.Function.Variadic
  ( NatVariadic, ListVariadic )
import Data.Product
  ( IsProduct
  , AreProducts
  , Distribute
  , MapHList
  )
import Data.Type.List
  ( KnownLength(sLength)
  , SameLength
  , type (:++:), Postpend
  )
import Data.Type.Map
  ( (:->), Key )
import FIR.AST
  ( AST(..)
  , Syntactic(Internal, toAST, fromAST)
  , primOp
  )
import FIR.Syntax.Optics
  ( KnownOptic(opticSing)
  , KnownComponents
  , (%:.:)
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
  , ScalarFromTy, TypeFromScalar
  , SPrimFunc(..), PrimFunc(..), DistDict(DistDict)
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
import Math.Linear
  ( Semimodule(..), LinearModule(..)
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
import qualified SPIRV.ScalarTy as SPIRV
import qualified SPIRV.PrimOp   as SPIRV

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

instance Archimedean (AST Word8 ) where
  mod    = primOp @Word8  @SPIRV.Mod
  rem    = primOp @Word8  @SPIRV.Rem
  div    = primOp @Word8  @SPIRV.Quot
instance Archimedean (AST Word16) where
  mod    = primOp @Word16 @SPIRV.Mod
  rem    = primOp @Word16 @SPIRV.Rem
  div    = primOp @Word16 @SPIRV.Quot
instance Archimedean (AST Word32) where
  mod    = primOp @Word32 @SPIRV.Mod
  rem    = primOp @Word32 @SPIRV.Rem
  div    = primOp @Word32 @SPIRV.Quot
instance Archimedean (AST Word64) where
  mod    = primOp @Word64 @SPIRV.Mod
  rem    = primOp @Word64 @SPIRV.Rem
  div    = primOp @Word64 @SPIRV.Quot
instance Archimedean (AST Int8  ) where
  mod    = primOp @Int8   @SPIRV.Mod
  rem    = primOp @Int8   @SPIRV.Rem
  div    = primOp @Int8   @SPIRV.Quot
instance Archimedean (AST Int16 ) where
  mod    = primOp @Int16  @SPIRV.Mod
  rem    = primOp @Int16  @SPIRV.Rem
  div    = primOp @Int16  @SPIRV.Quot
instance Archimedean (AST Int32 ) where
  mod    = primOp @Int32  @SPIRV.Mod
  rem    = primOp @Int32  @SPIRV.Rem
  div    = primOp @Int32  @SPIRV.Quot
instance Archimedean (AST Int64 ) where
  mod    = primOp @Int64  @SPIRV.Mod
  rem    = primOp @Int64  @SPIRV.Rem
  div    = primOp @Int64  @SPIRV.Quot
instance Archimedean (AST Half  ) where
  mod    = primOp @Half   @SPIRV.Mod
  rem    = primOp @Half   @SPIRV.Rem
  div a b = floor (a / b)
instance Archimedean (AST Float ) where
  mod    = primOp @Float  @SPIRV.Mod
  rem    = primOp @Float  @SPIRV.Rem
  div a b = floor (a / b)
instance Archimedean (AST Double) where
  mod    = primOp @Double @SPIRV.Mod
  rem    = primOp @Double @SPIRV.Rem
  div a b = floor (a / b)

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

-- Helpers to choose which conversion function to use.
--   * Can use identity function when @a ~ b@.
--   * For converting from different sign + different width integer types,
--   two operations are needed: resizing + bitcast.
--   The intermediate type is recorded.
data WhichConversion
  = UseIdentity
  | UseConversion
  | UseConversionThenBitcast Type

type family ChooseConversion (a :: Type) (b :: Type) :: WhichConversion where
  ChooseConversion a a = UseIdentity
  ChooseConversion a b =
    ChooseConversionFromScalars
      a ( ScalarFromTy a )
      b ( ScalarFromTy b )

type family ChooseConversionFromScalars
              ( a :: Type ) ( sa :: SPIRV.ScalarTy )
              ( b :: Type ) ( sb :: SPIRV.ScalarTy )
            :: WhichConversion
            where
  ChooseConversionFromScalars
    _ _
    _ ( SPIRV.Floating w )
      = UseConversion
  ChooseConversionFromScalars
    a ( SPIRV.Floating v )
    b _
      = TypeError
        (    Text "Cannot convert from type " :<>: ShowType a
        :$$: Text "to type " :<>: ShowType b
        )
  ChooseConversionFromScalars
    _ ( SPIRV.Integer s v )
    _ ( SPIRV.Integer s w )
      = UseConversion
  ChooseConversionFromScalars
    _ ( SPIRV.Integer r w )
    _ ( SPIRV.Integer s w )
      = UseConversion -- (strictly speaking this ends up being a bitcast)
  ChooseConversionFromScalars
    _ ( SPIRV.Integer r v )
    _ ( SPIRV.Integer s w )
      = UseConversionThenBitcast
          ( TypeFromScalar ( SPIRV.Integer r w ) )

class DispatchConversion a b (useIdentity :: WhichConversion) where
  conversion :: AST a -> AST b

instance ScalarTy a => DispatchConversion a a UseIdentity where
  conversion = id
instance (ScalarTy a, ScalarTy b, Convert '(a,b)) => DispatchConversion a b 'UseConversion where
  conversion = primOp @'(a,b) @SPIRV.Convert
instance ( ScalarTy a, ScalarTy b, ScalarTy x
         , Convert '(a,x), Convert '(x,b)
         )
       => DispatchConversion a b ('UseConversionThenBitcast x) where
  conversion
    = ( primOp @'(x,b) @SPIRV.Convert :: AST x -> AST b )
    . ( primOp @'(a,x) @SPIRV.Convert :: AST a -> AST x )

instance ( ScalarTy a, ScalarTy b, DispatchConversion a b (ChooseConversion a b) )
       => Convert '(AST a, AST b) where
  convert = conversion @a @b @(ChooseConversion a b)

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

instance Container (AST (V n a)) where
  type Overlapping  (AST (V n a)) k _
    = TypeError (    Text "optic: attempt to index a vector component with name " :<>: ShowType k
                :$$: Text "Maybe you intended to use a swizzle?"
                )

instance KnownNat m => Container (AST (M m n a)) where
  type Overlapping (AST (M m n a)) k _
    = TypeError ( Text "optic: attempt to index a matrix component with name " :<>: ShowType k )

instance Container (AST (Struct (as :: [Symbol :-> Type]))) where
  type Overlapping (AST (Struct (as :: [Symbol :-> Type]))) k i
    = k == Key (StructElemFromIndex (Text "key: ") i as i as)

instance Container (AST (Array n a)) where
  type Overlapping (AST (Array n a)) k _
        = TypeError ( Text "optic: attempt to index an array using name " :<>: ShowType k )

instance
  ( TypeError ( Text "Cannot recombine runtime arrays.") )
  => Container (AST (RuntimeArray a)) where
    type Overlapping (AST (RuntimeArray a)) _ _ = TypeError ( Text "Cannot recombine runtime arrays.")

-- *** Equalisers
--
-- $equalisers
-- Monomorphic containers, for equalisers.

instance (PrimTy a, KnownNat n) => MonoContainer (AST (V n a)) where
  type MonoType (AST (V n a)) = AST a
  setAll a _ = pureAST a
instance (PrimTy a, KnownNat n, KnownNat m) => MonoContainer (AST (M m n a)) where
  type MonoType (AST (M m n a)) = AST a
  setAll a _ = pureAST a
instance MonoContainer (Struct as) => MonoContainer (AST (Struct (as :: [Symbol :-> Type]))) where
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
type family MapAST (as :: [Type]) = (r :: [Type]) | r -> as where
  MapAST '[] = '[]
  MapAST (a ': as) = AST a ': MapAST as

type family MapMapAST (iss :: [[Type]]) = (r :: [[Type]]) | r -> iss where
  MapMapAST '[] = '[]
  MapMapAST (is ': iss) = MapAST is ': MapMapAST iss

instance forall
            ( s    ::   Type   )
            ( p    ::   Type   )
            ( p'   ::   Type   )
            ( iss  :: [[Type]] )
            ( iss' :: [[Type]] )
            ( js   ::  [Type]  )
            ( js'  ::  [Type]  )
            ( as   ::  [Type]  )
            ( as'  ::  [Type]  )
            ( os   :: ProductComponents iss       s  as  )
            ( os'  :: ProductComponents iss' (AST s) as' )
          . ( KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownLength js
            , KnownOptic (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ AST p
            , as' ~ MapAST as
            , AreProducts js iss as
            , js ~ MapHList iss
            , js' ~ MapAST js
            , PrimTy p
            )
          => KnownASTOptic
                ( Prod_ os' :: Optic js' (AST s) p' )
                ( Prod_ os  :: Optic js       s  p  )
          where
instance  {-# OVERLAPPING #-}
          forall
            ( s    ::   Type   )
            ( p    ::   Type   )
            ( p'   ::   Type   )
            ( iss  :: [[Type]] )
            ( iss' :: [[Type]] )
            ( js   ::  [Type]  )
            ( js'  ::  [Type]  )
            ( as   ::  [Type]  )
            ( as'  ::  [Type]  )
            ( os   :: ProductComponents iss       s  as  )
            ( os'  :: ProductComponents iss' (AST s) as' )
          . ( ComponentsGettable os
            , KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownOptic (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ AST p
            , as' ~ MapAST as
            , js' ~ MapAST js
            , AreProducts js iss as
            , js ~ MapHList iss
            , KnownLength js
            , PrimTy p
            , Syntactic (ListVariadic (js' `Postpend` AST s) p')
            , Internal (ListVariadic (js' `Postpend` AST s) p')
                ~ (ListVariadic (js `Postpend` s) p)
            )
          => Gettable (Prod_ os' :: Optic js' (AST s) p') where
instance  {-# OVERLAPPING #-}
          forall
            ( s    ::   Type   )
            ( p    ::   Type   )
            ( p'   ::   Type   )
            ( iss  :: [[Type]] )
            ( iss' :: [[Type]] )
            ( js   ::  [Type]  )
            ( js'  ::  [Type]  )
            ( as   ::  [Type]  )
            ( as'  ::  [Type]  )
            ( os   :: ProductComponents iss       s  as  )
            ( os'  :: ProductComponents iss' (AST s) as' )
          . ( ComponentsGettable os
            , KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownOptic (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ AST p
            , as' ~ MapAST as
            , js' ~ MapAST js
            , AreProducts js iss as
            , js ~ MapHList iss
            , KnownLength js
            , PrimTy p
            , Syntactic (ListVariadic (js' `Postpend` AST s) p')
            , Internal (ListVariadic (js' `Postpend` AST s) p')
                ~ (ListVariadic (js `Postpend` s) p)
            )
          => ReifiedGetter (Prod_ os' :: Optic js' (AST s) p') where
  view = fromAST
       $ View sLength ( opticSing @(Prod_ os :: Optic js s p) )

instance  {-# OVERLAPPING #-}
          forall
            ( s    ::   Type   )
            ( p    ::   Type   )
            ( p'   ::   Type   )
            ( iss  :: [[Type]] )
            ( iss' :: [[Type]] )
            ( js   ::  [Type]  )
            ( js'  ::  [Type]  )
            ( as   ::  [Type]  )
            ( as'  ::  [Type]  )
            ( os   :: ProductComponents iss       s  as  )
            ( os'  :: ProductComponents iss' (AST s) as' )
          . ( ComponentsSettable os
            , ArePairwiseDisjoint os
            , KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownOptic (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ AST p
            , as' ~ MapAST as
            , js' ~ MapAST js
            , AreProducts js iss as
            , js ~ MapHList iss
            , KnownLength js
            , PrimTy p
            , Syntactic (ListVariadic (js' `Postpend` p' `Postpend` AST s) (AST s))
            , Internal (ListVariadic (js' `Postpend` p' `Postpend` AST s) (AST s))
              ~ (ListVariadic (js `Postpend` p `Postpend` s) s)
            )
          => Settable (Prod_ os' :: Optic js' (AST s) p') where
instance  {-# OVERLAPPING #-}
          forall
            ( s    ::   Type   )
            ( p    ::   Type   )
            ( p'   ::   Type   )
            ( iss  :: [[Type]] )
            ( iss' :: [[Type]] )
            ( js   ::  [Type]  )
            ( js'  ::  [Type]  )
            ( as   ::  [Type]  )
            ( as'  ::  [Type]  )
            ( os   :: ProductComponents iss       s  as  )
            ( os'  :: ProductComponents iss' (AST s) as' )
          . ( ComponentsSettable os
            , ArePairwiseDisjoint os
            , KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownOptic (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ AST p
            , as' ~ MapAST as
            , js' ~ MapAST js
            , AreProducts js iss as
            , js ~ MapHList iss
            , KnownLength js
            , PrimTy p
            , Syntactic (ListVariadic (js' `Postpend` p' `Postpend` AST s) (AST s))
            , Internal (ListVariadic (js' `Postpend` p' `Postpend` AST s) (AST s))
              ~ (ListVariadic (js `Postpend` p `Postpend` s) s)
            )
          => ReifiedSetter (Prod_ os' :: Optic js' (AST s) p') where
  set = fromAST
      $ Set (sLength @_ @js) ( opticSing @(Prod_ os :: Optic js s p) )

class KnownASTOpticComponents ast_os os | ast_os -> os where
instance  ( iss ~ jss, as ~ '[], bs ~ '[] )
       => KnownASTOpticComponents
            ( EndProd_ :: ProductComponents iss (AST s) as )
            ( EndProd_ :: ProductComponents jss      s  bs )
          where
instance  ( KnownASTOptic o' o
          , KnownASTOpticComponents os' os
          , as' ~ MapAST as
          , iss' ~ MapMapAST iss
          )
        => KnownASTOpticComponents
             ( (o' `ProductO` os') :: ProductComponents iss' (AST s) ( a' ': as' ) )
             ( (o  `ProductO` os ) :: ProductComponents iss       s  ( a  ': as  ) )
        where

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
  distDict = DistDict
instance ( KnownNat m, KnownNat n ) => PrimFunc (M m n) where
  primFuncSing = SFuncMatrix @m @n
  distDict = DistDict
instance KnownNat n => PrimFunc (Array n) where
  primFuncSing = SFuncArray @n
  distDict = DistDict

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
  type Internal (a,b) = (Internal a, Internal b)
  toAST  (a,b) = Pair :$ toAST a :$ toAST b
  fromAST p = ( fromAST (Fst :$ p), fromAST (Snd :$ p) )
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
  fromAST v = buildV @n builder
    where builder :: forall i. (KnownNat i, CmpNat i n ~ Prelude.LT)
                  => Proxy i -> a
          builder _ = fromAST ( View sLength (opticSing @(Index i)) :$ v )

instance ( KnownNat m, KnownNat n
         , Syntactic a
         , PrimTy (Internal a)
         )
       => Syntactic (M m n a) where
  type Internal (M m n a) = M m n (Internal a)
  toAST (M m) = Mat :$ toAST m
  fromAST = M . fromAST . ( UnMat :$ )

-----------------------------------------------
-- * Vectors and matrices

-- ** Vectors
--
-- $vectors
-- Instances for:
--
-- 'Semimodule', 'LinearModule', 'Inner', 'Cross'.
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

instance (ScalarTy a, Ring a) => LinearModule Nat (AST (V 0 a)) where
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

  diag :: forall (n :: Nat). KnownNat n => AST a -> AST (M n n a)
  diag a = toAST (M mat)
    where
      indicator :: (KnownNat i, KnownNat j) => Proxy i -> Proxy j -> AST a
      indicator px1 px2 = case sameNat px1 px2 of
        Just _ -> a
        _      -> zero
      mat :: V n (V n (AST a))
      mat = buildV @n ( \ px1 -> buildV @n ( \ px2 -> indicator px1 px2 ) )

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
