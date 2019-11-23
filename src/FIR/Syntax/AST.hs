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

{-|
Module: FIR.Syntax.AST

This module, together with "FIR.Syntax.Program",
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
  , Container(FieldIndexFromName)
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
  ( (:->)((:->)), Key )
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
  ( Struct((:&), End) )
import FIR.Validation.Bounds
  ( StructIndexFromName )
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
  )
import Math.Logic.Bits
  ( Bits(..), BitShift(..) )
import Math.Logic.Class
  ( Eq(..), Boolean(..), Ord(..) )
import qualified SPIRV.ScalarTy as SPIRV
import qualified SPIRV.PrimOp   as SPIRV

--------------------------------------------------------------------------------------
-- Instances for AST type

-- * Logical operations
--
-- $logical
-- Instances for:
--
-- 'Boolean',
--
-- 'Eq', 'Ord' (note: not the "Prelude" type classes).
--
-- Note that rebindable syntax for if-then-else is provided
-- by the module "FIR.Syntax.IfThenElse".

instance Boolean (AST Bool) where
  true  = Lit True
  false = Lit False
  (&&)  = primOp @Bool @SPIRV.BoolAnd
  (||)  = primOp @Bool @SPIRV.BoolOr
  not   = primOp @Bool @SPIRV.BoolNot

instance ( PrimTy a, Eq a, Logic a ~ Bool )
  => Eq (AST a) where
  type Logic (AST a) = AST Bool
  (==) = primOp @a @SPIRV.Equal
  (/=) = primOp @a @SPIRV.NotEqual

instance ( ScalarTy a, Ord a, Logic a ~ Bool ) 
  => Ord (AST a) where
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
  invSqrt = primOp @a @SPIRV.FInvSqrt
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
         , KnownOptic (Field_ k :: Optic '[] s a)
         )
       => KnownASTOptic (Field_ k :: Optic empty1 (AST s) r) (Field_ k :: Optic '[] s a)
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
         , PrimTy a
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
         , PrimTy s
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
         , PrimTy a
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
         , PrimTy s
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
         , PrimTy a
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
         , PrimTy s
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
-- Container instances, for product optics.

instance Container (AST (V n a)) where
  type FieldIndexFromName (AST (V n a)) k
    = TypeError (    Text "optic: attempt to index a vector component with name " :<>: ShowType k
                :$$: Text "Maybe you intended to use a swizzle?"
                )
instance KnownNat m => Container (AST (M m n a)) where
instance Container (AST (Struct (as :: [Symbol :-> Type]))) where
  type FieldIndexFromName (AST (Struct (as :: [Symbol :-> Type]))) k
    = Key (StructIndexFromName k as)
instance Container (AST (Array n a)) where
instance Container (AST (RuntimeArray a)) where

-- *** Optic focusing on parts with a given type
--
-- $oftype
-- Setter instances for 'OfType' optic.

instance ( empty ~ '[]
         , r ~ AST ty
         , KnownOptic (OfType_ ty :: Optic '[] s ty)
         )
       => KnownASTOptic
            (OfType_ (AST ty) :: Optic empty (AST s) r )
            (OfType_      ty  :: Optic '[]        s  ty)
       where
instance {-# OVERLAPPING #-}
         ( empty ~ '[]
         , r ~ AST ty
         , Settable (OfType_ ty :: Optic '[] s ty)
         )
      => Settable (OfType_ (AST ty) :: Optic empty (AST s) r)
      where
instance {-# OVERLAPPING #-}
         ( empty ~ '[]
         , r ~ AST ty
         , Settable   (OfType_ ty :: Optic '[] s ty)
         , KnownOptic (OfType_ ty :: Optic '[] s ty)
         , PrimTy s
         , PrimTy ty
         )
      => ReifiedSetter (OfType_ (AST ty) :: Optic empty (AST s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(OfType_ ty :: Optic '[] s ty) )

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
         , ReifiedGetter ( ( o1' `ComposeO` o2' ) :: Optic ks' s b )
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
         , ReifiedSetter ( ( o1' `ComposeO` o2' ) :: Optic ks' s b )
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
            , ReifiedGetter (Prod_ os :: Optic js s p)
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
            , ReifiedSetter (Prod_ os :: Optic js s p)
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

instance ( Syntactic a, Syntactic b
         , PrimTy (Internal a)
         , PrimTy (Internal b)
         ) => Syntactic (a,b) where
  type Internal (a,b) = Struct '[ "_0" ':-> Internal a, "_1" ':-> Internal b ]
  toAST (a,b) = Struct ( toAST a :& toAST b :& End )
  fromAST struct =
    ( fromAST (view @(Index 0) struct)
    , fromAST (view @(Index 1) struct)
    )

instance ( Syntactic a, Syntactic b, Syntactic c
         , PrimTy (Internal a)
         , PrimTy (Internal b)
         , PrimTy (Internal c)
         ) => Syntactic (a,b,c) where
  type Internal (a,b,c)
    = Struct '[ "_0" ':-> Internal a, "_1" ':-> Internal b, "_2" ':-> Internal c ]
  toAST (a,b,c) = Struct ( toAST a :& toAST b :& toAST c :& End )
  fromAST struct =
    ( fromAST (view @(Index 0) struct)
    , fromAST (view @(Index 1) struct)
    , fromAST (view @(Index 2) struct)
    )

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
    where vecAdd :: AST (V i a) -> AST (V i a) -> AST (V i a)
          vecAdd = primOp @(V i a) @('Vectorise SPIRV.Add)
  (!-!) :: forall i j. (KnownNat i, KnownNat j)
        => AST (M i j a) -> AST (M i j a) -> AST (M i j a)
  x !-! y = Mat :$ ( vecSub <$$> (UnMat :$ x) <**> (UnMat :$ y) )
    where vecSub :: AST (V i a) -> AST (V i a) -> AST (V i a)
          vecSub = primOp @(V i a) @('Vectorise SPIRV.Sub)

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
