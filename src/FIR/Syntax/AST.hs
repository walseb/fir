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
{-# LANGUAGE QuantifiedConstraints      #-}
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
  ( -- switch statement
    switch

    -- functor/applicative for AST values
  , ASTApplicative(..), fmapAST, (<$$>)

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
  ( Type, Constraint )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Typeable
  ( Typeable )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, KnownNat
  , CmpNat, sameNat
  )

-- half
import Numeric.Half
  ( Half )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( Vector, generate )

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
import Data.Constraint.All
  ( All )
import Data.Finite.With
  ( withFinite )
import Data.Function.Variadic
  ( ListVariadic )
import Data.Product
  ( IsProduct
  , AreProducts
  , Distribute
  , MapHList
  )
import Data.Type.List
  ( KnownLength(sLength)
  , SameLength
  , type (:++:), Postpend, Snoc
  )
import Data.Type.Map
  ( (:->)((:->)), Key )
import FIR.AST
  ( AST, Code
  , Syntactic(Internal, toAST, fromAST)
  , InternalType
  , primOp
  , pattern (:$), pattern Lit
  , pattern Switch
  , pattern View, pattern Set
  , pattern Pure, pattern Ap
  , pattern MkVector, pattern Mat, pattern UnMat, pattern Array, pattern Struct
  )
import FIR.AST.Optics
  ( AugListVariadic )
import FIR.AST.Type
  ( AugType(Val, (:-->))
  , FunArgs, FunRes
  , ApplyFAug
  )
import FIR.Syntax.Optics
  ( KnownOptic(opticSing)
  , KnownComponents
  , (%:.:)
  , ValidAnIndexOptic
  )
import FIR.Prim.Array
  ( Array(MkArray), RuntimeArray )
import FIR.Prim.Op
  ( PrimTyVal, Vectorise(Vectorise) )
import FIR.Prim.Singletons
  ( PrimTy
  , SScalarTy(..), ScalarTy, IntegralTy
  , ScalarFromTy, TypeFromScalar
  , SPrimFunc(..), PrimFunc(..), DistDict(DistDict)
  , scalarTySing
  )
import FIR.Prim.Struct
  ( Struct((:&), End) )
import FIR.Validation.Bounds
  ( StructIndexFromName )
import Math.Algebra.Class
  ( AdditiveMonoid(..), CancellativeAdditiveMonoid(..), AdditiveGroup(..)
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
  , buildV
  )
import Math.Logic.Bits
  ( Bits(..), BitShift(..), BitCast(..) )
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

instance ( b ~ Bool ) => Boolean (Code b) where
  true  = Lit True
  false = Lit False
  (&&)  = primOp @Bool @SPIRV.BoolAnd
  (||)  = primOp @Bool @SPIRV.BoolOr
  not   = primOp @Bool @SPIRV.BoolNot

instance ( PrimTy a, Eq a, Logic a ~ Bool )
  => Eq (Code a) where
  type Logic (Code a) = Code Bool
  (==) = primOp @a @SPIRV.Equal
  (/=) = primOp @a @SPIRV.NotEqual

instance ( ScalarTy a, Ord a, Logic a ~ Bool ) 
  => Ord (Code a) where
  (<=) = primOp @a @SPIRV.LTE
  (>=) = primOp @a @SPIRV.GTE
  (<)  = primOp @a @SPIRV.LT
  (>)  = primOp @a @SPIRV.GT
  min  = primOp @a @SPIRV.Min
  max  = primOp @a @SPIRV.Max

switch :: ( Syntactic scrut
          , Internal scrut ~ Val vscrut
          , IntegralTy vscrut
          , Syntactic val
          , Internal val ~ Val vval
          , PrimTy vval
          )
       => scrut -- ^ Scrutinee.
       -> [ vscrut :-> val ] -- ^ Cases.
       -> val  -- ^ Default (fallthrough) case.
       -> val
switch scrut cases val = fromAST $ Switch (toAST scrut) (toAST val) (map ( \ (x :-> y) -> (x, toAST y) ) cases)

-- * Bitwise operations
--
-- $bitwise
-- Instances for:
--
-- 'Bits', 'BitShift' (note: not 'Data.Bits.Bits'), 'BitCast'.

instance (ScalarTy a, Bits a) => Bits (Code a) where
  (.&.)      = primOp @a @SPIRV.BitAnd
  (.|.)      = primOp @a @SPIRV.BitOr
  xor        = primOp @a @SPIRV.BitXor
  complement = primOp @a @SPIRV.BitNot
  zeroBits   = Lit ( zeroBits @a )

instance (ScalarTy a, ScalarTy s, BitShift '(a,s))
  => BitShift '(Code a, Code s) where
  shiftL = primOp @'(a,s) @SPIRV.BitShiftLeft
  shiftR = primOp @'(a,s) @SPIRV.BitShiftRightArithmetic

instance (ScalarTy a, ScalarTy b, BitCast a b)
  => BitCast (Code a) (Code b) where
  bitcast = primOp @'(a,b) @SPIRV.CastOp

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

instance (ScalarTy a, AdditiveMonoid a) => AdditiveMonoid (Code a) where
  (+)    = primOp @a @SPIRV.Add
  zero   = Lit (zero :: a)
  fromInteger = Lit . fromInteger
instance (ScalarTy a, Semiring a) => Semiring (Code a) where
  (*)    = primOp @a @SPIRV.Mul
instance (ScalarTy a, CancellativeAdditiveMonoid a) => CancellativeAdditiveMonoid (Code a) where
  (-)    = primOp @a @SPIRV.Sub
instance (ScalarTy a, AdditiveGroup a) => AdditiveGroup (Code a) where
  negate = primOp @a @SPIRV.Neg
instance (ScalarTy a, Signed a) => Signed (Code a) where
  abs    = primOp @a @SPIRV.Abs
  signum = primOp @a @SPIRV.Sign
instance (ScalarTy a, DivisionRing a) => DivisionRing (Code a) where
  (/)    = primOp @a @SPIRV.Div
  fromRational = Lit . fromRational

instance Archimedean (Code Word8 ) where
  mod    = primOp @Word8  @SPIRV.Mod
  rem    = primOp @Word8  @SPIRV.Rem
  div    = primOp @Word8  @SPIRV.Quot
instance Archimedean (Code Word16) where
  mod    = primOp @Word16 @SPIRV.Mod
  rem    = primOp @Word16 @SPIRV.Rem
  div    = primOp @Word16 @SPIRV.Quot
instance Archimedean (Code Word32) where
  mod    = primOp @Word32 @SPIRV.Mod
  rem    = primOp @Word32 @SPIRV.Rem
  div    = primOp @Word32 @SPIRV.Quot
instance Archimedean (Code Word64) where
  mod    = primOp @Word64 @SPIRV.Mod
  rem    = primOp @Word64 @SPIRV.Rem
  div    = primOp @Word64 @SPIRV.Quot
instance Archimedean (Code Int8  ) where
  mod    = primOp @Int8   @SPIRV.Mod
  rem    = primOp @Int8   @SPIRV.Rem
  div    = primOp @Int8   @SPIRV.Quot
instance Archimedean (Code Int16 ) where
  mod    = primOp @Int16  @SPIRV.Mod
  rem    = primOp @Int16  @SPIRV.Rem
  div    = primOp @Int16  @SPIRV.Quot
instance Archimedean (Code Int32 ) where
  mod    = primOp @Int32  @SPIRV.Mod
  rem    = primOp @Int32  @SPIRV.Rem
  div    = primOp @Int32  @SPIRV.Quot
instance Archimedean (Code Int64 ) where
  mod    = primOp @Int64  @SPIRV.Mod
  rem    = primOp @Int64  @SPIRV.Rem
  div    = primOp @Int64  @SPIRV.Quot
instance Archimedean (Code Half  ) where
  mod    = primOp @Half   @SPIRV.Mod
  rem    = primOp @Half   @SPIRV.Rem
  div a b = floor (a / b)
instance Archimedean (Code Float ) where
  mod    = primOp @Float  @SPIRV.Mod
  rem    = primOp @Float  @SPIRV.Rem
  div a b = floor (a / b)
instance Archimedean (Code Double) where
  mod    = primOp @Double @SPIRV.Mod
  rem    = primOp @Double @SPIRV.Rem
  div a b = floor (a / b)

instance (ScalarTy a, Floating a) => Floating (Code a) where
  pi         = Lit pi
  exp        = primOp @a @SPIRV.FExp
  log        = primOp @a @SPIRV.FLog
  sqrt       = primOp @a @SPIRV.FSqrt
  invSqrt    = primOp @a @SPIRV.FInvSqrt
  sin        = primOp @a @SPIRV.FSin
  cos        = primOp @a @SPIRV.FCos
  tan        = primOp @a @SPIRV.FTan
  asin       = primOp @a @SPIRV.FAsin
  acos       = primOp @a @SPIRV.FAcos
  atan       = primOp @a @SPIRV.FAtan
  sinh       = primOp @a @SPIRV.FSinh
  cosh       = primOp @a @SPIRV.FCosh
  tanh       = primOp @a @SPIRV.FTanh
  asinh      = primOp @a @SPIRV.FAsinh
  acosh      = primOp @a @SPIRV.FAcosh
  atanh      = primOp @a @SPIRV.FAtanh
  (**)       = primOp @a @SPIRV.FPow

instance (ScalarTy a, RealFloat a, Eq a, Logic a ~ Bool) => RealFloat (Code a) where
  atan2      = primOp @a @SPIRV.FAtan2
  isNaN      = primOp @a @SPIRV.FIsNaN
  isInfinite = primOp @a @SPIRV.FIsInf

instance (ScalarTy a, Integral a) => Integral (Code a) where
instance (ScalarTy a, Unsigned a) => Unsigned (Code a) where

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
  conversion :: Code a -> Code b

instance ScalarTy a => DispatchConversion a a UseIdentity where
  conversion = id
instance (ScalarTy a, ScalarTy b, Convert '(a,b)) => DispatchConversion a b 'UseConversion where
  conversion = primOp @'(a,b) @SPIRV.Convert
instance ( ScalarTy a, ScalarTy b, ScalarTy x
         , Convert '(a,x), Convert '(x,b)
         )
       => DispatchConversion a b ('UseConversionThenBitcast x) where
  conversion
    = ( primOp @'(x,b) @SPIRV.Convert :: Code x -> Code b )
    . ( primOp @'(a,x) @SPIRV.Convert :: Code a -> Code x )

instance ( ScalarTy a, ScalarTy b, DispatchConversion a b (ChooseConversion a b) )
       => Convert '(Code a, Code b) where
  convert = conversion @a @b @(ChooseConversion a b)

instance {-# OVERLAPPING #-}
         ( ScalarTy a, Rounding '(a,a) )
      => Rounding '(Code a, Code a) where
  truncate = primOp @'(a,a) @SPIRV.CTruncate
  round    = primOp @'(a,a) @SPIRV.CRound
  floor    = primOp @'(a,a) @SPIRV.CFloor
  ceiling  = primOp @'(a,a) @SPIRV.CCeiling

instance ( ScalarTy a, ScalarTy b
         , Floating a, Integral b
         , Rounding '(a,a), Rounding '(a,b)
         )
       => Rounding '(Code a, Code b) where
  truncate = primOp @'(a,b) @SPIRV.CTruncate
  round    = primOp @'(a,b) @SPIRV.CTruncate @(Code a -> Code b)
           . primOp @'(a,a) @SPIRV.CRound
  floor    = primOp @'(a,b) @SPIRV.CTruncate @(Code a -> Code b)
           . primOp @'(a,a) @SPIRV.CFloor
  ceiling  = primOp @'(a,b) @SPIRV.CTruncate @(Code a -> Code b)
           . primOp @'(a,a) @SPIRV.CCeiling


-----------------------------------------------
-- * Optics

class KnownOptic optic => KnownASTOptic astoptic optic | astoptic -> optic where

-- ** Simple instances

-- *** Name

instance ( empty ~ '[]
         , KnownSymbol k
         , r ~ Code a
         , KnownOptic (Field_ k :: Optic '[] s a)
         )
       => KnownASTOptic (Field_ k :: Optic empty1 (Code s) r) (Field_ k :: Optic '[] s a)
       where
instance ( KnownSymbol k
         , empty ~ '[]
         , Gettable (Field_ k :: Optic '[] s a)
         , r ~ Code a
         )
      => Gettable (Field_ k :: Optic empty (Code s) r)
      where
instance ( KnownSymbol k
         , empty ~ '[]
         , Gettable (Field_ k :: Optic '[] s a)
         , ReifiedGetter (Field_ k :: Optic '[] s a)
         , PrimTy a, PrimTy s
         , KnownOptic (Field_ k :: Optic '[] s a)
         , r ~ Code a
         )
      => ReifiedGetter (Field_ k :: Optic empty (Code s) r)
      where
  view = fromAST
       $ View
            sLength
            ( opticSing @(Field_ k :: Optic '[] s a) )
instance ( KnownSymbol k
         , empty ~ '[]
         , Settable (Field_ k :: Optic '[] s a)
         , r ~ Code a
         )
      => Settable (Field_ k :: Optic empty (Code s) r)
      where
instance ( KnownSymbol k
         , empty ~ '[]
         , Settable (Field_ k :: Optic '[] s a)
         , ReifiedSetter (Field_ k :: Optic '[] s a)
         , PrimTy s, PrimTy a
         , KnownOptic (Field_ k :: Optic '[] s a)
         , r ~ Code a
         )
      => ReifiedSetter (Field_ k :: Optic empty (Code s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(Field_ k :: Optic '[] s a) )

-- *** Run-time index

instance ( Integral ty, IntegralTy ty
         , ix' ~ '[ Code ty ]
         , r ~ Code a
         , ValidAnIndexOptic '[ty] s a
         , PrimTy s, PrimTy a
         )
       => KnownASTOptic (RTOptic_ :: Optic ix' (Code s) r) (RTOptic_ :: Optic '[ty] s a)
       where
instance ( Integral ty
         , ix ~ '[Code ty]
         , Gettable (RTOptic_ :: Optic '[ty] s a)
         , r ~ Code a
         )
      => Gettable (RTOptic_ :: Optic ix (Code s) r)
      where
instance ( Integral ty, IntegralTy ty
         , ix ~ '[Code ty]
         , Gettable (RTOptic_ :: Optic '[ty] s a)
         , ReifiedGetter (RTOptic_ :: Optic '[ty] s a)
         , PrimTy a, PrimTy s
         , KnownOptic (RTOptic_ :: Optic '[ty] s a)
         , r ~ Code a
         )
      => ReifiedGetter (RTOptic_ :: Optic ix (Code s) r)
      where
  view = fromAST
       $ View
            sLength
            ( opticSing @(RTOptic_ :: Optic '[ty] s a) )
instance ( Integral ty
         , ix ~ '[Code ty]
         , Settable (RTOptic_ :: Optic '[ty] s a)
         , r ~ Code a
         )
      => Settable (RTOptic_ :: Optic ix (Code s) r)
      where
instance ( Integral ty, IntegralTy ty
         , ix ~ '[Code ty]
         , Settable (RTOptic_ :: Optic '[ty] s a)
         , ReifiedSetter (RTOptic_ :: Optic '[ty] s a)
         , PrimTy s, PrimTy a
         , KnownOptic (RTOptic_ :: Optic '[ty] s a)
         , r ~ Code a
         )
      => ReifiedSetter (RTOptic_ :: Optic ix (Code s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(RTOptic_ :: Optic '[ty] s a) )

-- *** Compile-time index

instance ( empty ~ '[]
         , KnownNat i
         , r ~ Code a
         , KnownOptic (Field_ i :: Optic empty s a)
         )
       => KnownASTOptic (Field_ i :: Optic empty (Code s) r) (Field_ i :: Optic '[] s a)
       where
instance ( KnownNat i
         , empty ~ '[]
         , Gettable (Field_ i :: Optic '[] s a)
         , r ~ Code a
         )
      => Gettable (Field_ i :: Optic empty (Code s) r)
      where
instance ( KnownNat i
         , empty ~ '[]
         , Gettable (Field_ i :: Optic '[] s a)
         , ReifiedGetter (Field_ i :: Optic '[] s a)
         , PrimTy a, PrimTy s
         , KnownOptic (Field_ i :: Optic '[] s a)
         , r ~ Code a
         )
      => ReifiedGetter (Field_ i :: Optic empty (Code s) r)
      where
  view = fromAST
       $ View
            sLength
            ( opticSing @(Field_ i :: Optic '[] s a) )
instance ( KnownNat i
         , empty ~ '[]
         , Settable (Field_ i :: Optic '[] s a)
         , r ~ Code a
         )
      => Settable (Field_ i :: Optic empty (Code s) r)
      where
instance ( KnownNat i
         , empty ~ '[]
         , Settable (Field_ i :: Optic '[] s a)
         , ReifiedSetter (Field_ i :: Optic '[] s a)
         , PrimTy s, PrimTy a
         , KnownOptic (Field_ i :: Optic '[] s a)
         , r ~ Code a
         )
      => ReifiedSetter (Field_ i :: Optic empty (Code s) r)
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

instance Container (Code (V n a)) where
  type FieldIndexFromName (Code (V n a)) k
    = TypeError (    Text "optic: attempt to index a vector component with name " :<>: ShowType k
                :$$: Text "Maybe you intended to use a swizzle?"
                )
instance KnownNat m => Container (Code (M m n a)) where
instance Container (Code (Struct (as :: [Symbol :-> Type]))) where
  type FieldIndexFromName (Code (Struct (as :: [Symbol :-> Type]))) k
    = Key (StructIndexFromName k as)
instance Container (Code (Array n a)) where
instance Container (Code (RuntimeArray a)) where

-- *** Optic focusing on parts with a given type
--
-- $oftype
-- Setter instances for 'OfType' optic.

instance ( empty ~ '[]
         , r ~ Code ty
         , KnownOptic (OfType_ ty :: Optic '[] s ty)
         )
       => KnownASTOptic
            (OfType_ (Code ty) :: Optic empty (Code s) r )
            (OfType_      ty  :: Optic '[]        s  ty)
       where
instance {-# OVERLAPPING #-}
         ( empty ~ '[]
         , r ~ Code ty
         , Settable (OfType_ ty :: Optic '[] s ty)
         )
      => Settable (OfType_ (Code ty) :: Optic empty (Code s) r)
      where
instance {-# OVERLAPPING #-}
         ( empty ~ '[]
         , r ~ Code ty
         , Settable   (OfType_ ty :: Optic '[] s ty)
         , KnownOptic (OfType_ ty :: Optic '[] s ty)
         , PrimTy s
         , PrimTy ty
         )
      => ReifiedSetter (OfType_ (Code ty) :: Optic empty (Code s) r)
      where
  set = fromAST
       $ Set
            sLength
            ( opticSing @(OfType_ ty :: Optic '[] s ty) )

-- *** Compound instance resolution

-- **** Composites

instance {-# OVERLAPPING #-}
         forall is js ks s x y
                (o1 :: Optic is (Code s) x) (o2 :: Optic js x y)
                is' js' ks' a b
                (o1' :: Optic is' s a) (o2' :: Optic js' a b)
                .
         ( KnownASTOptic o1 o1'
         , KnownASTOptic o2 o2'
         , ks  ~ ( is  :++: js  )
         , ks' ~ ( is' :++: js' )
         , KnownLength ks'
         , x ~ Code a
         , y ~ Code b
         )
         => KnownASTOptic
             ( (o1  `ComposeO` o2 ) :: Optic ks (Code s) y )
             ( (o1' `ComposeO` o2') :: Optic ks'     s  b )
         where
instance {-# OVERLAPPING #-}
         forall is js ks s x y
                (o1 :: Optic is (Code s) x) (o2 :: Optic js x y)
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
         , x ~ Code a
         , y ~ Code b
         , Syntactic (ListVariadic (ks `Postpend` Code s) y)
         , Internal (ListVariadic (ks `Postpend` Code s) y)
            ~ AugListVariadic (ks' `Snoc` s) b
         )
         => ReifiedGetter ( (o1 `ComposeO` o2) :: Optic ks (Code s) y )
         where
  view = fromAST
       $ View sLength ( opticSing @o1' %:.: opticSing @o2' )
instance {-# OVERLAPPING #-}
         forall is js ks s x y
                (o1 :: Optic is (Code s) x) (o2 :: Optic js x y)
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
         , x ~ Code a
         , y ~ Code b
         , Syntactic (ListVariadic (ks `Postpend` y `Postpend` Code s) (Code s))
         , Internal (ListVariadic (ks `Postpend` y `Postpend` Code s) (Code s))
            ~ AugListVariadic (ks' `Snoc` b `Snoc` s) s
         )
         => ReifiedSetter ( (o1 `ComposeO` o2) :: Optic ks (Code s) y )
         where
  set = fromAST
      $ Set sLength ( opticSing @o1' %:.: opticSing @o2' )

-- **** Products
type family MapAST (as :: [Type]) = (r :: [Type]) | r -> as where
  MapAST '[] = '[]
  MapAST (a ': as) = Code a ': MapAST as

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
            ( os'  :: ProductComponents iss' (Code s) as' )
          . ( KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownLength js
            , KnownOptic (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ Code p
            , as' ~ MapAST as
            , AreProducts js iss as
            , js ~ MapHList iss
            , js' ~ MapAST js
            , PrimTy p
            )
          => KnownASTOptic
                ( Prod_ os' :: Optic js' (Code s) p' )
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
            ( os'  :: ProductComponents iss' (Code s) as' )
          . ( ComponentsGettable os
            , KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownOptic (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ Code p
            , as' ~ MapAST as
            , js' ~ MapAST js
            , AreProducts js iss as
            , js ~ MapHList iss
            , KnownLength js
            , PrimTy p
            , Syntactic (ListVariadic (js' `Postpend` Code s) p')
            , Internal (ListVariadic (js' `Postpend` Code s) p')
                ~ AugListVariadic (js `Snoc` s) p
            )
          => Gettable (Prod_ os' :: Optic js' (Code s) p') where
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
            ( os'  :: ProductComponents iss' (Code s) as' )
          . ( ComponentsGettable os
            , KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownOptic (Prod_ os :: Optic js s p)
            , ReifiedGetter (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ Code p
            , as' ~ MapAST as
            , js' ~ MapAST js
            , AreProducts js iss as
            , js ~ MapHList iss
            , KnownLength js
            , PrimTy p
            , Syntactic (ListVariadic (js' `Postpend` Code s) p')
            , Internal (ListVariadic (js' `Postpend` Code s) p')
                ~ AugListVariadic (js `Snoc` s) p
            )
          => ReifiedGetter (Prod_ os' :: Optic js' (Code s) p') where
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
            ( os'  :: ProductComponents iss' (Code s) as' )
          . ( ComponentsSettable os
            , ArePairwiseDisjoint os
            , KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownOptic (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ Code p
            , as' ~ MapAST as
            , js' ~ MapAST js
            , AreProducts js iss as
            , js ~ MapHList iss
            , KnownLength js
            , PrimTy p
            , Syntactic (ListVariadic (js' `Postpend` p' `Postpend` Code s) (Code s))
            , Internal (ListVariadic (js' `Postpend` p' `Postpend` Code s) (Code s))
              ~ AugListVariadic (js `Snoc` p `Snoc` s) s
            )
          => Settable (Prod_ os' :: Optic js' (Code s) p') where
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
            ( os'  :: ProductComponents iss' (Code s) as' )
          . ( ComponentsSettable os
            , ArePairwiseDisjoint os
            , KnownASTOpticComponents os' os
            , KnownComponents os
            , KnownOptic (Prod_ os :: Optic js s p)
            , ReifiedSetter (Prod_ os :: Optic js s p)
            , SameLength (Distribute iss  as ) as
            , SameLength (Distribute iss' as') as'
            , IsProduct p as
            , p' ~ Code p
            , as' ~ MapAST as
            , js' ~ MapAST js
            , AreProducts js iss as
            , js ~ MapHList iss
            , KnownLength js
            , PrimTy p
            , Syntactic (ListVariadic (js' `Postpend` p' `Postpend` Code s) (Code s))
            , Internal (ListVariadic (js' `Postpend` p' `Postpend` Code s) (Code s))
              ~ AugListVariadic (js `Snoc` p `Snoc` s) s
            )
          => ReifiedSetter (Prod_ os' :: Optic js' (Code s) p') where
  set = fromAST
      $ Set (sLength @_ @js) ( opticSing @(Prod_ os :: Optic js s p) )

class KnownASTOpticComponents ast_os os | ast_os -> os where
instance  ( iss ~ jss, as ~ '[], bs ~ '[] )
       => KnownASTOpticComponents
            ( EndProd_ :: ProductComponents iss (Code s) as )
            ( EndProd_ :: ProductComponents jss       s  bs )
          where
instance  ( KnownASTOptic o' o
          , KnownASTOpticComponents os' os
          , as' ~ MapAST as
          , iss' ~ MapMapAST iss
          )
        => KnownASTOpticComponents
             ( (o' `ProductO` os') :: ProductComponents iss' (Code s) ( a' ': as' ) )
             ( (o  `ProductO` os ) :: ProductComponents iss        s  ( a  ': as  ) )
        where

-----------------------------------------------
-- * Functor functionality

infixl 4 <$$>
infixl 4 <**>

class ( Typeable f, forall x. ASTApplicativeElt f x => PrimTy (f x) ) => ASTApplicative f where
  type ASTApplicativeElt f (x :: Type) :: Constraint
  pureAST
    :: ( FunRes a ~ Val va, PrimTy va
       , All PrimTyVal (FunArgs a)
       )
    => AST a -> AST (ApplyFAug f a)
  (<**>) :: ( Syntactic fb, Internal fb ~ ApplyFAug f b, PrimTy a )
         => ( AST ( Val (f a) :--> ApplyFAug f b ) ) -> Code (f a) -> fb

(<$$>), fmapAST
  :: forall f a b fb vb
  .  ( ASTApplicative f
     , PrimTy a
     , FunRes (Internal b) ~ Val vb
     , PrimTy vb
     , Syntactic b
     , Syntactic fb
     , Internal fb ~ ApplyFAug f (Internal b)
     , All PrimTyVal (FunArgs (Internal b))
     )
  => ( Code a -> b ) -> ( Code (f a) -> fb )
(<$$>) = fmapAST
fmapAST f fa = fromAST ( pureAST (toAST f) <**> fa )


instance KnownNat n => ASTApplicative (V n) where
  type ASTApplicativeElt (V n) x = PrimTy x
  pureAST = Pure ( Proxy @(V n) )
  fab <**> fa = fromAST $ Ap ( Proxy @(V n) ) fab fa

instance (KnownNat m, KnownNat n) => ASTApplicative (M m n) where
  type ASTApplicativeElt (M m n) x = ( ScalarTy x, Ring x )
  pureAST = Pure ( Proxy @(M m n) )
  fab <**> fa = fromAST $ Ap ( Proxy @(M m n) ) fab fa

instance KnownNat n => ASTApplicative (Array n) where
  type ASTApplicativeElt (Array n) x = PrimTy x
  pureAST = Pure ( Proxy @(Array n) )
  fab <**> fa = fromAST $ Ap ( Proxy @(Array n) ) fab fa

{-
instance 
  TypeError (     Text "The Code datatype does not have a Functor instance:"
             :$$: Text "    cannot map Haskell functions over internal types."
             :$$: Text "To map an internal function over an internal type, use 'fmapAST'/'<$$>'."
            ) => Prelude.Functor AST where
  fmap = error "unreachable"
-}

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
  type Internal () = Val ()
  toAST   = const ( Lit () )
  fromAST = const ()

instance ( Syntactic a, Syntactic b
         , Internal a ~ Val va, PrimTy va
         , Internal b ~ Val vb, PrimTy vb
         ) => Syntactic (a,b) where
  type Internal (a,b) =
    Val (Struct
      '[ "_0" ':-> InternalType a
       , "_1" ':-> InternalType b
       ] )
  toAST (a,b) = Struct ( toAST a :& toAST b :& End )
  fromAST struct =
    ( fromAST (view @(Index 0) struct)
    , fromAST (view @(Index 1) struct)
    )

instance ( Syntactic a, Syntactic b, Syntactic c
         , Internal a ~ Val va, PrimTy va
         , Internal b ~ Val vb, PrimTy vb
         , Internal c ~ Val vc, PrimTy vc
         ) => Syntactic (a,b,c) where
  type Internal (a,b,c) =
    Val ( Struct 
      '[ "_0" ':-> InternalType a
       , "_1" ':-> InternalType b
       , "_2" ':-> InternalType c
       ] )
  toAST (a,b,c) = Struct ( toAST a :& toAST b :& toAST c :& End )
  fromAST struct =
    ( fromAST (view @(Index 0) struct)
    , fromAST (view @(Index 1) struct)
    , fromAST (view @(Index 2) struct)
    )

instance  ( KnownNat n
          , Syntactic a
          , Internal a ~ Val v
          , PrimTy v
          )
        => Syntactic (V n a)
        where
  type Internal (V n a) = Val (V n (InternalType a))

  toAST :: V n a -> Code (V n (InternalType a))
  toAST = MkVector . Prelude.fmap toAST 

  fromAST :: Code (V n (InternalType a)) -> V n a
  fromAST v = buildV @n builder
    where builder :: forall i. (KnownNat i, CmpNat i n ~ Prelude.LT)
                  => Proxy i -> a
          builder _ = fromAST ( View sLength (opticSing @(Index i)) :$ v )

instance ( KnownNat m, KnownNat n
         , Syntactic a, Internal a ~ Val v, PrimTy v
         )
       => Syntactic (M m n a) where
  type Internal (M m n a) = Val (M m n (InternalType a))
  toAST (M m) = Mat :$ toAST m
  fromAST = M . fromAST . ( UnMat :$ )

instance ( KnownNat n
         , Syntactic a, Internal a ~ Val v, PrimTy v
         )
       => Syntactic (Array n a) where
  type Internal (Array n a) = Val (Array n (InternalType a))
  toAST arr = Array $ Prelude.fmap toAST arr
  fromAST arr = MkArray vec
    where
      vec :: Vector.Vector n a
      vec = Vector.generate ( withFinite ( \ ( _ :: Proxy i ) -> fromAST $ view @(Index i) arr ) )

-----------------------------------------------
-- * Vectors and matrices

-- ** Vectors
--
-- $vectors
-- Instances for:
--
-- 'Semimodule', 'LinearModule', 'Inner', 'Cross'.
instance (ScalarTy a, Semiring a) => Semimodule Nat (Code (V 0 a)) where
  type Scalar   (Code (V 0 a))       = Code      a
  type OfDim    (Code (V 0 a)) Nat n = Code (V n a)
  type ValidDim (Code (V 0 a)) Nat n = KnownNat n

  (^+^) :: forall n. KnownNat n
        => Code (V n a) -> Code (V n a) -> Code (V n a)
  (^+^) = primOp @(V n a) @('Vectorise SPIRV.Add)

  (^*) :: forall n. KnownNat n
       => Code (V n a) -> Code a -> Code (V n a)
  (^*)  = case scalarTySing @a of
    SHalf   -> primOp @(V n a) @SPIRV.VMulK
    SFloat  -> primOp @(V n a) @SPIRV.VMulK
    SDouble -> primOp @(V n a) @SPIRV.VMulK
    -- No "vector times scalar" operation in SPIR-V
    -- for integral scalar types.
    _  -> ( \ vec a -> toAST $ Prelude.fmap ( * a ) ( fromAST vec :: V n ( Code a ) ) )

instance (ScalarTy a, Ring a) => LinearModule Nat (Code (V 0 a)) where
  (^-^) :: forall n. KnownNat n
        => Code (V n a) -> Code (V n a) -> Code (V n a)
  (^-^) = primOp @(V n a) @('Vectorise SPIRV.Sub)

  (-^) :: forall n. KnownNat n => Code (V n a) -> Code (V n a)
  (-^) = primOp @(V n a) @('Vectorise SPIRV.Neg)

instance (ScalarTy a, Floating a) => Inner Nat (Code (V 0 a)) where
  (^.^) :: forall n. KnownNat n
        => Code (V n a) -> Code (V n a) -> Code a
  (^.^) = primOp @(V n a) @SPIRV.DotV

  normalise :: forall n. KnownNat n => Code (V n a) -> Code (V n a)
  normalise = primOp @(V n a) @SPIRV.NormaliseV

instance (ScalarTy a, Floating a) => Cross Nat (Code (V 0 a)) where
  type CrossDim (Code (V 0 a)) Nat n = ( n ~ 3 )

  cross :: Code (V 3 a) -> Code (V 3 a) -> Code (V 3 a)
  cross = primOp @(V 3 a) @SPIRV.CrossV


-- ** Matrices
--
-- $matrices
-- Instance for 'Matrix'.

type instance VectorOf (Code (M 0 0 a)) = Code (V 0 a)

instance (ScalarTy a, Floating a) => Matrix Nat (Code (M 0 0 a)) where
  type OfDims (Code (M 0 0 a)) Nat '(m,n) = Code (M m n a)

  diag :: forall (n :: Nat). KnownNat n => Code a -> Code (M n n a)
  diag a = toAST (M mat)
    where
      indicator :: (KnownNat i, KnownNat j) => Proxy i -> Proxy j -> Code a
      indicator px1 px2 = case sameNat px1 px2 of
        Just _ -> a
        _      -> zero
      mat :: V n (V n (Code a))
      mat = buildV @n ( \ px1 -> buildV @n ( \ px2 -> indicator px1 px2 ) )

  konst a = Mat :$ pureAST (pureAST a)

  transpose :: forall n m. (KnownNat n, KnownNat m)
            => Code (M n m a) -> Code (M m n a)
  transpose = primOp @'(a,n,m) @SPIRV.Transp

  inverse :: forall n. KnownNat n
          => Code (M n n a) -> Code (M n n a)
  inverse = primOp @'(a,n) @SPIRV.Inv
  
  determinant :: forall n. KnownNat n
              => Code (M n n a) -> Code a
  determinant = primOp @'(a,n) @SPIRV.Det

  -- no built-in matrix addition and subtraction, so we use the vector operations
  (!+!) :: forall i j. (KnownNat i, KnownNat j)
        => Code (M i j a) -> Code (M i j a) -> Code (M i j a)
  x !+! y = Mat :$ ( vecAdd <$$> (UnMat :$ x) <**> (UnMat :$ y) )
    where vecAdd :: Code (V i a) -> Code (V i a) -> Code (V i a)
          vecAdd = primOp @(V i a) @('Vectorise SPIRV.Add)
  (!-!) :: forall i j. (KnownNat i, KnownNat j)
        => Code (M i j a) -> Code (M i j a) -> Code (M i j a)
  x !-! y = Mat :$ ( vecSub <$$> (UnMat :$ x) <**> (UnMat :$ y) )
    where vecSub :: Code (V i a) -> Code (V i a) -> Code (V i a)
          vecSub = primOp @(V i a) @('Vectorise SPIRV.Sub)

  (!*!) :: forall i j k. (KnownNat i, KnownNat j, KnownNat k)
        => Code (M i j a) -> Code (M j k a) -> Code (M i k a)
  (!*!) = primOp @'(a,i,j,k) @SPIRV.MMulM

  (^*!) :: forall i j. (KnownNat i, KnownNat j)
        => Code (V i a) -> Code (M i j a) -> Code (V j a)
  (^*!) = primOp @'(a,i,j) @SPIRV.VMulM

  (!*^) :: forall i j. (KnownNat i, KnownNat j)
        => Code (M i j a) -> Code (V j a) -> Code (V i a)
  (!*^) = primOp @'(a,i,j) @SPIRV.MMulV

  (!*) :: forall i j. (KnownNat i, KnownNat j)
       => Code (M i j a) -> Code a -> Code (M i j a)
  (!*) = primOp @'(a,i,j) @SPIRV.MMulK
