{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Prim.Op

Recall that the AST has a 'FIR.AST.PrimOp' constructor
of the following type:

>   PrimOp :: PrimOp op a
>          => Proxy a -> Proxy op -> AST (PrimOpType op a)

This module provides the wrapper type class 'PrimOp' which relates
the internal SPIR-V operation names (as defined in "SPIRV.PrimOp")
with their types within the context of the AST.

This module also specifies which operations can be vectorised.
-}

module FIR.Prim.Op where

-- base
import Prelude
  ( Bool(..), Float, Functor(..), Maybe(..), Show(..)
  , ($), (<>), (.)
  , error, fromIntegral
  )
import Control.Applicative
  ( liftA2 )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word32, Word64 )
import GHC.TypeNats
  ( Nat, KnownNat, natVal )

-- fir
import Data.Constraint.All
  ( All )
import FIR.AST.Type
  ( AugType(Val, (:-->)), Eff, FunArgs, UnderlyingType, Nullary
  , ApplyFAug
  )
import FIR.Prim.RayTracing
  ( AccelerationStructure )
import FIR.Prim.Singletons
  ( PrimTy, ScalarTy
  , primTy, scalarTy
  )
import FIR.ProgramState
  ( ProgramState )
import Math.Linear
  ( V, M
  , Semimodule((^*)), Inner(dot, normalise), Cross(cross)
  , Matrix(..)
  )
import Math.Logic.Bits
  ( Bits(..), BitShift(..), BitCast(..) )
import Math.Logic.Class
  ( Boolean(..), Eq(..), Ord(..) )
import Math.Algebra.Class
  ( AdditiveMonoid(..), CancellativeAdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), Signed(..)
  , Archimedean(..)
  , DivisionRing(..)
  , Floating(..), RealFloat(..)
  , Convert(..), Rounding(..)
  )
import qualified SPIRV.PrimOp as SPIRV

-------------------------------------------------------------------------------
-- primitive operations

class All Nullary ( FunArgs (PrimOpAugType op a) ) => PrimOp (op :: opKind) (a :: k) | op -> k where
  type PrimOpAugType op a :: AugType
  op :: PrimOpType op a
  op = error
    ( "unsupported primitive operation " <> show ( opName @_ @_ @op @a ) )
  opName :: SPIRV.PrimOp
  opSing :: Maybe (SPrimOp a op)
  opSing = Nothing
  vectorisation :: KnownNat n => Maybe (VecPrimOpType n op a)
  vectorisation = Nothing

type PrimOpType op a = UnderlyingType ( PrimOpAugType op a )

-- some singletons for primitive operations
-- (only defining those that we use in code generation)
data SPrimOp (a :: k) (op :: opKind) :: Type where
  SMul :: (ScalarTy a, Semiring a) => SPrimOp a SPIRV.Mul
  
-- data type recording type-level information regarding vectorisation of primops
data VecPrimOpType (n :: Nat) op a where
  VecPrimOpType
    :: ( PrimOp ('Vectorise op) vec
       , All PrimTyVal (FunArgs (PrimOpAugType op a))
       , PrimOpAugType ('Vectorise op) vec ~ ApplyFAug (V n) (PrimOpAugType op a)
       )
    => Proxy vec -> VecPrimOpType n op a

newtype Vectorise a = Vectorise a

class    ( v ~ Val (UnderlyingType v), PrimTy (UnderlyingType v) ) => PrimTyVal v
instance ( v ~ Val (UnderlyingType v), PrimTy (UnderlyingType v) ) => PrimTyVal v

-------------------------------------------------------------------------------
-- instances

-- boolean operations
instance PrimOp SPIRV.BoolOr Bool where
  type PrimOpAugType SPIRV.BoolOr Bool = Val Bool :--> Val Bool :--> Val Bool
  op = (||)
  opName = SPIRV.BoolOp SPIRV.BoolOr
instance PrimOp SPIRV.BoolAnd Bool where
  type PrimOpAugType SPIRV.BoolAnd Bool = Val Bool :--> Val Bool :--> Val Bool
  op = (&&)
  opName = SPIRV.BoolOp SPIRV.BoolAnd
instance PrimOp SPIRV.BoolNot Bool where
  type PrimOpAugType SPIRV.BoolNot Bool = Val Bool :--> Val Bool
  op = not
  opName = SPIRV.BoolOp SPIRV.BoolNot

-- equality operations
instance (PrimTy a, Eq a, Logic a ~ Bool) => PrimOp SPIRV.Equal (a :: Type) where
  type PrimOpAugType SPIRV.Equal a = Val a :--> Val a :--> Val Bool
  op = (==)
  opName = SPIRV.EqOp SPIRV.Equal (primTy @a)
instance (PrimTy a, Eq a, Logic a ~ Bool) => PrimOp SPIRV.NotEqual (a :: Type) where
  type PrimOpAugType SPIRV.NotEqual a = Val a :--> Val a :--> Val Bool
  op = (/=)
  opName = SPIRV.EqOp SPIRV.NotEqual (primTy @a)

-- comparison
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.GT (a :: Type) where
  type PrimOpAugType SPIRV.GT a = Val a :--> Val a :--> Val Bool
  op = (>)
  opName = SPIRV.OrdOp SPIRV.GT (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.GTE (a :: Type) where
  type PrimOpAugType SPIRV.GTE a = Val a :--> Val a :--> Val Bool
  op = (>=)
  opName = SPIRV.OrdOp SPIRV.GTE (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.LT (a :: Type) where
  type PrimOpAugType SPIRV.LT a = Val a :--> Val a :--> Val Bool
  op = (<)
  opName = SPIRV.OrdOp SPIRV.LT (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.LTE (a :: Type) where
  type PrimOpAugType SPIRV.LTE a = Val a :--> Val a :--> Val Bool
  op = (<=)
  opName = SPIRV.OrdOp SPIRV.LTE (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.Min (a :: Type) where
  type PrimOpAugType SPIRV.Min a = Val a :--> Val a :--> Val a
  op = min
  opName = SPIRV.OrdOp SPIRV.Min (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.Max (a :: Type) where
  type PrimOpAugType SPIRV.Max a = Val a :--> Val a :--> Val a
  op = max
  opName = SPIRV.OrdOp SPIRV.Max (scalarTy @a)

-- bitwise operations
instance (ScalarTy a, Bits a) => PrimOp SPIRV.BitAnd (a :: Type) where
  type PrimOpAugType SPIRV.BitAnd a = Val a :--> Val a :--> Val a
  op = (.&.)
  opName = SPIRV.BitOp SPIRV.BitAnd (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitAnd a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Bits a) => PrimOp SPIRV.BitOr (a :: Type) where
  type PrimOpAugType SPIRV.BitOr a = Val a :--> Val a :--> Val a
  op = (.|.)
  opName = SPIRV.BitOp SPIRV.BitOr (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitOr a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Bits a) => PrimOp SPIRV.BitXor (a :: Type) where
  type PrimOpAugType SPIRV.BitXor a = Val a :--> Val a :--> Val a
  op = xor
  opName = SPIRV.BitOp SPIRV.BitXor (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitXor a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Bits a) => PrimOp SPIRV.BitNot (a :: Type) where
  type PrimOpAugType SPIRV.BitNot a = Val a :--> Val a
  op = complement
  opName = SPIRV.BitOp SPIRV.BitNot (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitNot a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
-- no logical right bit shift
instance forall (b :: Type) (s :: Type). (ScalarTy b, PrimTy s, BitShift '(b,s))
        => PrimOp SPIRV.BitShiftRightArithmetic '(b,s) where
  type PrimOpAugType SPIRV.BitShiftRightArithmetic '(b,s) = Val b :--> Val s :--> Val b
  op = shiftR
  opName = SPIRV.BitOp SPIRV.BitShiftRightArithmetic (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitShiftRightArithmetic '(b,s))
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n b, V n s))
instance forall (b :: Type) (s :: Type). (ScalarTy b, PrimTy s, BitShift '(b,s))
      => PrimOp SPIRV.BitShiftLeft '(b,s) where
  type PrimOpAugType SPIRV.BitShiftLeft '(b,s) = Val b :--> Val s :--> Val b
  op = shiftL
  opName = SPIRV.BitOp SPIRV.BitShiftLeft (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitShiftLeft '(b,s))
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n b, V n s))

-- bitcast operation
instance (ScalarTy a, ScalarTy b, BitCast a b) => PrimOp SPIRV.CastOp '(a,b) where
  type PrimOpAugType SPIRV.CastOp '(a,b) = Val a :--> Val b
  op = bitcast
  opName = SPIRV.CastOp (primTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CastOp '(a,b))
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))

-- numeric operations
instance (ScalarTy a, AdditiveMonoid a) => PrimOp SPIRV.Add (a :: Type) where
  type PrimOpAugType SPIRV.Add a = Val a :--> Val a :--> Val a
  op = (+)
  opName = SPIRV.NumOp SPIRV.Add (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Add a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Semiring a) => PrimOp SPIRV.Mul (a :: Type) where
  type PrimOpAugType SPIRV.Mul a = Val a :--> Val a :--> Val a
  op = (*)
  opName = SPIRV.NumOp SPIRV.Mul (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Mul a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
  opSing = Just SMul
instance (ScalarTy a, CancellativeAdditiveMonoid a) => PrimOp SPIRV.Sub (a :: Type) where
  type PrimOpAugType SPIRV.Sub a = Val a :--> Val a :--> Val a
  op = (-)
  opName = SPIRV.NumOp SPIRV.Sub (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Sub a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, AdditiveGroup a) => PrimOp SPIRV.Neg (a :: Type) where
  type PrimOpAugType SPIRV.Neg a = Val a :--> Val a
  op = negate
  opName = SPIRV.NumOp SPIRV.Neg (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Neg a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Signed a) => PrimOp SPIRV.Abs (a :: Type) where
  type PrimOpAugType SPIRV.Abs a = Val a :--> Val a
  op = abs
  opName = SPIRV.NumOp SPIRV.Abs (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Abs a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Signed a) => PrimOp SPIRV.Sign (a :: Type) where
  type PrimOpAugType SPIRV.Sign a = Val a :--> Val a
  op = signum
  opName = SPIRV.NumOp SPIRV.Sign (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Sign a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, DivisionRing a) => PrimOp SPIRV.Div (a :: Type) where
  type PrimOpAugType SPIRV.Div a = Val a :--> Val a :--> Val a
  op = (/)
  opName = SPIRV.NumOp SPIRV.Div (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Div a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Archimedean a) => PrimOp SPIRV.Mod (a :: Type) where
  type PrimOpAugType SPIRV.Mod a = Val a :--> Val a :--> Val a
  op = mod
  opName = SPIRV.NumOp SPIRV.Mod (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Mod a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Archimedean a) => PrimOp SPIRV.Rem (a :: Type) where
  type PrimOpAugType SPIRV.Rem a = Val a :--> Val a :--> Val a
  op = rem
  opName = SPIRV.NumOp SPIRV.Rem (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Rem a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Archimedean a) => PrimOp SPIRV.Quot (a :: Type) where
  type PrimOpAugType SPIRV.Quot a = Val a :--> Val a :--> Val a
  op = div
  opName = SPIRV.NumOp SPIRV.Quot (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Quot a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))


-- floating operations
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FSin (a :: Type) where
  type PrimOpAugType SPIRV.FSin a = Val a :--> Val a
  op = sin
  opName = SPIRV.FloatOp SPIRV.FSin (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FSin a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FCos (a :: Type) where
  type PrimOpAugType SPIRV.FCos a = Val a :--> Val a
  op = cos
  opName = SPIRV.FloatOp SPIRV.FCos (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FCos a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FTan (a :: Type) where
  type PrimOpAugType SPIRV.FTan a = Val a :--> Val a
  op = tan
  opName = SPIRV.FloatOp SPIRV.FTan (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FTan a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAsin (a :: Type) where
  type PrimOpAugType SPIRV.FAsin a = Val a :--> Val a
  op = asin
  opName = SPIRV.FloatOp SPIRV.FAsin (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAsin a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAcos (a :: Type) where
  type PrimOpAugType SPIRV.FAcos a = Val a :--> Val a
  op = acos
  opName = SPIRV.FloatOp SPIRV.FAcos (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAcos a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAtan (a :: Type) where
  type PrimOpAugType SPIRV.FAtan a = Val a :--> Val a
  op = atan
  opName = SPIRV.FloatOp SPIRV.FAtan (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAtan a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FSinh (a :: Type) where
  type PrimOpAugType SPIRV.FSinh a = Val a :--> Val a
  op = sinh
  opName = SPIRV.FloatOp SPIRV.FSinh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FSinh a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FCosh (a :: Type) where
  type PrimOpAugType SPIRV.FCosh a = Val a :--> Val a
  op = cosh
  opName = SPIRV.FloatOp SPIRV.FCosh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FCosh a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FTanh (a :: Type) where
  type PrimOpAugType SPIRV.FTanh a = Val a :--> Val a
  op = tanh
  opName = SPIRV.FloatOp SPIRV.FTanh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FTanh a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAsinh (a :: Type) where
  type PrimOpAugType SPIRV.FAsinh a = Val a :--> Val a
  op = asinh
  opName = SPIRV.FloatOp SPIRV.FAsinh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAsinh a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAcosh (a :: Type) where
  type PrimOpAugType SPIRV.FAcosh a = Val a :--> Val a
  op = acosh
  opName = SPIRV.FloatOp SPIRV.FAcosh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAcosh a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAtanh (a :: Type) where
  type PrimOpAugType SPIRV.FAtanh a = Val a :--> Val a
  op = atanh
  opName = SPIRV.FloatOp SPIRV.FAtanh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAtanh a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, RealFloat a) => PrimOp SPIRV.FAtan2 (a :: Type) where
  type PrimOpAugType SPIRV.FAtan2 a = Val a :--> Val a :--> Val a
  op = atan2
  opName = SPIRV.FloatOp SPIRV.FAtan2 (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAtan2 a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FPow (a :: Type) where
  type PrimOpAugType SPIRV.FPow a = Val a :--> Val a :--> Val a
  op = (**)
  opName = SPIRV.FloatOp SPIRV.FPow (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FPow a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FExp (a :: Type) where
  type PrimOpAugType SPIRV.FExp a = Val a :--> Val a
  op = exp
  opName = SPIRV.FloatOp SPIRV.FExp (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FExp a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FLog (a :: Type) where
  type PrimOpAugType SPIRV.FLog a = Val a :--> Val a
  op = log
  opName = SPIRV.FloatOp SPIRV.FLog (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FLog a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FSqrt (a :: Type) where
  type PrimOpAugType SPIRV.FSqrt a = Val a :--> Val a
  op = sqrt
  opName = SPIRV.FloatOp SPIRV.FSqrt (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FSqrt a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FInvSqrt (a :: Type) where
  type PrimOpAugType SPIRV.FInvSqrt a = Val a :--> Val a
  op = recip . sqrt
  opName = SPIRV.FloatOp SPIRV.FInvSqrt (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FInvSqrt a)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))


-- numeric conversions
instance ( ScalarTy a, ScalarTy b, Convert '(a,b) ) => PrimOp SPIRV.Convert '(a,b) where
  type PrimOpAugType SPIRV.Convert '(a,b) = Val a :--> Val b
  op = convert
  opName = SPIRV.ConvOp SPIRV.Convert (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Convert '(a,b))
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))
instance ( ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp SPIRV.CTruncate '(a,b) where
  type PrimOpAugType SPIRV.CTruncate '(a,b) = Val a :--> Val b
  op = truncate
  opName = SPIRV.ConvOp SPIRV.CTruncate (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CTruncate '(a,b))
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))
instance ( ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp SPIRV.CRound '(a,b) where
  type PrimOpAugType SPIRV.CRound '(a,b) = Val a :--> Val b
  op = round
  opName = SPIRV.ConvOp SPIRV.CRound (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CRound '(a,b))
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))
instance ( ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp SPIRV.CFloor '(a,b) where
  type PrimOpAugType SPIRV.CFloor '(a,b) = Val a :--> Val b
  op = floor
  opName = SPIRV.ConvOp SPIRV.CFloor (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CFloor '(a,b))
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))
instance ( ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp SPIRV.CCeiling '(a,b) where
  type PrimOpAugType SPIRV.CCeiling '(a,b) = Val a :--> Val b
  op = ceiling
  opName = SPIRV.ConvOp SPIRV.CCeiling (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CCeiling '(a,b))
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))

-- geometry primitive instructions
instance PrimOp SPIRV.EmitGeometryVertex (i :: ProgramState) where
  type PrimOpAugType SPIRV.EmitGeometryVertex i = Eff i i ()
  opName = SPIRV.GeomOp SPIRV.EmitGeometryVertex
instance PrimOp SPIRV.EndGeometryPrimitive (i :: ProgramState) where
  type PrimOpAugType SPIRV.EndGeometryPrimitive i = Eff i i ()
  opName = SPIRV.GeomOp SPIRV.EndGeometryPrimitive

-- memory synchronisation instructions
instance PrimOp SPIRV.ControlSync (i :: ProgramState) where
  type PrimOpAugType SPIRV.ControlSync i = Val Word32 :--> Val Word32 :--> Val Word32 :--> Eff i i ()
  opName = SPIRV.SyncOp SPIRV.ControlSync
instance PrimOp SPIRV.MemorySync (i :: ProgramState) where
  type PrimOpAugType SPIRV.MemorySync i = Val Word32 :--> Val Word32 :--> Eff i i ()
  opName = SPIRV.SyncOp SPIRV.MemorySync

-- ray tracing instructions
instance PrimOp SPIRV.RT_ReportIntersection (i :: ProgramState) where
  type PrimOpAugType SPIRV.RT_ReportIntersection i = Val Float :--> Val Word32 :--> Eff i i Bool
  opName = SPIRV.RayOp SPIRV.RT_ReportIntersection
instance PrimOp SPIRV.RT_IgnoreIntersection (i :: ProgramState) where
  type PrimOpAugType SPIRV.RT_IgnoreIntersection i = Eff i i ()
  opName = SPIRV.RayOp SPIRV.RT_IgnoreIntersection
instance PrimOp SPIRV.RT_TerminateRay (i :: ProgramState) where
  type PrimOpAugType SPIRV.RT_TerminateRay i = Eff i i ()
  opName = SPIRV.RayOp SPIRV.RT_TerminateRay
instance PrimOp SPIRV.RT_AccelerationStructureFromDeviceAddress '() where
  type PrimOpAugType SPIRV.RT_AccelerationStructureFromDeviceAddress _ = Val Word64 :--> Val AccelerationStructure
  opName = SPIRV.RayOp SPIRV.RT_AccelerationStructureFromDeviceAddress

-- vector operations
-- doing it by hand because I'm an idiot who doesn't know better

val :: forall n. KnownNat n => Word32
val = fromIntegral ( natVal (Proxy @n) )


instance ( KnownNat n, ScalarTy a, Bits a ) => PrimOp ('Vectorise SPIRV.BitAnd) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.BitAnd) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 (.&.)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitAnd @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Bits a  ) => PrimOp ('Vectorise SPIRV.BitOr) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.BitOr) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 (.|.)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitOr @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Bits a  ) => PrimOp ('Vectorise SPIRV.BitXor) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.BitXor) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 xor
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitXor @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Bits a ) => PrimOp ('Vectorise SPIRV.BitNot) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.BitNot) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap complement
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitNot @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy a, ScalarTy b, BitCast a b ) => PrimOp ('Vectorise SPIRV.CastOp) '(V n a, V n b) where
  type PrimOpAugType ('Vectorise SPIRV.CastOp) '(V n a, V n b) = Val (V n a) :--> Val (V n b)
  op = fmap bitcast
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CastOp @'(a,b))) (val @n) (scalarTy @b)

instance ( KnownNat n, ScalarTy b, PrimTy s, BitShift '(b,s) ) => PrimOp ('Vectorise SPIRV.BitShiftRightArithmetic) '(V n b, V n s) where
  type PrimOpAugType ('Vectorise SPIRV.BitShiftRightArithmetic) '(V n b, V n s) = Val (V n b) :--> Val (V n s) :--> Val (V n b)
  op = liftA2 shiftR
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitShiftRightArithmetic @'(b,s))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy b, PrimTy s, BitShift '(b,s) ) => PrimOp ('Vectorise SPIRV.BitShiftLeft) '(V n b, V n s) where
  type PrimOpAugType ('Vectorise SPIRV.BitShiftLeft) '(V n b, V n s) = Val (V n b) :--> Val (V n s) :--> Val (V n b)
  op = liftA2 shiftL
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitShiftLeft @'(b,s))) (val @n) (scalarTy @b)

instance ( KnownNat n, ScalarTy a, AdditiveMonoid a ) => PrimOp ('Vectorise SPIRV.Add) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Add) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 (+)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Add @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, CancellativeAdditiveMonoid a ) => PrimOp ('Vectorise SPIRV.Sub) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Sub) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 (-)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Sub @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, AdditiveGroup a ) => PrimOp ('Vectorise SPIRV.Neg) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Neg) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap negate
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Neg @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Semiring a ) => PrimOp ('Vectorise SPIRV.Mul) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Mul) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 (*)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Mul @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Signed a ) => PrimOp ('Vectorise SPIRV.Abs) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Abs) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap abs
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Abs @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Signed a ) => PrimOp ('Vectorise SPIRV.Sign) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Sign) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap signum
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Sign @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, DivisionRing a ) => PrimOp ('Vectorise SPIRV.Div) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Div) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 (/)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Div @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Archimedean a ) => PrimOp ('Vectorise SPIRV.Mod) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Mod) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 mod
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Mod @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Archimedean a ) => PrimOp ('Vectorise SPIRV.Rem) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Rem) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 rem
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Rem @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Archimedean a ) => PrimOp ('Vectorise SPIRV.Quot) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.Quot) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 div
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Quot @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy a, ScalarTy b, Convert '(a,b) ) => PrimOp ('Vectorise SPIRV.Convert) '(V n a, V n b) where
  type PrimOpAugType ('Vectorise SPIRV.Convert) '(V n a, V n b) = Val (V n a) :--> Val (V n b)
  op = fmap convert
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Convert @'(a,b))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp ('Vectorise SPIRV.CTruncate) '(V n a, V n b) where
  type PrimOpAugType ('Vectorise SPIRV.CTruncate) '(V n a, V n b) = Val (V n a) :--> Val (V n b)
  op = fmap truncate
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CTruncate @'(a,b))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp ('Vectorise SPIRV.CRound) '(V n a, V n b) where
  type PrimOpAugType ('Vectorise SPIRV.CRound) '(V n a, V n b) = Val (V n a) :--> Val (V n b)
  op = fmap round
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CRound @'(a,b))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp ('Vectorise SPIRV.CFloor) '(V n a, V n b) where
  type PrimOpAugType ('Vectorise SPIRV.CFloor) '(V n a, V n b) = Val (V n a) :--> Val (V n b)
  op = fmap floor
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CFloor @'(a,b))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp ('Vectorise SPIRV.CCeiling) '(V n a, V n b) where
  type PrimOpAugType ('Vectorise SPIRV.CCeiling) '(V n a, V n b) = Val (V n a) :--> Val (V n b)
  op = fmap ceiling
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CCeiling @'(a,b))) (val @n) (scalarTy @b)

instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FSin) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FSin) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap sin
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSin @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FCos) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FCos) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap cos
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FCos @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FTan) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FTan) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap tan
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FTan @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAsin) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FAsin) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap asin
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAsin @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAcos) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FAcos) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap acos
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAcos @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAtan) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FAtan) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap atan
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtan @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FSinh) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FSinh) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap sinh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSinh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FCosh) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FCosh) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap cosh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FCosh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FTanh) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FTanh) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap tanh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FTanh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAsinh) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FAsinh) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap asinh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAsinh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAcosh) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FAcosh) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap acosh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAcosh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAtanh) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FAtanh) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap atanh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtanh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, RealFloat a ) => PrimOp ('Vectorise SPIRV.FAtan2) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FAtan2) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 atan2
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtan2 @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FPow) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FPow) (V n a) = Val (V n a) :--> Val (V n a) :--> Val (V n a)
  op = liftA2 (**)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FPow @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FExp) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FExp) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap exp
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FExp @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FLog) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FLog) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap log
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FLog @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FSqrt) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FSqrt) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap sqrt
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSqrt @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FInvSqrt) (V n a) where
  type PrimOpAugType ('Vectorise SPIRV.FInvSqrt) (V n a) = Val (V n a) :--> Val (V n a)
  op = fmap ( recip . sqrt )
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FInvSqrt @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp SPIRV.DotV (V n a) where
  type PrimOpAugType SPIRV.DotV (V n a) = Val (V n a) :--> Val (V n a) :--> Val a
  op = dot
  opName = SPIRV.VecOp SPIRV.DotV (val @n) (scalarTy @a)
-- vector times scalar operation requires floating point type in SPIR-V
instance ( KnownNat n, ScalarTy a, Semiring a, Floating a ) => PrimOp SPIRV.VMulK (V n a) where 
  type PrimOpAugType SPIRV.VMulK (V n a) = Val (V n a) :--> Val a :--> Val (V n a)
  op = (^*)
  opName = SPIRV.VecOp SPIRV.VMulK (val @n) (scalarTy @a)
instance ( n ~ 3, ScalarTy a, Floating a ) => PrimOp SPIRV.CrossV (V n a) where
  type PrimOpAugType SPIRV.CrossV (V n a) = Val (V 3 a) :--> Val (V 3 a) :--> Val (V 3 a)
  op = cross
  opName = SPIRV.VecOp SPIRV.CrossV 3 (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a) => PrimOp SPIRV.NormaliseV (V n a) where
  type PrimOpAugType SPIRV.NormaliseV (V n a) = Val (V n a) :--> Val (V n a)
  op = normalise
  opName = SPIRV.VecOp SPIRV.NormaliseV (val @n) (scalarTy @a)


instance ( KnownNat i, KnownNat j, ScalarTy a, Floating a ) => PrimOp SPIRV.MMulK '(a,i,j) where
  type PrimOpAugType SPIRV.MMulK '(a,i,j) = Val (M i j a) :--> Val a :--> Val (M i j a)
  op = (!*)
  opName = SPIRV.MatOp SPIRV.MMulK (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a, Floating a ) => PrimOp SPIRV.MMulV '(a,i,j) where
  type PrimOpAugType SPIRV.MMulV '(a,i,j) = Val (M i j a) :--> Val (V j a) :--> Val (V i a)
  op = (!*^)
  opName = SPIRV.MatOp SPIRV.MMulV (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a, Floating a ) => PrimOp SPIRV.VMulM '(a,i,j) where
  type PrimOpAugType SPIRV.VMulM '(a,i,j) = Val (V i a) :--> Val (M i j a) :--> Val (V j a)
  op = (^*!)
  opName = SPIRV.MatOp SPIRV.VMulM (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, KnownNat k, ScalarTy a, Floating a ) => PrimOp SPIRV.MMulM '(a,i,j,k) where
  type PrimOpAugType SPIRV.MMulM '(a,i,j,k) = Val (M i j a) :--> Val (M j k a) :--> Val (M i k a)
  op = (!*!)
  opName = SPIRV.MatOp SPIRV.MMulM (val @i) (val @k) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a, Floating a ) => PrimOp SPIRV.Transp '(a,i,j) where
  type PrimOpAugType SPIRV.Transp '(a,i,j) = Val (M i j a) :--> Val (M j i a)
  op = transpose
  opName = SPIRV.MatOp SPIRV.Transp (val @j) (val @i) (scalarTy @a)
instance ( KnownNat i, ScalarTy a, Floating a ) => PrimOp SPIRV.Det '(a,i) where
  type PrimOpAugType SPIRV.Det '(a,i) = Val (M i i a) :--> Val a
  op = determinant
  opName = SPIRV.MatOp SPIRV.Det (val @i) (val @i) (scalarTy @a)
instance ( KnownNat i, ScalarTy a, Floating a ) => PrimOp SPIRV.Inv '(a,i) where
  type PrimOpAugType SPIRV.Inv '(a,i) = Val (M i i a) :--> Val (M i i a)
  op = inverse
  opName = SPIRV.MatOp SPIRV.Inv (val @i) (val @i) (scalarTy @a)
