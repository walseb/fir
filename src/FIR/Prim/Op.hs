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
  ( Maybe(..), Bool, (.)
  , Functor(..)
  , fromIntegral
  , ($) )
import Control.Applicative
  ( liftA2 )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat, KnownNat, natVal )

-- fir
import Control.Monad.Indexed
  ( (:=)(AtKey) )
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
  ( Bits(..), BitShift(..) )
import Math.Logic.Class
  ( Boolean(..), Eq(..), Ord(..) )
import Math.Algebra.Class
  ( AdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), Signed(..)
  , Archimedean(..)
  , DivisionRing(..)
  , Floating(..), RealFloat(..)
  , Convert(..), Rounding(..)
  )
import qualified SPIRV.PrimOp as SPIRV

-------------------------------------------------------------------------------
-- primitive operations

class PrimOp (op :: opKind) (a :: k) | op -> k where
  type PrimOpType op a :: Type
  op :: PrimOpType op a
  opName :: SPIRV.PrimOp
  opSing :: Maybe (SPrimOp a op)
  opSing = Nothing
  vectorisation :: KnownNat n => Maybe (VecPrimOpType n op)
  vectorisation = Nothing

-- some singletons for primitive operations
-- (only defining those that we use in code generation)
data SPrimOp (a :: k) (op :: opKind) :: Type where
  SMul :: (ScalarTy a, Semiring a) => SPrimOp a SPIRV.Mul
  
-- data type recording type-level information regarding vectorisation of primops
data VecPrimOpType (n :: Nat) op where
  VecPrimOpType :: PrimOp ('Vectorise op) vec => Proxy vec -> VecPrimOpType n op

newtype Vectorise a = Vectorise a

-------------------------------------------------------------------------------
-- instances

-- boolean operations
instance PrimOp SPIRV.BoolOr Bool where
  type PrimOpType SPIRV.BoolOr Bool = Bool -> Bool -> Bool
  op = (||)
  opName = SPIRV.BoolOp SPIRV.BoolOr
instance PrimOp SPIRV.BoolAnd Bool where
  type PrimOpType SPIRV.BoolAnd Bool = Bool -> Bool -> Bool
  op = (&&)
  opName = SPIRV.BoolOp SPIRV.BoolAnd
instance PrimOp SPIRV.BoolNot Bool where
  type PrimOpType SPIRV.BoolNot Bool = Bool -> Bool
  op = not
  opName = SPIRV.BoolOp SPIRV.BoolNot

-- equality operations
instance (PrimTy a, Eq a, Logic a ~ Bool) => PrimOp SPIRV.Equal (a :: Type) where
  type PrimOpType SPIRV.Equal a = a -> a -> Bool
  op = (==)
  opName = SPIRV.EqOp SPIRV.Equal (primTy @a)
instance (PrimTy a, Eq a, Logic a ~ Bool) => PrimOp SPIRV.NotEqual (a :: Type) where
  type PrimOpType SPIRV.NotEqual a = a -> a -> Bool
  op = (/=)
  opName = SPIRV.EqOp SPIRV.NotEqual (primTy @a)

-- comparison
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.GT (a :: Type) where
  type PrimOpType SPIRV.GT a = a -> a -> Bool
  op = (>)
  opName = SPIRV.OrdOp SPIRV.GT (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.GTE (a :: Type) where
  type PrimOpType SPIRV.GTE a = a -> a -> Bool
  op = (>=)
  opName = SPIRV.OrdOp SPIRV.GTE (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.LT (a :: Type) where
  type PrimOpType SPIRV.LT a = a -> a -> Bool
  op = (<)
  opName = SPIRV.OrdOp SPIRV.LT (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.LTE (a :: Type) where
  type PrimOpType SPIRV.LTE a = a -> a -> Bool
  op = (<=)
  opName = SPIRV.OrdOp SPIRV.LTE (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.Min (a :: Type) where
  type PrimOpType SPIRV.Min a = a -> a -> a
  op = min
  opName = SPIRV.OrdOp SPIRV.Min (scalarTy @a)
instance (ScalarTy a, Ord a, Logic a ~ Bool) => PrimOp SPIRV.Max (a :: Type) where
  type PrimOpType SPIRV.Max a = a -> a -> a
  op = max
  opName = SPIRV.OrdOp SPIRV.Max (scalarTy @a)

-- bitwise operations
instance (ScalarTy a, Bits a) => PrimOp SPIRV.BitAnd (a :: Type) where
  type PrimOpType SPIRV.BitAnd a = a -> a -> a
  op = (.&.)
  opName = SPIRV.BitOp SPIRV.BitAnd (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitAnd)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Bits a) => PrimOp SPIRV.BitOr (a :: Type) where
  type PrimOpType SPIRV.BitOr a = a -> a -> a
  op = (.|.)
  opName = SPIRV.BitOp SPIRV.BitOr (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitOr)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Bits a) => PrimOp SPIRV.BitXor (a :: Type) where
  type PrimOpType SPIRV.BitXor a = a -> a -> a
  op = xor
  opName = SPIRV.BitOp SPIRV.BitXor (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitXor)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Bits a) => PrimOp SPIRV.BitNot (a :: Type) where
  type PrimOpType SPIRV.BitNot a = a -> a
  op = complement
  opName = SPIRV.BitOp SPIRV.BitNot (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitNot)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
-- no logical right bit shift
instance forall (b :: Type) (s :: Type). (ScalarTy b, BitShift '(b,s)) => PrimOp SPIRV.BitShiftRightArithmetic '(b,s) where
  type PrimOpType SPIRV.BitShiftRightArithmetic '(b,s) = b -> s -> b
  op = shiftR
  opName = SPIRV.BitOp SPIRV.BitShiftRightArithmetic (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitShiftRightArithmetic)
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n b, V n s))
instance forall (b :: Type) (s :: Type). (ScalarTy b, BitShift '(b,s)) => PrimOp SPIRV.BitShiftLeft '(b,s) where
  type PrimOpType SPIRV.BitShiftLeft '(b,s) = b -> s -> b
  op = shiftL
  opName = SPIRV.BitOp SPIRV.BitShiftLeft (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.BitShiftLeft)
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n b, V n s))

-- numeric operations
instance (ScalarTy a, AdditiveMonoid a) => PrimOp SPIRV.Add (a :: Type) where
  type PrimOpType SPIRV.Add a = a -> a -> a
  op = (+)
  opName = SPIRV.NumOp SPIRV.Add (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Add)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Semiring a) => PrimOp SPIRV.Mul (a :: Type) where
  type PrimOpType SPIRV.Mul a = a -> a -> a
  op = (*)
  opName = SPIRV.NumOp SPIRV.Mul (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Mul)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
  opSing = Just SMul
instance (ScalarTy a, AdditiveGroup a) => PrimOp SPIRV.Sub (a :: Type) where
  type PrimOpType SPIRV.Sub a = a -> a -> a
  op = (-)
  opName = SPIRV.NumOp SPIRV.Sub (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Sub)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, AdditiveGroup a) => PrimOp SPIRV.Neg (a :: Type) where
  type PrimOpType SPIRV.Neg a = a -> a
  op = negate
  opName = SPIRV.NumOp SPIRV.Neg (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Neg)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Signed a) => PrimOp SPIRV.Abs (a :: Type) where
  type PrimOpType SPIRV.Abs a = a -> a
  op = abs
  opName = SPIRV.NumOp SPIRV.Abs (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Abs)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Signed a) => PrimOp SPIRV.Sign (a :: Type) where
  type PrimOpType SPIRV.Sign a = a -> a
  op = signum
  opName = SPIRV.NumOp SPIRV.Sign (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Sign)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, DivisionRing a) => PrimOp SPIRV.Div (a :: Type) where
  type PrimOpType SPIRV.Div a = a -> a -> a
  op = (/)
  opName = SPIRV.NumOp SPIRV.Div (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Div)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Archimedean a) => PrimOp SPIRV.Mod (a :: Type) where
  type PrimOpType SPIRV.Mod a = a -> a -> a
  op = mod
  opName = SPIRV.NumOp SPIRV.Mod (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Mod)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Archimedean a) => PrimOp SPIRV.Rem (a :: Type) where
  type PrimOpType SPIRV.Rem a = a -> a -> a
  op = rem
  opName = SPIRV.NumOp SPIRV.Rem (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Rem)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Archimedean a) => PrimOp SPIRV.Quot (a :: Type) where
  type PrimOpType SPIRV.Quot a = a -> a -> a
  op = div
  opName = SPIRV.NumOp SPIRV.Quot (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Quot)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))


-- floating operations
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FSin (a :: Type) where
  type PrimOpType SPIRV.FSin a = a -> a
  op = sin
  opName = SPIRV.FloatOp SPIRV.FSin (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FSin)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FCos (a :: Type) where
  type PrimOpType SPIRV.FCos a = a -> a
  op = cos
  opName = SPIRV.FloatOp SPIRV.FCos (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FCos)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FTan (a :: Type) where
  type PrimOpType SPIRV.FTan a = a -> a
  op = tan
  opName = SPIRV.FloatOp SPIRV.FTan (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FTan)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAsin (a :: Type) where
  type PrimOpType SPIRV.FAsin a = a -> a
  op = asin
  opName = SPIRV.FloatOp SPIRV.FAsin (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAsin)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAcos (a :: Type) where
  type PrimOpType SPIRV.FAcos a = a -> a
  op = acos
  opName = SPIRV.FloatOp SPIRV.FAcos (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAcos)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAtan (a :: Type) where
  type PrimOpType SPIRV.FAtan a = a -> a
  op = atan
  opName = SPIRV.FloatOp SPIRV.FAtan (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAtan)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FSinh (a :: Type) where
  type PrimOpType SPIRV.FSinh a = a -> a
  op = sinh
  opName = SPIRV.FloatOp SPIRV.FSinh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FSinh)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FCosh (a :: Type) where
  type PrimOpType SPIRV.FCosh a = a -> a
  op = cosh
  opName = SPIRV.FloatOp SPIRV.FCosh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FCosh)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FTanh (a :: Type) where
  type PrimOpType SPIRV.FTanh a = a -> a
  op = tanh
  opName = SPIRV.FloatOp SPIRV.FTanh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FTanh)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAsinh (a :: Type) where
  type PrimOpType SPIRV.FAsinh a = a -> a
  op = asinh
  opName = SPIRV.FloatOp SPIRV.FAsinh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAsinh)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAcosh (a :: Type) where
  type PrimOpType SPIRV.FAcosh a = a -> a
  op = acosh
  opName = SPIRV.FloatOp SPIRV.FAcosh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAcosh)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FAtanh (a :: Type) where
  type PrimOpType SPIRV.FAtanh a = a -> a
  op = atanh
  opName = SPIRV.FloatOp SPIRV.FAtanh (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAtanh)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, RealFloat a) => PrimOp SPIRV.FAtan2 (a :: Type) where
  type PrimOpType SPIRV.FAtan2 a = a -> a -> a
  op = atan2
  opName = SPIRV.FloatOp SPIRV.FAtan2 (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FAtan2)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FPow (a :: Type) where
  type PrimOpType SPIRV.FPow a = a -> a -> a
  op = (**)
  opName = SPIRV.FloatOp SPIRV.FPow (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FPow)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FExp (a :: Type) where
  type PrimOpType SPIRV.FExp a = a -> a
  op = exp
  opName = SPIRV.FloatOp SPIRV.FExp (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FExp)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FLog (a :: Type) where
  type PrimOpType SPIRV.FLog a = a -> a
  op = log
  opName = SPIRV.FloatOp SPIRV.FLog (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FLog)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FSqrt (a :: Type) where
  type PrimOpType SPIRV.FSqrt a = a -> a
  op = sqrt
  opName = SPIRV.FloatOp SPIRV.FSqrt (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FSqrt)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))
instance (ScalarTy a, Floating a) => PrimOp SPIRV.FInvsqrt (a :: Type) where
  type PrimOpType SPIRV.FInvsqrt a = a -> a
  op = recip . sqrt
  opName = SPIRV.FloatOp SPIRV.FInvsqrt (scalarTy @a)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.FInvsqrt)
  vectorisation = Just $ VecPrimOpType (Proxy @(V n a))


-- numeric conversions
instance ( ScalarTy a, ScalarTy b, Convert '(a,b) ) => PrimOp SPIRV.Convert '(a,b) where
  type PrimOpType SPIRV.Convert '(a,b) = a -> b
  op = convert
  opName = SPIRV.ConvOp SPIRV.Convert (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.Convert)
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))
instance ( ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp SPIRV.CTruncate '(a,b) where
  type PrimOpType SPIRV.CTruncate '(a,b) = a -> b
  op = truncate
  opName = SPIRV.ConvOp SPIRV.CTruncate (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CTruncate)
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))
instance ( ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp SPIRV.CRound '(a,b) where
  type PrimOpType SPIRV.CRound '(a,b) = a -> b
  op = round
  opName = SPIRV.ConvOp SPIRV.CRound (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CRound)
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))
instance ( ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp SPIRV.CFloor '(a,b) where
  type PrimOpType SPIRV.CFloor '(a,b) = a -> b
  op = floor
  opName = SPIRV.ConvOp SPIRV.CFloor (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CFloor)
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))
instance ( ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp SPIRV.CCeiling '(a,b) where
  type PrimOpType SPIRV.CCeiling '(a,b) = a -> b
  op = ceiling
  opName = SPIRV.ConvOp SPIRV.CCeiling (scalarTy @a) (scalarTy @b)
  vectorisation :: forall n. KnownNat n => Maybe (VecPrimOpType n SPIRV.CCeiling)
  vectorisation = Just $ VecPrimOpType (Proxy @'(V n a, V n b))

-- geometry primitive instructions
instance PrimOp SPIRV.EmitGeometryVertex (i :: ProgramState) where
  type PrimOpType SPIRV.EmitGeometryVertex i = (() := i) i
  op = AtKey ()
  opName = SPIRV.GeomOp SPIRV.EmitGeometryVertex
instance PrimOp SPIRV.EndGeometryPrimitive (i :: ProgramState) where
  type PrimOpType SPIRV.EndGeometryPrimitive i = (() := i) i
  op = AtKey ()
  opName = SPIRV.GeomOp SPIRV.EndGeometryPrimitive

-- vector operations
-- doing it by hand because I'm an idiot who doesn't know better

val :: forall n. KnownNat n => Word32
val = fromIntegral ( natVal (Proxy @n) )


instance ( KnownNat n, ScalarTy a, Bits a ) => PrimOp ('Vectorise SPIRV.BitAnd) (V n a) where
  type PrimOpType ('Vectorise SPIRV.BitAnd) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (.&.)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitAnd @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Bits a  ) => PrimOp ('Vectorise SPIRV.BitOr) (V n a) where
  type PrimOpType ('Vectorise SPIRV.BitOr) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (.|.)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitOr @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Bits a  ) => PrimOp ('Vectorise SPIRV.BitXor) (V n a) where
  type PrimOpType ('Vectorise SPIRV.BitXor) (V n a) = V n a -> V n a -> V n a
  op = liftA2 xor
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitXor @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Bits a ) => PrimOp ('Vectorise SPIRV.BitNot) (V n a) where
  type PrimOpType ('Vectorise SPIRV.BitNot) (V n a) = V n a -> V n a
  op = fmap complement
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitNot @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy b, BitShift '(b,s) ) => PrimOp ('Vectorise SPIRV.BitShiftRightArithmetic) '(V n b, V n s) where
  type PrimOpType ('Vectorise SPIRV.BitShiftRightArithmetic) '(V n b, V n s) = V n b -> V n s -> V n b
  op = liftA2 shiftR
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitShiftRightArithmetic @'(b,s))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy b, BitShift '(b,s) ) => PrimOp ('Vectorise SPIRV.BitShiftLeft) '(V n b, V n s) where
  type PrimOpType ('Vectorise SPIRV.BitShiftLeft) '(V n b, V n s) = V n b -> V n s -> V n b
  op = liftA2 shiftL
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitShiftLeft @'(b,s))) (val @n) (scalarTy @b)

instance ( KnownNat n, ScalarTy a, AdditiveMonoid a ) => PrimOp ('Vectorise SPIRV.Add) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Add) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (+)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Add @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, AdditiveGroup a ) => PrimOp ('Vectorise SPIRV.Sub) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Sub) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (-)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Sub @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, AdditiveGroup a ) => PrimOp ('Vectorise SPIRV.Neg) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Neg) (V n a) = V n a -> V n a
  op = fmap negate
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Neg @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Semiring a ) => PrimOp ('Vectorise SPIRV.Mul) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Mul) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (*)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Mul @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Signed a ) => PrimOp ('Vectorise SPIRV.Abs) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Abs) (V n a) = V n a -> V n a
  op = fmap abs
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Abs @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Signed a ) => PrimOp ('Vectorise SPIRV.Sign) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Sign) (V n a) = V n a -> V n a
  op = fmap signum
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Sign @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, DivisionRing a ) => PrimOp ('Vectorise SPIRV.Div) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Div) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (/)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Div @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Archimedean a ) => PrimOp ('Vectorise SPIRV.Mod) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Mod) (V n a) = V n a -> V n a -> V n a
  op = liftA2 mod
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Mod @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Archimedean a ) => PrimOp ('Vectorise SPIRV.Rem) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Rem) (V n a) = V n a -> V n a -> V n a
  op = liftA2 rem
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Rem @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Archimedean a ) => PrimOp ('Vectorise SPIRV.Quot) (V n a) where
  type PrimOpType ('Vectorise SPIRV.Quot) (V n a) = V n a -> V n a -> V n a
  op = liftA2 div
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Quot @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy a, ScalarTy b, Convert '(a,b) ) => PrimOp ('Vectorise SPIRV.Convert) '(V n a, V n b) where
  type PrimOpType ('Vectorise SPIRV.Convert) '(V n a, V n b) = V n a -> V n b
  op = fmap convert
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Convert @'(a,b))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp ('Vectorise SPIRV.CTruncate) '(V n a, V n b) where
  type PrimOpType ('Vectorise SPIRV.CTruncate) '(V n a, V n b) = V n a -> V n b
  op = fmap truncate
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CTruncate @'(a,b))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp ('Vectorise SPIRV.CRound) '(V n a, V n b) where
  type PrimOpType ('Vectorise SPIRV.CRound) '(V n a, V n b) = V n a -> V n b
  op = fmap round
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CRound @'(a,b))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp ('Vectorise SPIRV.CFloor) '(V n a, V n b) where
  type PrimOpType ('Vectorise SPIRV.CFloor) '(V n a, V n b) = V n a -> V n b
  op = fmap floor
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CFloor @'(a,b))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a, ScalarTy b, Rounding '(a,b) ) => PrimOp ('Vectorise SPIRV.CCeiling) '(V n a, V n b) where
  type PrimOpType ('Vectorise SPIRV.CCeiling) '(V n a, V n b) = V n a -> V n b
  op = fmap ceiling
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CCeiling @'(a,b))) (val @n) (scalarTy @b)

instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FSin) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FSin) (V n a) = V n a -> V n a
  op = fmap sin
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSin @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FCos) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FCos) (V n a) = V n a -> V n a
  op = fmap cos
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FCos @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FTan) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FTan) (V n a) = V n a -> V n a
  op = fmap tan
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FTan @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAsin) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FAsin) (V n a) = V n a -> V n a
  op = fmap asin
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAsin @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAcos) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FAcos) (V n a) = V n a -> V n a
  op = fmap acos
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAcos @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAtan) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FAtan) (V n a) = V n a -> V n a
  op = fmap atan
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtan @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FSinh) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FSinh) (V n a) = V n a -> V n a
  op = fmap sinh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSinh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FCosh) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FCosh) (V n a) = V n a -> V n a
  op = fmap cosh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FCosh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FTanh) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FTanh) (V n a) = V n a -> V n a
  op = fmap tanh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FTanh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAsinh) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FAsinh) (V n a) = V n a -> V n a
  op = fmap asinh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAsinh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAcosh) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FAcosh) (V n a) = V n a -> V n a
  op = fmap acosh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAcosh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FAtanh) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FAtanh) (V n a) = V n a -> V n a
  op = fmap atanh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtanh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, RealFloat a ) => PrimOp ('Vectorise SPIRV.FAtan2) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FAtan2) (V n a) = V n a -> V n a -> V n a
  op = liftA2 atan2
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtan2 @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FPow) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FPow) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (**)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FPow @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FExp) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FExp) (V n a) = V n a -> V n a
  op = fmap exp
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FExp @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FLog) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FLog) (V n a) = V n a -> V n a
  op = fmap log
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FLog @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FSqrt) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FSqrt) (V n a) = V n a -> V n a
  op = fmap sqrt
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSqrt @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp ('Vectorise SPIRV.FInvsqrt) (V n a) where
  type PrimOpType ('Vectorise SPIRV.FInvsqrt) (V n a) = V n a -> V n a
  op = fmap ( recip . sqrt )
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FInvsqrt @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy a, Floating a ) => PrimOp SPIRV.DotV (V n a) where
  type PrimOpType SPIRV.DotV (V n a) = V n a -> V n a -> a
  op = dot
  opName = SPIRV.VecOp SPIRV.DotV (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Semiring a ) => PrimOp SPIRV.VMulK (V n a) where
  type PrimOpType SPIRV.VMulK (V n a) = V n a -> a -> V n a
  op = (^*)
  opName = SPIRV.VecOp SPIRV.VMulK (val @n) (scalarTy @a)
instance ( n ~ 3, ScalarTy a, Floating a ) => PrimOp SPIRV.CrossV (V n a) where
  type PrimOpType SPIRV.CrossV (V n a) = V 3 a -> V 3 a -> V 3 a
  op = cross
  opName = SPIRV.VecOp SPIRV.CrossV 3 (scalarTy @a)
instance ( KnownNat n, ScalarTy a, Floating a) => PrimOp SPIRV.NormaliseV (V n a) where
  type PrimOpType SPIRV.NormaliseV (V n a) = V n a -> V n a
  op = normalise
  opName = SPIRV.VecOp SPIRV.NormaliseV (val @n) (scalarTy @a)


instance ( KnownNat i, KnownNat j, ScalarTy a, Floating a ) => PrimOp SPIRV.MMulK '(a,i,j) where
  type PrimOpType SPIRV.MMulK '(a,i,j) = M i j a -> a -> M i j a
  op = (!*)
  opName = SPIRV.MatOp SPIRV.MMulK (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a, Floating a ) => PrimOp SPIRV.MMulV '(a,i,j) where
  type PrimOpType SPIRV.MMulV '(a,i,j) = M i j a -> V j a -> V i a
  op = (!*^)
  opName = SPIRV.MatOp SPIRV.MMulV (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a, Floating a ) => PrimOp SPIRV.VMulM '(a,i,j) where
  type PrimOpType SPIRV.VMulM '(a,i,j) = V i a -> M i j a -> V j a
  op = (^*!)
  opName = SPIRV.MatOp SPIRV.VMulM (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, KnownNat k, ScalarTy a, Floating a ) => PrimOp SPIRV.MMulM '(a,i,j,k) where
  type PrimOpType SPIRV.MMulM '(a,i,j,k) = M i j a -> M j k a -> M i k a
  op = (!*!)
  opName = SPIRV.MatOp SPIRV.MMulM (val @i) (val @k) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a, Floating a ) => PrimOp SPIRV.Transp '(a,i,j) where
  type PrimOpType SPIRV.Transp '(a,i,j) = M i j a -> M j i a
  op = transpose
  opName = SPIRV.MatOp SPIRV.Transp (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, ScalarTy a, Floating a ) => PrimOp SPIRV.Det '(a,i) where
  type PrimOpType SPIRV.Det '(a,i) = M i i a -> a
  op = determinant
  opName = SPIRV.MatOp SPIRV.Det (val @i) (val @i) (scalarTy @a)
instance ( KnownNat i, ScalarTy a, Floating a ) => PrimOp SPIRV.Inv '(a,i) where
  type PrimOpType SPIRV.Inv '(a,i) = M i i a -> M i i a
  op = inverse
  opName = SPIRV.MatOp SPIRV.Inv (val @i) (val @i) (scalarTy @a)
