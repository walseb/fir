{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module FIR.Prim.Op where

-- base
import Prelude
  ( Maybe(..), Bool, (.)
  , Functor(..)
  , fromIntegral )
import Control.Applicative
  ( liftA2 )
import Data.Kind
  ( Type, Constraint )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( KnownNat, natVal )

-- fir
import Control.Monad.Indexed
  ( (:=)(AtKey) )
import FIR.ASTState
  ( ASTState )
import FIR.Prim.Singletons
  ( PrimTy, ScalarTy
  , primTy, scalarTy
  )
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
  type PrimOpConstraint op a :: Constraint
  type PrimOpType op a :: Type
  op :: PrimOpConstraint op a => PrimOpType op a
  opName :: SPIRV.PrimOp
  opSing :: Maybe (SPrimOp a op)
  opSing = Nothing

-------------------------------------------------------------------------------
-- singletons

data SPrimOp (a :: k) (op :: opKind) :: Type where
  {-
  SBoolOr  :: SPrimOp Bool SPIRV.BoolOr
  SBoolAnd :: SPrimOp Bool SPIRV.BoolAnd
  SBoolNot :: SPrimOp Bool SPIRV.BoolNot
  SEqual    :: SPrimOp (a :: Type) SPIRV.Equal
  SNotEqual :: SPrimOp (a :: Type) SPIRV.NotEqual
  SGT  :: SPrimOp (a :: Type) SPIRV.GT
  SGTE :: SPrimOp (a :: Type) SPIRV.GTE
  SLT  :: SPrimOp (a :: Type) SPIRV.LT
  SLTE :: SPrimOp (a :: Type) SPIRV.LTE
  SMin :: SPrimOp (a :: Type) SPIRV.Min
  SMax :: SPrimOp (a :: Type) SPIRV.Max
  -}
  SBitAnd :: ScalarTy a => SPrimOp a SPIRV.BitAnd
  SBitOr  :: ScalarTy a => SPrimOp a SPIRV.BitOr
  SBitXor :: ScalarTy a => SPrimOp a SPIRV.BitXor
  SBitNot :: ScalarTy a => SPrimOp a SPIRV.BitNot
  SBitShiftRightArithmetic :: (a ~ '(b,s), ScalarTy b) => SPrimOp (a :: (Type,Type)) SPIRV.BitShiftRightArithmetic
  SBitShiftLeft            :: (a ~ '(b,s), ScalarTy b) => SPrimOp (a :: (Type,Type)) SPIRV.BitShiftLeft
  SAdd  :: ScalarTy a => SPrimOp a SPIRV.Add
  SMul  :: ScalarTy a => SPrimOp a SPIRV.Mul
  SSub  :: ScalarTy a => SPrimOp a SPIRV.Sub
  SNeg  :: ScalarTy a => SPrimOp a SPIRV.Neg
  SAbs  :: ScalarTy a => SPrimOp a SPIRV.Abs
  SSign :: ScalarTy a => SPrimOp a SPIRV.Sign
  SDiv  :: ScalarTy a => SPrimOp a SPIRV.Div
  SMod  :: ScalarTy a => SPrimOp a SPIRV.Mod
  SRem  :: ScalarTy a => SPrimOp a SPIRV.Rem
  SSin     :: ScalarTy a => SPrimOp a SPIRV.FSin
  SCos     :: ScalarTy a => SPrimOp a SPIRV.FCos
  STan     :: ScalarTy a => SPrimOp a SPIRV.FTan
  SAsin    :: ScalarTy a => SPrimOp a SPIRV.FAsin
  SAcos    :: ScalarTy a => SPrimOp a SPIRV.FAcos
  SAtan    :: ScalarTy a => SPrimOp a SPIRV.FAtan
  SSinh    :: ScalarTy a => SPrimOp a SPIRV.FSinh
  SCosh    :: ScalarTy a => SPrimOp a SPIRV.FCosh
  STanh    :: ScalarTy a => SPrimOp a SPIRV.FTanh
  SAsinh   :: ScalarTy a => SPrimOp a SPIRV.FAsinh
  SAcosh   :: ScalarTy a => SPrimOp a SPIRV.FAcosh
  SAtanh   :: ScalarTy a => SPrimOp a SPIRV.FAtanh
  SAtan2   :: ScalarTy a => SPrimOp a SPIRV.FAtan2
  SPow     :: ScalarTy a => SPrimOp a SPIRV.FPow
  SExp     :: ScalarTy a => SPrimOp a SPIRV.FExp
  SLog     :: ScalarTy a => SPrimOp a SPIRV.FLog
  SSqrt    :: ScalarTy a => SPrimOp a SPIRV.FSqrt
  SInvsqrt :: ScalarTy a => SPrimOp a SPIRV.FInvsqrt
  SConvert   :: (ab ~ '(a,b), ScalarTy a, ScalarTy b) => SPrimOp (ab :: (Type,Type)) SPIRV.Convert
  STruncate  :: (ab ~ '(a,b), ScalarTy a, ScalarTy b) => SPrimOp (ab :: (Type,Type)) SPIRV.CTruncate
  SRound     :: (ab ~ '(a,b), ScalarTy a, ScalarTy b) => SPrimOp (ab :: (Type,Type)) SPIRV.CRound
  SFloor     :: (ab ~ '(a,b), ScalarTy a, ScalarTy b) => SPrimOp (ab :: (Type,Type)) SPIRV.CFloor
  SCeiling   :: (ab ~ '(a,b), ScalarTy a, ScalarTy b) => SPrimOp (ab :: (Type,Type)) SPIRV.CCeiling

-------------------------------------------------------------------------------
-- instances

-- boolean operations
instance PrimOp SPIRV.BoolOr Bool where
  type PrimOpConstraint SPIRV.BoolOr Bool = ()
  type PrimOpType SPIRV.BoolOr Bool = Bool -> Bool -> Bool
  op = (||)
  opName = SPIRV.BoolOp SPIRV.BoolOr
instance PrimOp SPIRV.BoolAnd Bool where
  type PrimOpConstraint SPIRV.BoolAnd Bool = ()
  type PrimOpType SPIRV.BoolAnd Bool = Bool -> Bool -> Bool
  op = (&&)
  opName = SPIRV.BoolOp SPIRV.BoolAnd
instance PrimOp SPIRV.BoolNot Bool where
  type PrimOpConstraint SPIRV.BoolNot Bool = ()
  type PrimOpType SPIRV.BoolNot Bool = Bool -> Bool
  op = not
  opName = SPIRV.BoolOp SPIRV.BoolNot

-- equality operations
instance PrimTy a => PrimOp SPIRV.Equal (a :: Type) where
  type PrimOpConstraint SPIRV.Equal a = (Eq a, Logic a ~ Bool)
  type PrimOpType SPIRV.Equal a = a -> a -> Bool
  op = (==)
  opName = SPIRV.EqOp SPIRV.Equal (primTy @a)
instance PrimTy a => PrimOp SPIRV.NotEqual (a :: Type) where
  type PrimOpConstraint SPIRV.NotEqual a = (PrimTy a, Eq a, Logic a ~ Bool)
  type PrimOpType SPIRV.NotEqual a = a -> a -> Bool
  op = (/=)
  opName = SPIRV.EqOp SPIRV.NotEqual (primTy @a)

-- comparison
instance ScalarTy a => PrimOp SPIRV.GT (a :: Type) where
  type PrimOpConstraint SPIRV.GT a = (Ord a, Logic a ~ Bool)
  type PrimOpType SPIRV.GT a = a -> a -> Bool
  op = (>)
  opName = SPIRV.OrdOp SPIRV.GT (scalarTy @a)
instance ScalarTy a => PrimOp SPIRV.GTE (a :: Type) where
  type PrimOpConstraint SPIRV.GTE a = (Ord a, Logic a ~ Bool)
  type PrimOpType SPIRV.GTE a = a -> a -> Bool
  op = (>=)
  opName = SPIRV.OrdOp SPIRV.GTE (scalarTy @a)
instance ScalarTy a => PrimOp SPIRV.LT (a :: Type) where
  type PrimOpConstraint SPIRV.LT a = (Ord a, Logic a ~ Bool)
  type PrimOpType SPIRV.LT a = a -> a -> Bool
  op = (<)
  opName = SPIRV.OrdOp SPIRV.LT (scalarTy @a)
instance ScalarTy a => PrimOp SPIRV.LTE (a :: Type) where
  type PrimOpConstraint SPIRV.LTE a = (Ord a, Logic a ~ Bool)
  type PrimOpType SPIRV.LTE a = a -> a -> Bool
  op = (<=)
  opName = SPIRV.OrdOp SPIRV.LTE (scalarTy @a)
instance ScalarTy a => PrimOp SPIRV.Min (a :: Type) where
  type PrimOpConstraint SPIRV.Min a = (Ord a, Logic a ~ Bool)
  type PrimOpType SPIRV.Min a = a -> a -> a
  op = min
  opName = SPIRV.OrdOp SPIRV.Min (scalarTy @a)
instance ScalarTy a => PrimOp SPIRV.Max (a :: Type) where
  type PrimOpConstraint SPIRV.Max a = (Ord a, Logic a ~ Bool)
  type PrimOpType SPIRV.Max a = a -> a -> a
  op = max
  opName = SPIRV.OrdOp SPIRV.Max (scalarTy @a)

-- bitwise operations
instance ScalarTy a => PrimOp SPIRV.BitAnd (a :: Type) where
  type PrimOpConstraint SPIRV.BitAnd a = Bits a
  type PrimOpType SPIRV.BitAnd a = a -> a -> a
  op = (.&.)
  opName = SPIRV.BitOp SPIRV.BitAnd (scalarTy @a)
  opSing = Just SBitAnd
instance ScalarTy a => PrimOp SPIRV.BitOr (a :: Type) where
  type PrimOpConstraint SPIRV.BitOr a = Bits a
  type PrimOpType SPIRV.BitOr a = a -> a -> a
  op = (.|.)
  opName = SPIRV.BitOp SPIRV.BitOr (scalarTy @a)
  opSing = Just SBitOr
instance ScalarTy a => PrimOp SPIRV.BitXor (a :: Type) where
  type PrimOpConstraint SPIRV.BitXor a = Bits a
  type PrimOpType SPIRV.BitXor a = a -> a -> a
  op = xor
  opName = SPIRV.BitOp SPIRV.BitXor (scalarTy @a)
  opSing = Just SBitXor
instance ScalarTy a => PrimOp SPIRV.BitNot (a :: Type) where
  type PrimOpConstraint SPIRV.BitNot a = Bits a
  type PrimOpType SPIRV.BitNot a = a -> a
  op = complement
  opName = SPIRV.BitOp SPIRV.BitNot (scalarTy @a)
  opSing = Just SBitNot
-- no logical right bit shift
instance forall (b :: Type) (s :: Type). ScalarTy b => PrimOp SPIRV.BitShiftRightArithmetic '(b,s) where
  type PrimOpConstraint SPIRV.BitShiftRightArithmetic '(b,s) = BitShift '(b,s)
  type PrimOpType SPIRV.BitShiftRightArithmetic '(b,s) = b -> s -> b
  op = shiftR
  opName = SPIRV.BitOp SPIRV.BitShiftRightArithmetic (scalarTy @b)
  opSing = Just SBitShiftRightArithmetic
instance forall (b :: Type) (s :: Type). ScalarTy b => PrimOp SPIRV.BitShiftLeft '(b,s) where
  type PrimOpConstraint SPIRV.BitShiftLeft '(b,s) = BitShift '(b,s)
  type PrimOpType SPIRV.BitShiftLeft '(b,s) = b -> s -> b
  op = shiftL
  opName = SPIRV.BitOp SPIRV.BitShiftLeft (scalarTy @b)
  opSing = Just SBitShiftLeft

-- numeric operations
instance ScalarTy a => PrimOp SPIRV.Add (a :: Type) where
  type PrimOpConstraint SPIRV.Add a = AdditiveMonoid a
  type PrimOpType SPIRV.Add a = a -> a -> a
  op = (+)
  opName = SPIRV.NumOp SPIRV.Add (scalarTy @a)
  opSing = Just SAdd
instance ScalarTy a => PrimOp SPIRV.Mul (a :: Type) where
  type PrimOpConstraint SPIRV.Mul a = Semiring a
  type PrimOpType SPIRV.Mul a = a -> a -> a
  op = (*)
  opName = SPIRV.NumOp SPIRV.Mul (scalarTy @a)
  opSing = Just SMul
instance ScalarTy a => PrimOp SPIRV.Sub (a :: Type) where
  type PrimOpConstraint SPIRV.Sub a = AdditiveGroup a
  type PrimOpType SPIRV.Sub a = a -> a -> a
  op = (-)
  opName = SPIRV.NumOp SPIRV.Sub (scalarTy @a)
  opSing = Just SSub
instance ScalarTy a => PrimOp SPIRV.Neg (a :: Type) where
  type PrimOpConstraint SPIRV.Neg a = AdditiveGroup a
  type PrimOpType SPIRV.Neg a = a -> a
  op = negate
  opName = SPIRV.NumOp SPIRV.Neg (scalarTy @a)
  opSing = Just SNeg
instance ScalarTy a => PrimOp SPIRV.Abs (a :: Type) where
  type PrimOpConstraint SPIRV.Abs a = Signed a
  type PrimOpType SPIRV.Abs a = a -> a
  op = abs
  opName = SPIRV.NumOp SPIRV.Abs (scalarTy @a)
  opSing = Just SAbs
instance ScalarTy a => PrimOp SPIRV.Sign (a :: Type) where
  type PrimOpConstraint SPIRV.Sign a = Signed a
  type PrimOpType SPIRV.Sign a = a -> a
  op = signum
  opName = SPIRV.NumOp SPIRV.Sign (scalarTy @a)
  opSing = Just SSign
instance ScalarTy a => PrimOp SPIRV.Div (a :: Type) where
  type PrimOpConstraint SPIRV.Div a = DivisionRing a
  type PrimOpType SPIRV.Div a = a -> a -> a
  op = (/)
  opName = SPIRV.NumOp SPIRV.Div (scalarTy @a)
  opSing = Just SDiv
instance ScalarTy a => PrimOp SPIRV.Mod (a :: Type) where
  type PrimOpConstraint SPIRV.Mod a = Archimedean a
  type PrimOpType SPIRV.Mod a = a -> a -> a
  op = mod
  opName = SPIRV.NumOp SPIRV.Mod (scalarTy @a)
  opSing = Just SMod
instance ScalarTy a => PrimOp SPIRV.Rem (a :: Type) where
  type PrimOpConstraint SPIRV.Rem a = Archimedean a
  type PrimOpType SPIRV.Rem a = a -> a -> a
  op = rem
  opName = SPIRV.NumOp SPIRV.Rem (scalarTy @a)
  opSing = Just SRem


-- floating operations
instance ScalarTy a => PrimOp SPIRV.FSin (a :: Type) where
  type PrimOpConstraint SPIRV.FSin a = Floating a
  type PrimOpType SPIRV.FSin a = a -> a
  op = sin
  opName = SPIRV.FloatOp SPIRV.FSin (scalarTy @a)
  opSing = Just SSin
instance ScalarTy a => PrimOp SPIRV.FCos (a :: Type) where
  type PrimOpConstraint SPIRV.FCos a = Floating a
  type PrimOpType SPIRV.FCos a = a -> a
  op = cos
  opName = SPIRV.FloatOp SPIRV.FCos (scalarTy @a)
  opSing = Just SCos
instance ScalarTy a => PrimOp SPIRV.FTan (a :: Type) where
  type PrimOpConstraint SPIRV.FTan a = Floating a
  type PrimOpType SPIRV.FTan a = a -> a
  op = tan
  opName = SPIRV.FloatOp SPIRV.FTan (scalarTy @a)
  opSing = Just STan
instance ScalarTy a => PrimOp SPIRV.FAsin (a :: Type) where
  type PrimOpConstraint SPIRV.FAsin a = Floating a
  type PrimOpType SPIRV.FAsin a = a -> a
  op = asin
  opName = SPIRV.FloatOp SPIRV.FAsin (scalarTy @a)
  opSing = Just SAsin
instance ScalarTy a => PrimOp SPIRV.FAcos (a :: Type) where
  type PrimOpConstraint SPIRV.FAcos a = Floating a
  type PrimOpType SPIRV.FAcos a = a -> a
  op = acos
  opName = SPIRV.FloatOp SPIRV.FAcos (scalarTy @a)
  opSing = Just SAcos
instance ScalarTy a => PrimOp SPIRV.FAtan (a :: Type) where
  type PrimOpConstraint SPIRV.FAtan a = Floating a
  type PrimOpType SPIRV.FAtan a = a -> a
  op = atan
  opName = SPIRV.FloatOp SPIRV.FAtan (scalarTy @a)
  opSing = Just SAtan
instance ScalarTy a => PrimOp SPIRV.FSinh (a :: Type) where
  type PrimOpConstraint SPIRV.FSinh a = Floating a
  type PrimOpType SPIRV.FSinh a = a -> a
  op = sinh
  opName = SPIRV.FloatOp SPIRV.FSinh (scalarTy @a)
  opSing = Just SSinh
instance ScalarTy a => PrimOp SPIRV.FCosh (a :: Type) where
  type PrimOpConstraint SPIRV.FCosh a = Floating a
  type PrimOpType SPIRV.FCosh a = a -> a
  op = cosh
  opName = SPIRV.FloatOp SPIRV.FCosh (scalarTy @a)
  opSing = Just SCosh
instance ScalarTy a => PrimOp SPIRV.FTanh (a :: Type) where
  type PrimOpConstraint SPIRV.FTanh a = Floating a
  type PrimOpType SPIRV.FTanh a = a -> a
  op = tanh
  opName = SPIRV.FloatOp SPIRV.FTanh (scalarTy @a)
  opSing = Just STanh
instance ScalarTy a => PrimOp SPIRV.FAsinh (a :: Type) where
  type PrimOpConstraint SPIRV.FAsinh a = Floating a
  type PrimOpType SPIRV.FAsinh a = a -> a
  op = asinh
  opName = SPIRV.FloatOp SPIRV.FAsinh (scalarTy @a)
  opSing = Just SAsinh
instance ScalarTy a => PrimOp SPIRV.FAcosh (a :: Type) where
  type PrimOpConstraint SPIRV.FAcosh a = Floating a
  type PrimOpType SPIRV.FAcosh a = a -> a
  op = acosh
  opName = SPIRV.FloatOp SPIRV.FAcosh (scalarTy @a)
  opSing = Just SAcosh
instance ScalarTy a => PrimOp SPIRV.FAtanh (a :: Type) where
  type PrimOpConstraint SPIRV.FAtanh a = Floating a
  type PrimOpType SPIRV.FAtanh a = a -> a
  op = atanh
  opName = SPIRV.FloatOp SPIRV.FAtanh (scalarTy @a)
  opSing = Just SAtanh
instance ScalarTy a => PrimOp SPIRV.FAtan2 (a :: Type) where
  type PrimOpConstraint SPIRV.FAtan2 a = RealFloat a
  type PrimOpType SPIRV.FAtan2 a = a -> a -> a
  op = atan2
  opName = SPIRV.FloatOp SPIRV.FAtan2 (scalarTy @a)
  opSing = Just SAtan2
instance ScalarTy a => PrimOp SPIRV.FPow (a :: Type) where
  type PrimOpConstraint SPIRV.FPow a = Floating a
  type PrimOpType SPIRV.FPow a = a -> a -> a
  op = (**)
  opName = SPIRV.FloatOp SPIRV.FPow (scalarTy @a)
  opSing = Just SPow
instance ScalarTy a => PrimOp SPIRV.FExp (a :: Type) where
  type PrimOpConstraint SPIRV.FExp a = Floating a
  type PrimOpType SPIRV.FExp a = a -> a
  op = exp
  opName = SPIRV.FloatOp SPIRV.FExp (scalarTy @a)
  opSing = Just SExp
instance ScalarTy a => PrimOp SPIRV.FLog (a :: Type) where
  type PrimOpConstraint SPIRV.FLog a = Floating a
  type PrimOpType SPIRV.FLog a = a -> a
  op = log
  opName = SPIRV.FloatOp SPIRV.FLog (scalarTy @a)
  opSing = Just SLog
instance ScalarTy a => PrimOp SPIRV.FSqrt (a :: Type) where
  type PrimOpConstraint SPIRV.FSqrt a = Floating a
  type PrimOpType SPIRV.FSqrt a = a -> a
  op = sqrt
  opName = SPIRV.FloatOp SPIRV.FSqrt (scalarTy @a)
  opSing = Just SSqrt
instance ScalarTy a => PrimOp SPIRV.FInvsqrt (a :: Type) where
  type PrimOpConstraint SPIRV.FInvsqrt a = Floating a
  type PrimOpType SPIRV.FInvsqrt a = a -> a
  op = recip . sqrt
  opName = SPIRV.FloatOp SPIRV.FInvsqrt (scalarTy @a)
  opSing = Just SInvsqrt


-- numeric conversions
instance ( ScalarTy a, ScalarTy b ) => PrimOp SPIRV.Convert '(a,b) where
  type PrimOpConstraint SPIRV.Convert '(a,b) = Convert '(a,b)
  type PrimOpType SPIRV.Convert '(a,b) = a -> b
  op = convert
  opName = SPIRV.ConvOp SPIRV.Convert (scalarTy @a) (scalarTy @b)
  opSing = Just SConvert
instance ( ScalarTy a, ScalarTy b ) => PrimOp SPIRV.CTruncate '(a,b) where
  type PrimOpConstraint SPIRV.CTruncate '(a,b) = Rounding '(a,b)
  type PrimOpType SPIRV.CTruncate '(a,b) = a -> b
  op = truncate
  opName = SPIRV.ConvOp SPIRV.CTruncate (scalarTy @a) (scalarTy @b)
  opSing = Just STruncate
instance ( ScalarTy a, ScalarTy b ) => PrimOp SPIRV.CRound '(a,b) where
  type PrimOpConstraint SPIRV.CRound '(a,b) = Rounding '(a,b)
  type PrimOpType SPIRV.CRound '(a,b) = a -> b
  op = round
  opName = SPIRV.ConvOp SPIRV.CRound (scalarTy @a) (scalarTy @b)
  opSing = Just SRound
instance ( ScalarTy a, ScalarTy b ) => PrimOp SPIRV.CFloor '(a,b) where
  type PrimOpConstraint SPIRV.CFloor '(a,b) = Rounding '(a,b)
  type PrimOpType SPIRV.CFloor '(a,b) = a -> b
  op = floor
  opName = SPIRV.ConvOp SPIRV.CFloor (scalarTy @a) (scalarTy @b)
  opSing = Just SFloor
instance ( ScalarTy a, ScalarTy b ) => PrimOp SPIRV.CCeiling '(a,b) where
  type PrimOpConstraint SPIRV.CCeiling '(a,b) = Rounding '(a,b)
  type PrimOpType SPIRV.CCeiling '(a,b) = a -> b
  op = ceiling
  opName = SPIRV.ConvOp SPIRV.CCeiling (scalarTy @a) (scalarTy @b)
  opSing = Just SCeiling

-- geometry primitive instructions
instance PrimOp SPIRV.EmitGeometryVertex (i :: ASTState) where
  type PrimOpConstraint SPIRV.EmitGeometryVertex i = ()
  type PrimOpType SPIRV.EmitGeometryVertex i = (() := i) i
  op = AtKey ()
  opName = SPIRV.GeomOp SPIRV.EmitGeometryVertex
  opSing = Nothing
instance PrimOp SPIRV.EndGeometryPrimitive (i :: ASTState) where
  type PrimOpConstraint SPIRV.EndGeometryPrimitive i = ()
  type PrimOpType SPIRV.EndGeometryPrimitive i = (() := i) i
  op = AtKey ()
  opName = SPIRV.GeomOp SPIRV.EndGeometryPrimitive
  opSing = Nothing

-- vector operations
-- doing it by hand because I'm an idiot who doesn't know better
newtype Vectorise a = Vectorise a

val :: forall n. KnownNat n => Word32
val = fromIntegral ( natVal (Proxy @n) )


instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.BitAnd) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.BitAnd) (V n a) = Bits a
  type PrimOpType ('Vectorise SPIRV.BitAnd) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (.&.)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitAnd @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.BitOr) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.BitOr) (V n a) = Bits a
  type PrimOpType ('Vectorise SPIRV.BitOr) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (.|.)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitOr @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.BitXor) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.BitXor) (V n a) = Bits a
  type PrimOpType ('Vectorise SPIRV.BitXor) (V n a) = V n a -> V n a -> V n a
  op = liftA2 xor
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitXor @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.BitNot) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.BitNot) (V n a) = Bits a
  type PrimOpType ('Vectorise SPIRV.BitNot) (V n a) = V n a -> V n a
  op = fmap complement
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Add @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy b ) => PrimOp ('Vectorise SPIRV.BitShiftRightArithmetic) '(V n b, V n s) where
  type PrimOpConstraint ('Vectorise SPIRV.BitShiftRightArithmetic) '(V n b, V n s) = BitShift '(b,s)
  type PrimOpType ('Vectorise SPIRV.BitShiftRightArithmetic) '(V n b, V n s) = V n b -> V n s -> V n b
  op = liftA2 shiftR
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitShiftRightArithmetic @'(b,s))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy b ) => PrimOp ('Vectorise SPIRV.BitShiftLeft) '(V n b, V n s) where
  type PrimOpConstraint ('Vectorise SPIRV.BitShiftLeft) '(V n b, V n s) = BitShift '(b,s)
  type PrimOpType ('Vectorise SPIRV.BitShiftLeft) '(V n b, V n s) = V n b -> V n s -> V n b
  op = liftA2 shiftL
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.BitShiftLeft @'(b,s))) (val @n) (scalarTy @b)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Add) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Add) (V n a) = AdditiveMonoid a
  type PrimOpType ('Vectorise SPIRV.Add) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (+)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Add @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Sub) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Sub) (V n a) = AdditiveGroup a
  type PrimOpType ('Vectorise SPIRV.Sub) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (-)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Sub @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Neg) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Neg) (V n a) = AdditiveGroup a
  type PrimOpType ('Vectorise SPIRV.Neg) (V n a) = V n a -> V n a
  op = fmap negate
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Neg @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Mul) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Mul) (V n a) = Semiring a
  type PrimOpType ('Vectorise SPIRV.Mul) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (*)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Mul @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Abs) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Abs) (V n a) = Signed a
  type PrimOpType ('Vectorise SPIRV.Abs) (V n a) = V n a -> V n a
  op = fmap abs
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Abs @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Sign) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Sign) (V n a) = Signed a
  type PrimOpType ('Vectorise SPIRV.Sign) (V n a) = V n a -> V n a
  op = fmap signum
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Sign @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Div) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Div) (V n a) = DivisionRing a
  type PrimOpType ('Vectorise SPIRV.Div) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (/)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Div @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Mod) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Mod) (V n a) = Archimedean a
  type PrimOpType ('Vectorise SPIRV.Mod) (V n a) = V n a -> V n a -> V n a
  op = liftA2 mod
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Mod @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.Rem) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.Rem) (V n a) = Archimedean a
  type PrimOpType ('Vectorise SPIRV.Rem) (V n a) = V n a -> V n a -> V n a
  op = liftA2 mod
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Rem @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy a, ScalarTy b ) => PrimOp ('Vectorise SPIRV.Convert) '(V n a, V n b) where
  type PrimOpConstraint ('Vectorise SPIRV.Convert) '(V n a, V n b) = Convert '(a,b)
  type PrimOpType ('Vectorise SPIRV.Convert) '(V n a, V n b) = V n a -> V n b
  op = fmap convert
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.Convert @'(a,b))) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, ScalarTy b ) => PrimOp ('Vectorise SPIRV.CTruncate) '(V n a, V n b) where
  type PrimOpConstraint ('Vectorise SPIRV.CTruncate) '(V n a, V n b) = Rounding '(a,b)
  type PrimOpType ('Vectorise SPIRV.CTruncate) '(V n a, V n b) = V n a -> V n b
  op = fmap truncate
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CTruncate @'(a,b))) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, ScalarTy b ) => PrimOp ('Vectorise SPIRV.CRound) '(V n a, V n b) where
  type PrimOpConstraint ('Vectorise SPIRV.CRound) '(V n a, V n b) = Rounding '(a,b)
  type PrimOpType ('Vectorise SPIRV.CRound) '(V n a, V n b) = V n a -> V n b
  op = fmap round
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CRound @'(a,b))) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, ScalarTy b ) => PrimOp ('Vectorise SPIRV.CFloor) '(V n a, V n b) where
  type PrimOpConstraint ('Vectorise SPIRV.CFloor) '(V n a, V n b) = Rounding '(a,b)
  type PrimOpType ('Vectorise SPIRV.CFloor) '(V n a, V n b) = V n a -> V n b
  op = fmap floor
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CFloor @'(a,b))) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a, ScalarTy b ) => PrimOp ('Vectorise SPIRV.CCeiling) '(V n a, V n b) where
  type PrimOpConstraint ('Vectorise SPIRV.CCeiling) '(V n a, V n b) = Rounding '(a,b)
  type PrimOpType ('Vectorise SPIRV.CCeiling) '(V n a, V n b) = V n a -> V n b
  op = fmap ceiling
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.CCeiling @'(a,b))) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FSin) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FSin) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FSin) (V n a) = V n a -> V n a
  op = fmap sin
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSin @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FCos) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FCos) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FCos) (V n a) = V n a -> V n a
  op = fmap cos
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FCos @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FTan) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FTan) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FTan) (V n a) = V n a -> V n a
  op = fmap tan
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FTan @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FAsin) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FAsin) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FAsin) (V n a) = V n a -> V n a
  op = fmap asin
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAsin @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FAcos) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FAcos) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FAcos) (V n a) = V n a -> V n a
  op = fmap acos
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAcos @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FAtan) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FAtan) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FAtan) (V n a) = V n a -> V n a
  op = fmap atan
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtan @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FSinh) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FSinh) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FSinh) (V n a) = V n a -> V n a
  op = fmap sinh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSinh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FCosh) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FCosh) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FCosh) (V n a) = V n a -> V n a
  op = fmap cosh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FCosh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FTanh) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FTanh) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FTanh) (V n a) = V n a -> V n a
  op = fmap tanh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FTanh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FAsinh) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FAsinh) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FAsinh) (V n a) = V n a -> V n a
  op = fmap asinh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAsinh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FAcosh) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FAcosh) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FAcosh) (V n a) = V n a -> V n a
  op = fmap acosh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAcosh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FAtanh) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FAtanh) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FAtanh) (V n a) = V n a -> V n a
  op = fmap atanh
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtanh @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FAtan2) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FAtan2) (V n a) = RealFloat a
  type PrimOpType ('Vectorise SPIRV.FAtan2) (V n a) = V n a -> V n a -> V n a
  op = liftA2 atan2
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FAtan2 @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FPow) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FPow) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FPow) (V n a) = V n a -> V n a -> V n a
  op = liftA2 (**)
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FPow @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FExp) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FExp) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FExp) (V n a) = V n a -> V n a
  op = fmap exp
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FExp @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FLog) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FLog) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FLog) (V n a) = V n a -> V n a
  op = fmap log
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FLog @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FSqrt) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FSqrt) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FSqrt) (V n a) = V n a -> V n a
  op = fmap sqrt
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FSqrt @a)) (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp ('Vectorise SPIRV.FInvsqrt) (V n a) where
  type PrimOpConstraint ('Vectorise SPIRV.FInvsqrt) (V n a) = Floating a
  type PrimOpType ('Vectorise SPIRV.FInvsqrt) (V n a) = V n a -> V n a
  op = fmap ( recip . sqrt )
  opName = SPIRV.VecOp (SPIRV.Vectorise (opName @_ @_ @SPIRV.FInvsqrt @a)) (val @n) (scalarTy @a)

instance ( KnownNat n, ScalarTy a ) => PrimOp SPIRV.DotV (V n a) where
  type PrimOpConstraint SPIRV.DotV (V n a) = Floating a
  type PrimOpType SPIRV.DotV (V n a) = V n a -> V n a -> a
  op = dot
  opName = SPIRV.VecOp SPIRV.DotV (val @n) (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp SPIRV.VMulK (V n a) where
  type PrimOpConstraint SPIRV.VMulK (V n a) = Semiring a
  type PrimOpType SPIRV.VMulK (V n a) = V n a -> a -> V n a
  op = (^*)
  opName = SPIRV.VecOp SPIRV.VMulK (val @n) (scalarTy @a)
instance ( n ~ 3, ScalarTy a ) => PrimOp SPIRV.CrossV (V n a) where
  type PrimOpConstraint SPIRV.CrossV (V n a) = Floating a
  type PrimOpType SPIRV.CrossV (V n a) = V 3 a -> V 3 a -> V 3 a
  op = cross
  opName = SPIRV.VecOp SPIRV.CrossV 3 (scalarTy @a)
instance ( KnownNat n, ScalarTy a ) => PrimOp SPIRV.NormaliseV (V n a) where
  type PrimOpConstraint SPIRV.NormaliseV (V n a) = Floating a
  type PrimOpType SPIRV.NormaliseV (V n a) = V n a -> V n a
  op = normalise
  opName = SPIRV.VecOp SPIRV.NormaliseV (val @n) (scalarTy @a)


instance ( KnownNat i, KnownNat j, ScalarTy a ) => PrimOp SPIRV.MMulK '(a,i,j) where
  type PrimOpConstraint SPIRV.MMulK '(a,i,j) = Floating a
  type PrimOpType SPIRV.MMulK '(a,i,j) = M i j a -> a -> M i j a
  op = (!*)
  opName = SPIRV.MatOp SPIRV.MMulK (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a ) => PrimOp SPIRV.MMulV '(a,i,j) where
  type PrimOpConstraint SPIRV.MMulV '(a,i,j) = Floating a
  type PrimOpType SPIRV.MMulV '(a,i,j) = M i j a -> V j a -> V i a
  op = (!*^)
  opName = SPIRV.MatOp SPIRV.MMulV (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a ) => PrimOp SPIRV.VMulM '(a,i,j) where
  type PrimOpConstraint SPIRV.VMulM '(a,i,j) = Floating a
  type PrimOpType SPIRV.VMulM '(a,i,j) = V i a -> M i j a -> V j a
  op = (^*!)
  opName = SPIRV.MatOp SPIRV.VMulM (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, KnownNat j, KnownNat k, ScalarTy a ) => PrimOp SPIRV.MMulM '(a,i,j,k) where
  type PrimOpConstraint SPIRV.MMulM '(a,i,j,k) = Floating a
  type PrimOpType SPIRV.MMulM '(a,i,j,k) = M i j a -> M j k a -> M i k a
  op = (!*!)
  opName = SPIRV.MatOp SPIRV.MMulM (val @i) (val @k) (scalarTy @a)
instance ( KnownNat i, KnownNat j, ScalarTy a ) => PrimOp SPIRV.Transp '(a,i,j) where
  type PrimOpConstraint SPIRV.Transp '(a,i,j) = Floating a
  type PrimOpType SPIRV.Transp '(a,i,j) = M i j a -> M j i a
  op = transpose
  opName = SPIRV.MatOp SPIRV.Transp (val @i) (val @j) (scalarTy @a)
instance ( KnownNat i, ScalarTy a ) => PrimOp SPIRV.Det '(a,i) where
  type PrimOpConstraint SPIRV.Det '(a,i) = Floating a
  type PrimOpType SPIRV.Det '(a,i) = M i i a -> a
  op = determinant
  opName = SPIRV.MatOp SPIRV.Det (val @i) (val @i) (scalarTy @a)
instance ( KnownNat i, ScalarTy a ) => PrimOp SPIRV.Inv '(a,i) where
  type PrimOpConstraint SPIRV.Inv '(a,i) = Floating a
  type PrimOpType SPIRV.Inv '(a,i) = M i i a -> M i i a
  op = inverse
  opName = SPIRV.MatOp SPIRV.Inv (val @i) (val @i) (scalarTy @a)
