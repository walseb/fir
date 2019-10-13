{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}

{-|
Module: SPIRV.PrimOp

This module categorises the raw primitive OpCodes from "SPIRV.Operation".

This allows easier use of operations by generalising over certain aspects,
such as unifying the various arithmetic and logical operations.
For instance, instead of having to distinguish:

  * @UMin@, unsigned integer minimum,
  * @SMin@, signed integer minimum, and
  * @FMin@, floating point minimum,
one can simply refer to the @Min@ operation defined in this module,
and the functions within this module will dispatch the correct primitive
operation depending on the types involved.

Refer also to the module "FIR.Prim.Op", which ties these into the rest of the library.
-}

module SPIRV.PrimOp
  ( PrimOp(..)
  , BoolPrimOp(..), EqPrimOp(..), OrdPrimOp(..)
  , BitPrimOp(..)
  , NumPrimOp(..), FloatPrimOp(..)
  , VecPrimOp(..), MatPrimOp(..)
  , ConvPrimOp(..)
  , GeomPrimOp(..)
  , opAndReturnType, op
  ) where

-- base
import Control.Arrow
  ( second )
import Data.Word
  ( Word32 )
import Prelude
  hiding ( Ordering(..) )

-- fir
import SPIRV.Operation
import SPIRV.PrimTy
  ( PrimTy(..) )
import SPIRV.ScalarTy
  ( ScalarTy(..), Signedness(..) )

-------------------------------------------------------------------------------
-- names of primitive operations

data PrimOp where
  BoolOp  :: BoolPrimOp                                  -> PrimOp
  EqOp    :: EqPrimOp                        -> PrimTy   -> PrimOp
  OrdOp   :: OrdPrimOp                       -> ScalarTy -> PrimOp
  BitOp   :: BitPrimOp                       -> ScalarTy -> PrimOp
  NumOp   :: NumPrimOp                       -> ScalarTy -> PrimOp
  FloatOp :: FloatPrimOp                     -> ScalarTy -> PrimOp
  VecOp   :: VecPrimOp   -> Word32           -> ScalarTy -> PrimOp
  MatOp   :: MatPrimOp   -> Word32 -> Word32 -> ScalarTy -> PrimOp
  ConvOp  :: ConvPrimOp  -> ScalarTy         -> ScalarTy -> PrimOp
  GeomOp  :: GeomPrimOp                                  -> PrimOp
  deriving stock Show

data BoolPrimOp
  = BoolOr
  | BoolAnd
  | BoolNot
  deriving stock Show

data EqPrimOp
  = Equal
  | NotEqual
  deriving stock Show

data OrdPrimOp
  = GT
  | GTE
  | LT
  | LTE
  | Min
  | Max
  deriving stock Show

data BitPrimOp
  = BitAnd
  | BitOr
  | BitXor
  | BitNot
  | BitShiftRightLogical
  | BitShiftRightArithmetic
  | BitShiftLeft
  deriving stock Show

data NumPrimOp
  -- additive monoid
  = Add
  -- semiring
  | Mul
  -- additive group
  | Sub
  | Neg
  -- signed
  | Abs
  | Sign
  -- division ring
  | Div
  -- archimedean ordered group
  | Mod
  | Rem
  | Quot
  deriving stock Show

data FloatPrimOp
  = FSin
  | FCos
  | FTan
  | FAsin
  | FAcos
  | FAtan
  | FSinh
  | FCosh
  | FTanh
  | FAsinh
  | FAcosh
  | FAtanh
  | FAtan2
  | FPow
  | FExp
  | FLog
  | FSqrt
  | FInvsqrt
  deriving stock Show

data VecPrimOp
  = Vectorise PrimOp
  | DotV
  | VMulK
  | CrossV
  | NormaliseV
  deriving stock Show

data MatPrimOp
  = MMulK
  | MMulV
  | VMulM
  | MMulM
  | Transp
  | Det
  | Inv
  | Out
  deriving stock Show

data ConvPrimOp
  = Convert
  | CTruncate
  | CRound
  | CCeiling
  | CFloor
  deriving stock Show

data GeomPrimOp
  = EmitGeometryVertex
  | EndGeometryPrimitive
  deriving stock Show

opAndReturnType :: PrimOp -> (Operation, PrimTy)
opAndReturnType (BoolOp boolOp )
  = ( booleanOp  boolOp
    , Boolean
    )
opAndReturnType (EqOp eqOp s)
  = ( equalityOp eqOp s
    , Boolean
    )
opAndReturnType (OrdOp ordOp s)
  = orderOp ordOp s
opAndReturnType (BitOp bitOp s)
  = bitwiseOp bitOp s
opAndReturnType (NumOp numOp s)
  = second Scalar (numericOp numOp s)
opAndReturnType (FloatOp flOp s)
  = ( floatingOp flOp
    , Scalar s -- should be a Floating type always
    )
opAndReturnType (VecOp vecOp n s)
  = vectorOp vecOp n s
opAndReturnType (MatOp matOp n m s)
  = matrixOp matOp n m s
opAndReturnType (ConvOp cOp s1 s2)
  = second Scalar (convOp cOp s1 s2)
opAndReturnType (GeomOp gOp)
  = geomOp gOp

op :: PrimOp -> Operation
op = fst . opAndReturnType

booleanOp :: BoolPrimOp -> Operation
booleanOp BoolOr  = LogicalOr
booleanOp BoolAnd = LogicalAnd
booleanOp BoolNot = LogicalNot

equalityOp :: EqPrimOp -> PrimTy -> Operation
equalityOp Equal    (Scalar (Floating  _)) = FOrdEqual -- not reflexive!
equalityOp Equal    (Scalar (Integer _ _)) = IEqual
equalityOp Equal    Boolean                = LogicalEqual
equalityOp NotEqual (Scalar (Floating  _)) = FOrdNotEqual
equalityOp NotEqual (Scalar (Integer _ _)) = INotEqual
equalityOp NotEqual Boolean                = LogicalNotEqual
equalityOp primOp ty = error $ "internal error: unsupported type " ++ show ty ++ " with equality operation " ++ show primOp

orderOp :: OrdPrimOp -> ScalarTy -> (Operation, PrimTy)
orderOp GT  (Integer Unsigned _) = ( UGreaterThan        , Boolean )
orderOp GT  (Integer Signed   _) = ( SGreaterThan        , Boolean )
orderOp GT  (Floating         _) = ( FOrdGreaterThan     , Boolean )
orderOp GTE (Integer Unsigned _) = ( UGreaterThanEqual   , Boolean )
orderOp GTE (Integer Signed   _) = ( SGreaterThanEqual   , Boolean )
orderOp GTE (Floating         _) = ( FOrdGreaterThanEqual, Boolean )
orderOp LT  (Integer Unsigned _) = ( ULessThan           , Boolean )
orderOp LT  (Integer Signed   _) = ( SLessThan           , Boolean )
orderOp LT  (Floating         _) = ( FOrdLessThan        , Boolean )
orderOp LTE (Integer Unsigned _) = ( ULessThanEqual      , Boolean )
orderOp LTE (Integer Signed   _) = ( SLessThanEqual      , Boolean )
orderOp LTE (Floating         _) = ( FOrdLessThanEqual   , Boolean )
orderOp Min (Integer Unsigned w) = ( UMin , Scalar (Integer Unsigned w) )
orderOp Min (Integer Signed   w) = ( SMin , Scalar (Integer Signed   w) )
orderOp Min (Floating         w) = ( FMin , Scalar (Floating         w) )
orderOp Max (Integer Unsigned w) = ( UMax , Scalar (Integer Unsigned w) )
orderOp Max (Integer Signed   w) = ( SMax , Scalar (Integer Signed   w) )
orderOp Max (Floating         w) = ( FMax , Scalar (Floating         w) )

bitwiseOp :: BitPrimOp -> ScalarTy -> (Operation, PrimTy)
bitwiseOp BitAnd s = (BitwiseAnd, Scalar s)
bitwiseOp BitOr  s = (BitwiseOr , Scalar s)
bitwiseOp BitXor s = (BitwiseXor, Scalar s)
bitwiseOp BitNot s = (Not       , Scalar s)
bitwiseOp BitShiftRightLogical    s = (ShiftRightLogical   , Scalar s)
bitwiseOp BitShiftRightArithmetic s = (ShiftRightArithmetic, Scalar s)
bitwiseOp BitShiftLeft            s = (ShiftLeftLogical    , Scalar s)

numericOp :: NumPrimOp -> ScalarTy -> (Operation, ScalarTy)
-- additive monoid
numericOp Add  (Floating         w) = ( FAdd   , Floating         w )
numericOp Add  (Integer s        w) = ( IAdd   , Integer s        w )
-- semiring
numericOp Mul  (Floating         w) = ( FMul   , Floating         w )
numericOp Mul  (Integer s        w) = ( IMul   , Integer s        w )
-- additive group
numericOp Sub  (Floating         w) = ( FSub   , Floating         w )
numericOp Sub  (Integer s        w) = ( ISub   , Integer s        w ) -- technically can call subtraction on unsigned integer types
numericOp Neg  (Floating         w) = ( FNegate, Floating         w )
numericOp Neg  (Integer Signed   w) = ( SNegate, Integer Signed   w )
numericOp Neg  (Integer Unsigned _) = error "internal error: 'negate' called on unsigned type"
-- signed
numericOp Abs  (Floating         w) = ( FAbs   , Floating         w )
numericOp Abs  (Integer Signed   w) = ( SAbs   , Integer Signed   w )
numericOp Abs  (Integer Unsigned _) = error "internal error: 'abs' called on unsigned type"
numericOp Sign (Floating         w) = ( FSign  , Floating         w )
numericOp Sign (Integer Signed   w) = ( SSign  , Integer Signed   w )
numericOp Sign (Integer Unsigned _) = error "internal error: 'signum' called on unsigned type"
-- division ring
numericOp Div  (Floating         w) = ( FDiv   , Floating         w )
numericOp Div  (Integer  _       _) = error "internal error: Div used with integral type"
-- archimedean ordered group
numericOp Mod  (Floating         w) = ( FMod   , Floating         w )
numericOp Mod  (Integer Signed   w) = ( SMod   , Integer Signed   w )
numericOp Mod  (Integer Unsigned w) = ( UMod   , Integer Unsigned w )
numericOp Rem  (Floating         w) = ( FRem   , Floating         w )
numericOp Rem  (Integer Signed   w) = ( SRem   , Integer Signed   w )
numericOp Rem  (Integer Unsigned w) = ( UMod   , Integer Unsigned w ) -- URem pointless for unsigned type
numericOp Quot (Integer Signed   w) = ( SDiv   , Integer Signed   w )
numericOp Quot (Integer Unsigned w) = ( UDiv   , Integer Unsigned w )
numericOp Quot (Floating         _) = error "internal error: Quot used with floating-point type"

floatingOp :: FloatPrimOp -> Operation
floatingOp FSin     = Sin
floatingOp FCos     = Cos
floatingOp FTan     = Tan
floatingOp FAsin    = Asin
floatingOp FAcos    = Acos
floatingOp FAtan    = Atan
floatingOp FSinh    = Sinh
floatingOp FCosh    = Cosh
floatingOp FTanh    = Tanh
floatingOp FAsinh   = Asinh
floatingOp FAcosh   = Acosh
floatingOp FAtanh   = Atanh
floatingOp FAtan2   = Atan2
floatingOp FPow     = Pow
floatingOp FExp     = Exp
floatingOp FLog     = Log
floatingOp FSqrt    = Sqrt
floatingOp FInvsqrt = Invsqrt

vectorOp :: VecPrimOp -> Word32 -> ScalarTy -> (Operation, PrimTy)
vectorOp (Vectorise prim) n s  = ( op prim, Vector n (Scalar s) )
vectorOp DotV   _ (Floating w) = ( Dot, Scalar (Floating w) )
vectorOp DotV   _ _            = error "internal error: dot product: vector elements must be of floating-point type."
vectorOp VMulK  n (Floating w) = ( VectorTimesScalar, Vector n (Scalar (Floating w)) )
vectorOp VMulK  _ _            = error "internal error: scalar multiplication: vector elements must be of floating-point type."
vectorOp CrossV n (Floating w) = ( Cross, Vector n (Scalar (Floating w)) )
vectorOp CrossV _ _            = error "internal error: cross product: vector elements must be of floating-point type."
vectorOp NormaliseV n (Floating w) = ( Normalize, Vector n (Scalar (Floating w)) )
vectorOp NormaliseV _ _            = error "internal error: normalise: vector elements must be of floating-point type."

matrixOp :: MatPrimOp -> Word32 -> Word32 -> ScalarTy -> (Operation, PrimTy)
matrixOp MMulK  n m s = ( MatrixTimesScalar, Matrix n m s )
matrixOp MMulV  n _ s = ( MatrixTimesVector, Vector n   (Scalar s) )
matrixOp VMulM  n _ s = ( VectorTimesMatrix, Vector n   (Scalar s) )
matrixOp MMulM  n m s = ( MatrixTimesMatrix, Matrix n m s )
matrixOp Transp n m s = ( Transpose        , Matrix n m s )
matrixOp Det    _ _ s = ( Determinant      , Scalar     s )
matrixOp Inv    n m s = ( MatrixInverse    , Matrix n m s )
matrixOp Out    n m s = ( OuterProduct     , Matrix n m s )

convOp :: ConvPrimOp -> ScalarTy -> ScalarTy -> (Operation, ScalarTy)
convOp Convert (Integer Signed   _) (Floating         w) = ( ConvertSToF, Floating         w )
convOp Convert (Integer Unsigned _) (Floating         w) = ( ConvertUToF, Floating         w )
convOp Convert (Floating         _) (Integer Signed   w) = ( ConvertFToS, Integer Signed   w )
convOp Convert (Floating         _) (Integer Unsigned w) = ( ConvertFToU, Integer Unsigned w )
convOp Convert (Integer Unsigned v) (Integer Signed   w)
  | v == w = ( BitCast    , Integer Signed   w )
  | otherwise = error "internal error: unsupported conversion between integer types of different width and sign"
convOp Convert (Integer Signed   v) (Integer Unsigned w)
  | v == w = ( BitCast    , Integer Unsigned w )
  | otherwise = error "internal error: unsupported conversion between integer types of different width and sign"
convOp Convert (Floating         v) (Floating         w)
  | v /= w = ( FConvert, Floating         w)
convOp Convert (Integer Signed   v) (Integer Signed   w)
  | v /= w = ( SConvert, Integer Signed   w)
convOp Convert (Integer Unsigned v) (Integer Unsigned w)
  | v /= w = ( UConvert, Integer Unsigned w)
convOp CTruncate (Floating _) (Integer Signed   w) = ( ConvertFToS, Integer Signed   w )
convOp CTruncate (Floating _) (Integer Unsigned w) = ( ConvertFToU, Integer Unsigned w )
convOp CTruncate (Floating v) (Floating w)
  | v == w    = ( Trunc, Floating w )
  | otherwise = error "internal error: unsupported truncation between floating point types of different widths"
convOp CRound    (Floating v) (Floating w)
  | v == w    = ( Round, Floating w )
  | otherwise = error "internal error: unsupported rounding between floating point types of different widths"
convOp CFloor    (Floating v) (Floating w)
  | v == w    = ( Floor, Floating w )
  | otherwise = error "internal error: unsupported floor operation between floating point types of different widths"
convOp CCeiling (Floating v) (Floating w)
  | v == w    = ( Ceil, Floating w )
  | otherwise = error "internal error: unsupported ceiling operation between floating point types of different widths"
convOp cOp a b
  = error $ "internal error: unsupported operation " ++ show cOp ++ " from type " ++ show a ++ " to type " ++ show b

geomOp :: GeomPrimOp -> (Operation, PrimTy)
geomOp EmitGeometryVertex   = ( EmitVertex  , Unit )
geomOp EndGeometryPrimitive = ( EndPrimitive, Unit )
