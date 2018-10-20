{-# LANGUAGE GADTs           #-}

module SPIRV.PrimOp
  ( PrimOp(..)
  , BoolPrimOp(..), EqPrimOp(..), OrdPrimOp(..)
  , NumPrimOp(..), FloatPrimOp(..)
  , VecPrimOp(..), MatPrimOp(..)
  , ConvPrimOp(..)
  , opAndReturnType, op
  ) where

-- base
import Prelude hiding( Ordering(..) )

-- fir
import SPIRV.Operation
import SPIRV.PrimTy( PrimTy(..), Signedness(..), Dim )

-------------------------------------------------------------------------------
-- primitive operations

data PrimOp where
  BoolOp  :: BoolPrimOp                          -> PrimOp
  EqOp    :: EqPrimOp                  -> PrimTy -> PrimOp
  OrdOp   :: OrdPrimOp                 -> PrimTy -> PrimOp
  NumOp   :: NumPrimOp                 -> PrimTy -> PrimOp
  FloatOp :: FloatPrimOp               -> PrimTy -> PrimOp
  VecOp   :: VecPrimOp   -> Dim        -> PrimTy -> PrimOp
  MatOp   :: MatPrimOp   -> Dim -> Dim -> PrimTy -> PrimOp
  ConvOp  :: ConvPrimOp  -> PrimTy     -> PrimTy -> PrimOp
  deriving Show

data BoolPrimOp
  = Or
  | And
  | Not
  deriving Show

data EqPrimOp
  = Equal
  | NotEqual
  deriving Show

data OrdPrimOp
  = GT
  | GTE
  | LT
  | LTE
  | Min
  | Max
  deriving Show

data NumPrimOp
  -- additive group
  = Add
  -- semiring
  | Mul
  -- ring
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
  deriving Show

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
  deriving Show

data VecPrimOp
  = AddV
  | SubV
  | NegV
  | DotV
  | VMulK
  | CrossV
  deriving Show

data MatPrimOp
  = MMulK
  | MMulV
  | VMulM
  | MMulM
  | Transp
  | Det
  | Inv
  | Out
  deriving Show

data ConvPrimOp
  = Convert
  -- | CTruncate
  -- | CRound
  -- | CCeiling
  -- | CFloor
  deriving Show

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
opAndReturnType (NumOp numOp s)
  = numericOp numOp s
opAndReturnType (FloatOp flOp s)
  = ( floatingOp flOp
    , s -- should be a Floating type always
    )
opAndReturnType (VecOp vecOp n s)
  = vectorOp vecOp n s
opAndReturnType (MatOp matOp n m s)
  = matrixOp matOp n m s
opAndReturnType (ConvOp cOp s1 s2)
  = convOp cOp s1 s2

op :: PrimOp -> Operation
op = fst . opAndReturnType

booleanOp :: BoolPrimOp -> Operation
booleanOp Or  = LogicalOr
booleanOp And = LogicalAnd
booleanOp Not = LogicalNot

equalityOp :: EqPrimOp -> PrimTy -> Operation
equalityOp Equal    (Floating  _) = FOrdEqual -- not reflexive!
equalityOp Equal    (Integer _ _) = IEqual
equalityOp NotEqual (Floating  _) = FOrdNotEqual
equalityOp NotEqual (Integer _ _) = INotEqual
equalityOp primOp ty = error $ "unsupported type " ++ show ty ++ " with equality operation " ++ show primOp

orderOp :: OrdPrimOp -> PrimTy -> (Operation, PrimTy)
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
orderOp Min (Integer Unsigned w) = ( UMin , Integer Unsigned w )
orderOp Min (Integer Signed   w) = ( SMin , Integer Signed   w )
orderOp Min (Floating         w) = ( FMin , Floating         w )
orderOp Max (Integer Unsigned w) = ( UMax , Integer Unsigned w )
orderOp Max (Integer Signed   w) = ( SMax , Integer Signed   w )
orderOp Max (Floating         w) = ( FMax , Floating         w )
orderOp primOp ty = error $ "unsupported type " ++ show ty ++ " with order operation " ++ show primOp

numericOp :: NumPrimOp -> PrimTy -> (Operation, PrimTy)
-- additive group
numericOp Add  (Floating         w) = ( FAdd   , Floating         w )
numericOp Add  (Integer s        w) = ( IAdd   , Integer s        w )
-- semiring
numericOp Mul  (Floating         w) = ( FMul   , Floating         w )
numericOp Mul  (Integer s        w) = ( IMul   , Integer s        w )
-- ring
numericOp Sub  (Floating         w) = ( FSub   , Floating         w )
numericOp Sub  (Integer s        w) = ( ISub   , Integer s        w ) -- technically can call subtraction on unsigned integer types
numericOp Neg  (Floating         w) = ( FNegate, Floating         w )
numericOp Neg  (Integer Signed   w) = ( SNegate, Integer Signed   w )
numericOp Neg  (Integer Unsigned _) = error "'negate' called on unsigned type"
-- signed
numericOp Abs  (Floating         w) = ( FAbs   , Floating         w )
numericOp Abs  (Integer Signed   w) = ( SAbs   , Integer Signed   w )
numericOp Abs  (Integer Unsigned _) = error "'abs' called on unsigned type"
numericOp Sign (Floating         w) = ( FSign  , Floating         w )
numericOp Sign (Integer Signed   w) = ( SSign  , Integer Signed   w )
numericOp Sign (Integer Unsigned _) = error "'signum' called on unsigned type"
-- division ring
numericOp Div  (Floating         w) = ( FDiv   , Floating         w )
numericOp Div  (Integer Signed   w) = ( SDiv   , Integer Signed   w )
numericOp Div  (Integer Unsigned w) = ( UDiv   , Integer Unsigned w )
-- archimedean ordered group
numericOp Mod  (Floating         w) = ( FMod   , Floating         w )
numericOp Mod  (Integer Signed   w) = ( SMod   , Integer Signed   w )
numericOp Mod  (Integer Unsigned w) = ( UMod   , Integer Unsigned w )
numericOp Rem  (Floating         w) = ( FRem   , Floating         w )
numericOp Rem  (Integer Signed   w) = ( SRem   , Integer Signed   w )
numericOp Rem  (Integer Unsigned w) = ( UMod   , Integer Unsigned w ) -- URem pointless for unsigned type
numericOp primOp ty = error $ "unsupported type " ++ show ty ++ " with numeric operation " ++ show primOp

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

vectorOp :: VecPrimOp -> Dim -> PrimTy -> (Operation, PrimTy)
-- re-use numeric operations on vectors
vectorOp AddV   n s            = ( fst $ numericOp Add s, Vector n s )
vectorOp SubV   n s            = ( fst $ numericOp Sub s, Vector n s )
vectorOp NegV   n s            = ( fst $ numericOp Neg s, Vector n s )
vectorOp DotV   _ (Floating w) = ( Dot, Floating w )
vectorOp DotV   _ _            = error "Dot product: vector elements must be of floating-point type."
vectorOp VMulK  n (Floating w) = ( VectorTimesScalar, Vector n (Floating w) )
vectorOp VMulK  _ _            = error "Scalar multiplication: vector elements must be of floating-point type (sorry!)."
vectorOp CrossV n (Floating w) = ( Cross, Vector n (Floating w) )
vectorOp CrossV _ _            = error "Cross product: vector elements must be of floating-point type."

matrixOp :: MatPrimOp -> Dim -> Dim -> PrimTy -> (Operation, PrimTy)
matrixOp MMulK  n m s = ( MatrixTimesScalar, Matrix n m s )
matrixOp MMulV  n _ s = ( MatrixTimesVector, Vector n   s )
matrixOp VMulM  n _ s = ( VectorTimesMatrix, Vector n   s )
matrixOp MMulM  n m s = ( MatrixTimesMatrix, Matrix n m s )
matrixOp Transp n m s = ( Transpose        , Matrix n m s )
matrixOp Det    _ _ s = ( Determinant      ,            s )
matrixOp Inv    n m s = ( MatrixInverse    , Matrix n m s )
matrixOp Out    n m s = ( OuterProduct     , Matrix n m s )

convOp :: ConvPrimOp -> PrimTy -> PrimTy -> (Operation, PrimTy)
convOp Convert (Integer Signed   _) (Floating         w) = ( ConvertSToF   , Floating         w )
convOp Convert (Integer Unsigned _) (Floating         w) = ( ConvertUToF   , Floating         w )
convOp Convert (Floating         _) (Integer Signed   w) = ( ConvertFToS   , Integer Signed   w )
convOp Convert (Floating         _) (Integer Unsigned w) = ( ConvertFToU   , Integer Unsigned w )
convOp Convert (Integer Unsigned _) (Integer Signed   w) = ( SatConvertUToS, Integer Signed   w )
convOp Convert (Integer Signed   _) (Integer Unsigned w) = ( SatConvertSToU, Integer Unsigned w )
convOp Convert (Floating         v) (Floating         w)
  | v /= w = ( FConvert, Floating         w)
convOp Convert (Integer Signed   v) (Integer Signed   w)
  | v /= w = ( SConvert, Integer Signed   w)
convOp Convert (Integer Unsigned v) (Integer Unsigned w)
  | v /= w = ( UConvert, Integer Unsigned w)
convOp _ ty1 ty2 = error $ "unsupported conversion from " ++ show ty1 ++ " to " ++ show ty2