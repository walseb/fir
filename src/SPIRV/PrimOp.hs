{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  , SyncPrimOp(..)
  , GroupPrimOp(..), GroupNumOp(..), GroupBitwiseOp(..), GroupLogicOp(..)
  , RayPrimOp(..)
  , opAndReturnType, op
  ) where

-- base
import Prelude
  hiding ( Ordering(..) )
import Control.Arrow
  ( second )
import Data.Word
  ( Word32 )

-- fir
import SPIRV.Operation
import SPIRV.PrimTy
  ( PrimTy(..) )
import SPIRV.ScalarTy
  ( ScalarTy(..), Signedness(..) )
import SPIRV.Stage
  ( Backend(Vulkan, OpenCL) )

-------------------------------------------------------------------------------
-- names of primitive operations

data PrimOp where
  BoolOp  :: BoolPrimOp                                  -> PrimOp
  EqOp    :: EqPrimOp                        -> PrimTy   -> PrimOp
  OrdOp   :: OrdPrimOp                       -> ScalarTy -> PrimOp
  BitOp   :: BitPrimOp                       -> ScalarTy -> PrimOp
  NumOp   :: NumPrimOp                       -> ScalarTy -> PrimOp
  FloatOp :: FloatPrimOp                     -> ScalarTy -> PrimOp
  VecOp   :: VecPrimOp   -> Word32           -> PrimTy   -> PrimOp
  MatOp   :: MatPrimOp   -> Word32 -> Word32 -> ScalarTy -> PrimOp
  ConvOp  :: ConvPrimOp  -> ScalarTy         -> ScalarTy -> PrimOp
  CastOp  ::                                    PrimTy   -> PrimOp
  GeomOp  :: GeomPrimOp                                  -> PrimOp
  SyncOp  :: SyncPrimOp                                  -> PrimOp
  GroupOp :: GroupPrimOp                                 -> PrimOp
  RayOp   :: RayPrimOp                                   -> PrimOp
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
  | FInvSqrt
  | FIsNaN
  | FIsInf
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

data SyncPrimOp
  = ControlSync
  | MemorySync
  deriving stock Show

data GroupPrimOp
  = GroupNumOp     GroupNumOp     ScalarTy
  | GroupBitwiseOp GroupBitwiseOp ScalarTy
  | GroupLogicOp   GroupLogicOp
  deriving stock Show

data GroupNumOp
  = GroupAdd
  | GroupMin
  | GroupMax
  | GroupMul
  deriving stock Show

data GroupBitwiseOp
  = GroupBitwiseAnd
  | GroupBitwiseOr
  | GroupBitwiseXor
  deriving stock Show

data GroupLogicOp
  = GroupLogicalAnd
  | GroupLogicalOr
  | GroupLogicalXor
  deriving stock Show

data RayPrimOp
  = RT_ReportIntersection
  | RT_IgnoreIntersection
  | RT_TerminateRay
  | RT_AccelerationStructureFromDeviceAddress
  deriving stock Show

backendOp :: Backend -> Operation -> Operation -> Operation
backendOp Vulkan o _ = o
backendOp OpenCL _ o = o

opAndReturnType :: Backend -> PrimOp -> (Operation, PrimTy)
opAndReturnType _ (BoolOp boolOp)
  = ( booleanOp  boolOp
    , Boolean
    )
opAndReturnType _ (EqOp eqOp s)
  = ( equalityOp eqOp s
    , Boolean
    )
opAndReturnType bk (OrdOp ordOp s)
  = orderOp bk ordOp s
opAndReturnType _ (BitOp bitOp s)
  = bitwiseOp bitOp s
opAndReturnType _ (CastOp s)
  = castOp s
opAndReturnType bk (NumOp numOp s)
  = second Scalar (numericOp bk numOp s)
opAndReturnType bk (FloatOp flOp s)
  = floatingOp bk flOp s
opAndReturnType bk (VecOp vecOp n ty)
  = vectorOp bk vecOp n ty
opAndReturnType bk (MatOp matOp n m s)
  = matrixOp bk matOp n m s
opAndReturnType bk (ConvOp cOp s1 s2)
  = second Scalar (convOp bk cOp s1 s2)
opAndReturnType _ (GeomOp gOp)
  = geomOp gOp
opAndReturnType _ (SyncOp sOp)
  = syncOp sOp
opAndReturnType bk (GroupOp gOp)
  = groupOp bk gOp
opAndReturnType _ (RayOp rOp)
  = rayOp rOp

op :: Backend -> PrimOp -> Operation
op bkend = fst . opAndReturnType bkend

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

orderOp :: Backend -> OrdPrimOp -> ScalarTy -> (Operation, PrimTy)
orderOp _  GT  (Integer Unsigned _) = ( UGreaterThan        , Boolean )
orderOp _  GT  (Integer Signed   _) = ( SGreaterThan        , Boolean )
orderOp _  GT  (Floating         _) = ( FOrdGreaterThan     , Boolean )
orderOp _  GTE (Integer Unsigned _) = ( UGreaterThanEqual   , Boolean )
orderOp _  GTE (Integer Signed   _) = ( SGreaterThanEqual   , Boolean )
orderOp _  GTE (Floating         _) = ( FOrdGreaterThanEqual, Boolean )
orderOp _  LT  (Integer Unsigned _) = ( ULessThan           , Boolean )
orderOp _  LT  (Integer Signed   _) = ( SLessThan           , Boolean )
orderOp _  LT  (Floating         _) = ( FOrdLessThan        , Boolean )
orderOp _  LTE (Integer Unsigned _) = ( ULessThanEqual      , Boolean )
orderOp _  LTE (Integer Signed   _) = ( SLessThanEqual      , Boolean )
orderOp _  LTE (Floating         _) = ( FOrdLessThanEqual   , Boolean )
orderOp bk Min (Integer Unsigned w) = ( backendOp bk GLSL_UMin OpenCL_UMin , Scalar (Integer Unsigned w) )
orderOp bk Min (Integer Signed   w) = ( backendOp bk GLSL_SMin OpenCL_SMin , Scalar (Integer Signed   w) )
orderOp bk Min (Floating         w) = ( backendOp bk GLSL_FMin OpenCL_FMin , Scalar (Floating         w) )
orderOp bk Max (Integer Unsigned w) = ( backendOp bk GLSL_UMax OpenCL_UMax , Scalar (Integer Unsigned w) )
orderOp bk Max (Integer Signed   w) = ( backendOp bk GLSL_SMax OpenCL_SMax , Scalar (Integer Signed   w) )
orderOp bk Max (Floating         w) = ( backendOp bk GLSL_FMax OpenCL_FMax , Scalar (Floating         w) )

bitwiseOp :: BitPrimOp -> ScalarTy -> (Operation, PrimTy)
bitwiseOp BitAnd s = (BitwiseAnd, Scalar s)
bitwiseOp BitOr  s = (BitwiseOr , Scalar s)
bitwiseOp BitXor s = (BitwiseXor, Scalar s)
bitwiseOp BitNot s = (Not       , Scalar s)
bitwiseOp BitShiftRightLogical    s = (ShiftRightLogical   , Scalar s)
bitwiseOp BitShiftRightArithmetic s = (ShiftRightArithmetic, Scalar s)
bitwiseOp BitShiftLeft            s = (ShiftLeftLogical    , Scalar s)

castOp :: PrimTy -> (Operation, PrimTy)
castOp s = (BitCast, s)

numericOp :: Backend -> NumPrimOp -> ScalarTy -> (Operation, ScalarTy)
-- additive monoid
numericOp _  Add  (Floating         w) = ( FAdd   , Floating         w )
numericOp _  Add  (Integer s        w) = ( IAdd   , Integer s        w )
-- semiring
numericOp _  Mul  (Floating         w) = ( FMul   , Floating         w )
numericOp _  Mul  (Integer s        w) = ( IMul   , Integer s        w )
-- additive group
numericOp _  Sub  (Floating         w) = ( FSub   , Floating         w )
numericOp _  Sub  (Integer s        w) = ( ISub   , Integer s        w )
numericOp _  Neg  (Floating         w) = ( FNegate, Floating         w )
numericOp _  Neg  (Integer Signed   w) = ( SNegate, Integer Signed   w )
numericOp _  Neg  (Integer Unsigned _) = error "internal error: 'negate' called on unsigned type"
-- signed
numericOp bk Abs  (Floating         w) = ( backendOp bk GLSL_FAbs OpenCL_FAbs  , Floating         w )
numericOp bk Abs  (Integer Signed   w) = ( backendOp bk GLSL_SAbs OpenCL_SAbs  , Integer Signed   w )
numericOp _  Abs  (Integer Unsigned _) = error "internal error: 'abs' called on unsigned type"
numericOp bk Sign (Floating         w) = ( backendOp bk GLSL_FSign OpenCL_FSign, Floating         w )
numericOp bk Sign (Integer Signed   w) 
  | OpenCL <- bk = error "TODO: OpenCL backend does not support 'signum' operation on integer types"
  | otherwise =  ( GLSL_SSign  , Integer Signed   w )
numericOp _  Sign (Integer Unsigned _) = error "internal error: 'signum' called on unsigned type"
-- division ring
numericOp _  Div  (Floating         w) = ( FDiv   , Floating         w )
numericOp _  Div  (Integer  _       _) = error "internal error: Div used with integral type"
-- archimedean ordered group
numericOp _  Mod  (Floating         w) = ( FMod   , Floating         w )
numericOp _  Mod  (Integer Signed   w) = ( SMod   , Integer Signed   w )
numericOp _  Mod  (Integer Unsigned w) = ( UMod   , Integer Unsigned w )
numericOp _  Rem  (Floating         w) = ( FRem   , Floating         w )
numericOp _  Rem  (Integer Signed   w) = ( SRem   , Integer Signed   w )
numericOp _  Rem  (Integer Unsigned w) = ( UMod   , Integer Unsigned w ) -- URem pointless for unsigned type
numericOp _  Quot (Integer Signed   w) = ( SDiv   , Integer Signed   w )
numericOp _  Quot (Integer Unsigned w) = ( UDiv   , Integer Unsigned w )
numericOp _  Quot (Floating         _) = error "internal error: Quot used with floating-point type"

floatingOp :: Backend -> FloatPrimOp -> ScalarTy -> (Operation, PrimTy)
floatingOp bk FSin     s = ( backendOp bk GLSL_Sin     OpenCL_Sin     , Scalar s )
floatingOp bk FCos     s = ( backendOp bk GLSL_Cos     OpenCL_Cos     , Scalar s )
floatingOp bk FTan     s = ( backendOp bk GLSL_Tan     OpenCL_Tan     , Scalar s )
floatingOp bk FAsin    s = ( backendOp bk GLSL_Asin    OpenCL_Asin    , Scalar s )
floatingOp bk FAcos    s = ( backendOp bk GLSL_Acos    OpenCL_Acos    , Scalar s )
floatingOp bk FAtan    s = ( backendOp bk GLSL_Atan    OpenCL_Atan    , Scalar s )
floatingOp bk FSinh    s = ( backendOp bk GLSL_Sinh    OpenCL_Sinh    , Scalar s )
floatingOp bk FCosh    s = ( backendOp bk GLSL_Cosh    OpenCL_Cosh    , Scalar s )
floatingOp bk FTanh    s = ( backendOp bk GLSL_Tanh    OpenCL_Tanh    , Scalar s )
floatingOp bk FAsinh   s = ( backendOp bk GLSL_Asinh   OpenCL_Asinh   , Scalar s )
floatingOp bk FAcosh   s = ( backendOp bk GLSL_Acosh   OpenCL_Acosh   , Scalar s )
floatingOp bk FAtanh   s = ( backendOp bk GLSL_Atanh   OpenCL_Atanh   , Scalar s )
floatingOp bk FAtan2   s = ( backendOp bk GLSL_Atan2   OpenCL_Atan2   , Scalar s )
floatingOp bk FPow     s = ( backendOp bk GLSL_Pow     OpenCL_Pow     , Scalar s )
floatingOp bk FExp     s = ( backendOp bk GLSL_Exp     OpenCL_Exp     , Scalar s )
floatingOp bk FLog     s = ( backendOp bk GLSL_Log     OpenCL_Log     , Scalar s )
floatingOp bk FSqrt    s = ( backendOp bk GLSL_Sqrt    OpenCL_Sqrt    , Scalar s )
floatingOp bk FInvSqrt s = ( backendOp bk GLSL_InvSqrt OpenCL_InvSqrt , Scalar s )
floatingOp _  FIsNaN   _ = ( IsNan, Boolean )
floatingOp _  FIsInf   _ = ( IsInf, Boolean )

vectorOp :: Backend -> VecPrimOp -> Word32 -> PrimTy -> (Operation, PrimTy)
vectorOp bk (Vectorise prim) n ty = ( op bk prim, Vector n ty )
vectorOp _  DotV       _ (Scalar (Floating w)) = ( Dot, Scalar (Floating w) )
vectorOp _  DotV       _ _                     = error "internal error: dot product: vector elements must be of floating-point type."
vectorOp _  VMulK      n (Scalar (Floating w)) = ( VectorTimesScalar, Vector n (Scalar (Floating w)) )
vectorOp _  VMulK      _ _                     = error "internal error: scalar multiplication: vector elements must be of floating-point type."
vectorOp bk CrossV     n (Scalar (Floating w)) = ( backendOp bk GLSL_Cross OpenCL_Cross, Vector n (Scalar (Floating w)) )
vectorOp _  CrossV     _ _                     = error "internal error: cross product: vector elements must be of floating-point type."
vectorOp bk NormaliseV n (Scalar (Floating w)) = ( backendOp bk GLSL_Normalize OpenCL_Normalize, Vector n (Scalar (Floating w)) )
vectorOp _  NormaliseV _ _                     = error "internal error: normalise: vector elements must be of floating-point type."

matrixOp :: Backend -> MatPrimOp -> Word32 -> Word32 -> ScalarTy -> (Operation, PrimTy)
matrixOp _  MMulK  n m s = ( MatrixTimesScalar, Matrix n m s )
matrixOp _  MMulV  n _ s = ( MatrixTimesVector, Vector n   (Scalar s) )
matrixOp _  VMulM  n _ s = ( VectorTimesMatrix, Vector n   (Scalar s) )
matrixOp _  MMulM  n m s = ( MatrixTimesMatrix, Matrix n m s )
matrixOp _  Transp n m s = ( Transpose        , Matrix n m s )
matrixOp bk Det    _ _ s
  | Vulkan <- bk = ( GLSL_Determinant      , Scalar     s )
  | otherwise    = error "todo: determinant not implemented in OpenCL backend"
matrixOp bk Inv    n m s
  | Vulkan <- bk = ( GLSL_MatrixInverse    , Matrix n m s )
  | otherwise    = error "todo: matrix inverse not implemented in OpenCL backend"
matrixOp _  Out    n m s = ( OuterProduct     , Matrix n m s )

convOp :: Backend -> ConvPrimOp -> ScalarTy -> ScalarTy -> (Operation, ScalarTy)
convOp _  Convert (Integer Signed   _) (Floating         w) = ( ConvertSToF, Floating         w )
convOp _  Convert (Integer Unsigned _) (Floating         w) = ( ConvertUToF, Floating         w )
convOp _  Convert (Floating         _) (Integer Signed   w) = ( ConvertFToS, Integer Signed   w )
convOp _  Convert (Floating         _) (Integer Unsigned w) = ( ConvertFToU, Integer Unsigned w )
convOp bk Convert (Integer Unsigned v) (Integer Signed   w)
  | OpenCL <- bk = ( SatConvertUToS, Integer Signed w )
  | v == w = ( BitCast, Integer Signed   w )
  | otherwise = error "internal error: unsupported conversion between integer types of different width and sign"
convOp bk Convert (Integer Signed   v) (Integer Unsigned w)
  | OpenCL <- bk = ( SatConvertSToU, Integer Unsigned w )
  | v == w = ( BitCast    , Integer Unsigned w )
  | otherwise = error "internal error: unsupported conversion between integer types of different width and sign"
convOp _  Convert (Floating         v) (Floating         w)
  | v /= w = ( FConvert, Floating         w)
convOp _  Convert (Integer Signed   v) (Integer Signed   w)
  | v /= w = ( SConvert, Integer Signed   w)
convOp _  Convert (Integer Unsigned v) (Integer Unsigned w)
  | v /= w = ( UConvert, Integer Unsigned w)
convOp _  CTruncate (Floating _) (Integer Signed   w) = ( ConvertFToS, Integer Signed   w )
convOp _  CTruncate (Floating _) (Integer Unsigned w) = ( ConvertFToU, Integer Unsigned w )
convOp bk CTruncate (Floating v) (Floating w)
  | v == w    = ( backendOp bk GLSL_Trunc OpenCL_Trunc, Floating w )
  | otherwise = error "internal error: unsupported truncation between floating point types of different widths"
convOp bk CRound    (Floating v) (Floating w)
  | v == w    = ( backendOp bk GLSL_Round OpenCL_Round, Floating w )
  | otherwise = error "internal error: unsupported rounding between floating point types of different widths"
convOp bk CFloor    (Floating v) (Floating w)
  | v == w    = ( backendOp bk GLSL_Floor OpenCL_Floor, Floating w )
  | otherwise = error "internal error: unsupported floor operation between floating point types of different widths"
convOp bk CCeiling (Floating v) (Floating w)
  | v == w    = ( backendOp bk GLSL_Ceil OpenCL_Ceil, Floating w )
  | otherwise = error "internal error: unsupported ceiling operation between floating point types of different widths"
convOp _  cOp a b
  = error $ "internal error: unsupported operation " ++ show cOp ++ " from type " ++ show a ++ " to type " ++ show b

geomOp :: GeomPrimOp -> (Operation, PrimTy)
geomOp EmitGeometryVertex   = ( EmitVertex  , Unit )
geomOp EndGeometryPrimitive = ( EndPrimitive, Unit )

syncOp :: SyncPrimOp -> (Operation, PrimTy)
syncOp ControlSync = ( ControlBarrier, Unit )
syncOp MemorySync  = ( MemoryBarrier , Unit )

groupOp :: Backend -> GroupPrimOp -> (Operation, PrimTy)
groupOp bk (GroupNumOp gpOp s) = second Scalar $ groupNumOp bk gpOp s
groupOp OpenCL gpOp = error $ "internal error: OpenCL backend does not support group operation " ++ show gpOp
groupOp _ (GroupBitwiseOp gpOp s) = (groupBitwiseOp gpOp, Scalar s)
groupOp _ (GroupLogicOp   gpOp)   = (groupLogicOp gpOp, Boolean)

groupNumOp :: Backend -> GroupNumOp -> ScalarTy -> (Operation, ScalarTy)
groupNumOp OpenCL GroupAdd (Floating         w) = ( GroupFAdd, Floating         w )
groupNumOp OpenCL GroupAdd (Integer s        w) = ( GroupIAdd, Integer s        w )
groupNumOp OpenCL GroupMax (Floating         w) = ( GroupFMax, Floating         w )
groupNumOp OpenCL GroupMax (Integer Signed   w) = ( GroupSMax, Integer Signed   w )
groupNumOp OpenCL GroupMax (Integer Unsigned w) = ( GroupUMin, Integer Unsigned w )
groupNumOp OpenCL GroupMin (Floating         w) = ( GroupFMin, Floating         w )
groupNumOp OpenCL GroupMin (Integer Signed   w) = ( GroupSMin, Integer Signed   w )
groupNumOp OpenCL GroupMin (Integer Unsigned w) = ( GroupUMin, Integer Unsigned w )
groupNumOp Vulkan GroupAdd (Floating         w) = ( GroupNonUniformFAdd, Floating         w )
groupNumOp Vulkan GroupAdd (Integer s        w) = ( GroupNonUniformIAdd, Integer s        w )
groupNumOp Vulkan GroupMul (Floating         w) = ( GroupNonUniformFMul, Floating         w )
groupNumOp Vulkan GroupMul (Integer s        w) = ( GroupNonUniformIMul, Integer s        w )
groupNumOp Vulkan GroupMax (Floating         w) = ( GroupNonUniformFMax, Floating         w )
groupNumOp Vulkan GroupMax (Integer Signed   w) = ( GroupNonUniformSMax, Integer Signed   w )
groupNumOp Vulkan GroupMax (Integer Unsigned w) = ( GroupNonUniformUMax, Integer Unsigned w )
groupNumOp Vulkan GroupMin (Floating         w) = ( GroupNonUniformFMin, Floating         w )
groupNumOp Vulkan GroupMin (Integer Signed   w) = ( GroupNonUniformSMin, Integer Signed   w )
groupNumOp Vulkan GroupMin (Integer Unsigned w) = ( GroupNonUniformUMin, Integer Unsigned w )
groupNumOp OpenCL GroupMul _ =
  error "internal error: OpenCL backend does not support GroupMul operation"

groupBitwiseOp :: GroupBitwiseOp -> Operation
groupBitwiseOp GroupBitwiseAnd = GroupNonUniformBitwiseAnd
groupBitwiseOp GroupBitwiseOr  = GroupNonUniformBitwiseOr
groupBitwiseOp GroupBitwiseXor = GroupNonUniformBitwiseXor

groupLogicOp :: GroupLogicOp -> Operation
groupLogicOp GroupLogicalAnd = GroupNonUniformLogicalAnd
groupLogicOp GroupLogicalOr  = GroupNonUniformLogicalOr
groupLogicOp GroupLogicalXor = GroupNonUniformLogicalXor

rayOp :: RayPrimOp -> (Operation, PrimTy)
rayOp RT_ReportIntersection                     = ( ReportIntersection, Boolean )
rayOp RT_IgnoreIntersection                     = ( IgnoreIntersection, Unit )
rayOp RT_TerminateRay                           = ( TerminateRay, Unit )
rayOp RT_AccelerationStructureFromDeviceAddress = ( ConvertUToAccelerationStructure, AccelerationStructure )
