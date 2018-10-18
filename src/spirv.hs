{-# LANGUAGE GADTs           #-}

module SPIRV where

import Prelude hiding(Ordering(..))

data Integrality
  = Floating
  | Signed
  | Unsigned
  deriving ( Eq, Show )

newtype Width = Width { width :: Int }
  deriving ( Eq, Show )

data Scalar = Scalar Integrality Width
  deriving ( Eq, Show )

data PrimOp where
  BoolOp  :: BoolPrimOp  -> PrimOp
  EqOp    :: EqPrimOp    -> Scalar -> PrimOp
  OrdOp   :: OrdPrimOp   -> Scalar -> PrimOp
  NumOp   :: NumPrimOp   -> Scalar -> PrimOp
  FloatOp :: FloatPrimOp -> Scalar -> PrimOp
  VecOp   :: VecPrimOp   -> Scalar -> PrimOp
  MatOp   :: MatPrimOp   -> Scalar -> PrimOp
  ConvOp  :: ConvPrimOp  -> Scalar -> Scalar -> PrimOp
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

-- raw SPIR-V operation names
data SPIRVOp
  -- native SPIRV instructions
  -- boolean operations
  = LogicalOr
  | LogicalAnd
  | LogicalNot
  -- comparisons
  | IEqual
  | INotEqual
  | UGreaterThan
  | SGreaterThan
  | UGreaterThanEqual
  | SGreaterThanEqual
  | ULessThan
  | SLessThan
  | ULessThanEqual
  | SLessThanEqual
  | FOrdEqual
  | FUnordEqual
  | FOrdNotEqual
  | FUnordNotEqual
  | FOrdLessThan
  | FUnordLessThan
  | FOrdGreaterThan
  | FUnordGreaterThan
  | FOrdLessThanEqual
  | FUnordLessThanEqual
  | FOrdGreaterThanEqual
  | FUnordGreaterThanEqual
  -- numeric operations
  | FAdd
  | IAdd
  | FSub
  | ISub
  | FMul
  | IMul
  | FNegate
  | SNegate
  | FDiv
  | SDiv
  | UDiv
  | FMod
  | FRem
  | SMod
  | UMod
  | SRem
  -- no URem, as it would be identical to UMod
  -- vector and matrix operations (most vector operations re-use numeric operations)
  | Dot
  | VectorTimesScalar
  | MatrixTimesScalar
  | MatrixTimesVector
  | VectorTimesMatrix
  | MatrixTimesMatrix
  | Transpose
  | OuterProduct
  -- numeric conversions
  | ConvertFToU
  | ConvertFToS
  | ConvertSToF
  | ConvertUToF
  | UConvert
  | SConvert
  | FConvert
  | SatConvertSToU
  | SatConvertUToS
  --
  -- GLSL extended instructions
  -- comparison
  | FMin
  | UMin
  | SMin
  | FMax
  | UMax
  | SMax
  -- vectors & matrices
  | Cross
  | Determinant
  | MatrixInverse
  --   signed
  | SAbs
  | FAbs
  | SSign
  | FSign
  --   floating
  | Sin
  | Cos
  | Tan
  | Asin
  | Acos
  | Atan
  | Sinh
  | Cosh
  | Tanh
  | Asinh
  | Acosh
  | Atanh
  | Atan2
  | Pow
  | Exp
  | Log
  | Sqrt
  | Invsqrt
  deriving Show

op :: PrimOp -> SPIRVOp
op (BoolOp     boolOp ) = booleanOp  boolOp
op (EqOp       eqOp  (Scalar inty      _) ) = equalityOp eqOp   inty
op (OrdOp      ordOp (Scalar inty      _) ) = orderOp    ordOp  inty
op (NumOp      numOp (Scalar inty      _) ) = numericOp  numOp  inty
op (VecOp      vecOp (Scalar inty      _) ) = vectorOp   vecOp  inty
op (MatOp      matOp _                    ) = matrixOp   matOp
op (FloatOp    flOp  (Scalar Floating  _) ) = floatingOp flOp
op (FloatOp    flOp  notFloaty            ) 
  = error $ "floating-point operation " 
          ++ show flOp
          ++ " called on non-floating-point type "
          ++ show notFloaty
op (ConvOp     cOp   (Scalar inty1 w1) (Scalar inty2 w2) )
  = convOp cOp inty1 w1 inty2 w2

booleanOp :: BoolPrimOp -> SPIRVOp
booleanOp Or  = LogicalOr
booleanOp And = LogicalAnd
booleanOp Not = LogicalNot

equalityOp :: EqPrimOp -> Integrality -> SPIRVOp
equalityOp Equal    Floating = FOrdEqual -- not reflexive!
equalityOp Equal    _        = IEqual
equalityOp NotEqual Floating = FOrdNotEqual
equalityOp NotEqual _        = INotEqual

orderOp :: OrdPrimOp -> Integrality -> SPIRVOp
orderOp GT  Unsigned = UGreaterThan
orderOp GT  Signed   = SGreaterThan
orderOp GT  Floating = FOrdGreaterThan 
orderOp GTE Unsigned = UGreaterThanEqual
orderOp GTE Signed   = SGreaterThanEqual
orderOp GTE Floating = FOrdGreaterThanEqual
orderOp LT  Unsigned = ULessThan
orderOp LT  Signed   = SLessThan
orderOp LT  Floating = FOrdLessThan
orderOp LTE Unsigned = ULessThanEqual
orderOp LTE Signed   = SLessThanEqual
orderOp LTE Floating = FOrdLessThanEqual
orderOp Min Unsigned = UMin
orderOp Min Signed   = SMin
orderOp Min Floating = FMin
orderOp Max Unsigned = UMax
orderOp Max Signed   = SMax
orderOp Max Floating = FMax

numericOp :: NumPrimOp -> Integrality -> SPIRVOp
-- additive group
numericOp Add      Floating = FAdd
numericOp Add      _        = IAdd
-- semiring
numericOp Mul      Floating = FMul
numericOp Mul      _        = IMul
-- ring
numericOp Sub      Floating = FSub
numericOp Sub      _        = ISub -- technically can call subtraction on unsigned integer types
numericOp Neg      Floating = FNegate
numericOp Neg      Signed   = SNegate
numericOp Neg      Unsigned = error "'negate' called on unsigned type"
-- signed
numericOp Abs      Floating = FAbs
numericOp Abs      Signed   = SAbs
numericOp Abs      Unsigned = error "'abs' called on unsigned type"
numericOp Sign     Floating = FSign
numericOp Sign     Signed   = SSign
numericOp Sign     Unsigned = error "'signum' called on unsigned type"
-- division ring
numericOp Div      Floating = FDiv
numericOp Div      Signed   = SDiv
numericOp Div      Unsigned = UDiv
-- archimedean ordered group
numericOp Mod      Floating = FMod
numericOp Mod      Signed   = SMod
numericOp Mod      Unsigned = UMod
numericOp Rem      Floating = FRem
numericOp Rem      Signed   = SRem
numericOp Rem      Unsigned = UMod -- URem pointless for unsigned type

floatingOp :: FloatPrimOp -> SPIRVOp
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

vectorOp :: VecPrimOp -> Integrality -> SPIRVOp
-- re-use numeric operations on vectors
vectorOp AddV  inty      = numericOp Add inty
vectorOp SubV  inty      = numericOp Sub inty
vectorOp NegV  inty      = numericOp Neg inty
vectorOp DotV  Floating  = Dot
vectorOp DotV  _         = error "Dot product: vector elements must be of floating-point type."
vectorOp VMulK Floating  = VectorTimesScalar
vectorOp VMulK _         = error "Scalar multiplication: vector elements must be of floating-point type (sorry!)."
vectorOp CrossV Floating = Cross
vectorOp CrossV _        = error "Cross product: vector elements must be of floating-point type."

matrixOp :: MatPrimOp -> SPIRVOp
matrixOp MMulK  = MatrixTimesScalar
matrixOp MMulV  = MatrixTimesVector
matrixOp VMulM  = VectorTimesMatrix
matrixOp MMulM  = MatrixTimesMatrix
matrixOp Transp = Transpose
matrixOp Det    = Determinant
matrixOp Inv    = MatrixInverse
matrixOp Out    = OuterProduct

convOp :: ConvPrimOp -> Integrality -> Width -> Integrality -> Width -> SPIRVOp
convOp Convert Signed   _ Floating _ = ConvertSToF
convOp Convert Unsigned _ Floating _ = ConvertUToF
convOp Convert Floating _ Signed   _ = ConvertFToS
convOp Convert Floating _ Unsigned _ = ConvertFToU
convOp Convert Unsigned _ Signed   _ = SatConvertUToS
convOp Convert Signed   _ Unsigned _ = SatConvertSToU
convOp Convert inty1 w1 inty2 w2
  | inty1 == inty2 =
      if w1 == w2
      then error "Numeric conversion: types must be of different widths."
      else case inty1 of
          Floating -> FConvert
          Signed   -> SConvert
          Unsigned -> UConvert
convOp Convert _ _ _ _ = error "unreachable"

data Extension
  = GLSL

data OpCode 
  = SPIRVOpCode Int
  | ExtOpCode Extension Int

getOpCode :: SPIRVOp -> OpCode
-- conversion instructions
getOpCode ConvertFToU    = SPIRVOpCode 109
getOpCode ConvertFToS    = SPIRVOpCode 110
getOpCode ConvertSToF    = SPIRVOpCode 111
getOpCode ConvertUToF    = SPIRVOpCode 112
getOpCode UConvert       = SPIRVOpCode 113
getOpCode SConvert       = SPIRVOpCode 114
getOpCode FConvert       = SPIRVOpCode 115
getOpCode SatConvertSToU = SPIRVOpCode 118
getOpCode SatConvertUToS = SPIRVOpCode 119
-- numeric instructions
getOpCode SNegate = SPIRVOpCode 126
getOpCode FNegate = SPIRVOpCode 127
getOpCode IAdd    = SPIRVOpCode 128
getOpCode FAdd    = SPIRVOpCode 129
getOpCode ISub    = SPIRVOpCode 130
getOpCode FSub    = SPIRVOpCode 131
getOpCode IMul    = SPIRVOpCode 132
getOpCode FMul    = SPIRVOpCode 133
getOpCode UDiv    = SPIRVOpCode 134
getOpCode SDiv    = SPIRVOpCode 135
getOpCode FDiv    = SPIRVOpCode 136
getOpCode UMod    = SPIRVOpCode 137
getOpCode SRem    = SPIRVOpCode 138
getOpCode SMod    = SPIRVOpCode 139
getOpCode FRem    = SPIRVOpCode 140
getOpCode FMod    = SPIRVOpCode 141
-- vector and matrix instructions
getOpCode Transpose         = SPIRVOpCode 84
getOpCode VectorTimesScalar = SPIRVOpCode 142
getOpCode MatrixTimesScalar = SPIRVOpCode 143
getOpCode VectorTimesMatrix = SPIRVOpCode 144
getOpCode MatrixTimesVector = SPIRVOpCode 145
getOpCode MatrixTimesMatrix = SPIRVOpCode 146
getOpCode OuterProduct      = SPIRVOpCode 147
getOpCode Dot               = SPIRVOpCode 148
-- boolean instructions
getOpCode LogicalOr  = SPIRVOpCode 166
getOpCode LogicalAnd = SPIRVOpCode 167
getOpCode LogicalNot = SPIRVOpCode 168
-- control flow
--getOpCode Select     = SPIRVOpCode 169
-- comparisons
getOpCode IEqual                = SPIRVOpCode 170
getOpCode INotEqual             = SPIRVOpCode 171
getOpCode UGreaterThan          = SPIRVOpCode 172
getOpCode SGreaterThan          = SPIRVOpCode 173
getOpCode UGreaterThanEqual     = SPIRVOpCode 174
getOpCode SGreaterThanEqual     = SPIRVOpCode 175
getOpCode ULessThan             = SPIRVOpCode 176 
getOpCode SLessThan             = SPIRVOpCode 177
getOpCode ULessThanEqual        = SPIRVOpCode 178
getOpCode SLessThanEqual        = SPIRVOpCode 179
getOpCode FOrdEqual             = SPIRVOpCode 180
getOpCode FUnordEqual           = SPIRVOpCode 181
getOpCode FOrdNotEqual          = SPIRVOpCode 182
getOpCode FUnordNotEqual        = SPIRVOpCode 183
getOpCode FOrdLessThan          = SPIRVOpCode 184
getOpCode FUnordLessThan        = SPIRVOpCode 185
getOpCode FOrdGreaterThan       = SPIRVOpCode 186
getOpCode FUnordGreaterThan     = SPIRVOpCode 187
getOpCode FOrdLessThanEqual     = SPIRVOpCode 188
getOpCode FUnordLessThanEqual   = SPIRVOpCode 189
getOpCode FOrdGreaterThanEqual  = SPIRVOpCode 190
getOpCode FUnordGreaterThanEqual= SPIRVOpCode 191
-- extended instructions
getOpCode FAbs          = ExtOpCode GLSL 4
getOpCode SAbs          = ExtOpCode GLSL 5
getOpCode FSign         = ExtOpCode GLSL 6
getOpCode SSign         = ExtOpCode GLSL 7
getOpCode Sin           = ExtOpCode GLSL 13
getOpCode Cos           = ExtOpCode GLSL 14
getOpCode Tan           = ExtOpCode GLSL 15
getOpCode Asin          = ExtOpCode GLSL 16
getOpCode Acos          = ExtOpCode GLSL 17
getOpCode Atan          = ExtOpCode GLSL 18
getOpCode Sinh          = ExtOpCode GLSL 19
getOpCode Cosh          = ExtOpCode GLSL 20
getOpCode Tanh          = ExtOpCode GLSL 21
getOpCode Asinh         = ExtOpCode GLSL 22
getOpCode Acosh         = ExtOpCode GLSL 23
getOpCode Atanh         = ExtOpCode GLSL 24
getOpCode Atan2         = ExtOpCode GLSL 25
getOpCode Pow           = ExtOpCode GLSL 26
getOpCode Exp           = ExtOpCode GLSL 27
getOpCode Log           = ExtOpCode GLSL 28
getOpCode Sqrt          = ExtOpCode GLSL 31
getOpCode Invsqrt       = ExtOpCode GLSL 32
getOpCode Determinant   = ExtOpCode GLSL 33
getOpCode MatrixInverse = ExtOpCode GLSL 34
getOpCode FMin          = ExtOpCode GLSL 37
getOpCode UMin          = ExtOpCode GLSL 38
getOpCode SMin          = ExtOpCode GLSL 39
getOpCode FMax          = ExtOpCode GLSL 40
getOpCode UMax          = ExtOpCode GLSL 41
getOpCode SMax          = ExtOpCode GLSL 42
getOpCode Cross         = ExtOpCode GLSL 68

getNameAndOpCode :: PrimOp -> (String, OpCode)
getNameAndOpCode primOp = ( show spirvOp, getOpCode spirvOp )
  where spirvOp :: SPIRVOp
        spirvOp = op primOp