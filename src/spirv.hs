{-# LANGUAGE PatternSynonyms #-}

module SPIRV where

-- compound datatype
data PrimOp
  -- numeric operations
  = NumOp  NumPrimOp
  -- integral operations
  | IntOp  IntPrimOp
  -- boolean operations
  | BoolOp BoolPrimOp
  -- vector operations
  | VecOp  VecPrimOp
  -- matrix operations
  | MatOp  MatPrimOp
  deriving Show

-- numeric operations
data NumPrimOp
  = Add
  | Sub
  | Mul
  | Div
  | Neg
  | Abs
  | Sign
  deriving Show

-- integral operations
data IntPrimOp
  = Mod
  | Rem
  deriving Show

-- boolean operations
data BoolPrimOp
  = Or
  | And
  | Not
  deriving Show

-- vector operations
data VecPrimOp
  = AddV
  | SubV
  | NegV
  | DotV
  | VMulK
  deriving Show

-- matrix operations
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

-- raw SPIR-V operation names
data SPIRVOp
  -- numeric operations
  = FAdd
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
  | SAbs  -- GLSL extended instruction
  | FAbs  -- GLSL extended instruction
  | SSign -- GLSL extended instruction
  | FSign -- GLSL extended instruction
  -- integral operations
  | SMod
  | UMod
  | SRem
  | URem
  -- boolean operations
  | LogicalOr
  | LogicalAnd
  | LogicalNot
  -- vector operations (most vector operations re-use numeric operations)
  | Dot
  | VectorTimesScalar
  -- matrix operations
  | MatrixTimesScalar
  | MatrixTimesVector
  | VectorTimesMatrix
  | MatrixTimesMatrix
  | Transpose
  | Determinant     -- GLSL extended instruction
  | MatrixInverse   -- GLSL extended instruction
  | OuterProduct
  deriving Show

data PrimTy
  = Numeric Scalar
  | Boolean
  | Vector Scalar
  | Matrix Scalar
  deriving Show

data Integrality
  = Floating
  | Signed
  | Unsigned
  deriving Show

newtype Width = Width { width :: Int }
  deriving Show

data Scalar = Scalar Integrality Width
  deriving Show

pattern NumScal :: Integrality -> Width -> PrimTy
pattern NumScal inty w = Numeric (Scalar inty w)
pattern VecScal :: Integrality -> Width -> PrimTy
pattern VecScal inty w = Vector  (Scalar inty w)
pattern MatScal :: Integrality -> Width -> PrimTy
pattern MatScal inty w = Matrix  (Scalar inty w)


op :: PrimOp -> PrimTy -> SPIRVOp
-- numeric operations
op (NumOp numOp) (NumScal inty _) = numericOp numOp inty
op (NumOp numOp) not_numty        = error "not a numeric type"
-- integral operations
op (IntOp intOp) (NumScal Floating _) = error "not an integral type (floating)"
op (IntOp intOp) (NumScal inty     _) = integralOp intOp inty
op (IntOp intOp) not_numty            = error "not an integral type"
-- boolean operations
op (BoolOp boolOp) Boolean    = booleanOp boolOp
op (BoolOp boolOp) not_boolty = error "not a boolean type"
-- vector operations
op (VecOp vecOp) (VecScal inty _) = vectorOp vecOp inty
op (VecOp vecOp) not_vecty        = error "not a vector type"
-- matrix operations
op (MatOp matOp) (MatScal inty _) = matrixOp matOp inty
op (MatOp matOp) not_matty        = error "not a matrix type"

numericOp :: NumPrimOp -> Integrality -> SPIRVOp
numericOp Add  Floating = FAdd
numericOp Add  _        = IAdd
numericOp Sub  Floating = FSub
numericOp Sub  _        = ISub -- can call substraction on unsigned integer types
numericOp Mul  Floating = FMul
numericOp Mul  _        = IMul
numericOp Neg  Floating = FNegate
numericOp Neg  Signed   = SNegate
numericOp Neg  Unsigned = error "Cannot call 'negate' on unsigned integer type."
numericOp Div  Floating = FDiv
numericOp Div  Signed   = SDiv
numericOp Div  Unsigned = UDiv
numericOp Abs  Floating = FAbs
numericOp Abs  Signed   = SAbs
numericOp Abs  Unsigned = error "Cannot call 'abs' on unsigned integer type."
numericOp Sign Floating = FSign
numericOp Sign Signed   = SSign
numericOp Sign Unsigned = error "Cannot call 'signum' on unsigned integer type."

integralOp :: IntPrimOp -> Integrality -> SPIRVOp
integralOp Mod Signed   = SMod
integralOp Mod Unsigned = UMod
integralOp Rem Signed   = SRem
integralOp Rem Unsigned = URem

booleanOp :: BoolPrimOp -> SPIRVOp
booleanOp Or  = LogicalOr
booleanOp And = LogicalAnd
booleanOp Not = LogicalNot

vectorOp :: VecPrimOp -> Integrality -> SPIRVOp
vectorOp AddV  inty     = numericOp Add inty
vectorOp SubV  inty     = numericOp Sub inty
vectorOp NegV  inty     = numericOp Neg inty
vectorOp DotV  Floating = Dot
vectorOp DotV  _        = error "Dot product: vector elements must support floating-point operations."
vectorOp VMulK Floating = VectorTimesScalar
vectorOp VMulK _        = error "Scalar multiplication: vector elements must support floating-point operations (sorry!)."

matrixOp :: MatPrimOp -> Integrality -> SPIRVOp
matrixOp MMulK  inty = MatrixTimesScalar
matrixOp MMulV  inty = MatrixTimesVector
matrixOp VMulM  inty = VectorTimesMatrix
matrixOp MMulM  inty = MatrixTimesMatrix
matrixOp Transp inty = Transpose
matrixOp Det    inty = Determinant
matrixOp Inv    inty = MatrixInverse
matrixOp Out    inty = OuterProduct


data Extension
  = GLSL

data OpCode 
  = SPIRVOpCode Int
  | ExtOpCode Extension Int

getOpCode :: SPIRVOp -> OpCode
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
--getOpCode FRem    = SPIRVOpCode 140
--getOpCode FMod    = SPIRVOpCode 141
getOpCode VectorTimesScalar = SPIRVOpCode 142
getOpCode MatrixTimesScalar = SPIRVOpCode 143
getOpCode VectorTimesMatrix = SPIRVOpCode 144
getOpCode MatrixTimesVector = SPIRVOpCode 145
getOpCode MatrixTimesMatrix = SPIRVOpCode 146
getOpCode OuterProduct      = SPIRVOpCode 147
getOpCode Dot               = SPIRVOpCode 148
--
getOpCode LogicalOr  = SPIRVOpCode 166
getOpCode LogicalAnd = SPIRVOpCode 167
getOpCode LogicalNot = SPIRVOpCode 168
-- extended instructions
getOpCode FAbs          = ExtOpCode GLSL 4
getOpCode SAbs          = ExtOpCode GLSL 5
getOpCode FSign         = ExtOpCode GLSL 6
getOpCode SSign         = ExtOpCode GLSL 7
getOpCode Determinant   = ExtOpCode GLSL 33
getOpCode MatrixInverse = ExtOpCode GLSL 34

getNameAndOpCode :: PrimTy -> PrimOp -> (String, OpCode)
getNameAndOpCode primTy primOp = ( show spirvOp, getOpCode spirvOp )
  where spirvOp :: SPIRVOp
        spirvOp = op primOp primTy