module SPIRV.OpCodes where

-- fir
import SPIRV.Types(OpType(..))
import SPIRV.PrimOps(Op(..))

--------------------------------------------------
-- opcodes

data Extension
  = GLSL

data OpCode 
  = SPIRVOpCode Int
  | ExtOpCode Extension Int

--------------------------------------------------
-- type constructors

opTypeCode :: OpType -> OpCode
opTypeCode Void     = SPIRVOpCode 19
opTypeCode Bool     = SPIRVOpCode 20
opTypeCode Int      = SPIRVOpCode 21
opTypeCode Float    = SPIRVOpCode 22
opTypeCode Vector   = SPIRVOpCode 23
opTypeCode Matrix   = SPIRVOpCode 24
opTypeCode Function = SPIRVOpCode 33

--------------------------------------------------
-- operations

opCode :: Op -> OpCode
-- conversion instructions
opCode ConvertFToU    = SPIRVOpCode 109
opCode ConvertFToS    = SPIRVOpCode 110
opCode ConvertSToF    = SPIRVOpCode 111
opCode ConvertUToF    = SPIRVOpCode 112
opCode UConvert       = SPIRVOpCode 113
opCode SConvert       = SPIRVOpCode 114
opCode FConvert       = SPIRVOpCode 115
opCode SatConvertSToU = SPIRVOpCode 118
opCode SatConvertUToS = SPIRVOpCode 119
-- numeric instructions
opCode SNegate = SPIRVOpCode 126
opCode FNegate = SPIRVOpCode 127
opCode IAdd    = SPIRVOpCode 128
opCode FAdd    = SPIRVOpCode 129
opCode ISub    = SPIRVOpCode 130
opCode FSub    = SPIRVOpCode 131
opCode IMul    = SPIRVOpCode 132
opCode FMul    = SPIRVOpCode 133
opCode UDiv    = SPIRVOpCode 134
opCode SDiv    = SPIRVOpCode 135
opCode FDiv    = SPIRVOpCode 136
opCode UMod    = SPIRVOpCode 137
opCode SRem    = SPIRVOpCode 138
opCode SMod    = SPIRVOpCode 139
opCode FRem    = SPIRVOpCode 140
opCode FMod    = SPIRVOpCode 141
-- vector and matrix instructions
opCode Transpose         = SPIRVOpCode 84
opCode VectorTimesScalar = SPIRVOpCode 142
opCode MatrixTimesScalar = SPIRVOpCode 143
opCode VectorTimesMatrix = SPIRVOpCode 144
opCode MatrixTimesVector = SPIRVOpCode 145
opCode MatrixTimesMatrix = SPIRVOpCode 146
opCode OuterProduct      = SPIRVOpCode 147
opCode Dot               = SPIRVOpCode 148
-- boolean instructions
opCode LogicalOr  = SPIRVOpCode 166
opCode LogicalAnd = SPIRVOpCode 167
opCode LogicalNot = SPIRVOpCode 168
-- comparisons
opCode IEqual                = SPIRVOpCode 170
opCode INotEqual             = SPIRVOpCode 171
opCode UGreaterThan          = SPIRVOpCode 172
opCode SGreaterThan          = SPIRVOpCode 173
opCode UGreaterThanEqual     = SPIRVOpCode 174
opCode SGreaterThanEqual     = SPIRVOpCode 175
opCode ULessThan             = SPIRVOpCode 176 
opCode SLessThan             = SPIRVOpCode 177
opCode ULessThanEqual        = SPIRVOpCode 178
opCode SLessThanEqual        = SPIRVOpCode 179
opCode FOrdEqual             = SPIRVOpCode 180
opCode FUnordEqual           = SPIRVOpCode 181
opCode FOrdNotEqual          = SPIRVOpCode 182
opCode FUnordNotEqual        = SPIRVOpCode 183
opCode FOrdLessThan          = SPIRVOpCode 184
opCode FUnordLessThan        = SPIRVOpCode 185
opCode FOrdGreaterThan       = SPIRVOpCode 186
opCode FUnordGreaterThan     = SPIRVOpCode 187
opCode FOrdLessThanEqual     = SPIRVOpCode 188
opCode FUnordLessThanEqual   = SPIRVOpCode 189
opCode FOrdGreaterThanEqual  = SPIRVOpCode 190
opCode FUnordGreaterThanEqual= SPIRVOpCode 191
-- extended instructions
opCode FAbs          = ExtOpCode GLSL 4
opCode SAbs          = ExtOpCode GLSL 5
opCode FSign         = ExtOpCode GLSL 6
opCode SSign         = ExtOpCode GLSL 7
opCode Sin           = ExtOpCode GLSL 13
opCode Cos           = ExtOpCode GLSL 14
opCode Tan           = ExtOpCode GLSL 15
opCode Asin          = ExtOpCode GLSL 16
opCode Acos          = ExtOpCode GLSL 17
opCode Atan          = ExtOpCode GLSL 18
opCode Sinh          = ExtOpCode GLSL 19
opCode Cosh          = ExtOpCode GLSL 20
opCode Tanh          = ExtOpCode GLSL 21
opCode Asinh         = ExtOpCode GLSL 22
opCode Acosh         = ExtOpCode GLSL 23
opCode Atanh         = ExtOpCode GLSL 24
opCode Atan2         = ExtOpCode GLSL 25
opCode Pow           = ExtOpCode GLSL 26
opCode Exp           = ExtOpCode GLSL 27
opCode Log           = ExtOpCode GLSL 28
opCode Sqrt          = ExtOpCode GLSL 31
opCode Invsqrt       = ExtOpCode GLSL 32
opCode Determinant   = ExtOpCode GLSL 33
opCode MatrixInverse = ExtOpCode GLSL 34
opCode FMin          = ExtOpCode GLSL 37
opCode UMin          = ExtOpCode GLSL 38
opCode SMin          = ExtOpCode GLSL 39
opCode FMax          = ExtOpCode GLSL 40
opCode UMax          = ExtOpCode GLSL 41
opCode SMax          = ExtOpCode GLSL 42
opCode Cross         = ExtOpCode GLSL 68