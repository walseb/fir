module SPIRV.OpCodes
  ( Extension(..), extensionName
  , OpCode(..)
  , opTypeCode
  , opCode
  ) where

-- base
import Data.Word(Word16, Word32)

-- fir
import SPIRV.Types(Ty(..))
import SPIRV.PrimOps(Op(..))

--------------------------------------------------
-- opcodes

data Extension
  = GLSL
  deriving ( Show, Eq, Ord, Enum, Bounded )

extensionName :: Extension -> String
extensionName GLSL = "GLSL.std.450"

data OpCode 
  = OpCode Word16
  | ExtOpCode Extension Word32

--------------------------------------------------
-- type constructors

opTypeCode :: Ty -> OpCode
opTypeCode Void     = OpCode 19
opTypeCode Bool     = OpCode 20
opTypeCode Int      = OpCode 21
opTypeCode Float    = OpCode 22
opTypeCode Vector   = OpCode 23
opTypeCode Matrix   = OpCode 24
opTypeCode Function = OpCode 33

--------------------------------------------------
-- operations

opCode :: Op -> OpCode
-- conversion instructions
opCode ConvertFToU    = OpCode 109
opCode ConvertFToS    = OpCode 110
opCode ConvertSToF    = OpCode 111
opCode ConvertUToF    = OpCode 112
opCode UConvert       = OpCode 113
opCode SConvert       = OpCode 114
opCode FConvert       = OpCode 115
opCode SatConvertSToU = OpCode 118
opCode SatConvertUToS = OpCode 119
-- numeric instructions
opCode SNegate = OpCode 126
opCode FNegate = OpCode 127
opCode IAdd    = OpCode 128
opCode FAdd    = OpCode 129
opCode ISub    = OpCode 130
opCode FSub    = OpCode 131
opCode IMul    = OpCode 132
opCode FMul    = OpCode 133
opCode UDiv    = OpCode 134
opCode SDiv    = OpCode 135
opCode FDiv    = OpCode 136
opCode UMod    = OpCode 137
opCode SRem    = OpCode 138
opCode SMod    = OpCode 139
opCode FRem    = OpCode 140
opCode FMod    = OpCode 141
-- vector and matrix instructions
opCode Transpose         = OpCode 84
opCode VectorTimesScalar = OpCode 142
opCode MatrixTimesScalar = OpCode 143
opCode VectorTimesMatrix = OpCode 144
opCode MatrixTimesVector = OpCode 145
opCode MatrixTimesMatrix = OpCode 146
opCode OuterProduct      = OpCode 147
opCode Dot               = OpCode 148
-- boolean instructions
opCode LogicalOr  = OpCode 166
opCode LogicalAnd = OpCode 167
opCode LogicalNot = OpCode 168
-- comparisons
opCode IEqual                = OpCode 170
opCode INotEqual             = OpCode 171
opCode UGreaterThan          = OpCode 172
opCode SGreaterThan          = OpCode 173
opCode UGreaterThanEqual     = OpCode 174
opCode SGreaterThanEqual     = OpCode 175
opCode ULessThan             = OpCode 176 
opCode SLessThan             = OpCode 177
opCode ULessThanEqual        = OpCode 178
opCode SLessThanEqual        = OpCode 179
opCode FOrdEqual             = OpCode 180
opCode FUnordEqual           = OpCode 181
opCode FOrdNotEqual          = OpCode 182
opCode FUnordNotEqual        = OpCode 183
opCode FOrdLessThan          = OpCode 184
opCode FUnordLessThan        = OpCode 185
opCode FOrdGreaterThan       = OpCode 186
opCode FUnordGreaterThan     = OpCode 187
opCode FOrdLessThanEqual     = OpCode 188
opCode FUnordLessThanEqual   = OpCode 189
opCode FOrdGreaterThanEqual  = OpCode 190
opCode FUnordGreaterThanEqual= OpCode 191
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