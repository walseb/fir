{-# LANGUAGE PatternSynonyms   #-}

module SPIRV.Operation where

-- base
import Data.Word(Word16, Word32)

-- fir
import SPIRV.Extension(ExtInst(GLSL))

--------------------------------------------------
-- operation data type

data Operation
  = OpCode Word16
  | ExtOpCode ExtInst Word32
  deriving Eq

instance Show Operation where
  show op@(OpCode _) = "Op" ++ showOperation op
  show op@(ExtOpCode ext _) = "OpExt(" ++ show ext ++")" ++ showOperation op

--------------------------------------------------
-- operations

pattern Nop   :: Operation
pattern Nop   = OpCode 0
pattern Undef :: Operation
pattern Undef = OpCode 1
-- (optional) annotations
pattern SourceContinued :: Operation
pattern SourceContinued = OpCode 2
pattern Source          :: Operation
pattern Source          = OpCode 3
pattern SourceExtension :: Operation
pattern SourceExtension = OpCode 4
pattern Name            :: Operation
pattern Name            = OpCode 5
pattern MemberName      :: Operation
pattern MemberName      = OpCode 6
pattern String          :: Operation
pattern String          = OpCode 7
pattern Line            :: Operation
pattern Line            = OpCode 8
-- no 9
-- extensions
pattern Extension     :: Operation
pattern Extension     = OpCode 10
pattern ExtInstImport :: Operation
pattern ExtInstImport = OpCode 11
pattern ExtInst      :: Operation
pattern ExtInst      = OpCode 12
-- no 13
pattern MemoryModel   :: Operation
pattern MemoryModel   = OpCode 14
pattern EntryPoint    :: Operation
pattern EntryPoint    = OpCode 15
pattern ExecutionMode :: Operation
pattern ExecutionMode = OpCode 16
pattern Capability    :: Operation
pattern Capability    = OpCode 17
-- no 18
-- type constructors
pattern TypeVoid           :: Operation
pattern TypeVoid           = OpCode 19
pattern TypeBool           :: Operation
pattern TypeBool           = OpCode 20
pattern TypeInt            :: Operation
pattern TypeInt            = OpCode 21
pattern TypeFloat          :: Operation
pattern TypeFloat          = OpCode 22
pattern TypeVector         :: Operation
pattern TypeVector         = OpCode 23
pattern TypeMatrix         :: Operation
pattern TypeMatrix         = OpCode 24
pattern TypeImage          :: Operation
pattern TypeImage          = OpCode 25
pattern TypeSampler        :: Operation
pattern TypeSampler        = OpCode 26
pattern TypeSampledImage   :: Operation
pattern TypeSampledImage   = OpCode 27
pattern TypeArray          :: Operation
pattern TypeArray          = OpCode 28
pattern TypeRuntimeArray   :: Operation
pattern TypeRuntimeArray   = OpCode 29
pattern TypeStruct         :: Operation
pattern TypeStruct         = OpCode 30
pattern TypeOpaque         :: Operation
pattern TypeOpaque         = OpCode 31
pattern TypePointer        :: Operation
pattern TypePointer        = OpCode 32
pattern TypeFunction       :: Operation
pattern TypeFunction       = OpCode 33
pattern TypeEvent          :: Operation
pattern TypeEvent          = OpCode 34
pattern TypeDeviceEvent    :: Operation
pattern TypeDeviceEvent    = OpCode 35
pattern TypeReserveId      :: Operation
pattern TypeReserveId      = OpCode 36
pattern TypeQueue          :: Operation
pattern TypeQueue          = OpCode 37
pattern TypePipe           :: Operation
pattern TypePipe           = OpCode 38
pattern TypeForwardPointer :: Operation
pattern TypeForwardPointer = OpCode 39
-- no 40
-- constants
pattern ConstantTrue      :: Operation
pattern ConstantTrue      = OpCode 41
pattern ConstantFalse     :: Operation
pattern ConstantFalse     = OpCode 42
pattern Constant          :: Operation
pattern Constant          = OpCode 43
pattern ConstantComposite :: Operation
pattern ConstantComposite = OpCode 44
pattern ConstantSampler   :: Operation
pattern ConstantSampler   = OpCode 45
pattern ConstantNull      :: Operation
pattern ConstantNull      = OpCode 46
-- no 47
-- specialisation constants
pattern SpecConstantTrue      :: Operation
pattern SpecConstantTrue      = OpCode 48
pattern SpecConstantFalse     :: Operation
pattern SpecConstantFalse     = OpCode 49
pattern SpecConstant          :: Operation
pattern SpecConstant          = OpCode 50
pattern SpecConstantComposite :: Operation
pattern SpecConstantComposite = OpCode 51
pattern SpecConstantOp        :: Operation
pattern SpecConstantOp        = OpCode 52
-- no 53
-- functions
pattern Function          :: Operation
pattern Function          = OpCode 54
pattern FunctionParameter :: Operation
pattern FunctionParameter = OpCode 55
pattern FunctionEnd       :: Operation
pattern FunctionEnd       = OpCode 56
pattern FunctionCall      :: Operation
pattern FunctionCall      = OpCode 57
-- no 58
-- variables & pointers
pattern Variable               :: Operation
pattern Variable               = OpCode 59
pattern ImageTexelPointer      :: Operation
pattern ImageTexelPointer      = OpCode 60
pattern Load                   :: Operation
pattern Load                   = OpCode 61
pattern Store                  :: Operation
pattern Store                  = OpCode 62
pattern CopyMemory             :: Operation
pattern CopyMemory             = OpCode 63
pattern CopyMemorySized        :: Operation
pattern CopyMemorySized        = OpCode 54
pattern AccessChain            :: Operation
pattern AccessChain            = OpCode 65
pattern InBoundsAccessChain    :: Operation
pattern InBoundsAccessChain    = OpCode 66
pattern PtrAccessChain         :: Operation
pattern PtrAccessChain         = OpCode 67
pattern ArrayLength            :: Operation
pattern ArrayLength            = OpCode 68
pattern GenericPtrMemSemantics :: Operation
pattern GenericPtrMemSemantics = OpCode 69
pattern InBoundsPtrAccessChain :: Operation
pattern InBoundsPtrAccessChain = OpCode 70
-- decorations
pattern Decorate            :: Operation
pattern Decorate            = OpCode 71
pattern MemberDecorate      :: Operation
pattern MemberDecorate      = OpCode 72
pattern DecorationGroup     :: Operation
pattern DecorationGroup     = OpCode 73
pattern GroupDecorate       :: Operation
pattern GroupDecorate       = OpCode 74
pattern GroupMemberDecorate :: Operation
pattern GroupMemberDecorate = OpCode 75
-- no 76
-- vector indexing
pattern VectorExtractDynamic :: Operation
pattern VectorExtractDynamic = OpCode 77
pattern VectorInsertDynamic  :: Operation
pattern VectorInsertDynamic  = OpCode 78
pattern VectorShuffle        :: Operation
pattern VectorShuffle        = OpCode 79
-- structs
pattern CompositeConstruct :: Operation
pattern CompositeConstruct = OpCode 80
pattern CompositeExtract   :: Operation
pattern CompositeExtract   = OpCode 81
pattern CompositeInsert    :: Operation
pattern CompositeInsert    = OpCode 82
pattern CopyObject         :: Operation
pattern CopyObject         = OpCode 83
-- operation 84 (Transpose) defined later (141.5)
-- no 85
-- image instructions
-- no 108
-- conversion instructions
pattern ConvertFToU    :: Operation
pattern ConvertFToU    = OpCode 109
pattern ConvertFToS    :: Operation
pattern ConvertFToS    = OpCode 110
pattern ConvertSToF    :: Operation
pattern ConvertSToF    = OpCode 111
pattern ConvertUToF    :: Operation
pattern ConvertUToF    = OpCode 112
pattern UConvert       :: Operation
pattern UConvert       = OpCode 113
pattern SConvert       :: Operation
pattern SConvert       = OpCode 114
pattern FConvert       :: Operation
pattern FConvert       = OpCode 115
pattern SatConvertSToU :: Operation
pattern SatConvertSToU = OpCode 118
pattern SatConvertUToS :: Operation
pattern SatConvertUToS = OpCode 119
-- pointer casting
-- no 125
-- numeric instructions
pattern SNegate :: Operation
pattern SNegate = OpCode 126
pattern FNegate :: Operation
pattern FNegate = OpCode 127
pattern IAdd    :: Operation
pattern IAdd    = OpCode 128
pattern FAdd    :: Operation
pattern FAdd    = OpCode 129
pattern ISub    :: Operation
pattern ISub    = OpCode 130
pattern FSub    :: Operation
pattern FSub    = OpCode 131
pattern IMul    :: Operation
pattern IMul    = OpCode 132
pattern FMul    :: Operation
pattern FMul    = OpCode 133
pattern UDiv    :: Operation
pattern UDiv    = OpCode 134
pattern SDiv    :: Operation
pattern SDiv    = OpCode 135
pattern FDiv    :: Operation
pattern FDiv    = OpCode 136
pattern UMod    :: Operation
pattern UMod    = OpCode 137
pattern SRem    :: Operation
pattern SRem    = OpCode 138
pattern SMod    :: Operation
pattern SMod    = OpCode 139
pattern FRem    :: Operation
pattern FRem    = OpCode 140
pattern FMod    :: Operation
pattern FMod    = OpCode 141
-- vector and matrix instructions
pattern Transpose         :: Operation
pattern Transpose         = OpCode 84
pattern VectorTimesScalar :: Operation
pattern VectorTimesScalar = OpCode 142
pattern MatrixTimesScalar :: Operation
pattern MatrixTimesScalar = OpCode 143
pattern VectorTimesMatrix :: Operation
pattern VectorTimesMatrix = OpCode 144
pattern MatrixTimesVector :: Operation
pattern MatrixTimesVector = OpCode 145
pattern MatrixTimesMatrix :: Operation
pattern MatrixTimesMatrix = OpCode 146
pattern OuterProduct      :: Operation
pattern OuterProduct      = OpCode 147
pattern Dot               :: Operation
pattern Dot               = OpCode 148
-- boolean instructions
pattern LogicalOr  :: Operation
pattern LogicalOr  = OpCode 166
pattern LogicalAnd :: Operation
pattern LogicalAnd = OpCode 167
pattern LogicalNot :: Operation
pattern LogicalNot = OpCode 168
-- integer comparisons
pattern IEqual            :: Operation
pattern IEqual            = OpCode 170
pattern INotEqual         :: Operation
pattern INotEqual         = OpCode 171
pattern UGreaterThan      :: Operation
pattern UGreaterThan      = OpCode 172
pattern SGreaterThan      :: Operation
pattern SGreaterThan      = OpCode 173
pattern UGreaterThanEqual :: Operation
pattern UGreaterThanEqual = OpCode 174
pattern SGreaterThanEqual :: Operation
pattern SGreaterThanEqual = OpCode 175
pattern ULessThan         :: Operation
pattern ULessThan         = OpCode 176 
pattern SLessThan         :: Operation
pattern SLessThan         = OpCode 177
pattern ULessThanEqual    :: Operation
pattern ULessThanEqual    = OpCode 178
pattern SLessThanEqual    :: Operation
pattern SLessThanEqual    = OpCode 179
-- floating point comparisons
pattern FOrdEqual              :: Operation
pattern FOrdEqual              = OpCode 180
pattern FUnordEqual            :: Operation
pattern FUnordEqual            = OpCode 181
pattern FOrdNotEqual           :: Operation
pattern FOrdNotEqual           = OpCode 182
pattern FUnordNotEqual         :: Operation
pattern FUnordNotEqual         = OpCode 183
pattern FOrdLessThan           :: Operation
pattern FOrdLessThan           = OpCode 184
pattern FUnordLessThan         :: Operation
pattern FUnordLessThan         = OpCode 185
pattern FOrdGreaterThan        :: Operation
pattern FOrdGreaterThan        = OpCode 186
pattern FUnordGreaterThan      :: Operation
pattern FUnordGreaterThan      = OpCode 187
pattern FOrdLessThanEqual      :: Operation
pattern FOrdLessThanEqual      = OpCode 188
pattern FUnordLessThanEqual    :: Operation
pattern FUnordLessThanEqual    = OpCode 189
pattern FOrdGreaterThanEqual   :: Operation
pattern FOrdGreaterThanEqual   = OpCode 190
pattern FUnordGreaterThanEqual :: Operation
pattern FUnordGreaterThanEqual = OpCode 191
-- GLSL extended instructions
pattern FAbs          :: Operation
pattern FAbs          = ExtOpCode GLSL 4
pattern SAbs          :: Operation
pattern SAbs          = ExtOpCode GLSL 5
pattern FSign         :: Operation
pattern FSign         = ExtOpCode GLSL 6
pattern SSign         :: Operation
pattern SSign         = ExtOpCode GLSL 7
pattern Sin           :: Operation
pattern Sin           = ExtOpCode GLSL 13
pattern Cos           :: Operation
pattern Cos           = ExtOpCode GLSL 14
pattern Tan           :: Operation
pattern Tan           = ExtOpCode GLSL 15
pattern Asin          :: Operation
pattern Asin          = ExtOpCode GLSL 16
pattern Acos          :: Operation
pattern Acos          = ExtOpCode GLSL 17
pattern Atan          :: Operation
pattern Atan          = ExtOpCode GLSL 18
pattern Sinh          :: Operation
pattern Sinh          = ExtOpCode GLSL 19
pattern Cosh          :: Operation
pattern Cosh          = ExtOpCode GLSL 20
pattern Tanh          :: Operation
pattern Tanh          = ExtOpCode GLSL 21
pattern Asinh         :: Operation
pattern Asinh         = ExtOpCode GLSL 22
pattern Acosh         :: Operation
pattern Acosh         = ExtOpCode GLSL 23
pattern Atanh         :: Operation
pattern Atanh         = ExtOpCode GLSL 24
pattern Atan2         :: Operation
pattern Atan2         = ExtOpCode GLSL 25
pattern Pow           :: Operation
pattern Pow           = ExtOpCode GLSL 26
pattern Exp           :: Operation
pattern Exp           = ExtOpCode GLSL 27
pattern Log           :: Operation
pattern Log           = ExtOpCode GLSL 28
pattern Sqrt          :: Operation
pattern Sqrt          = ExtOpCode GLSL 31
pattern Invsqrt       :: Operation
pattern Invsqrt       = ExtOpCode GLSL 32
pattern Determinant   :: Operation
pattern Determinant   = ExtOpCode GLSL 33
pattern MatrixInverse :: Operation
pattern MatrixInverse = ExtOpCode GLSL 34
pattern FMin          :: Operation
pattern FMin          = ExtOpCode GLSL 37
pattern UMin          :: Operation
pattern UMin          = ExtOpCode GLSL 38
pattern SMin          :: Operation
pattern SMin          = ExtOpCode GLSL 39
pattern FMax          :: Operation
pattern FMax          = ExtOpCode GLSL 40
pattern UMax          :: Operation
pattern UMax          = ExtOpCode GLSL 41
pattern SMax          :: Operation
pattern SMax          = ExtOpCode GLSL 42
pattern Cross         :: Operation
pattern Cross         = ExtOpCode GLSL 68

showOperation :: Operation -> String
showOperation Nop = "Nop"
showOperation Undef = "Undef"
showOperation SourceContinued = "SourceContinued"
showOperation Source = "Source"
showOperation SourceExtension = "SourceExtension"
showOperation Name = "Name"
showOperation MemberName = "MemberName"
showOperation String = "String"
showOperation Line = "Line"
showOperation ExtInst = "ExtInst"
showOperation ExtInstImport = "ExtInstImport"
showOperation MemoryModel = "MemoryModel"
showOperation EntryPoint = "EntryPoint"
showOperation ExecutionMode = "ExecutionMode"
showOperation Capability = "Capability"
showOperation TypeVoid = "TypeVoid"
showOperation TypeBool = "TypeBool"
showOperation TypeInt = "TypeInt"
showOperation TypeFloat = "TypeFloat"
showOperation TypeVector = "TypeVector"
showOperation TypeMatrix = "TypeMatrix"
showOperation TypeImage = "TypeImage"
showOperation TypeSampler = "TypeSampler"
showOperation TypeSampledImage = "TypeSampledImage"
showOperation TypeArray = "TypeArray"
showOperation TypeRuntimeArray = "TypeRuntimeArray"
showOperation TypeStruct = "TypeStruct"
showOperation TypeOpaque = "TypeOpaque"
showOperation TypePointer = "TypePointer"
showOperation TypeFunction = "TypeFunction"
showOperation TypeEvent = "TypeEvent"
showOperation TypeDeviceEvent = "TypeDeviceEvent"
showOperation TypeReserveId = "TypeReserveId"
showOperation TypeQueue = "TypeQueue"
showOperation TypePipe = "TypePipe"
showOperation TypeForwardPointer = "TypeForwardPointer"
showOperation ConstantTrue = "ConstantTrue"
showOperation ConstantFalse = "ConstantFalse"
showOperation Constant = "Constant"
showOperation ConstantComposite = "ConstantComposite"
showOperation ConstantSampler = "ConstantSampler"
showOperation ConstantNull = "ConstantNull"
showOperation SpecConstantTrue = "SpecConstantTrue"
showOperation SpecConstantFalse = "SpecConstantFalse"
showOperation SpecConstant = "SpecConstant"
showOperation SpecConstantComposite = "SpecConstantComposite"
showOperation SpecConstantOp = "SpecConstantOp"
showOperation Function = "Function"
showOperation FunctionParameter = "FunctionParameter"
showOperation FunctionEnd = "FunctionEnd"
showOperation FunctionCall = "FunctionCall"
showOperation Variable = "Variable"
showOperation ImageTexelPointer = "ImageTexelPointer"
showOperation Load = "Load"
showOperation Store = "Store"
showOperation CopyMemory = "CopyMemory"
showOperation CopyMemorySized = "CopyMemorySized"
showOperation AccessChain = "AccessChain"
showOperation InBoundsAccessChain = "InBoundsAccessChain"
showOperation PtrAccessChain = "PtrAccessChain"
showOperation ArrayLength = "ArrayLength"
showOperation GenericPtrMemSemantics = "GenericPtrMemSemantics"
showOperation InBoundsPtrAccessChain = "InBoundsPtrAccessChain"
showOperation Decorate = "Decorate"
showOperation MemberDecorate = "MemberDecorate"
showOperation DecorationGroup = "DecorationGroup"
showOperation GroupDecorate = "GroupDecorate"
showOperation GroupMemberDecorate = "GroupMemberDecorate"
showOperation VectorExtractDynamic = "VectorExtractDynamic"
showOperation VectorInsertDynamic = "VectorInsertDynamic"
showOperation VectorShuffle = "VectorShuffle"
showOperation CompositeConstruct = "CompositeConstruct"
showOperation CompositeExtract = "CompositeExtract"
showOperation CompositeInsert = "CompositeInsert"
showOperation CopyObject = "CopyObject"
showOperation ConvertFToU = "ConvertFToU"
showOperation ConvertFToS = "ConvertFToS"
showOperation ConvertSToF = "ConvertSToF"
showOperation ConvertUToF = "ConvertUToF"
showOperation UConvert = "UConvert"
showOperation SConvert = "SConvert"
showOperation FConvert = "FConvert"
showOperation SatConvertSToU = "SatConvertSToU"
showOperation SatConvertUToS = "SatConvertUToS"
showOperation SNegate = "SNegate"
showOperation FNegate = "FNegate"
showOperation IAdd = "IAdd"
showOperation FAdd = "FAdd"
showOperation ISub = "ISub"
showOperation FSub = "FSub"
showOperation IMul = "IMul"
showOperation FMul = "FMul"
showOperation UDiv = "UDiv"
showOperation SDiv = "SDiv"
showOperation FDiv = "FDiv"
showOperation UMod = "UMod"
showOperation SRem = "SRem"
showOperation SMod = "SMod"
showOperation FRem = "FRem"
showOperation FMod = "FMod"
showOperation Transpose = "Transpose"
showOperation VectorTimesScalar = "VectorTimesScalar"
showOperation MatrixTimesScalar = "MatrixTimesScalar"
showOperation VectorTimesMatrix = "VectorTimesMatrix"
showOperation MatrixTimesVector = "MatrixTimesVector"
showOperation MatrixTimesMatrix = "MatrixTimesMatrix"
showOperation OuterProduct = "OuterProduct"
showOperation Dot = "Dot"
showOperation LogicalOr = "LogicalOr"
showOperation LogicalAnd = "LogicalAnd"
showOperation LogicalNot = "LogicalNot"
showOperation IEqual = "IEqual"
showOperation INotEqual = "INotEqual"
showOperation UGreaterThan = "UGreaterThan"
showOperation SGreaterThan = "SGreaterThan"
showOperation UGreaterThanEqual = "UGreaterThanEqual"
showOperation SGreaterThanEqual = "SGreaterThanEqual"
showOperation ULessThan = "ULessThan"
showOperation SLessThan = "SLessThan"
showOperation ULessThanEqual = "ULessThanEqual"
showOperation SLessThanEqual = "SLessThanEqual"
showOperation FOrdEqual = "FOrdEqual"
showOperation FUnordEqual = "FUnordEqual"
showOperation FOrdNotEqual = "FOrdNotEqual"
showOperation FUnordNotEqual = "FUnordNotEqual"
showOperation FOrdLessThan = "FOrdLessThan"
showOperation FUnordLessThan = "FUnordLessThan"
showOperation FOrdGreaterThan = "FOrdGreaterThan"
showOperation FUnordGreaterThan = "FUnordGreaterThan"
showOperation FOrdLessThanEqual = "FOrdLessThanEqual"
showOperation FUnordLessThanEqual = "FUnordLessThanEqual"
showOperation FOrdGreaterThanEqual = "FOrdGreaterThanEqual"
showOperation FUnordGreaterThanEqual = "FUnordGreaterThanEqual"
showOperation FAbs = "FAbs"
showOperation SAbs = "SAbs"
showOperation FSign = "FSign"
showOperation SSign = "SSign"
showOperation Sin = "Sin"
showOperation Cos = "Cos"
showOperation Tan = "Tan"
showOperation Asin = "Asin"
showOperation Acos = "Acos"
showOperation Atan = "Atan"
showOperation Sinh = "Sinh"
showOperation Cosh = "Cosh"
showOperation Tanh = "Tanh"
showOperation Asinh = "Asinh"
showOperation Acosh = "Acosh"
showOperation Atanh = "Atanh"
showOperation Atan2 = "Atan2"
showOperation Pow = "Pow"
showOperation Exp = "Exp"
showOperation Log = "Log"
showOperation Sqrt = "Sqrt"
showOperation Invsqrt = "Invsqrt"
showOperation Determinant = "Determinant"
showOperation MatrixInverse = "MatrixInverse"
showOperation FMin = "FMin"
showOperation UMin = "UMin"
showOperation SMin = "SMin"
showOperation FMax = "FMax"
showOperation UMax = "UMax"
showOperation SMax = "SMax"
showOperation Cross = "Cross"
showOperation (OpCode i) = show i
showOperation (ExtOpCode _ i) = show i