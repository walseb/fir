{-# LANGUAGE PatternSynonyms #-}

module SPIRV.Operation where

-- base
import Data.Word(Word16, Word32)

-- fir
import SPIRV.Extension(ExtInst(GLSL))

--------------------------------------------------
-- operation data type

data Operation
  = Code Word16
  | ExtCode ExtInst Word32
  deriving Eq

instance Show Operation where
  show op@(Code _) = "Op" ++ showOperation op
  show op@(ExtCode ext _) = "OpExt(" ++ show ext ++")" ++ showOperation op

--------------------------------------------------
-- operations

pattern Nop   :: Operation
pattern Nop   = Code 0
pattern Undef :: Operation
pattern Undef = Code 1
-- (optional) annotations
pattern SourceContinued :: Operation
pattern SourceContinued = Code 2
pattern Source          :: Operation
pattern Source          = Code 3
pattern SourceExtension :: Operation
pattern SourceExtension = Code 4
pattern Name            :: Operation
pattern Name            = Code 5
pattern MemberName      :: Operation
pattern MemberName      = Code 6
pattern String          :: Operation
pattern String          = Code 7
pattern Line            :: Operation
pattern Line            = Code 8
-- no 9
-- extensions
pattern Extension     :: Operation
pattern Extension     = Code 10
pattern ExtInstImport :: Operation
pattern ExtInstImport = Code 11
pattern ExtInst      :: Operation
pattern ExtInst      = Code 12
-- no 13
pattern MemoryModel   :: Operation
pattern MemoryModel   = Code 14
pattern EntryPoint    :: Operation
pattern EntryPoint    = Code 15
pattern ExecutionMode :: Operation
pattern ExecutionMode = Code 16
pattern Capability    :: Operation
pattern Capability    = Code 17
-- no 18
-- type constructors
pattern TypeVoid           :: Operation
pattern TypeVoid           = Code 19
pattern TypeBool           :: Operation
pattern TypeBool           = Code 20
pattern TypeInt            :: Operation
pattern TypeInt            = Code 21
pattern TypeFloat          :: Operation
pattern TypeFloat          = Code 22
pattern TypeVector         :: Operation
pattern TypeVector         = Code 23
pattern TypeMatrix         :: Operation
pattern TypeMatrix         = Code 24
pattern TypeImage          :: Operation
pattern TypeImage          = Code 25
pattern TypeSampler        :: Operation
pattern TypeSampler        = Code 26
pattern TypeSampledImage   :: Operation
pattern TypeSampledImage   = Code 27
pattern TypeArray          :: Operation
pattern TypeArray          = Code 28
pattern TypeRuntimeArray   :: Operation
pattern TypeRuntimeArray   = Code 29
pattern TypeStruct         :: Operation
pattern TypeStruct         = Code 30
pattern TypeOpaque         :: Operation
pattern TypeOpaque         = Code 31
pattern TypePointer        :: Operation
pattern TypePointer        = Code 32
pattern TypeFunction       :: Operation
pattern TypeFunction       = Code 33
pattern TypeEvent          :: Operation
pattern TypeEvent          = Code 34
pattern TypeDeviceEvent    :: Operation
pattern TypeDeviceEvent    = Code 35
pattern TypeReserveId      :: Operation
pattern TypeReserveId      = Code 36
pattern TypeQueue          :: Operation
pattern TypeQueue          = Code 37
pattern TypePipe           :: Operation
pattern TypePipe           = Code 38
pattern TypeForwardPointer :: Operation
pattern TypeForwardPointer = Code 39
-- no 40
-- constants
pattern ConstantTrue      :: Operation
pattern ConstantTrue      = Code 41
pattern ConstantFalse     :: Operation
pattern ConstantFalse     = Code 42
pattern Constant          :: Operation
pattern Constant          = Code 43
pattern ConstantComposite :: Operation
pattern ConstantComposite = Code 44
pattern ConstantSampler   :: Operation
pattern ConstantSampler   = Code 45
pattern ConstantNull      :: Operation
pattern ConstantNull      = Code 46
-- no 47
-- specialisation constants
pattern SpecConstantTrue      :: Operation
pattern SpecConstantTrue      = Code 48
pattern SpecConstantFalse     :: Operation
pattern SpecConstantFalse     = Code 49
pattern SpecConstant          :: Operation
pattern SpecConstant          = Code 50
pattern SpecConstantComposite :: Operation
pattern SpecConstantComposite = Code 51
pattern SpecConstantOp        :: Operation
pattern SpecConstantOp        = Code 52
-- no 53
-- functions
pattern Function          :: Operation
pattern Function          = Code 54
pattern FunctionParameter :: Operation
pattern FunctionParameter = Code 55
pattern FunctionEnd       :: Operation
pattern FunctionEnd       = Code 56
pattern FunctionCall      :: Operation
pattern FunctionCall      = Code 57
-- no 58
-- variables & pointers
pattern Variable               :: Operation
pattern Variable               = Code 59
pattern ImageTexelPointer      :: Operation
pattern ImageTexelPointer      = Code 60
pattern Load                   :: Operation
pattern Load                   = Code 61
pattern Store                  :: Operation
pattern Store                  = Code 62
pattern CopyMemory             :: Operation
pattern CopyMemory             = Code 63
pattern CopyMemorySized        :: Operation
pattern CopyMemorySized        = Code 54
pattern AccessChain            :: Operation
pattern AccessChain            = Code 65
pattern InBoundsAccessChain    :: Operation
pattern InBoundsAccessChain    = Code 66
pattern PtrAccessChain         :: Operation
pattern PtrAccessChain         = Code 67
pattern ArrayLength            :: Operation
pattern ArrayLength            = Code 68
pattern GenericPtrMemSemantics :: Operation
pattern GenericPtrMemSemantics = Code 69
pattern InBoundsPtrAccessChain :: Operation
pattern InBoundsPtrAccessChain = Code 70
-- decorations
pattern Decorate            :: Operation
pattern Decorate            = Code 71
pattern MemberDecorate      :: Operation
pattern MemberDecorate      = Code 72
pattern DecorationGroup     :: Operation
pattern DecorationGroup     = Code 73
pattern GroupDecorate       :: Operation
pattern GroupDecorate       = Code 74
pattern GroupMemberDecorate :: Operation
pattern GroupMemberDecorate = Code 75
-- no 76
-- vector indexing
pattern VectorExtractDynamic :: Operation
pattern VectorExtractDynamic = Code 77
pattern VectorInsertDynamic  :: Operation
pattern VectorInsertDynamic  = Code 78
pattern VectorShuffle        :: Operation
pattern VectorShuffle        = Code 79
-- structs
pattern CompositeConstruct :: Operation
pattern CompositeConstruct = Code 80
pattern CompositeExtract   :: Operation
pattern CompositeExtract   = Code 81
pattern CompositeInsert    :: Operation
pattern CompositeInsert    = Code 82
pattern CopyObject         :: Operation
pattern CopyObject         = Code 83
-- operation 84 (Transpose) defined later (141.5)
-- no 85
-- image instructions
-- no 108
-- conversion instructions
pattern ConvertFToU    :: Operation
pattern ConvertFToU    = Code 109
pattern ConvertFToS    :: Operation
pattern ConvertFToS    = Code 110
pattern ConvertSToF    :: Operation
pattern ConvertSToF    = Code 111
pattern ConvertUToF    :: Operation
pattern ConvertUToF    = Code 112
pattern UConvert       :: Operation
pattern UConvert       = Code 113
pattern SConvert       :: Operation
pattern SConvert       = Code 114
pattern FConvert       :: Operation
pattern FConvert       = Code 115
pattern SatConvertSToU :: Operation
pattern SatConvertSToU = Code 118
pattern SatConvertUToS :: Operation
pattern SatConvertUToS = Code 119
-- pointer casting
-- no 125
-- numeric instructions
pattern SNegate :: Operation
pattern SNegate = Code 126
pattern FNegate :: Operation
pattern FNegate = Code 127
pattern IAdd    :: Operation
pattern IAdd    = Code 128
pattern FAdd    :: Operation
pattern FAdd    = Code 129
pattern ISub    :: Operation
pattern ISub    = Code 130
pattern FSub    :: Operation
pattern FSub    = Code 131
pattern IMul    :: Operation
pattern IMul    = Code 132
pattern FMul    :: Operation
pattern FMul    = Code 133
pattern UDiv    :: Operation
pattern UDiv    = Code 134
pattern SDiv    :: Operation
pattern SDiv    = Code 135
pattern FDiv    :: Operation
pattern FDiv    = Code 136
pattern UMod    :: Operation
pattern UMod    = Code 137
pattern SRem    :: Operation
pattern SRem    = Code 138
pattern SMod    :: Operation
pattern SMod    = Code 139
pattern FRem    :: Operation
pattern FRem    = Code 140
pattern FMod    :: Operation
pattern FMod    = Code 141
-- vector and matrix instructions
pattern Transpose         :: Operation
pattern Transpose         = Code 84
pattern VectorTimesScalar :: Operation
pattern VectorTimesScalar = Code 142
pattern MatrixTimesScalar :: Operation
pattern MatrixTimesScalar = Code 143
pattern VectorTimesMatrix :: Operation
pattern VectorTimesMatrix = Code 144
pattern MatrixTimesVector :: Operation
pattern MatrixTimesVector = Code 145
pattern MatrixTimesMatrix :: Operation
pattern MatrixTimesMatrix = Code 146
pattern OuterProduct      :: Operation
pattern OuterProduct      = Code 147
pattern Dot               :: Operation
pattern Dot               = Code 148
-- boolean instructions
pattern LogicalEqual    :: Operation
pattern LogicalEqual    = Code 164
pattern LogicalNotEqual :: Operation
pattern LogicalNotEqual = Code 165
pattern LogicalOr       :: Operation
pattern LogicalOr       = Code 166
pattern LogicalAnd      :: Operation
pattern LogicalAnd      = Code 167
pattern LogicalNot      :: Operation
pattern LogicalNot      = Code 168
-- integer comparisons
pattern IEqual            :: Operation
pattern IEqual            = Code 170
pattern INotEqual         :: Operation
pattern INotEqual         = Code 171
pattern UGreaterThan      :: Operation
pattern UGreaterThan      = Code 172
pattern SGreaterThan      :: Operation
pattern SGreaterThan      = Code 173
pattern UGreaterThanEqual :: Operation
pattern UGreaterThanEqual = Code 174
pattern SGreaterThanEqual :: Operation
pattern SGreaterThanEqual = Code 175
pattern ULessThan         :: Operation
pattern ULessThan         = Code 176 
pattern SLessThan         :: Operation
pattern SLessThan         = Code 177
pattern ULessThanEqual    :: Operation
pattern ULessThanEqual    = Code 178
pattern SLessThanEqual    :: Operation
pattern SLessThanEqual    = Code 179
-- floating point comparisons
pattern FOrdEqual              :: Operation
pattern FOrdEqual              = Code 180
pattern FUnordEqual            :: Operation
pattern FUnordEqual            = Code 181
pattern FOrdNotEqual           :: Operation
pattern FOrdNotEqual           = Code 182
pattern FUnordNotEqual         :: Operation
pattern FUnordNotEqual         = Code 183
pattern FOrdLessThan           :: Operation
pattern FOrdLessThan           = Code 184
pattern FUnordLessThan         :: Operation
pattern FUnordLessThan         = Code 185
pattern FOrdGreaterThan        :: Operation
pattern FOrdGreaterThan        = Code 186
pattern FUnordGreaterThan      :: Operation
pattern FUnordGreaterThan      = Code 187
pattern FOrdLessThanEqual      :: Operation
pattern FOrdLessThanEqual      = Code 188
pattern FUnordLessThanEqual    :: Operation
pattern FUnordLessThanEqual    = Code 189
pattern FOrdGreaterThanEqual   :: Operation
pattern FOrdGreaterThanEqual   = Code 190
pattern FUnordGreaterThanEqual :: Operation
pattern FUnordGreaterThanEqual = Code 191
-- control flow
pattern Phi               :: Operation
pattern Phi               = Code 245
pattern LoopMerge         :: Operation
pattern LoopMerge         = Code 246
pattern SelectionMerge    :: Operation
pattern SelectionMerge    = Code 247
pattern Label             :: Operation
pattern Label             = Code 248
pattern Branch            :: Operation
pattern Branch            = Code 249
pattern BranchConditional :: Operation
pattern BranchConditional = Code 250
pattern Switch            :: Operation
pattern Switch            = Code 251
pattern Kill              :: Operation
pattern Kill              = Code 252
pattern Return            :: Operation
pattern Return            = Code 253
pattern ReturnValue       :: Operation
pattern ReturnValue       = Code 254
pattern Unreachable       :: Operation
pattern Unreachable       = Code 255
-- GLSL extended instructions
pattern FAbs          :: Operation
pattern FAbs          = ExtCode GLSL 4
pattern SAbs          :: Operation
pattern SAbs          = ExtCode GLSL 5
pattern FSign         :: Operation
pattern FSign         = ExtCode GLSL 6
pattern SSign         :: Operation
pattern SSign         = ExtCode GLSL 7
pattern Sin           :: Operation
pattern Sin           = ExtCode GLSL 13
pattern Cos           :: Operation
pattern Cos           = ExtCode GLSL 14
pattern Tan           :: Operation
pattern Tan           = ExtCode GLSL 15
pattern Asin          :: Operation
pattern Asin          = ExtCode GLSL 16
pattern Acos          :: Operation
pattern Acos          = ExtCode GLSL 17
pattern Atan          :: Operation
pattern Atan          = ExtCode GLSL 18
pattern Sinh          :: Operation
pattern Sinh          = ExtCode GLSL 19
pattern Cosh          :: Operation
pattern Cosh          = ExtCode GLSL 20
pattern Tanh          :: Operation
pattern Tanh          = ExtCode GLSL 21
pattern Asinh         :: Operation
pattern Asinh         = ExtCode GLSL 22
pattern Acosh         :: Operation
pattern Acosh         = ExtCode GLSL 23
pattern Atanh         :: Operation
pattern Atanh         = ExtCode GLSL 24
pattern Atan2         :: Operation
pattern Atan2         = ExtCode GLSL 25
pattern Pow           :: Operation
pattern Pow           = ExtCode GLSL 26
pattern Exp           :: Operation
pattern Exp           = ExtCode GLSL 27
pattern Log           :: Operation
pattern Log           = ExtCode GLSL 28
pattern Sqrt          :: Operation
pattern Sqrt          = ExtCode GLSL 31
pattern Invsqrt       :: Operation
pattern Invsqrt       = ExtCode GLSL 32
pattern Determinant   :: Operation
pattern Determinant   = ExtCode GLSL 33
pattern MatrixInverse :: Operation
pattern MatrixInverse = ExtCode GLSL 34
pattern FMin          :: Operation
pattern FMin          = ExtCode GLSL 37
pattern UMin          :: Operation
pattern UMin          = ExtCode GLSL 38
pattern SMin          :: Operation
pattern SMin          = ExtCode GLSL 39
pattern FMax          :: Operation
pattern FMax          = ExtCode GLSL 40
pattern UMax          :: Operation
pattern UMax          = ExtCode GLSL 41
pattern SMax          :: Operation
pattern SMax          = ExtCode GLSL 42
pattern Cross         :: Operation
pattern Cross         = ExtCode GLSL 68

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
showOperation Phi = "Phi"
showOperation LoopMerge = "LoopMerge"
showOperation SelectionMerge = "SelectionMerge"
showOperation Label = "Label"
showOperation Branch = "Branch"
showOperation BranchConditional = "BranchConditional"
showOperation Switch = "Switch"
showOperation Kill = "Kill"
showOperation Return = "Return"
showOperation ReturnValue = "ReturnValue"
showOperation Unreachable = "Unreachable"
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
showOperation (Code i) = show i
showOperation (ExtCode _ i) = show i