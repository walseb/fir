{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}

{-|
Module: SPIRV.Operation

Raw SPIR-V OpCodes. Used only at the value-level.

See "SPIRV.PrimOp" which categorises operations.

-}

module SPIRV.Operation where

-- base
import Data.Word
  ( Word16, Word32 )

-- fir
import SPIRV.Extension
  ( ExtInst(GLSL, OpenCL) )
import qualified SPIRV.Extension
  ( ExtInst(DebugPrintf) )

--------------------------------------------------
-- operation data type

data Operation
  = Code Word16
  | ExtCode ExtInst Word32
  deriving stock Eq

instance Show Operation where
  show op@(Code _) = showOperation op
  show op@(ExtCode ext _) = "Ext(" ++ show ext ++") " ++ showOperation op

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
pattern SampledImage :: Operation
pattern SampledImage = Code 86
pattern ImageSampleImplicitLod :: Operation
pattern ImageSampleImplicitLod = Code 87
pattern ImageSampleExplicitLod :: Operation
pattern ImageSampleExplicitLod = Code 88
pattern ImageSampleDrefImplicitLod :: Operation
pattern ImageSampleDrefImplicitLod = Code 89
pattern ImageSampleDrefExplicitLod :: Operation
pattern ImageSampleDrefExplicitLod = Code 90
pattern ImageSampleProjImplicitLod :: Operation
pattern ImageSampleProjImplicitLod = Code 91
pattern ImageSampleProjExplicitLod :: Operation
pattern ImageSampleProjExplicitLod = Code 92
pattern ImageSampleProjDrefImplicitLod :: Operation
pattern ImageSampleProjDrefImplicitLod = Code 93
pattern ImageSampleProjDrefExplicitLod :: Operation
pattern ImageSampleProjDrefExplicitLod = Code 94
pattern ImageFetch :: Operation
pattern ImageFetch = Code 95
pattern ImageGather :: Operation
pattern ImageGather = Code 96
pattern ImageDrefGather :: Operation
pattern ImageDrefGather = Code 97
pattern ImageRead :: Operation
pattern ImageRead = Code 98
pattern ImageWrite :: Operation
pattern ImageWrite = Code 99
pattern Image :: Operation
pattern Image = Code 100
-- image queries
-- sparse image instructions
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
-- bit cast
pattern BitCast :: Operation
pattern BitCast = Code 124
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
-- IEEE floating point logical instructions
pattern IsNan :: Operation
pattern IsNan = Code 156
pattern IsInf :: Operation
pattern IsInf = Code 157
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
pattern Select          :: Operation
pattern Select          = Code 169
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
-- bitwise operations
pattern ShiftRightLogical    :: Operation
pattern ShiftRightLogical    = Code 194
pattern ShiftRightArithmetic :: Operation
pattern ShiftRightArithmetic = Code 195
pattern ShiftLeftLogical     :: Operation
pattern ShiftLeftLogical     = Code 196
pattern BitwiseOr            :: Operation
pattern BitwiseOr            = Code 197
pattern BitwiseXor           :: Operation
pattern BitwiseXor           = Code 198
pattern BitwiseAnd           :: Operation
pattern BitwiseAnd           = Code 199
pattern Not                  :: Operation
pattern Not                  = Code 200
-- geometry primitives
pattern EmitVertex :: Operation
pattern EmitVertex = Code 218
pattern EndPrimitive :: Operation
pattern EndPrimitive = Code 219
pattern EmitStreamVertex :: Operation
pattern EmitStreamVertex = Code 220
pattern EndStreamPrimitive :: Operation
pattern EndStreamPrimitive = Code 221
-- memory synchronisation
pattern ControlBarrier :: Operation
pattern ControlBarrier = Code 224
pattern MemoryBarrier :: Operation
pattern MemoryBarrier = Code 225
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
-- ray tracing instructions
pattern TraceRay                        :: Operation
pattern TraceRay                        = Code 4445
pattern ExecuteCallable                 :: Operation
pattern ExecuteCallable                 = Code 4446
pattern ConvertUToAccelerationStructure :: Operation
pattern ConvertUToAccelerationStructure = Code 4447
pattern IgnoreIntersection              :: Operation
pattern IgnoreIntersection              = Code 4448
pattern TerminateRay                    :: Operation
pattern TerminateRay                    = Code 4449
pattern ReportIntersection              :: Operation
pattern ReportIntersection              = Code 5334
pattern TypeAccelerationStructure       :: Operation
pattern TypeAccelerationStructure       = Code 5341
-- ray query instructions
pattern TypeRayQuery                                                  :: Operation
pattern TypeRayQuery                                                  = Code 4472
pattern RayQueryInitialize                                            :: Operation
pattern RayQueryInitialize                                            = Code 4473
pattern RayQueryTerminate                                             :: Operation
pattern RayQueryTerminate                                             = Code 4474
pattern RayQueryGenerateIntersection                                  :: Operation
pattern RayQueryGenerateIntersection                                  = Code 4475
pattern RayQueryConfirmIntersection                                   :: Operation
pattern RayQueryConfirmIntersection                                   = Code 4476
pattern RayQueryProceed                                               :: Operation
pattern RayQueryProceed                                               = Code 4477
pattern RayQueryGetIntersectionType                                   :: Operation
pattern RayQueryGetIntersectionType                                   = Code 4479
pattern RayQueryGetRayTMin                                            :: Operation
pattern RayQueryGetRayTMin                                            = Code 6016
pattern RayQueryGetRayFlags                                           :: Operation
pattern RayQueryGetRayFlags                                           = Code 6017
pattern RayQueryGetIntersectionT                                      :: Operation
pattern RayQueryGetIntersectionT                                      = Code 6018
pattern RayQueryGetIntersectionInstanceCustomIndex                    :: Operation
pattern RayQueryGetIntersectionInstanceCustomIndex                    = Code 6019
pattern RayQueryGetIntersectionInstanceId                             :: Operation
pattern RayQueryGetIntersectionInstanceId                             = Code 6020
pattern RayQueryGetIntersectionInstanceShaderBindingTableRecordOffset :: Operation
pattern RayQueryGetIntersectionInstanceShaderBindingTableRecordOffset = Code 6021
pattern RayQueryGetIntersectionGeometryIndex                          :: Operation
pattern RayQueryGetIntersectionGeometryIndex                          = Code 6022
pattern RayQueryGetIntersectionPrimitiveIndex                         :: Operation
pattern RayQueryGetIntersectionPrimitiveIndex                         = Code 6023
pattern RayQueryGetIntersectionBarycentrics                           :: Operation
pattern RayQueryGetIntersectionBarycentrics                           = Code 6024
pattern RayQueryGetIntersectionFrontFace                              :: Operation
pattern RayQueryGetIntersectionFrontFace                              = Code 6025
pattern RayQueryGetIntersectionCandidateAABBOpaque                    :: Operation
pattern RayQueryGetIntersectionCandidateAABBOpaque                    = Code 6026
pattern RayQueryGetIntersectionObjectRayDirection                     :: Operation
pattern RayQueryGetIntersectionObjectRayDirection                     = Code 6027
pattern RayQueryGetIntersectionObjectRayOrigin                        :: Operation
pattern RayQueryGetIntersectionObjectRayOrigin                        = Code 6028
pattern RayQueryGetWorldRayDirection                                  :: Operation
pattern RayQueryGetWorldRayDirection                                  = Code 6029
pattern RayQueryGetWorldRayOrigin                                     :: Operation
pattern RayQueryGetWorldRayOrigin                                     = Code 6030
pattern RayQueryGetIntersectionObjectToWorld                          :: Operation
pattern RayQueryGetIntersectionObjectToWorld                          = Code 6031
pattern RayQueryGetIntersectionWorldToObject                          :: Operation
pattern RayQueryGetIntersectionWorldToObject                          = Code 6032
-- GLSL extended instructions
pattern GLSL_Round         :: Operation
pattern GLSL_Round         = ExtCode GLSL 1
pattern GLSL_RoundEven     :: Operation
pattern GLSL_RoundEven     = ExtCode GLSL 2
pattern GLSL_Trunc         :: Operation
pattern GLSL_Trunc         = ExtCode GLSL 3
pattern GLSL_FAbs          :: Operation
pattern GLSL_FAbs          = ExtCode GLSL 4
pattern GLSL_SAbs          :: Operation
pattern GLSL_SAbs          = ExtCode GLSL 5
pattern GLSL_FSign         :: Operation
pattern GLSL_FSign         = ExtCode GLSL 6
pattern GLSL_SSign         :: Operation
pattern GLSL_SSign         = ExtCode GLSL 7
pattern GLSL_Floor         :: Operation
pattern GLSL_Floor         = ExtCode GLSL 8
pattern GLSL_Ceil          :: Operation
pattern GLSL_Ceil          = ExtCode GLSL 9
pattern GLSL_Fract         :: Operation
pattern GLSL_Fract         = ExtCode GLSL 10
pattern GLSL_Sin           :: Operation
pattern GLSL_Sin           = ExtCode GLSL 13
pattern GLSL_Cos           :: Operation
pattern GLSL_Cos           = ExtCode GLSL 14
pattern GLSL_Tan           :: Operation
pattern GLSL_Tan           = ExtCode GLSL 15
pattern GLSL_Asin          :: Operation
pattern GLSL_Asin          = ExtCode GLSL 16
pattern GLSL_Acos          :: Operation
pattern GLSL_Acos          = ExtCode GLSL 17
pattern GLSL_Atan          :: Operation
pattern GLSL_Atan          = ExtCode GLSL 18
pattern GLSL_Sinh          :: Operation
pattern GLSL_Sinh          = ExtCode GLSL 19
pattern GLSL_Cosh          :: Operation
pattern GLSL_Cosh          = ExtCode GLSL 20
pattern GLSL_Tanh          :: Operation
pattern GLSL_Tanh          = ExtCode GLSL 21
pattern GLSL_Asinh         :: Operation
pattern GLSL_Asinh         = ExtCode GLSL 22
pattern GLSL_Acosh         :: Operation
pattern GLSL_Acosh         = ExtCode GLSL 23
pattern GLSL_Atanh         :: Operation
pattern GLSL_Atanh         = ExtCode GLSL 24
pattern GLSL_Atan2         :: Operation
pattern GLSL_Atan2         = ExtCode GLSL 25
pattern GLSL_Pow           :: Operation
pattern GLSL_Pow           = ExtCode GLSL 26
pattern GLSL_Exp           :: Operation
pattern GLSL_Exp           = ExtCode GLSL 27
pattern GLSL_Log           :: Operation
pattern GLSL_Log           = ExtCode GLSL 28
pattern GLSL_Sqrt          :: Operation
pattern GLSL_Sqrt          = ExtCode GLSL 31
pattern GLSL_InvSqrt       :: Operation
pattern GLSL_InvSqrt       = ExtCode GLSL 32
pattern GLSL_Determinant   :: Operation
pattern GLSL_Determinant   = ExtCode GLSL 33
pattern GLSL_MatrixInverse :: Operation
pattern GLSL_MatrixInverse = ExtCode GLSL 34
pattern GLSL_FMin          :: Operation
pattern GLSL_FMin          = ExtCode GLSL 37
pattern GLSL_UMin          :: Operation
pattern GLSL_UMin          = ExtCode GLSL 38
pattern GLSL_SMin          :: Operation
pattern GLSL_SMin          = ExtCode GLSL 39
pattern GLSL_FMax          :: Operation
pattern GLSL_FMax          = ExtCode GLSL 40
pattern GLSL_UMax          :: Operation
pattern GLSL_UMax          = ExtCode GLSL 41
pattern GLSL_SMax          :: Operation
pattern GLSL_SMax          = ExtCode GLSL 42
pattern GLSL_Cross         :: Operation
pattern GLSL_Cross         = ExtCode GLSL 68
pattern GLSL_Normalize     :: Operation
pattern GLSL_Normalize     = ExtCode GLSL 69
-- OpenCL extended instructions
  -- floating point instructions
pattern OpenCL_Acos :: Operation
pattern OpenCL_Acos = ExtCode OpenCL 0
pattern OpenCL_Acosh :: Operation
pattern OpenCL_Acosh = ExtCode OpenCL 1
pattern OpenCL_Asin :: Operation
pattern OpenCL_Asin = ExtCode OpenCL 3
pattern OpenCL_Asinh :: Operation
pattern OpenCL_Asinh = ExtCode OpenCL 4
pattern OpenCL_Atan :: Operation
pattern OpenCL_Atan = ExtCode OpenCL 6
pattern OpenCL_Atan2 :: Operation
pattern OpenCL_Atan2 = ExtCode OpenCL 7
pattern OpenCL_Atanh :: Operation
pattern OpenCL_Atanh = ExtCode OpenCL 8
pattern OpenCL_Cbrt :: Operation
pattern OpenCL_Cbrt = ExtCode OpenCL 11
pattern OpenCL_Ceil :: Operation
pattern OpenCL_Ceil = ExtCode OpenCL 12
pattern OpenCL_Cos :: Operation
pattern OpenCL_Cos = ExtCode OpenCL 14
pattern OpenCL_Cosh :: Operation
pattern OpenCL_Cosh = ExtCode OpenCL 15
pattern OpenCL_Erfc :: Operation
pattern OpenCL_Erfc = ExtCode OpenCL 17
pattern OpenCL_Erf :: Operation
pattern OpenCL_Erf = ExtCode OpenCL 18
pattern OpenCL_Exp :: Operation
pattern OpenCL_Exp = ExtCode OpenCL 19
pattern OpenCL_Exp2 :: Operation
pattern OpenCL_Exp2 = ExtCode OpenCL 20
pattern OpenCL_Exp10 :: Operation
pattern OpenCL_Exp10 = ExtCode OpenCL 21
pattern OpenCL_Expm1 :: Operation
pattern OpenCL_Expm1 = ExtCode OpenCL 22
pattern OpenCL_FAbs :: Operation
pattern OpenCL_FAbs = ExtCode OpenCL 23
pattern OpenCL_FDim :: Operation
pattern OpenCL_FDim = ExtCode OpenCL 24
pattern OpenCL_Floor :: Operation
pattern OpenCL_Floor = ExtCode OpenCL 25
pattern OpenCL_FMAdd :: Operation
pattern OpenCL_FMAdd = ExtCode OpenCL 26
pattern OpenCL_FMax :: Operation
pattern OpenCL_FMax = ExtCode OpenCL 27
pattern OpenCL_FMin :: Operation
pattern OpenCL_FMin = ExtCode OpenCL 28
pattern OpenCL_FMod :: Operation
pattern OpenCL_FMod = ExtCode OpenCL 29
pattern OpenCL_Fract :: Operation
pattern OpenCL_Fract = ExtCode OpenCL 30
pattern OpenCL_FRExp :: Operation
pattern OpenCL_FRExp = ExtCode OpenCL 31
pattern OpenCL_Hypot :: Operation
pattern OpenCL_Hypot = ExtCode OpenCL 32
pattern OpenCL_LogGamma :: Operation
pattern OpenCL_LogGamma = ExtCode OpenCL 35
pattern OpenCL_Log :: Operation
pattern OpenCL_Log = ExtCode OpenCL 37
pattern OpenCL_Log2 :: Operation
pattern OpenCL_Log2 = ExtCode OpenCL 38
pattern OpenCL_Log10 :: Operation
pattern OpenCL_Log10 = ExtCode OpenCL 39
pattern OpenCL_Log1P :: Operation
pattern OpenCL_Log1P = ExtCode OpenCL 40
pattern OpenCL_LogB :: Operation
pattern OpenCL_LogB = ExtCode OpenCL 41
pattern OpenCL_MAdd :: Operation
pattern OpenCL_MAdd = ExtCode OpenCL 42
pattern OpenCL_MaxMag :: Operation
pattern OpenCL_MaxMag = ExtCode OpenCL 43
pattern OpenCL_MinMag :: Operation
pattern OpenCL_MinMag = ExtCode OpenCL 44
pattern OpenCL_ModF :: Operation
pattern OpenCL_ModF = ExtCode OpenCL 45
pattern OpenCL_NaN :: Operation
pattern OpenCL_NaN = ExtCode OpenCL 46
pattern OpenCL_NextAfter :: Operation
pattern OpenCL_NextAfter = ExtCode OpenCL 47
pattern OpenCL_Pow :: Operation
pattern OpenCL_Pow = ExtCode OpenCL 48
pattern OpenCL_FRem :: Operation
pattern OpenCL_FRem = ExtCode OpenCL 51
pattern OpenCL_FRemQuot :: Operation
pattern OpenCL_FRemQuot = ExtCode OpenCL 52
pattern OpenCL_RoundEven :: Operation
pattern OpenCL_RoundEven = ExtCode OpenCL 53
pattern OpenCL_Round :: Operation
pattern OpenCL_Round = ExtCode OpenCL 55
pattern OpenCL_InvSqrt :: Operation
pattern OpenCL_InvSqrt = ExtCode OpenCL 56
pattern OpenCL_Sin :: Operation
pattern OpenCL_Sin = ExtCode OpenCL 57
pattern OpenCL_SinCos :: Operation
pattern OpenCL_SinCos = ExtCode OpenCL 58
pattern OpenCL_Sinh :: Operation
pattern OpenCL_Sinh = ExtCode OpenCL 59
pattern OpenCL_Sqrt :: Operation
pattern OpenCL_Sqrt = ExtCode OpenCL 61
pattern OpenCL_Tan :: Operation
pattern OpenCL_Tan = ExtCode OpenCL 62
pattern OpenCL_Tanh :: Operation
pattern OpenCL_Tanh = ExtCode OpenCL 63
pattern OpenCL_Gamma :: Operation
pattern OpenCL_Gamma = ExtCode OpenCL 65
pattern OpenCL_Trunc :: Operation
pattern OpenCL_Trunc = ExtCode OpenCL 66
pattern OpenCL_FSign :: Operation
pattern OpenCL_FSign = ExtCode OpenCL 103
  -- integral instructions
pattern OpenCL_SAbs :: Operation
pattern OpenCL_SAbs = ExtCode OpenCL 141
pattern OpenCL_SAbsDiff :: Operation
pattern OpenCL_SAbsDiff = ExtCode OpenCL 142
pattern OpenCL_SMax :: Operation
pattern OpenCL_SMax = ExtCode OpenCL 156
pattern OpenCL_UMax :: Operation
pattern OpenCL_UMax = ExtCode OpenCL 157
pattern OpenCL_SMin :: Operation
pattern OpenCL_SMin = ExtCode OpenCL 158
pattern OpenCL_UMin :: Operation
pattern OpenCL_UMin = ExtCode OpenCL 159
pattern OpenCL_UAbs :: Operation
pattern OpenCL_UAbs = ExtCode OpenCL 201
pattern OpenCL_UAbsDiff :: Operation
pattern OpenCL_UAbsDiff = ExtCode OpenCL 202
  -- vectors and matrices
pattern OpenCL_Cross :: Operation
pattern OpenCL_Cross = ExtCode OpenCL 104
pattern OpenCL_Normalize :: Operation
pattern OpenCL_Normalize = ExtCode OpenCL 107
-- Non-semantic DebugPrintf instruction
pattern DebugPrintf :: Operation
pattern DebugPrintf = ExtCode SPIRV.Extension.DebugPrintf 1

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
showOperation SampledImage = "SampledImage"
showOperation ImageSampleImplicitLod = "ImageSampleImplicitLod"
showOperation ImageSampleExplicitLod = "ImageSampleExplicitLod"
showOperation ImageSampleDrefImplicitLod = "ImageSampleDrefImplicitLod"
showOperation ImageSampleDrefExplicitLod = "ImageSampleDrefExplicitLod"
showOperation ImageSampleProjImplicitLod = "ImageSampleProjImplicitLod"
showOperation ImageSampleProjExplicitLod = "ImageSampleProjExplicitLod"
showOperation ImageSampleProjDrefImplicitLod = "ImageSampleProjDrefImplicitLod"
showOperation ImageSampleProjDrefExplicitLod = "ImageSampleProjDrefExplicitLod"
showOperation ImageFetch = "ImageFetch"
showOperation ImageGather = "ImageGather"
showOperation ImageDrefGather = "ImageDrefGather"
showOperation ImageRead = "ImageRead"
showOperation ImageWrite = "ImageWrite"
showOperation Image = "Image"
showOperation ConvertFToU = "ConvertFToU"
showOperation ConvertFToS = "ConvertFToS"
showOperation ConvertSToF = "ConvertSToF"
showOperation ConvertUToF = "ConvertUToF"
showOperation UConvert = "UConvert"
showOperation SConvert = "SConvert"
showOperation FConvert = "FConvert"
showOperation SatConvertSToU = "SatConvertSToU"
showOperation SatConvertUToS = "SatConvertUToS"
showOperation BitCast = "BitCast"
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
showOperation LogicalEqual = "LogicalEqual"
showOperation LogicalNotEqual = "LogicalNotEqual"
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
showOperation ShiftRightLogical    = "ShiftRightLogical"
showOperation ShiftRightArithmetic = "ShiftRightArithmetic"
showOperation ShiftLeftLogical     = "ShiftLeftLogical"
showOperation BitwiseOr            = "BitwiseOr"
showOperation BitwiseXor           = "BitwiseXor"
showOperation BitwiseAnd           = "BitwiseAnd"
showOperation Not                  = "Not"
showOperation EmitVertex         = "EmitVertex"
showOperation EndPrimitive       = "EndPrimitive"
showOperation EmitStreamVertex   = "EmitStreamVertex"
showOperation EndStreamPrimitive = "EndStreamPrimitive"
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
-- ray tracing instructions
showOperation TraceRay                        = "TraceRay"
showOperation ExecuteCallable                 = "ExecuteCallable"
showOperation ConvertUToAccelerationStructure = "ConvertUToAccelerationStructure"
showOperation IgnoreIntersection              = "IgnoreIntersection"
showOperation TerminateRay                    = "TerminateRay"
showOperation ReportIntersection              = "ReportIntersection"
showOperation TypeAccelerationStructure       = "TypeAccelerationStructure"
-- ray query instructions
showOperation TypeRayQuery                                                  = "TypeRayQuery"
showOperation RayQueryInitialize                                            = "RayQueryInitialize"
showOperation RayQueryTerminate                                             = "RayQueryTerminate"
showOperation RayQueryGenerateIntersection                                  = "RayQueryGenerateIntersection"
showOperation RayQueryConfirmIntersection                                   = "RayQueryConfirmIntersection"
showOperation RayQueryProceed                                               = "RayQueryProceed"
showOperation RayQueryGetIntersectionType                                   = "RayQueryGetIntersectionType"
showOperation RayQueryGetRayTMin                                            = "RayQueryGetRayTMin"
showOperation RayQueryGetRayFlags                                           = "RayQueryGetRayFlags"
showOperation RayQueryGetIntersectionT                                      = "RayQueryGetIntersectionT"
showOperation RayQueryGetIntersectionInstanceCustomIndex                    = "RayQueryGetIntersectionInstanceCustomIndex"
showOperation RayQueryGetIntersectionInstanceId                             = "RayQueryGetIntersectionInstanceId"
showOperation RayQueryGetIntersectionInstanceShaderBindingTableRecordOffset = "RayQueryGetIntersectionInstanceShaderBindingTableRecordOffset"
showOperation RayQueryGetIntersectionGeometryIndex                          = "RayQueryGetIntersectionGeometryIndex"
showOperation RayQueryGetIntersectionPrimitiveIndex                         = "RayQueryGetIntersectionPrimitiveIndex"
showOperation RayQueryGetIntersectionBarycentrics                           = "RayQueryGetIntersectionBarycentrics"
showOperation RayQueryGetIntersectionFrontFace                              = "RayQueryGetIntersectionFrontFace"
showOperation RayQueryGetIntersectionCandidateAABBOpaque                    = "RayQueryGetIntersectionCandidateAABBOpaque"
showOperation RayQueryGetIntersectionObjectRayDirection                     = "RayQueryGetIntersectionObjectRayDirection"
showOperation RayQueryGetIntersectionObjectRayOrigin                        = "RayQueryGetIntersectionObjectRayOrigin"
showOperation RayQueryGetWorldRayDirection                                  = "RayQueryGetWorldRayDirection"
showOperation RayQueryGetWorldRayOrigin                                     = "RayQueryGetWorldRayOrigin"
showOperation RayQueryGetIntersectionObjectToWorld                          = "RayQueryGetIntersectionObjectToWorld"
showOperation RayQueryGetIntersectionWorldToObject                          = "RayQueryGetIntersectionWorldToObject"
-- GLSL instructions
showOperation GLSL_Round = "Round"
showOperation GLSL_RoundEven = "RoundEven"
showOperation GLSL_Trunc = "Trunc"
showOperation GLSL_FAbs = "FAbs"
showOperation GLSL_SAbs = "SAbs"
showOperation GLSL_FSign = "FSign"
showOperation GLSL_SSign = "SSign"
showOperation GLSL_Floor = "Floor"
showOperation GLSL_Ceil = "Ceil"
showOperation GLSL_Fract = "Fract"
showOperation GLSL_Sin = "Sin"
showOperation GLSL_Cos = "Cos"
showOperation GLSL_Tan = "Tan"
showOperation GLSL_Asin = "Asin"
showOperation GLSL_Acos = "Acos"
showOperation GLSL_Atan = "Atan"
showOperation GLSL_Sinh = "Sinh"
showOperation GLSL_Cosh = "Cosh"
showOperation GLSL_Tanh = "Tanh"
showOperation GLSL_Asinh = "Asinh"
showOperation GLSL_Acosh = "Acosh"
showOperation GLSL_Atanh = "Atanh"
showOperation GLSL_Atan2 = "Atan2"
showOperation GLSL_Pow = "Pow"
showOperation GLSL_Exp = "Exp"
showOperation GLSL_Log = "Log"
showOperation GLSL_Sqrt = "Sqrt"
showOperation GLSL_InvSqrt = "InvSqrt"
showOperation GLSL_Determinant = "Determinant"
showOperation GLSL_MatrixInverse = "MatrixInverse"
showOperation GLSL_FMin = "FMin"
showOperation GLSL_UMin = "UMin"
showOperation GLSL_SMin = "SMin"
showOperation GLSL_FMax = "FMax"
showOperation GLSL_UMax = "UMax"
showOperation GLSL_SMax = "SMax"
showOperation GLSL_Cross = "Cross"
showOperation GLSL_Normalize = "Normalize"
-- OpenCL instructions
showOperation OpenCL_Acos      = "Acos"
showOperation OpenCL_Acosh     = "Acosh"
showOperation OpenCL_Asin      = "Asin"
showOperation OpenCL_Asinh     = "Asinh"
showOperation OpenCL_Atan      = "Atan"
showOperation OpenCL_Atan2     = "Atan2"
showOperation OpenCL_Atanh     = "Atanh"
showOperation OpenCL_Cbrt      = "Cbrt"
showOperation OpenCL_Ceil      = "Ceil"
showOperation OpenCL_Cos       = "Cos"
showOperation OpenCL_Cosh      = "Cosh"
showOperation OpenCL_Erfc      = "Erfc"
showOperation OpenCL_Erf       = "Erf"
showOperation OpenCL_Exp       = "Exp"
showOperation OpenCL_Exp2      = "Exp2"
showOperation OpenCL_Exp10     = "Exp10"
showOperation OpenCL_Expm1     = "Expm1"
showOperation OpenCL_FAbs      = "FAbs"
showOperation OpenCL_FDim      = "FDim"
showOperation OpenCL_Floor     = "Floor"
showOperation OpenCL_FMAdd     = "FMAdd"
showOperation OpenCL_FMax      = "FMax"
showOperation OpenCL_FMin      = "FMin"
showOperation OpenCL_FMod      = "FMod"
showOperation OpenCL_Fract     = "Fract"
showOperation OpenCL_FRExp     = "FRExp"
showOperation OpenCL_Hypot     = "Hypot"
showOperation OpenCL_LogGamma  = "LogGamma"
showOperation OpenCL_Log       = "Log"
showOperation OpenCL_Log2      = "Log2"
showOperation OpenCL_Log10     = "Log10"
showOperation OpenCL_Log1P     = "Log1P"
showOperation OpenCL_LogB      = "LogB"
showOperation OpenCL_MAdd      = "MAdd"
showOperation OpenCL_MaxMag    = "MaxMag"
showOperation OpenCL_MinMag    = "MinMag"
showOperation OpenCL_ModF      = "ModF"
showOperation OpenCL_NaN       = "NaN"
showOperation OpenCL_NextAfter = "NextAfter"
showOperation OpenCL_Pow       = "Pow"
showOperation OpenCL_FRem      = "FRem"
showOperation OpenCL_FRemQuot  = "FRemQuot"
showOperation OpenCL_RoundEven = "RoundEven"
showOperation OpenCL_Round     = "Round"
showOperation OpenCL_InvSqrt   = "InvSqrt"
showOperation OpenCL_Sin       = "Sin"
showOperation OpenCL_SinCos    = "SinCos"
showOperation OpenCL_Sinh      = "Sinh"
showOperation OpenCL_Sqrt      = "Sqrt"
showOperation OpenCL_Tan       = "Tan"
showOperation OpenCL_Tanh      = "Tanh"
showOperation OpenCL_Gamma     = "Gamma"
showOperation OpenCL_Trunc     = "Trunc"
showOperation OpenCL_FSign     = "FSign"
showOperation OpenCL_SAbs      = "SAbs"
showOperation OpenCL_SAbsDiff  = "SAbsDiff"
showOperation OpenCL_SMax      = "SMax"
showOperation OpenCL_UMax      = "UMax"
showOperation OpenCL_SMin      = "SMin"
showOperation OpenCL_UMin      = "UMin"
showOperation OpenCL_UAbs      = "UAbs"
showOperation OpenCL_UAbsDiff  = "UAbsDiff"
showOperation OpenCL_Cross     = "Cross"
showOperation OpenCL_Normalize = "Normalize"
showOperation DebugPrintf      = "DebugPrintf"
showOperation (Code i) = show i
showOperation (ExtCode _ i) = show i
