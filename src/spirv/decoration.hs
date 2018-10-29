{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

module SPIRV.Decoration where

-- base
import Data.Word(Word32)

-- fir
import Data.Binary.Class.Put(Put)

--------------------------------------------------

newtype Decoration = Decoration Word32
  deriving ( Eq, Ord, Put )

instance Show Decoration where
  show decoration = "Decoration" ++ showDecoration decoration


pattern RelaxedPrecision :: Decoration
pattern RelaxedPrecision = Decoration 0

pattern SpecId :: Decoration
pattern SpecId = Decoration 1

pattern Block :: Decoration
pattern Block = Decoration 2

pattern BufferBlock :: Decoration
pattern BufferBlock = Decoration 3

pattern RowMajor :: Decoration
pattern RowMajor = Decoration 4

pattern ColMajor :: Decoration
pattern ColMajor = Decoration 5

pattern ArrayStride :: Decoration
pattern ArrayStride = Decoration 6

pattern MatrixStride :: Decoration
pattern MatrixStride = Decoration 7

pattern GLSLShared :: Decoration
pattern GLSLShared = Decoration 8

pattern GLSLPacked :: Decoration
pattern GLSLPacked = Decoration 9

pattern CPacked :: Decoration
pattern CPacked = Decoration 10

pattern Builtin :: Decoration
pattern Builtin = Decoration 11

-- no 12

pattern NoPerspective :: Decoration
pattern NoPerspective = Decoration 13

pattern Flat :: Decoration
pattern Flat = Decoration 14

pattern Patch :: Decoration
pattern Patch = Decoration 15

pattern Centroid :: Decoration
pattern Centroid = Decoration 16

pattern Sample :: Decoration
pattern Sample = Decoration 17

pattern Invariant :: Decoration
pattern Invariant = Decoration 18

pattern Restrict :: Decoration
pattern Restrict = Decoration 19

pattern Aliased :: Decoration
pattern Aliased = Decoration 20

pattern Volatile :: Decoration
pattern Volatile = Decoration 21

pattern Constant :: Decoration
pattern Constant = Decoration 22

pattern Coherent :: Decoration
pattern Coherent = Decoration 23

pattern NonWritable :: Decoration
pattern NonWritable = Decoration 24

pattern NonReadable :: Decoration
pattern NonReadable = Decoration 25

pattern Uniform :: Decoration
pattern Uniform = Decoration 26

-- no 27

pattern SaturatedConversion :: Decoration
pattern SaturatedConversion = Decoration 28

pattern Stream :: Decoration
pattern Stream = Decoration 29

pattern Location :: Decoration
pattern Location = Decoration 30

pattern Component :: Decoration
pattern Component = Decoration 31

pattern Index :: Decoration
pattern Index = Decoration 32

pattern Binding :: Decoration
pattern Binding = Decoration 33

pattern DescriptorSet :: Decoration
pattern DescriptorSet = Decoration 34

pattern Offset :: Decoration
pattern Offset = Decoration 35

pattern XfbBuffer :: Decoration
pattern XfbBuffer = Decoration 36

pattern XfbStride :: Decoration
pattern XfbStride = Decoration 37

pattern FuncParamAttr :: Decoration
pattern FuncParamAttr = Decoration 38

pattern FPRoundingMode :: Decoration
pattern FPRoundingMode = Decoration 39

pattern FPFastMathMode :: Decoration
pattern FPFastMathMode = Decoration 40

pattern LinkageAttributes :: Decoration
pattern LinkageAttributes = Decoration 41

pattern NoContraction :: Decoration
pattern NoContraction = Decoration 42

pattern InputAttachmentIndex :: Decoration
pattern InputAttachmentIndex = Decoration 43

pattern Alignment :: Decoration
pattern Alignment = Decoration 44



showDecoration :: Decoration -> String
showDecoration RelaxedPrecision = "RelaxedPrecision"
showDecoration SpecId = "SpecId"
showDecoration Block = "Block"
showDecoration BufferBlock = "BufferBlock"
showDecoration RowMajor = "RowMajor"
showDecoration ColMajor = "ColMajor"
showDecoration ArrayStride = "ArrayStride"
showDecoration MatrixStride = "MatrixStride"
showDecoration GLSLShared = "GLSLShared"
showDecoration GLSLPacked = "GLSLPacked"
showDecoration CPacked = "CPacked"
showDecoration Builtin = "Builtin"
showDecoration NoPerspective = "NoPerspective"
showDecoration Flat = "Flat"
showDecoration Patch = "Patch"
showDecoration Centroid = "Centroid"
showDecoration Sample = "Sample"
showDecoration Invariant = "Invariant"
showDecoration Restrict = "Restrict"
showDecoration Aliased = "Aliased"
showDecoration Volatile = "Volatile"
showDecoration Constant = "Constant"
showDecoration Coherent = "Coherent"
showDecoration NonWritable = "NonWritable"
showDecoration NonReadable = "NonReadable"
showDecoration Uniform = "Uniform"
showDecoration SaturatedConversion = "SaturatedConversion"
showDecoration Stream = "Stream"
showDecoration Location = "Location"
showDecoration Component = "Component"
showDecoration Index = "Index"
showDecoration Binding = "Binding"
showDecoration DescriptorSet = "DescriptorSet"
showDecoration Offset = "Offset"
showDecoration XfbBuffer = "XfbBuffer"
showDecoration XfbStride = "XfbStride"
showDecoration FuncParamAttr = "FuncParamAttr"
showDecoration FPRoundingMode = "FPRoundingMode"
showDecoration FPFastMathMode = "FPFastMathMode"
showDecoration LinkageAttributes = "LinkageAttributes"
showDecoration NoContraction = "NoContraction"
showDecoration InputAttachmentIndex = "InputAttachmentIndex"
showDecoration Alignment = "Alignment"
showDecoration (Decoration i) = show i