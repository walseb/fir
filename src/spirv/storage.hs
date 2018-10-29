{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

module SPIRV.Storage where

-- base
import Data.Word(Word32)

-- fir
import Data.Binary.Class.Put(Put)

--------------------------------------------------

newtype StorageClass = StorageClass Word32
  deriving ( Eq, Ord, Put )

instance Show StorageClass where
  show storageClass = "StorageClass " ++ showStorageClass storageClass

pattern UniformConstant :: StorageClass
pattern UniformConstant = StorageClass 0

pattern Input :: StorageClass
pattern Input = StorageClass 1

pattern Uniform :: StorageClass
pattern Uniform = StorageClass 2

pattern Output :: StorageClass
pattern Output = StorageClass 3

pattern Workgroup :: StorageClass
pattern Workgroup = StorageClass 4

pattern CrossWorkgroup :: StorageClass
pattern CrossWorkgroup = StorageClass 5

pattern Private :: StorageClass
pattern Private = StorageClass 6

pattern Function :: StorageClass
pattern Function = StorageClass 7

pattern Generic :: StorageClass
pattern Generic = StorageClass 8

pattern PushConstant :: StorageClass
pattern PushConstant = StorageClass 9

pattern AtomicCounter :: StorageClass
pattern AtomicCounter = StorageClass 10

pattern Image :: StorageClass
pattern Image = StorageClass 11

pattern StorageBuffer :: StorageClass
pattern StorageBuffer = StorageClass 12


showStorageClass :: StorageClass -> String
showStorageClass UniformConstant = "UniformConstant"
showStorageClass Input = "Input"
showStorageClass Uniform = "Uniform"
showStorageClass Output = "Output"
showStorageClass Workgroup = "Workgroup"
showStorageClass CrossWorkgroup = "CrossWorkgroup"
showStorageClass Private = "Private"
showStorageClass Function = "Function"
showStorageClass Generic = "Generic"
showStorageClass PushConstant = "PushConstant"
showStorageClass AtomicCounter = "AtomicCounter"
showStorageClass Image = "Image"
showStorageClass StorageBuffer = "StorageBuffer"
showStorageClass (StorageClass i) = show i