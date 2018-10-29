{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}

module SPIRV.Capability where

-- base
import Data.Word(Word32)

-- fir
import Data.Binary.Class.Put(Put)
import qualified SPIRV.PrimOp as PrimOp
import qualified SPIRV.PrimTy as PrimTy
import SPIRV.Operation hiding (Capability)
import SPIRV.PrimTy(Width(..))

--------------------------------------------------

newtype Capability = Capability Word32
  deriving ( Eq, Ord, Put )

instance Show Capability where
  show capability = "Capability " ++ showCapability capability


pattern Matrix :: Capability
pattern Matrix = Capability 0

pattern Shader :: Capability
pattern Shader = Capability 1

pattern Geometry :: Capability
pattern Geometry = Capability 2

pattern Tessellation :: Capability
pattern Tessellation = Capability 3

pattern Addresses :: Capability
pattern Addresses = Capability 4

pattern Linkage :: Capability
pattern Linkage = Capability 5

pattern Kernel :: Capability
pattern Kernel = Capability 6

pattern Vector16 :: Capability
pattern Vector16 = Capability 7

pattern Float16Buffer :: Capability
pattern Float16Buffer = Capability 8

pattern Float16 :: Capability
pattern Float16 = Capability 9

pattern Float64 :: Capability
pattern Float64 = Capability 10

pattern Int64 :: Capability
pattern Int64 = Capability 11

pattern Int64Atomics :: Capability
pattern Int64Atomics = Capability 12

pattern ImageBasic :: Capability
pattern ImageBasic = Capability 13

pattern ImageReadWrite :: Capability
pattern ImageReadWrite = Capability 14

pattern ImageMipmap :: Capability
pattern ImageMipmap = Capability 15

-- no 16

pattern Pipes :: Capability
pattern Pipes = Capability 17

pattern Groups :: Capability
pattern Groups = Capability 18

pattern DeviceEnqueue :: Capability
pattern DeviceEnqueue = Capability 19

pattern LiteralSampler :: Capability
pattern LiteralSampler = Capability 20

pattern AtomicStorage :: Capability
pattern AtomicStorage = Capability 21

pattern Int16 :: Capability
pattern Int16 = Capability 22

pattern TessellationPointSize :: Capability
pattern TessellationPointSize = Capability 23

pattern GeometryPointSize :: Capability
pattern GeometryPointSize = Capability 24

pattern ImageGatherExtended :: Capability
pattern ImageGatherExtended = Capability 25

-- no 26

pattern StorageImageMultisample :: Capability
pattern StorageImageMultisample = Capability 27

pattern UniformBufferArrayDynamicIndexing :: Capability
pattern UniformBufferArrayDynamicIndexing = Capability 28

pattern SampledImageArrayDynamicIndexing :: Capability
pattern SampledImageArrayDynamicIndexing = Capability 29

pattern StorageBufferArrayDynamicIndexing :: Capability
pattern StorageBufferArrayDynamicIndexing = Capability 30

pattern StorageImageArrayDynamicIndexing :: Capability
pattern StorageImageArrayDynamicIndexing = Capability 31

pattern ClipDistance :: Capability
pattern ClipDistance = Capability 32

pattern CullDistance :: Capability
pattern CullDistance = Capability 33

pattern ImageCubeArray :: Capability
pattern ImageCubeArray = Capability 34

pattern SampleRateShading :: Capability
pattern SampleRateShading = Capability 35

pattern ImageRect :: Capability
pattern ImageRect = Capability 36

pattern SampledRect :: Capability
pattern SampledRect = Capability 37

pattern GenericPointer :: Capability
pattern GenericPointer = Capability 38

pattern Int8 :: Capability
pattern Int8 = Capability 39

pattern InputAttachment :: Capability
pattern InputAttachment = Capability 40

pattern SparseResidency :: Capability
pattern SparseResidency = Capability 41

pattern MinLod :: Capability
pattern MinLod = Capability 42

pattern Sampled1D :: Capability
pattern Sampled1D = Capability 43

pattern Image1D :: Capability
pattern Image1D = Capability 44

pattern SampledCubeArray :: Capability
pattern SampledCubeArray = Capability 45

pattern SampledBuffer :: Capability
pattern SampledBuffer = Capability 46

pattern ImageBuffer :: Capability
pattern ImageBuffer = Capability 47

pattern ImageMSArray :: Capability
pattern ImageMSArray = Capability 48

pattern StorageImageExtendedFormats :: Capability
pattern StorageImageExtendedFormats = Capability 49

pattern ImageQuery :: Capability
pattern ImageQuery = Capability 50

pattern DerivativeControl :: Capability
pattern DerivativeControl = Capability 51

pattern InterpolationFunction :: Capability
pattern InterpolationFunction = Capability 52

pattern TransformFeedback :: Capability
pattern TransformFeedback = Capability 53

pattern GeometryStreams :: Capability
pattern GeometryStreams = Capability 54

pattern StorageImageReadWithoutFormat :: Capability
pattern StorageImageReadWithoutFormat = Capability 55

pattern StorageImageWriteWithoutFormat :: Capability
pattern StorageImageWriteWithoutFormat = Capability 56

pattern MultiViewport :: Capability
pattern MultiViewport = Capability 57



showCapability :: Capability -> String
showCapability Matrix = "Matrix"
showCapability Shader = "Shader"
showCapability Geometry = "Geometry"
showCapability Tessellation = "Tessellation"
showCapability Addresses = "Addresses"
showCapability Linkage = "Linkage"
showCapability Kernel = "Kernel"
showCapability Vector16 = "Vector16"
showCapability Float16Buffer = "Float16Buffer"
showCapability Float16 = "Float16"
showCapability Float64 = "Float64"
showCapability Int64 = "Int64"
showCapability Int64Atomics = "Int64Atomics"
showCapability ImageBasic = "ImageBasic"
showCapability ImageReadWrite = "ImageReadWrite"
showCapability ImageMipmap = "ImageMipmap"
showCapability Pipes = "Pipes"
showCapability Groups = "Groups"
showCapability DeviceEnqueue = "DeviceEnqueue"
showCapability LiteralSampler = "LiteralSampler"
showCapability AtomicStorage = "AtomicStorage"
showCapability Int16 = "Int16"
showCapability TessellationPointSize = "TessellationPointSize"
showCapability GeometryPointSize = "GeometryPointSize"
showCapability ImageGatherExtended = "ImageGatherExtended"
showCapability StorageImageMultisample = "StorageImageMultisample"
showCapability UniformBufferArrayDynamicIndexing = "UniformBufferArrayDynamicIndexing"
showCapability SampledImageArrayDynamicIndexing = "SampledImageArrayDynamicIndexing"
showCapability StorageBufferArrayDynamicIndexing = "StorageBufferArrayDynamicIndexing"
showCapability StorageImageArrayDynamicIndexing = "StorageImageArrayDynamicIndexing"
showCapability ClipDistance = "ClipDistance"
showCapability CullDistance = "CullDistance"
showCapability ImageCubeArray = "ImageCubeArray"
showCapability SampleRateShading = "SampleRateShading"
showCapability ImageRect = "ImageRect"
showCapability SampledRect = "SampledRect"
showCapability GenericPointer = "GenericPointer"
showCapability Int8 = "Int8"
showCapability InputAttachment = "InputAttachment"
showCapability SparseResidency = "SparseResidency"
showCapability MinLod = "MinLod"
showCapability Sampled1D = "Sampled1D"
showCapability Image1D = "Image1D"
showCapability SampledCubeArray = "SampledCubeArray"
showCapability SampledBuffer = "SampledBuffer"
showCapability ImageBuffer = "ImageBuffer"
showCapability ImageMSArray = "ImageMSArray"
showCapability StorageImageExtendedFormats = "StorageImageExtendedFormats"
showCapability ImageQuery = "ImageQuery"
showCapability DerivativeControl = "DerivativeControl"
showCapability InterpolationFunction = "InterpolationFunction"
showCapability TransformFeedback = "TransformFeedback"
showCapability GeometryStreams = "GeometryStreams"
showCapability StorageImageReadWithoutFormat = "StorageImageReadWithoutFormat"
showCapability StorageImageWriteWithoutFormat = "StorageImageWriteWithoutFormat"
showCapability MultiViewport = "MultiViewport"
showCapability (Capability i) = show i


primOpCapabilities :: PrimOp.PrimOp -> [ Capability ]
primOpCapabilities ( PrimOp.op -> SatConvertSToU ) = [ Kernel ] -- why?
primOpCapabilities ( PrimOp.op -> SatConvertUToS ) = [ Kernel ] -- why?
primOpCapabilities ( PrimOp.MatOp _ _ _ _        ) = [ Matrix ]
primOpCapabilities _                               = [ ]

primTyCapabilities :: PrimTy.PrimTy -> [ Capability ]
primTyCapabilities ( PrimTy.Matrix _ _ ty ) = Matrix : primTyCapabilities (PrimTy.Scalar ty)
primTyCapabilities ( PrimTy.Vector n   ty )
    | PrimTy.dim n > 4 = Vector16 : primTyCapabilities (PrimTy.Scalar ty)
    | otherwise =            primTyCapabilities (PrimTy.Scalar ty)
primTyCapabilities ( PrimTy.Scalar (PrimTy.Integer _ W8 ) ) = [ Int8  ]
primTyCapabilities ( PrimTy.Scalar (PrimTy.Integer _ W16) ) = [ Int16 ]
primTyCapabilities ( PrimTy.Scalar (PrimTy.Integer _ W64) ) = [ Int64 ]
primTyCapabilities ( PrimTy.Scalar (PrimTy.Integer _ _  ) ) = [ ]
primTyCapabilities ( PrimTy.Scalar (PrimTy.Floating  W16) ) = [ Float16 ]
primTyCapabilities ( PrimTy.Scalar (PrimTy.Floating  W64) ) = [ Float64 ]
primTyCapabilities ( PrimTy.Scalar (PrimTy.Floating  _  ) ) = [ ]
primTyCapabilities _                                        = [ ]