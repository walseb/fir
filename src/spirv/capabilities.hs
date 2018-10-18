module SPIRV.Capabilities where

import SPIRV.PrimOps(Op(..))
import SPIRV.Types(PrimTy(..), Width(..))

data Capability
  = Matrix
  | Shader
  | Geometry
  | Tessellation
  | Addresses
  | Linkage
  | Kernel
  | Vector16
  | Float16Buffer
  | Float16
  | Float64
  | Int64
  | Int64Atomics
  | ImageBasic
  | ImageReadWrite
  | ImageMipmap
  | UNDEFINED_CAPABILITY1
  | Pipes
  | Groups
  | DeviceEnqueue
  | LiteralSampler
  | AtomicStorage
  | Int16
  | TessellationPointSize
  | GeometryPointSize
  | ImageGatherExtended
  | UNDEFINED_CAPABILITY2
  | StorageImageMultisample
  | UniformBufferArrayDynamicIndexing
  | SampledImageArrayDynamicIndexing
  | StorageBufferArrayDynamicIndexing
  | StorageImageArrayDynamicIndexing
  | ClipDistance
  | CullDistance
  | ImageCubeArray
  | SampleRateShading
  | ImageRect
  | SampledRect
  | GenericPointer
  | Int8
  | InputAttachment
  | SparseResidency
  | MinLod
  | Sampled1D
  | Image1D
  | SampledCubeArray
  | SampledBuffer
  | ImageBuffer
  | ImageMSArray
  | StorageImageExtendedFormats
  | ImageQuery
  | DerivativeControl
  | InterpolationFunction
  | TransformFeedback
  | GeometryStreams
  | StorageImageReadWithoutFormat
  | StorageImageWriteWithoutFormat
  | MultiViewPort
  deriving ( Show, Eq, Ord, Enum, Bounded )

primOpCapabilities :: Op -> [ Capability ]
primOpCapabilities SatConvertSToU    = [ Kernel ] -- why?
primOpCapabilities SatConvertUToS    = [ Kernel ] -- why?
primOpCapabilities Transpose         = [ Matrix ]
primOpCapabilities VectorTimesScalar = [ Matrix ]
primOpCapabilities MatrixTimesScalar = [ Matrix ]
primOpCapabilities MatrixTimesVector = [ Matrix ]
primOpCapabilities VectorTimesMatrix = [ Matrix ]
primOpCapabilities MatrixTimesMatrix = [ Matrix ]
primOpCapabilities OuterProduct      = [ Matrix ]
primOpCapabilities _                 = []

typeCapabilities :: PrimTy -> [ Capability ]
typeCapabilities (Mat _ _ ty) = Matrix : typeCapabilities ty
typeCapabilities (Vec n   ty)
  | n > 4     = Vector16 : typeCapabilities ty
  | otherwise =            typeCapabilities ty
typeCapabilities (Integer _ W8 ) = [ Int8  ]
typeCapabilities (Integer _ W16) = [ Int16 ]
typeCapabilities (Integer _ W64) = [ Int64 ]
typeCapabilities (Integer _ _  ) = [ ]
typeCapabilities (Floating  W16) = [ Float16 ]
typeCapabilities (Floating  W64) = [ Float64 ]
typeCapabilities (Floating  _  ) = []
typeCapabilities _               = []