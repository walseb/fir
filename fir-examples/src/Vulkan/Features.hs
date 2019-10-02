{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Features where

-- base
import Data.Foldable
  ( traverse_ )

-- containers
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( toList )

-- vulkan-api
import qualified Graphics.Vulkan          as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- fir
import FIR
  ( ModuleRequirements(..) )
import qualified FIR as SPIRV

----------------------------------------------------------------------------

requiredFeatures :: ModuleRequirements -> IO (Vulkan.VkPhysicalDeviceFeatures)
requiredFeatures ModuleRequirements { .. }
  = Vulkan.newVkData \ptr -> do
      Vulkan.clearStorable ptr
      writeCapabilityFeatures requiredCapabilities ptr

writeCapabilityFeatures :: Set SPIRV.Capability -> Vulkan.Ptr Vulkan.VkPhysicalDeviceFeatures -> IO ()
writeCapabilityFeatures caps ptr = traverse_ ( writeCapabilityFeature ptr ) ( Set.toList caps )

writeCapabilityFeature :: Vulkan.Ptr Vulkan.VkPhysicalDeviceFeatures -> SPIRV.Capability -> IO ()
writeCapabilityFeature ptr SPIRV.SampledCubeArray  = Vulkan.writeField @"imageCubeArray"     ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.ImageCubeArray    = Vulkan.writeField @"imageCubeArray"     ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.Geometry          = Vulkan.writeField @"geometryShader"     ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.Tessellation      = Vulkan.writeField @"tessellationShader" ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.SampleRateShading = Vulkan.writeField @"sampleRateShading"  ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.TessellationPointSize = Vulkan.writeField @"shaderTessellationAndGeometryPointSize" ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.GeometryPointSize     = Vulkan.writeField @"shaderTessellationAndGeometryPointSize" ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.ImageGatherExtended   = Vulkan.writeField @"shaderImageGatherExtended"              ptr Vulkan.VK_TRUE
-- shaderStorageImageExtendedFormats
writeCapabilityFeature ptr SPIRV.StorageImageMultisample           = Vulkan.writeField @"shaderStorageImageMultisample"           ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.StorageImageReadWithoutFormat     = Vulkan.writeField @"shaderStorageImageReadWithoutFormat"     ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.StorageImageWriteWithoutFormat    = Vulkan.writeField @"shaderStorageImageWriteWithoutFormat"    ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.UniformBufferArrayDynamicIndexing = Vulkan.writeField @"shaderUniformBufferArrayDynamicIndexing" ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.SampledImageArrayDynamicIndexing  = Vulkan.writeField @"shaderSampledImageArrayDynamicIndexing"  ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.StorageBufferArrayDynamicIndexing = Vulkan.writeField @"shaderStorageBufferArrayDynamicIndexing" ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.StorageImageArrayDynamicIndexing  = Vulkan.writeField @"shaderStorageImageArrayDynamicIndexing"  ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.ClipDistance    = Vulkan.writeField @"shaderClipDistance"      ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.CullDistance    = Vulkan.writeField @"shaderCullDistance"      ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.Float64         = Vulkan.writeField @"shaderFloat64"           ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.Int64           = Vulkan.writeField @"shaderInt64"             ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.Int16           = Vulkan.writeField @"shaderInt16"             ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.SparseResidency = Vulkan.writeField @"shaderResourceResidency" ptr Vulkan.VK_TRUE
writeCapabilityFeature ptr SPIRV.MinLod          = Vulkan.writeField @"shaderResourceMinLod"    ptr Vulkan.VK_TRUE
writeCapabilityFeature _ _ = pure ()

{-
-- VkPhysicalDeviceShaderFloat16Int8FeaturesKHR not yet implemented in 'vulkan-api' package
setCapabilityShaderFloat16Int8Feature
  :: SPIRV.Capability
  -> Ptr Vulkan.VkPhysicalDeviceShaderFloat16Int8FeaturesKHR
  -> IO ()
setCapabilityShaderFloat16Int8Feature Float16 ptr = Vulkan.writeField @"shaderFloat16" ptr Vulkan.VK_TRUE
setCapabilityShaderFloat16Int8Feature Int8    ptr = Vulkan.writeField @"shaderInt8"    ptr Vulkan.VK_TRUE
-}