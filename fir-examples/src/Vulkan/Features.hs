{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Vulkan.Features where

-- base
import Data.Foldable
  ( foldl' )

-- vulkan
import qualified Vulkan

-- fir
import FIR
  ( ModuleRequirements(..) )
import qualified FIR as SPIRV

----------------------------------------------------------------------------

requiredFeatures :: ModuleRequirements -> Vulkan.PhysicalDeviceFeatures2 '[ Vulkan.PhysicalDeviceShaderFloat16Int8Features ] 
requiredFeatures ( ModuleRequirements { requiredCapabilities } ) =
  Vulkan.PhysicalDeviceFeatures2
    { Vulkan.next     = ( shaderFloat16Int8Features, () )
    , Vulkan.features = foldl' enableCapabilityFeature Vulkan.zero requiredCapabilities
    }
    where
      shaderFloat16Int8Features :: Vulkan.PhysicalDeviceShaderFloat16Int8Features
      shaderFloat16Int8Features =
        Vulkan.PhysicalDeviceShaderFloat16Int8Features
          { Vulkan.shaderFloat16 = SPIRV.Float16 `elem` requiredCapabilities
          , Vulkan.shaderInt8    = SPIRV.Int8    `elem` requiredCapabilities
          }

enableCapabilityFeature :: Vulkan.PhysicalDeviceFeatures -> SPIRV.Capability -> Vulkan.PhysicalDeviceFeatures
enableCapabilityFeature feats SPIRV.SampledCubeArray                 = feats { Vulkan.imageCubeArray                           = True }
enableCapabilityFeature feats SPIRV.ImageCubeArray                   = feats { Vulkan.imageCubeArray                           = True }
enableCapabilityFeature feats SPIRV.Geometry                         = feats { Vulkan.geometryShader                           = True }
enableCapabilityFeature feats SPIRV.Tessellation                     = feats { Vulkan.tessellationShader                       = True }
enableCapabilityFeature feats SPIRV.SampleRateShading                = feats { Vulkan.sampleRateShading                        = True }
enableCapabilityFeature feats SPIRV.TessellationPointSize            = feats { Vulkan.shaderTessellationAndGeometryPointSize   = True }
enableCapabilityFeature feats SPIRV.GeometryPointSize                = feats { Vulkan.shaderTessellationAndGeometryPointSize   = True }
enableCapabilityFeature feats SPIRV.ImageGatherExtended              = feats { Vulkan.shaderImageGatherExtended                = True }
-- shaderStorageImageExtendedFormats
enableCapabilityFeature feats SPIRV.StorageImageMultisample           = feats { Vulkan.shaderStorageImageMultisample           = True }
enableCapabilityFeature feats SPIRV.StorageImageReadWithoutFormat     = feats { Vulkan.shaderStorageImageReadWithoutFormat     = True }
enableCapabilityFeature feats SPIRV.StorageImageWriteWithoutFormat    = feats { Vulkan.shaderStorageImageWriteWithoutFormat    = True }
enableCapabilityFeature feats SPIRV.UniformBufferArrayDynamicIndexing = feats { Vulkan.shaderUniformBufferArrayDynamicIndexing = True }
enableCapabilityFeature feats SPIRV.SampledImageArrayDynamicIndexing  = feats { Vulkan.shaderSampledImageArrayDynamicIndexing  = True }
enableCapabilityFeature feats SPIRV.StorageBufferArrayDynamicIndexing = feats { Vulkan.shaderStorageBufferArrayDynamicIndexing = True }
enableCapabilityFeature feats SPIRV.StorageImageArrayDynamicIndexing  = feats { Vulkan.shaderStorageImageArrayDynamicIndexing  = True }
enableCapabilityFeature feats SPIRV.ClipDistance                      = feats { Vulkan.shaderClipDistance                      = True }
enableCapabilityFeature feats SPIRV.CullDistance                      = feats { Vulkan.shaderCullDistance                      = True }
enableCapabilityFeature feats SPIRV.Float64                           = feats { Vulkan.shaderFloat64                           = True }
enableCapabilityFeature feats SPIRV.Int64                             = feats { Vulkan.shaderInt64                             = True }
enableCapabilityFeature feats SPIRV.Int16                             = feats { Vulkan.shaderInt16                             = True }
enableCapabilityFeature feats SPIRV.SparseResidency                   = feats { Vulkan.shaderResourceResidency                 = True }
enableCapabilityFeature feats SPIRV.MinLod                            = feats { Vulkan.shaderResourceMinLod                    = True }
enableCapabilityFeature feats _                                       = feats
