{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Vulkan.Backend where

-- base
import Control.Arrow
  ( second )
import Control.Category
  ( (>>>) )
import Control.Monad
  ( guard, unless, void )
import Data.Bits
  ( Bits((.&.)) )
import Data.Coerce
  ( coerce )
import Data.Foldable
  ( toList, for_ )
import Data.List
  ( sortOn )
import Data.Maybe
  ( fromMaybe )
import Data.Ord
  ( Down(..) )
import Data.Semigroup
  ( First(..) )
import Data.Traversable
  ( for )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat, KnownNat )

-- bytestring
import Data.ByteString
  ( ByteString )

-- finite-typelits
import Data.Finite
  ( Finite )

-- logging-effect
import Control.Monad.Log
  ( logInfo )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, allocate )

-- sdl2
import qualified SDL.Video.Vulkan

-- text-short
import qualified Data.Text.Short as ShortText
  ( pack )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( (!?), empty, find, fromList, head, imapMaybe, singleton, toList )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( Vector )

-- vulkan
import qualified Vulkan
import qualified Vulkan.CStruct.Extends as Vulkan
  ( SomeStruct(SomeStruct) )
import qualified Vulkan.Zero as Vulkan

-- fir-examples
import Vulkan.Memory
import Vulkan.Monad
import Vulkan.Pipeline

-----------------------------------------------------------------------------------------------------

data ValidationLayerName
  = LunarG
  | Khronos
  deriving stock ( Eq, Show )

vulkanInstanceInfo
 :: forall m
 .  MonadVulkan m
 => ByteString
 -> m ( Vulkan.InstanceCreateInfo '[] )
vulkanInstanceInfo appName = do

  ( availableLayers :: Boxed.Vector Vulkan.LayerProperties ) <- snd <$> Vulkan.enumerateInstanceLayerProperties

  let
    validationLayer :: Maybe ValidationLayerName
    validationLayer
      = coerce 
      . foldMap
        (  (  Vulkan.layerName :: Vulkan.LayerProperties -> ByteString )
        >>> \case
              "VK_LAYER_LUNARG_standard_validation" -> Just ( First LunarG  )
              "VK_LAYER_KHRONOS_validation"         -> Just ( First Khronos )
              _                                     -> Nothing
        )
      $ availableLayers

    enabledLayers :: [ ByteString ]
    enabledLayers = case validationLayer of
      Nothing      -> []
      Just LunarG  -> [ "VK_LAYER_LUNARG_standard_validation" ]
      Just Khronos -> [ "VK_LAYER_KHRONOS_validation" ]

    appInfo :: Vulkan.ApplicationInfo
    appInfo =
      Vulkan.ApplicationInfo
        { Vulkan.applicationName    = Just appName
        , Vulkan.applicationVersion = 0
        , Vulkan.engineName         = Just "fir"
        , Vulkan.engineVersion      = 0
        , Vulkan.apiVersion         = Vulkan.API_VERSION_1_2
        }

    createInfo :: Vulkan.InstanceCreateInfo '[]
    createInfo =
      Vulkan.InstanceCreateInfo
        { Vulkan.next                  = ()
        , Vulkan.flags                 = Vulkan.zero
        , Vulkan.applicationInfo       = Just appInfo
        , Vulkan.enabledLayerNames     = Boxed.Vector.fromList enabledLayers
        , Vulkan.enabledExtensionNames = mempty
        }

  case validationLayer of
    Nothing -> logInfo "Validation layer unavailable. Is the Vulkan SDK installed?"
    Just _  -> logInfo ( "Enabled validation layers " <> ShortText.pack ( show enabledLayers ) )

  pure createInfo


createPhysicalDevice :: MonadIO m => Vulkan.Instance -> m Vulkan.PhysicalDevice
createPhysicalDevice vk = do
  physicalDevices <- snd <$> Vulkan.enumeratePhysicalDevices vk

  typedDevices <-
    for physicalDevices \ physicalDevice -> do
      properties <- Vulkan.getPhysicalDeviceProperties physicalDevice
      pure ( physicalDevice, Vulkan.deviceType properties )

  case Boxed.Vector.find ( isSuitableDeviceType . snd ) typedDevices of
    Nothing       -> error "Could not find a suitable physical device"
    Just ( d, _ ) -> pure d

  where
    isSuitableDeviceType :: Vulkan.PhysicalDeviceType -> Bool
    isSuitableDeviceType
      = flip elem
          [ Vulkan.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          , Vulkan.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          ]


findQueueFamilyIndex
  :: MonadIO m
  => Vulkan.PhysicalDevice
  -> [ Vulkan.QueueFlags ]
  -> m Int
findQueueFamilyIndex physicalDevice requiredFlags = do
  queueFamilies <- Vulkan.getPhysicalDeviceQueueFamilyProperties physicalDevice
  let
    capableFamilyIndices :: Boxed.Vector Int
    capableFamilyIndices = ( `Boxed.Vector.imapMaybe` queueFamilies ) \ i queueFamily -> do
      let
        flags :: Vulkan.QueueFlags
        flags = Vulkan.queueFlags queueFamily
      for_ requiredFlags
        ( \ f ->
            guard ( flags .&. f > Vulkan.zero )
        )
      pure i
  case capableFamilyIndices Boxed.Vector.!? 0 of
    Nothing -> error "No queue family has sufficient capabilities"
    Just i  -> pure i

chooseSwapchainFormat
  :: MonadIO m
  => Vulkan.SurfaceFormatKHR
  -> Vulkan.PhysicalDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m Vulkan.SurfaceFormatKHR
chooseSwapchainFormat
  preferredFormat@( Vulkan.SurfaceFormatKHR fmt_p spc_p )
  physicalDevice
  surface
  = do
      surfaceFormats <- snd <$> Vulkan.getPhysicalDeviceSurfaceFormatsKHR physicalDevice ( Vulkan.SurfaceKHR surface )

      case sortOn ( Down . score ) ( Boxed.Vector.toList surfaceFormats ) of
        [] -> error "No formats found."
        ( best : _ )
          | Vulkan.FORMAT_UNDEFINED <- ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) best
            -> pure preferredFormat
          | otherwise
            -> pure best

    where
      match :: Eq a => a -> a -> Int
      match a b
        | a == b    = 1
        | otherwise = 0

      score :: Vulkan.SurfaceFormatKHR -> Int
      score ( Vulkan.SurfaceFormatKHR fmt spc )
        = match fmt fmt_p
        + match spc spc_p

createSwapchain
  :: ( MonadIO m, MonadVulkan m )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> Vulkan.SurfaceFormatKHR
  -> Vulkan.ImageUsageFlags
  -> m ( Vulkan.SwapchainKHR, Vulkan.Extent2D )
createSwapchain physicalDevice device surface surfaceFormat imageUsage = do

  surfaceCapabilities <- Vulkan.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice ( Vulkan.SurfaceKHR surface )

  let
    minImageCount, maxImageCount, imageCount :: Word32
    minImageCount = ( Vulkan.minImageCount :: Vulkan.SurfaceCapabilitiesKHR -> Word32 ) surfaceCapabilities
    maxImageCount = ( Vulkan.maxImageCount :: Vulkan.SurfaceCapabilitiesKHR -> Word32 ) surfaceCapabilities
    imageCount
      | maxImageCount == 0 = minImageCount + 1 -- no maximum
      | otherwise = min ( minImageCount + 1 ) maxImageCount

    currentExtent :: Vulkan.Extent2D
    currentExtent = ( Vulkan.currentExtent :: Vulkan.SurfaceCapabilitiesKHR -> Vulkan.Extent2D ) surfaceCapabilities

    currentTransform :: Vulkan.SurfaceTransformFlagBitsKHR
    currentTransform = ( Vulkan.currentTransform :: Vulkan.SurfaceCapabilitiesKHR -> Vulkan.SurfaceTransformFlagBitsKHR ) surfaceCapabilities

    swapchainCreateInfo :: Vulkan.SwapchainCreateInfoKHR '[]
    swapchainCreateInfo =
      Vulkan.SwapchainCreateInfoKHR
        { Vulkan.next                  = ()
        , Vulkan.flags                 = Vulkan.zero
        , Vulkan.surface               = Vulkan.SurfaceKHR surface
        , Vulkan.minImageCount         = imageCount
        , Vulkan.imageFormat           = ( Vulkan.format     :: Vulkan.SurfaceFormatKHR -> Vulkan.Format        ) surfaceFormat
        , Vulkan.imageColorSpace       = ( Vulkan.colorSpace :: Vulkan.SurfaceFormatKHR -> Vulkan.ColorSpaceKHR ) surfaceFormat
        , Vulkan.imageExtent           = currentExtent
        , Vulkan.imageArrayLayers      = 1
        , Vulkan.imageUsage            = imageUsage
        , Vulkan.imageSharingMode      = Vulkan.SHARING_MODE_EXCLUSIVE
        , Vulkan.queueFamilyIndices    = Boxed.Vector.empty
        , Vulkan.preTransform          = currentTransform
        , Vulkan.compositeAlpha        = Vulkan.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , Vulkan.presentMode           = Vulkan.PRESENT_MODE_FIFO_KHR
        , Vulkan.clipped               = True
        , Vulkan.oldSwapchain          = Vulkan.NULL_HANDLE
        }

  swapchain <- snd <$> Vulkan.withSwapchainKHR device swapchainCreateInfo Nothing allocate
  pure ( swapchain, currentExtent )

getSwapchainImages
  :: MonadIO m
  => Vulkan.Device
  -> Vulkan.SwapchainKHR
  -> m ( Boxed.Vector Vulkan.Image )
getSwapchainImages device swapchain = snd <$> Vulkan.getSwapchainImagesKHR device swapchain


createFramebuffer
  :: ( MonadVulkan m, Foldable f )
  => Vulkan.Device
  -> Vulkan.RenderPass
  -> Vulkan.Extent2D
  -> f Vulkan.ImageView
  -> m Vulkan.Framebuffer
createFramebuffer dev renderPass extent attachments = snd <$> Vulkan.withFramebuffer dev createInfo Nothing allocate
  where
    createInfo :: Vulkan.FramebufferCreateInfo '[]
    createInfo =
      Vulkan.FramebufferCreateInfo
        { Vulkan.next        = ()
        , Vulkan.flags       = Vulkan.zero
        , Vulkan.renderPass  = renderPass
        , Vulkan.attachments = Boxed.Vector.fromList . toList $ attachments
        , Vulkan.width       = ( Vulkan.width  :: Vulkan.Extent2D -> Word32 ) extent
        , Vulkan.height      = ( Vulkan.height :: Vulkan.Extent2D -> Word32 ) extent
        , Vulkan.layers      = 1
        }


data ImageInfo
  = ImageInfo
  { imageType        :: Vulkan.ImageType
  , imageExtent      :: Vulkan.Extent3D
  , imageFormat      :: Vulkan.Format
  , imageLayout      :: Vulkan.ImageLayout
  , imageMipLevels   :: Word32
  , imageArrayLayers :: Word32
  , imageSamples     :: Vulkan.SampleCountFlagBits
  , imageTiling      :: Vulkan.ImageTiling
  , imageUsage       :: Vulkan.ImageUsageFlags
  }

pattern Default2DImageInfo :: Vulkan.Extent3D -> Vulkan.Format -> Vulkan.ImageUsageFlags -> ImageInfo
pattern Default2DImageInfo extent3D fmt usage
  = ImageInfo
  { imageType        = Vulkan.IMAGE_TYPE_2D
  , imageExtent      = extent3D
  , imageFormat      = fmt
  , imageLayout      = Vulkan.IMAGE_LAYOUT_UNDEFINED
  , imageMipLevels   = 1
  , imageArrayLayers = 1
  , imageSamples     = Vulkan.SAMPLE_COUNT_1_BIT
  , imageTiling      = Vulkan.IMAGE_TILING_OPTIMAL
  , imageUsage       = usage
  }

createImage
  :: MonadVulkan m
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> ImageInfo
  -> Vulkan.MemoryPropertyFlags
  -> m (Vulkan.Image, Vulkan.DeviceMemory)
createImage physicalDevice device ImageInfo { .. } reqs
  = let createInfo :: Vulkan.ImageCreateInfo '[]
        createInfo =
          Vulkan.ImageCreateInfo
            { Vulkan.next               = ()
            , Vulkan.flags              = Vulkan.zero
            , Vulkan.imageType          = imageType
            , Vulkan.format             = imageFormat
            , Vulkan.extent             = imageExtent
            , Vulkan.mipLevels          = imageMipLevels
            , Vulkan.arrayLayers        = imageArrayLayers
            , Vulkan.samples            = imageSamples
            , Vulkan.tiling             = imageTiling
            , Vulkan.usage              = imageUsage
            , Vulkan.sharingMode        = Vulkan.SHARING_MODE_EXCLUSIVE
            , Vulkan.queueFamilyIndices = Boxed.Vector.empty
            , Vulkan.initialLayout      = imageLayout
            }
    in do
      ( _, image ) <- Vulkan.withImage device createInfo Nothing allocate
      memReqs      <- Vulkan.getImageMemoryRequirements device image
      ( _, memory ) <- allocateMemory physicalDevice device memReqs reqs Vulkan.zero
      Vulkan.bindImageMemory device image memory 0
      pure (image, memory)


createImageView
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.Image
  -> Vulkan.ImageViewType
  -> Vulkan.Format
  -> Vulkan.ImageAspectFlags
  -> m Vulkan.ImageView
createImageView dev image viewType fmt aspect = snd <$> Vulkan.withImageView dev createInfo Nothing allocate
  where
    components :: Vulkan.ComponentMapping
    components =
      Vulkan.ComponentMapping
        { Vulkan.r = Vulkan.COMPONENT_SWIZZLE_IDENTITY
        , Vulkan.g = Vulkan.COMPONENT_SWIZZLE_IDENTITY
        , Vulkan.b = Vulkan.COMPONENT_SWIZZLE_IDENTITY
        , Vulkan.a = Vulkan.COMPONENT_SWIZZLE_IDENTITY
        }

    subResourceRange :: Vulkan.ImageSubresourceRange
    subResourceRange =
      Vulkan.ImageSubresourceRange
        { Vulkan.aspectMask     = aspect
        , Vulkan.baseMipLevel   = 0
        , Vulkan.levelCount     = 1
        , Vulkan.baseArrayLayer = 0
        , Vulkan.layerCount     = 1
        }

    createInfo :: Vulkan.ImageViewCreateInfo '[]
    createInfo =
      Vulkan.ImageViewCreateInfo
        { Vulkan.next             = ()
        , Vulkan.flags            = Vulkan.zero
        , Vulkan.image            = image
        , Vulkan.viewType         = viewType
        , Vulkan.format           = fmt
        , Vulkan.components       = components
        , Vulkan.subresourceRange = subResourceRange
        }

cmdTransitionImageLayout
  :: MonadVulkan m
  => Vulkan.CommandBuffer
  -> Vulkan.Image
  -> Vulkan.ImageLayout
  -> Vulkan.ImageLayout
  -> (Vulkan.PipelineStageFlags, Vulkan.AccessFlags)
  -> (Vulkan.PipelineStageFlags, Vulkan.AccessFlags)
  -> m ()
cmdTransitionImageLayout
  commandBuffer
  image
  oldLayout newLayout
  (srcStage, srcMask) (dstStage, dstMask)
  = let
      subresourceRange :: Vulkan.ImageSubresourceRange
      subresourceRange =
        Vulkan.ImageSubresourceRange
          { Vulkan.aspectMask     = Vulkan.IMAGE_ASPECT_COLOR_BIT
          , Vulkan.baseMipLevel   = 0
          , Vulkan.levelCount     = 1
          , Vulkan.baseArrayLayer = 0
          , Vulkan.layerCount     = 1
          }

      imageBarrier :: Vulkan.ImageMemoryBarrier '[]
      imageBarrier =
        Vulkan.ImageMemoryBarrier
          { Vulkan.next                = ()
          , Vulkan.srcAccessMask       = srcMask
          , Vulkan.dstAccessMask       = dstMask
          , Vulkan.oldLayout           = oldLayout
          , Vulkan.newLayout           = newLayout
          , Vulkan.image               = image
          , Vulkan.subresourceRange    = subresourceRange
          , Vulkan.srcQueueFamilyIndex = Vulkan.QUEUE_FAMILY_IGNORED
          , Vulkan.dstQueueFamilyIndex = Vulkan.QUEUE_FAMILY_IGNORED
          }

    in cmdPipelineBarrier
        commandBuffer
        srcStage
        dstStage
        []
        []
        [ Vulkan.SomeStruct imageBarrier ]

createSampler
  :: MonadVulkan m
  => Vulkan.Device
  -> m Vulkan.Sampler
createSampler dev = snd <$> Vulkan.withSampler dev createInfo Nothing allocate
  where
    createInfo :: Vulkan.SamplerCreateInfo '[]
    createInfo =
      Vulkan.SamplerCreateInfo
        { Vulkan.next                    = ()
        , Vulkan.flags                   = Vulkan.zero
        , Vulkan.magFilter               = Vulkan.FILTER_NEAREST
        , Vulkan.minFilter               = Vulkan.FILTER_NEAREST
        , Vulkan.mipmapMode              = Vulkan.SAMPLER_MIPMAP_MODE_NEAREST
        , Vulkan.addressModeU            = Vulkan.SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
        , Vulkan.addressModeV            = Vulkan.SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
        , Vulkan.addressModeW            = Vulkan.SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
        , Vulkan.mipLodBias              = 0
        , Vulkan.anisotropyEnable        = False
        , Vulkan.maxAnisotropy           = 0
        , Vulkan.compareEnable           = False
        , Vulkan.compareOp               = Vulkan.COMPARE_OP_ALWAYS
        , Vulkan.minLod                  = 0
        , Vulkan.maxLod                  = 1
        , Vulkan.borderColor             = Vulkan.BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
        , Vulkan.unnormalizedCoordinates = False
        }


createCommandPool
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPoolCreateFlagBits
  -> Word32
  -> m Vulkan.CommandPool
createCommandPool dev flags queueFamilyIndex = snd <$> Vulkan.withCommandPool dev createInfo Nothing allocate
  where
    createInfo :: Vulkan.CommandPoolCreateInfo
    createInfo =
      Vulkan.CommandPoolCreateInfo
        { Vulkan.flags            = flags
        , Vulkan.queueFamilyIndex = queueFamilyIndex
        }


allocateCommandBuffer
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPool
  -> m ( ReleaseKey, Vulkan.CommandBuffer )
allocateCommandBuffer dev commandPool = second Boxed.Vector.head <$> Vulkan.withCommandBuffers dev allocInfo allocate
  where
    allocInfo :: Vulkan.CommandBufferAllocateInfo
    allocInfo =
      Vulkan.CommandBufferAllocateInfo
        { Vulkan.commandPool        = commandPool
        , Vulkan.level              = Vulkan.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vulkan.commandBufferCount = 1
        }

allocatePrimaryCommandBuffers
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPool
  -> Word32
  -> m ( ReleaseKey, Boxed.Vector Vulkan.CommandBuffer )
allocatePrimaryCommandBuffers dev commandPool count = Vulkan.withCommandBuffers dev allocInfo allocate
  where
    allocInfo :: Vulkan.CommandBufferAllocateInfo
    allocInfo =
      Vulkan.CommandBufferAllocateInfo
        { Vulkan.commandPool        = commandPool
        , Vulkan.level              = Vulkan.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vulkan.commandBufferCount = count
        }


cmdBeginRenderPass
  :: MonadIO m
  => Vulkan.CommandBuffer
  -> Vulkan.RenderPass
  -> Vulkan.Framebuffer
  -> [Vulkan.ClearValue] -- indexed by framebuffer attachments
  -> Vulkan.Extent2D
  -> m ()
cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues extent =
  let
    zeroZero :: Vulkan.Offset2D
    zeroZero =
      Vulkan.Offset2D
        { Vulkan.x = 0
        , Vulkan.y = 0
        }

    renderArea :: Vulkan.Rect2D
    renderArea =
      Vulkan.Rect2D
        { Vulkan.offset = zeroZero
        , Vulkan.extent = extent
        }

    beginInfo :: Vulkan.RenderPassBeginInfo '[]
    beginInfo =
      Vulkan.RenderPassBeginInfo
        { Vulkan.next        = ()
        , Vulkan.renderPass  = renderPass
        , Vulkan.framebuffer = framebuffer
        , Vulkan.renderArea  = renderArea
        , Vulkan.clearValues = Boxed.Vector.fromList clearValues
        }
  in
    Vulkan.cmdBeginRenderPass
      commandBuffer
      beginInfo
      Vulkan.SUBPASS_CONTENTS_INLINE

cmdNextSubpass :: MonadIO m => Vulkan.CommandBuffer -> m ()
cmdNextSubpass commandBuffer = Vulkan.cmdNextSubpass commandBuffer Vulkan.SUBPASS_CONTENTS_INLINE


cmdEndRenderPass :: MonadIO m => Vulkan.CommandBuffer -> m ()
cmdEndRenderPass =Vulkan.cmdEndRenderPass

data SwapchainInfo (n :: Nat)
  = SwapchainInfo
      { swapchain       :: Vulkan.SwapchainKHR
      , swapchainImages :: V.Vector n Vulkan.Image
      , swapchainExtent :: Vulkan.Extent2D
      , surfaceFormat   :: Vulkan.SurfaceFormatKHR
      }

acquireNextImage
  :: ( MonadIO m, KnownNat n )
  => Vulkan.Device
  -> SwapchainInfo n
  -> Vulkan.Semaphore
  -> m (Finite n)
acquireNextImage device (SwapchainInfo { swapchain }) signal
  = fromIntegral . snd <$> Vulkan.acquireNextImageKHR device swapchain maxBound signal Vulkan.NULL_HANDLE

present
  :: ( MonadIO m, Integral i )
  => Vulkan.Queue
  -> Vulkan.SwapchainKHR
  -> i
  -> [Vulkan.Semaphore]
  -> m ()
present queue swapchain imageIndex wait = void $ Vulkan.queuePresentKHR queue presentInfo
  where
    presentInfo :: Vulkan.PresentInfoKHR '[]
    presentInfo =
      Vulkan.PresentInfoKHR
        { Vulkan.next           = ()
        , Vulkan.waitSemaphores = Boxed.Vector.fromList wait
        , Vulkan.swapchains     = Boxed.Vector.singleton swapchain
        , Vulkan.imageIndices   = Boxed.Vector.singleton ( fromIntegral imageIndex )
        , Vulkan.results        = Vulkan.zero
        }

        

getQueue :: MonadIO m => Vulkan.Device -> Int -> m Vulkan.Queue
getQueue device queueFamilyIndex = Vulkan.getDeviceQueue device ( fromIntegral queueFamilyIndex ) 0


createSemaphore :: MonadVulkan m => Vulkan.Device -> m ( ReleaseKey, Vulkan.Semaphore )
createSemaphore device = Vulkan.withSemaphore device semaphoreCreateInfo Nothing allocate
  where
    semaphoreCreateInfo :: Vulkan.SemaphoreCreateInfo '[]
    semaphoreCreateInfo =
      Vulkan.SemaphoreCreateInfo
        { Vulkan.next  = ()
        , Vulkan.flags = Vulkan.zero
        }


createFence :: MonadVulkan m => Vulkan.Device -> m ( ReleaseKey, Vulkan.Fence )
createFence device = Vulkan.withFence device fenceCreateInfo Nothing allocate

  where
    fenceCreateInfo :: Vulkan.FenceCreateInfo '[]
    fenceCreateInfo =
      Vulkan.FenceCreateInfo
        { Vulkan.next  = ()
        , Vulkan.flags = Vulkan.zero
        }


data Wait a = WaitAll [a] | WaitAny [a]
  deriving ( Eq, Show )

waitForFences :: MonadIO m => Vulkan.Device -> Wait Vulkan.Fence -> m ()
waitForFences device fences = void $ Vulkan.waitForFences device ( Boxed.Vector.fromList fenceList ) waitAll maxBound
  where
    waitAll   :: Bool
    fenceList :: [Vulkan.Fence]
    (waitAll, fenceList) =
      case fences of
        WaitAll l -> ( True , l )
        WaitAny l -> ( False, l )

cmdBindPipeline :: MonadVulkan m => Vulkan.CommandBuffer -> VkPipeline -> m ()
cmdBindPipeline commandBuffer pipeline =
  Vulkan.cmdBindPipeline
    commandBuffer
    ( bindPoint  pipeline )
    ( vkPipeline pipeline )

cmdBindDescriptorSets
  :: MonadVulkan m
  => Vulkan.CommandBuffer
  -> Vulkan.PipelineLayout
  -> VkPipeline
  -> [ Vulkan.DescriptorSet ]
  -> m ()
cmdBindDescriptorSets commandBuffer pipelineLayout pipeline descriptorSets =
    Vulkan.cmdBindDescriptorSets
      commandBuffer
      ( bindPoint pipeline )
      pipelineLayout
      0 -- first set: set 0
      ( Boxed.Vector.fromList descriptorSets )
      Boxed.Vector.empty -- no dynamic offsets

bindPoint :: VkPipeline -> Vulkan.PipelineBindPoint
bindPoint ( GraphicsPipeline   {} ) = Vulkan.PIPELINE_BIND_POINT_GRAPHICS
bindPoint ( ComputePipeline    {} ) = Vulkan.PIPELINE_BIND_POINT_COMPUTE
bindPoint ( RayTracingPipeline {} ) = Vulkan.PIPELINE_BIND_POINT_RAY_TRACING_KHR

cmdPipelineBarrier
  :: MonadIO m
  => Vulkan.CommandBuffer
  -> Vulkan.PipelineStageFlags
  -> Vulkan.PipelineStageFlags
  -> [Vulkan.MemoryBarrier]
  -> [Vulkan.BufferMemoryBarrier]
  -> [Vulkan.SomeStruct Vulkan.ImageMemoryBarrier]
  -> m ()
cmdPipelineBarrier
  commandBuffer
  srcStageMask dstStageMask
  memoryBarriers bufferMemoryBarriers imageMemoryBarriers
    = Vulkan.cmdPipelineBarrier
        commandBuffer
        srcStageMask
        dstStageMask
        Vulkan.DEPENDENCY_BY_REGION_BIT
        ( Boxed.Vector.fromList memoryBarriers )
        ( Boxed.Vector.fromList bufferMemoryBarriers )
        ( Boxed.Vector.fromList imageMemoryBarriers )


assertSurfacePresentable
  :: MonadIO m
  => Vulkan.PhysicalDevice
  -> Int
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m ()
assertSurfacePresentable physicalDevice queueFamilyIndex surface = do
  isPresentable <-
    Vulkan.getPhysicalDeviceSurfaceSupportKHR
      physicalDevice
      ( fromIntegral queueFamilyIndex )
      ( Vulkan.SurfaceKHR surface )

  unless isPresentable ( error "Surface is not presentable" )


submitCommandBuffer
  :: MonadIO m
  => Vulkan.Queue
  -> Vulkan.CommandBuffer
  -> [ ( Vulkan.Semaphore, Vulkan.PipelineStageFlags ) ]
  -> [ Vulkan.Semaphore ]
  -> Maybe Vulkan.Fence
  -> m ()
submitCommandBuffer queue commandBuffer wait signal mbFence =
  Vulkan.queueSubmit queue ( Boxed.Vector.singleton $ Vulkan.SomeStruct submitInfo ) ( fromMaybe Vulkan.NULL_HANDLE mbFence )
    where
      submitInfo :: Vulkan.SubmitInfo '[]
      submitInfo =
        Vulkan.SubmitInfo
          { Vulkan.next             = ()
          , Vulkan.waitSemaphores   = Boxed.Vector.fromList $ map fst wait
          , Vulkan.waitDstStageMask = Boxed.Vector.fromList $ map snd wait
          , Vulkan.commandBuffers   = Boxed.Vector.singleton ( Vulkan.commandBufferHandle commandBuffer )
          , Vulkan.signalSemaphores = Boxed.Vector.fromList signal
          }

beginCommandBuffer :: MonadIO m => Vulkan.CommandBuffer -> m ()
beginCommandBuffer commandBuffer = Vulkan.beginCommandBuffer commandBuffer commandBufferBeginInfo
  where
    commandBufferBeginInfo :: Vulkan.CommandBufferBeginInfo '[]
    commandBufferBeginInfo =
      Vulkan.CommandBufferBeginInfo
        { Vulkan.next            = ()
        , Vulkan.flags           = Vulkan.zero
        , Vulkan.inheritanceInfo = Nothing
        }

endCommandBuffer :: MonadVulkan m => Vulkan.CommandBuffer -> m ()
endCommandBuffer = Vulkan.endCommandBuffer
