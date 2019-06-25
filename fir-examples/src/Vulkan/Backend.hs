{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Vulkan.Backend where

-- base
import Control.Arrow
  ( (&&&) )
import Control.Monad
  ( (>=>), guard, unless )
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Data.Bits
  ( (.&.) )
import Data.Foldable
  ( for_ )
import Data.List
  hiding ( transpose )
import Data.Maybe
  ( fromMaybe )
import Data.Ord
  ( Down(..) )
import Data.Traversable
  ( for )
import qualified Foreign
import qualified Foreign.Marshal

-- managed
import Control.Monad.Managed
  ( MonadManaged )

-- sdl2
import qualified SDL.Video.Vulkan

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir-examples
import Vulkan.Memory
import Vulkan.Monad

-----------------------------------------------------------------------------------------------------

createVulkanInstance :: MonadManaged m => [ Vulkan.CString ] -> m Vulkan.VkInstance
createVulkanInstance neededExtensions =
  let
    createInfo :: Vulkan.VkInstanceCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL_HANDLE
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"pApplicationInfo" Vulkan.VK_NULL_HANDLE
        &* Vulkan.setStrListCountAndRef
              @"enabledLayerCount" @"ppEnabledLayerNames"
              [ "VK_LAYER_LUNARG_standard_validation" ]
        &* Vulkan.setListCountAndRef
              @"enabledExtensionCount" @"ppEnabledExtensionNames"
              neededExtensions
        )
  in
    managedVulkanResource
      ( Vulkan.vkCreateInstance ( Vulkan.unsafePtr createInfo ) )
      Vulkan.vkDestroyInstance


createPhysicalDevice :: MonadIO m => Vulkan.VkInstance -> m Vulkan.VkPhysicalDevice
createPhysicalDevice vk = liftIO do
  physicalDevices <-
    fetchAll
      ( \nPtr ptr ->
          Vulkan.vkEnumeratePhysicalDevices vk nPtr ptr
            >>= throwVkResult
      )

  typedDevices <-
    for physicalDevices $ \physicalDevice -> do
      properties <-
        allocaAndPeek
          ( Vulkan.vkGetPhysicalDeviceProperties physicalDevice )

      pure ( physicalDevice, Vulkan.getField @"deviceType" properties )

  case filter (isSuitableDeviceType . snd) typedDevices of
    [] -> fail "Could not find a suitable physical device"
    ( ( d, _deviceType ) : _ds )
      -> pure d

  where

    isSuitableDeviceType :: Vulkan.VkPhysicalDeviceType -> Bool
    isSuitableDeviceType
      = flip elem
          [ Vulkan.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          , Vulkan.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          ]


findQueueFamilyIndex
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> [ Vulkan.VkQueueFlags ]
  -> m Int
findQueueFamilyIndex physicalDevice requiredFlags = liftIO do
  queueFamilies <- fetchAll ( Vulkan.vkGetPhysicalDeviceQueueFamilyProperties physicalDevice )

  let
    capableFamilyIndices :: [Int]
    capableFamilyIndices = do
      ( i, queueFamily ) <- zip [0..] queueFamilies

      let
        flags :: Vulkan.VkQueueFlags
        flags = Vulkan.getField @"queueFlags" queueFamily

      for_ requiredFlags
        ( \f ->
            guard ( flags .&. f > Vulkan.VK_ZERO_FLAGS )
        )

      pure i

  case capableFamilyIndices of
    []        -> fail "No queue family has sufficient capabilities"
    ( i : _ ) -> pure i


createLogicalDevice
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Int
  -> Maybe Vulkan.VkPhysicalDeviceFeatures
  -> m Vulkan.VkDevice
createLogicalDevice physicalDevice queueFamilyIndex mbFeatures =
  let
    queueCreateInfo :: Vulkan.VkDeviceQueueCreateInfo
    queueCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        &* Vulkan.set @"pNext" Foreign.nullPtr
        &* Vulkan.set @"queueFamilyIndex" ( fromIntegral queueFamilyIndex )
        &* Vulkan.setListCountAndRef @"queueCount" @"pQueuePriorities" [ 1.0 :: Float ]
        )



    deviceCreateInfo :: Vulkan.VkDeviceCreateInfo
    deviceCreateInfo = case mbFeatures of
      Nothing ->
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
          &* Vulkan.set @"pNext" Foreign.nullPtr
          &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
          &* Vulkan.setListCountAndRef
                @"queueCreateInfoCount"
                @"pQueueCreateInfos"
                [ queueCreateInfo ]
          &* Vulkan.setListCountAndRef
                @"enabledLayerCount"
                @"ppEnabledLayerNames"
                []
          &* Vulkan.setListCountAndRef
                @"enabledExtensionCount"
                @"ppEnabledExtensionNames"
                [ Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME ]
          &* Vulkan.set @"pEnabledFeatures" Vulkan.vkNullPtr
          )
      Just features ->
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
          &* Vulkan.set @"pNext" Foreign.nullPtr
          &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
          &* Vulkan.setListCountAndRef
                @"queueCreateInfoCount"
                @"pQueueCreateInfos"
                [ queueCreateInfo ]
          &* Vulkan.setListCountAndRef
                @"enabledLayerCount"
                @"ppEnabledLayerNames"
                []
          &* Vulkan.setListCountAndRef
                @"enabledExtensionCount"
                @"ppEnabledExtensionNames"
                [ Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME ]
          &* Vulkan.setVkRef @"pEnabledFeatures" features
          )

  in
    managedVulkanResource
      ( Vulkan.vkCreateDevice physicalDevice ( Vulkan.unsafePtr deviceCreateInfo ) )
      Vulkan.vkDestroyDevice


chooseSwapchainFormat
  :: MonadIO m
  => Vulkan.VkSurfaceFormatKHR
  -> Vulkan.VkPhysicalDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m Vulkan.VkSurfaceFormatKHR
chooseSwapchainFormat
  preferredFormat@(VkSurfaceFormatKHR fmt_p spc_p)
  physicalDevice
  surface
  = liftIO do
      surfaceFormats <-
        fetchAll
          ( \surfaceFormatCountPtr surfaceFormatsPtr ->
            Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR
              physicalDevice
              ( Vulkan.VkPtr surface )
              surfaceFormatCountPtr
              surfaceFormatsPtr
              >>= throwVkResult
          )

      case sortOn ( Down . score ) surfaceFormats of
        [] -> fail "No formats found."
        ( best : _ )
          | Vulkan.VK_FORMAT_UNDEFINED <- Vulkan.getField @"format" best
            -> pure preferredFormat
          | otherwise
            -> pure best

    where
      match :: Eq a => a -> a -> Int
      match a b
        | a == b    = 1
        | otherwise = 0

      score :: Vulkan.VkSurfaceFormatKHR -> Int
      score (VkSurfaceFormatKHR fmt spc)
        = match fmt fmt_p
        + match spc spc_p

{-# COMPLETE VkSurfaceFormatKHR #-}
pattern VkSurfaceFormatKHR :: Vulkan.VkFormat -> Vulkan.VkColorSpaceKHR -> Vulkan.VkSurfaceFormatKHR
pattern VkSurfaceFormatKHR { format, colorSpace }
  <- ( Vulkan.getField @"format" &&& Vulkan.getField @"colorSpace"
         -> (format, colorSpace)
     )
    where VkSurfaceFormatKHR fmt spc
            = Vulkan.createVk
                (  Vulkan.set @"format"     fmt
                &* Vulkan.set @"colorSpace" spc
                )

createSwapchain
  :: ( MonadIO m, MonadManaged m )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> Vulkan.VkSurfaceFormatKHR
  -> Vulkan.VkImageUsageFlags
  -> m ( Vulkan.VkSwapchainKHR, Vulkan.VkExtent2D )
createSwapchain physicalDevice device surface surfaceFormat imageUsage = do

  surfaceCapabilities <- liftIO $
    allocaAndPeek
      ( Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR
          physicalDevice
          ( Vulkan.VkPtr surface )
          >=> throwVkResult
      )

  let
    minImageCount, maxImageCount, imageCount :: Vulkan.Word32
    minImageCount = Vulkan.getField @"minImageCount" surfaceCapabilities
    maxImageCount = Vulkan.getField @"maxImageCount" surfaceCapabilities
    imageCount
      | maxImageCount == 0 = minImageCount + 1 -- no maximum
      | otherwise = min ( minImageCount + 1 ) maxImageCount

    currentExtent :: Vulkan.VkExtent2D
    currentExtent = Vulkan.getField @"currentExtent" surfaceCapabilities

    currentTransform :: Vulkan.VkSurfaceTransformBitmaskKHR Vulkan.FlagBit
    currentTransform = Vulkan.getField @"currentTransform" surfaceCapabilities

    swapchainCreateInfo :: Vulkan.VkSwapchainCreateInfoKHR
    swapchainCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"                 Vulkan.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
        &* Vulkan.set @"pNext"                 Foreign.nullPtr
        &* Vulkan.set @"surface"               ( Vulkan.VkPtr surface )
        &* Vulkan.set @"minImageCount"         imageCount
        &* Vulkan.set @"imageFormat"           ( format surfaceFormat )
        &* Vulkan.set @"imageColorSpace"       ( colorSpace surfaceFormat )
        &* Vulkan.set @"imageExtent"           currentExtent
        &* Vulkan.set @"imageArrayLayers"      1
        &* Vulkan.set @"imageUsage"            imageUsage
        &* Vulkan.set @"imageSharingMode"      Vulkan.VK_SHARING_MODE_EXCLUSIVE
        &* Vulkan.set @"queueFamilyIndexCount" 0
        &* Vulkan.set @"pQueueFamilyIndices"   Vulkan.vkNullPtr
        &* Vulkan.set @"preTransform"          currentTransform
        &* Vulkan.set @"compositeAlpha"        Vulkan.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        &* Vulkan.set @"presentMode"           Vulkan.VK_PRESENT_MODE_FIFO_KHR
        &* Vulkan.set @"clipped"               Vulkan.VK_TRUE
        &* Vulkan.set @"oldSwapchain"          Vulkan.VK_NULL_HANDLE
        )

  swapchain <-
    managedVulkanResource
      ( Vulkan.vkCreateSwapchainKHR  device ( Vulkan.unsafePtr swapchainCreateInfo ) )
      ( Vulkan.vkDestroySwapchainKHR device )

  pure ( swapchain, currentExtent )


getSwapchainImages
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> m [ Vulkan.VkImage ]
getSwapchainImages device swapchain
  = liftIO $
      fetchAll
        ( \imageCountPtr imagesPtr ->
            Vulkan.vkGetSwapchainImagesKHR
              device
              swapchain
              imageCountPtr
              imagesPtr
            >>= throwVkResult
        )


createFramebuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> [Vulkan.VkImageView]
  -> m Vulkan.VkFramebuffer
createFramebuffer dev renderPass extent attachments =
  let
    createInfo :: Vulkan.VkFramebufferCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.setListCountAndRef @"attachmentCount" @"pAttachments" attachments
        &* Vulkan.set @"width"  ( Vulkan.getField @"width"  extent )
        &* Vulkan.set @"height" ( Vulkan.getField @"height" extent )
        &* Vulkan.set @"layers" 1
        )
  in
    managedVulkanResource
      ( Vulkan.vkCreateFramebuffer  dev ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyFramebuffer dev )


data ImageInfo
  = ImageInfo
  { imageType        :: Vulkan.VkImageType
  , imageExtent      :: Vulkan.VkExtent3D
  , imageFormat      :: Vulkan.VkFormat
  , imageLayout      :: Vulkan.VkImageLayout
  , imageMipLevels   :: Vulkan.Word32
  , imageArrayLayers :: Vulkan.Word32
  , imageSamples     :: Vulkan.VkSampleCountFlagBits
  , imageTiling      :: Vulkan.VkImageTiling
  , imageUsage       :: Vulkan.VkImageUsageFlags
  }

pattern Default2DImageInfo :: Vulkan.VkExtent3D -> Vulkan.VkFormat -> Vulkan.VkImageUsageFlags -> ImageInfo
pattern Default2DImageInfo extent3D fmt usage
  = ImageInfo
  { imageType        = Vulkan.VK_IMAGE_TYPE_2D
  , imageExtent      = extent3D
  , imageFormat      = fmt
  , imageLayout      = Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
  , imageMipLevels   = 1
  , imageArrayLayers = 1
  , imageSamples     = Vulkan.VK_SAMPLE_COUNT_1_BIT
  , imageTiling      = Vulkan.VK_IMAGE_TILING_OPTIMAL
  , imageUsage       = usage
  }

createImage
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> ImageInfo
  -> [ Vulkan.VkMemoryPropertyFlags ]
  -> m (Vulkan.VkImage, Vulkan.VkDeviceMemory)
createImage physicalDevice device ImageInfo { .. } reqs
  = let createInfo :: Vulkan.VkImageCreateInfo
        createInfo =
          Vulkan.createVk
            (  Vulkan.set @"sType"       Vulkan.VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
            &* Vulkan.set @"pNext"       Vulkan.vkNullPtr
            &* Vulkan.set @"flags"       Vulkan.VK_ZERO_FLAGS
            &* Vulkan.set @"imageType"   imageType
            &* Vulkan.set @"format"      imageFormat
            &* Vulkan.set @"extent"      imageExtent
            &* Vulkan.set @"mipLevels"   imageMipLevels
            &* Vulkan.set @"arrayLayers" imageArrayLayers
            &* Vulkan.set @"samples"     imageSamples
            &* Vulkan.set @"tiling"      imageTiling
            &* Vulkan.set @"usage"       imageUsage
            &* Vulkan.set @"sharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
            &* Vulkan.set @"queueFamilyIndexCount" 0
            &* Vulkan.set @"pQueueFamilyIndices"   Vulkan.VK_NULL
            &* Vulkan.set @"initialLayout"         imageLayout
            )
    in do
      image <- managedVulkanResource
                  ( Vulkan.vkCreateImage  device ( Vulkan.unsafePtr createInfo ) )
                  ( Vulkan.vkDestroyImage device )

      memReqs <- allocaAndPeek ( Vulkan.vkGetImageMemoryRequirements device image )

      memory <- allocateMemory physicalDevice device memReqs reqs

      liftIO
        ( Vulkan.vkBindImageMemory device image memory 0
            >>= throwVkResult
        )

      pure (image, memory)


createImageView
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkImage
  -> Vulkan.VkImageViewType
  -> Vulkan.VkFormat
  -> Vulkan.VkImageAspectFlags
  -> m Vulkan.VkImageView
createImageView dev image viewType fmt aspect =
  let
    components :: Vulkan.VkComponentMapping
    components =
      Vulkan.createVk
        (  Vulkan.set @"r" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"g" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"b" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"a" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        )

    subResourceRange :: Vulkan.VkImageSubresourceRange
    subResourceRange =
      Vulkan.createVk
        (  Vulkan.set @"aspectMask"     aspect
        &* Vulkan.set @"baseMipLevel"   0
        &* Vulkan.set @"levelCount"     1
        &* Vulkan.set @"baseArrayLayer" 0
        &* Vulkan.set @"layerCount"     1
        )

    createInfo :: Vulkan.VkImageViewCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
        &* Vulkan.set @"pNext"      Vulkan.vkNullPtr
        &* Vulkan.set @"flags"      Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"image"      image
        &* Vulkan.set @"viewType"   viewType
        &* Vulkan.set @"format"     fmt
        &* Vulkan.set @"components" components
        &* Vulkan.set @"subresourceRange" subResourceRange
        )
  in
    managedVulkanResource
      ( Vulkan.vkCreateImageView  dev ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyImageView dev )

cmdTransitionImageLayout
  :: MonadManaged m
  => Vulkan.VkCommandBuffer
  -> Vulkan.VkImage
  -> Vulkan.VkImageLayout
  -> Vulkan.VkImageLayout
  -> (Vulkan.VkPipelineStageFlags, Vulkan.VkAccessFlags)
  -> (Vulkan.VkPipelineStageFlags, Vulkan.VkAccessFlags)
  -> m ()
cmdTransitionImageLayout
  commandBuffer
  image
  oldLayout newLayout
  (srcStage, srcMask) (dstStage, dstMask)
  = let
      subresourceRange :: Vulkan.VkImageSubresourceRange
      subresourceRange =
        Vulkan.createVk
          (  Vulkan.set @"aspectMask"     Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
          &* Vulkan.set @"baseMipLevel"   0
          &* Vulkan.set @"levelCount"     1
          &* Vulkan.set @"baseArrayLayer" 0
          &* Vulkan.set @"layerCount"     1
          )

      imageBarrier :: Vulkan.VkImageMemoryBarrier
      imageBarrier =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
          &* Vulkan.set @"pNext" Vulkan.vkNullPtr
          &* Vulkan.set @"srcAccessMask" srcMask
          &* Vulkan.set @"dstAccessMask" dstMask
          &* Vulkan.set @"oldLayout"     oldLayout
          &* Vulkan.set @"newLayout"     newLayout
          &* Vulkan.set @"image"               image
          &* Vulkan.set @"subresourceRange"    subresourceRange
          &* Vulkan.set @"srcQueueFamilyIndex" Vulkan.VK_QUEUE_FAMILY_IGNORED
          &* Vulkan.set @"dstQueueFamilyIndex" Vulkan.VK_QUEUE_FAMILY_IGNORED
          )

    in cmdPipelineBarrier
        commandBuffer
        srcStage
        dstStage
        []
        []
        [ imageBarrier ]

createSampler
  :: MonadManaged m
  => Vulkan.VkDevice
  -> m Vulkan.VkSampler
createSampler dev =
  let
    createInfo :: Vulkan.VkSamplerCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"magFilter" Vulkan.VK_FILTER_NEAREST
        &* Vulkan.set @"minFilter" Vulkan.VK_FILTER_NEAREST
        &* Vulkan.set @"mipmapMode" Vulkan.VK_SAMPLER_MIPMAP_MODE_NEAREST
        &* Vulkan.set @"addressModeU" Vulkan.VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
        &* Vulkan.set @"addressModeV" Vulkan.VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
        &* Vulkan.set @"addressModeW" Vulkan.VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
        &* Vulkan.set @"mipLodBias" 0
        &* Vulkan.set @"anisotropyEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"maxAnisotropy" 0
        &* Vulkan.set @"compareEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"compareOp" Vulkan.VK_COMPARE_OP_ALWAYS
        &* Vulkan.set @"minLod" 0
        &* Vulkan.set @"maxLod" 1
        &* Vulkan.set @"borderColor" Vulkan.VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
        )

  in managedVulkanResource
       ( Vulkan.vkCreateSampler  dev ( Vulkan.unsafePtr createInfo ) )
       ( Vulkan.vkDestroySampler dev )


createCommandPool
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Int
  -> m Vulkan.VkCommandPool
createCommandPool dev queueFamilyIndex =
  let
    createInfo :: Vulkan.VkCommandPoolCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"queueFamilyIndex" ( fromIntegral queueFamilyIndex )
        )

  in
    managedVulkanResource
      ( Vulkan.vkCreateCommandPool dev ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyCommandPool dev )


allocateCommandBuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> m Vulkan.VkCommandBuffer
allocateCommandBuffer dev commandPool =
  let
    allocInfo :: Vulkan.VkCommandBufferAllocateInfo
    allocInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"        Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
        &* Vulkan.set @"pNext"        Vulkan.vkNullPtr
        &* Vulkan.set @"commandPool"  commandPool
        &* Vulkan.set @"level"        Vulkan.VK_COMMAND_BUFFER_LEVEL_PRIMARY
        &* Vulkan.set @"commandBufferCount" 1
        )
  in
    manageBracket
      ( allocaAndPeek
          ( Vulkan.vkAllocateCommandBuffers dev ( Vulkan.unsafePtr allocInfo )
              >=> throwVkResult
          )
      )
      ( \a ->
          Foreign.Marshal.withArray [ a ]
            ( Vulkan.vkFreeCommandBuffers dev commandPool 1 )
      )


cmdBeginRenderPass
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> Vulkan.VkRenderPass
  -> Vulkan.VkFramebuffer
  -> [Vulkan.VkClearValue] -- indexed by framebuffer attachments
  -> Vulkan.VkExtent2D
  -> m ()
cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues extent =
  let
    zeroZero :: Vulkan.VkOffset2D
    zeroZero =
      Vulkan.createVk
        (  Vulkan.set @"x" 0
        &* Vulkan.set @"y" 0
        )

    renderArea :: Vulkan.VkRect2D
    renderArea =
      Vulkan.createVk
        (  Vulkan.set @"offset" zeroZero
        &* Vulkan.set @"extent" extent
        )

    beginInfo :: Vulkan.VkRenderPassBeginInfo
    beginInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"       Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
        &* Vulkan.set @"pNext"       Vulkan.vkNullPtr
        &* Vulkan.set @"renderPass"  renderPass
        &* Vulkan.set @"framebuffer" framebuffer
        &* Vulkan.set @"renderArea"  renderArea
        &* Vulkan.setListCountAndRef
                @"clearValueCount"
                @"pClearValues"
                clearValues
        )
  in
    liftIO $
      Vulkan.vkCmdBeginRenderPass
        commandBuffer
        ( Vulkan.unsafePtr beginInfo )
        Vulkan.VK_SUBPASS_CONTENTS_INLINE

cmdNextSubpass :: MonadIO m => Vulkan.VkCommandBuffer -> m ()
cmdNextSubpass commandBuffer =
  liftIO $ Vulkan.vkCmdNextSubpass commandBuffer Vulkan.VK_SUBPASS_CONTENTS_INLINE


cmdEndRenderPass :: MonadIO m => Vulkan.VkCommandBuffer -> m ()
cmdEndRenderPass = liftIO . Vulkan.vkCmdEndRenderPass

acquireNextImage
  :: MonadIO m
  => Vulkan.VkDevice
  -> Vulkan.VkSwapchainKHR
  -> Vulkan.VkSemaphore
  -> m Int
acquireNextImage device swapchain signal
  = liftIO . fmap fromIntegral
  $ allocaAndPeek
      ( Vulkan.vkAcquireNextImageKHR
          device
          swapchain
          maxBound
          signal
          Vulkan.VK_NULL_HANDLE
        >=> throwVkResult
      )

present
  :: MonadIO m
  => Vulkan.VkQueue
  -> Vulkan.VkSwapchainKHR
  -> Int
  -> [Vulkan.VkSemaphore]
  -> m ()
present queue swapchain imageIndex wait
  = let
      presentInfo :: Vulkan.VkPresentInfoKHR
      presentInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
          &* Vulkan.set @"pNext" Vulkan.vkNullPtr
          &* Vulkan.setListCountAndRef @"waitSemaphoreCount" @"pWaitSemaphores" wait
          &* Vulkan.setListCountAndRef @"swapchainCount" @"pSwapchains" [ swapchain ]
          &* Vulkan.setListRef @"pImageIndices" [ fromIntegral imageIndex ]
          &* Vulkan.set @"pResults" Vulkan.vkNullPtr
          )
    in
      liftIO $
        Vulkan.vkQueuePresentKHR queue ( Vulkan.unsafePtr presentInfo )
        >>= throwVkResult

getQueue :: MonadIO m => Vulkan.VkDevice -> Int -> m Vulkan.VkQueue
getQueue device queueFamilyIndex
  = liftIO $
      allocaAndPeek
        ( Vulkan.vkGetDeviceQueue
            device
            ( fromIntegral queueFamilyIndex )
            0
        )


createSemaphore :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkSemaphore
createSemaphore device =
  let
    createInfo :: Vulkan.VkSemaphoreCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL_HANDLE
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        )
  in
    managedVulkanResource
      ( Vulkan.vkCreateSemaphore  device ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroySemaphore device )


createFence :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkFence
createFence device =

  let fenceCreateInfo :: Vulkan.VkFenceCreateInfo
      fenceCreateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL_HANDLE
          &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
          )

  in
    managedVulkanResource
      ( Vulkan.vkCreateFence  device ( Vulkan.unsafePtr fenceCreateInfo ) )
      ( Vulkan.vkDestroyFence device )


data Wait a = WaitAll [a] | WaitAny [a]
  deriving ( Eq, Show )

waitForFences :: MonadIO m => Vulkan.VkDevice -> Wait Vulkan.VkFence -> m ()
waitForFences device fences = liftIO $
  Foreign.Marshal.withArray fenceList $ \fencesPtr ->
    Vulkan.vkWaitForFences device
      ( fromIntegral $ length fenceList )
      fencesPtr
      waitAll
      maxBound
    >>= throwVkResult

    where waitAll   :: Vulkan.VkBool32
          fenceList :: [Vulkan.VkFence]
          (waitAll, fenceList)
            = case fences of
                WaitAll l -> ( Vulkan.VK_TRUE , l )
                WaitAny l -> ( Vulkan.VK_FALSE, l )

cmdPipelineBarrier
  :: MonadIO m
  => Vulkan.VkCommandBuffer
  -> Vulkan.VkPipelineStageFlags
  -> Vulkan.VkPipelineStageFlags
  -> [Vulkan.VkMemoryBarrier]
  -> [Vulkan.VkBufferMemoryBarrier]
  -> [Vulkan.VkImageMemoryBarrier]
  -> m ()
cmdPipelineBarrier
  commandBuffer
  srcStageMask dstStageMask
  memoryBarriers bufferMemoryBarriers imageMemoryBarriers
    = liftIO $
        Foreign.Marshal.withArray memoryBarriers $ \memoryBarriersPtr ->
          Foreign.Marshal.withArray bufferMemoryBarriers $ \bufferMemoryBarriersPtr ->
            Foreign.Marshal.withArray imageMemoryBarriers $ \imageMemoryBarriersPtr ->
              Vulkan.vkCmdPipelineBarrier
                commandBuffer
                srcStageMask
                dstStageMask
                Vulkan.VK_DEPENDENCY_BY_REGION_BIT
                ( fromIntegral ( length memoryBarriers ) )
                memoryBarriersPtr
                ( fromIntegral ( length bufferMemoryBarriers) )
                bufferMemoryBarriersPtr
                ( fromIntegral ( length imageMemoryBarriers ) )
                imageMemoryBarriersPtr



assertSurfacePresentable
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> Int
  -> SDL.Video.Vulkan.VkSurfaceKHR
  -> m ()
assertSurfacePresentable physicalDevice queueFamilyIndex surface = liftIO do
  bool <-
    allocaAndPeek
      ( Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR
          physicalDevice
          ( fromIntegral queueFamilyIndex )
          ( Vulkan.VkPtr surface )
          >=> throwVkResult
      )

  unless ( bool == Vulkan.VK_TRUE ) ( fail "Unsupported surface" )


submitCommandBuffer
  :: MonadIO m
  => Vulkan.VkQueue
  -> Vulkan.VkCommandBuffer
  -> [ ( Vulkan.VkSemaphore, Vulkan.VkPipelineStageFlags ) ]
  -> [ Vulkan.VkSemaphore ]
  -> Maybe Vulkan.VkFence
  -> m ()
submitCommandBuffer queue commandBuffer wait signal mbFence =
  let
    submitInfo :: Vulkan.VkSubmitInfo
    submitInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SUBMIT_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.setListCountAndRef
              @"waitSemaphoreCount"
              @"pWaitSemaphores"
              ( map fst wait )
        &* Vulkan.setListRef @"pWaitDstStageMask" ( map snd wait )
        &* Vulkan.setListCountAndRef
              @"commandBufferCount"
              @"pCommandBuffers"
              [ commandBuffer ]
        &* Vulkan.setListCountAndRef
           @"signalSemaphoreCount"
           @"pSignalSemaphores"
           signal
        )
  in liftIO $
      Foreign.Marshal.withArray [ submitInfo ] $ \submits ->
        Vulkan.vkQueueSubmit queue 1 submits (fromMaybe Vulkan.vkNullPtr mbFence)
        >>= throwVkResult

beginCommandBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> m ()
beginCommandBuffer commandBuffer =
  let
    commandBufferBeginInfo :: Vulkan.VkCommandBufferBeginInfo
    commandBufferBeginInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"pInheritanceInfo" Vulkan.vkNullPtr
        )
  in liftIO $
        Vulkan.vkBeginCommandBuffer
          commandBuffer
          ( Vulkan.unsafePtr commandBufferBeginInfo )
          >>= throwVkResult


endCommandBuffer :: MonadIO m => Vulkan.VkCommandBuffer -> m ()
endCommandBuffer = liftIO . Vulkan.vkEndCommandBuffer >=> throwVkResult
