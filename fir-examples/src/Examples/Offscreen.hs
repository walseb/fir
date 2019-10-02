{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Examples.Offscreen ( offscreen ) where

-- base
import Control.Monad
  ( (>=>) )
import Control.Monad.IO.Class
  ( liftIO )
import Data.Bits
import Data.Coerce
  ( coerce )
import Data.Word
  ( Word8, Word32 )
import qualified Foreign
import qualified Foreign.Marshal

-- JuicyPixels
import Codec.Picture.Types
  ( Image(..), PixelRGBA8(..) )
import Codec.Picture.Png
  ( writePng )

-- managed
import Control.Monad.Managed
  ( MonadManaged, runManaged )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

-- vector
import qualified Data.Vector.Storable as Vector

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct(..)
  , ModuleRequirements
  )
import Math.Linear
  ( pattern V3 )

-- fir-examples
import Examples.Offscreen.Shaders
import Simulation.Observer
import Vulkan.Backend
import Vulkan.Buffer
import Vulkan.Features
  ( requiredFeatures )
import Vulkan.Monad
import Vulkan.Pipeline

----------------------------------------------------------------------------

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Vertex shader"  , compileVertexShader  )
        , ("Fragment shader", compileFragmentShader)
        ]
     )

appName :: String
appName = "fir-examples - Offscreen"

offscreen :: IO ()
offscreen = runManaged do

  ( reqs :: ModuleRequirements ) <-
    case shaderCompilationResult of
      Left  err  -> error $ "Shader compilation was unsuccessful:\n" <> ShortText.unpack err
      Right reqs -> logMsg ( "Shaders were succesfully compiled." ) *> pure reqs

  vulkanInstance   <- logMsg "Creating Vulkan instance"      *> createVulkanInstance appName []
  physicalDevice   <- logMsg "Creating physical device"      *> createPhysicalDevice vulkanInstance
  queueFamilyIndex <- logMsg "Finding suitable queue family"
      *> findQueueFamilyIndex physicalDevice [Vulkan.VK_QUEUE_GRAPHICS_BIT]

  features <- liftIO ( requiredFeatures reqs )
  device   <- logMsg "Creating logical device"       *> createLogicalDevice  physicalDevice queueFamilyIndex features


  let
    colFmt :: Vulkan.VkFormat
    colFmt = Vulkan.VK_FORMAT_B8G8R8A8_UNORM

    depthFmt :: Vulkan.VkFormat
    depthFmt = Vulkan.VK_FORMAT_D32_SFLOAT

    width, height :: Num a => a
    width  = 1920
    height = 1080

    extent :: Vulkan.VkExtent2D
    extent =
      Vulkan.createVk
        (  Vulkan.set @"width"  width
        &* Vulkan.set @"height" height
        )

    extent3D :: Vulkan.VkExtent3D
    extent3D =
      Vulkan.createVk
        (  Vulkan.set @"width"  width
        &* Vulkan.set @"height" height
        &* Vulkan.set @"depth"  1
        )

  renderPass <- logMsg "Creating a render pass" *> createRenderPass device colFmt depthFmt

  let
    colorImageInfo, depthImageInfo, screenshotImageInfo :: ImageInfo
    colorImageInfo =
      Default2DImageInfo extent3D colFmt
        ( Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT )
    depthImageInfo =
      Default2DImageInfo extent3D depthFmt
        Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
    screenshotImageInfo =
      ( Default2DImageInfo extent3D colFmt
        Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
      ) { imageTiling = Vulkan.VK_IMAGE_TILING_LINEAR }  -- host visible image needs linear tiling

  (colorImage, _) <-
    createImage physicalDevice device
      colorImageInfo
      [ ]
  colorImageView <-
    createImageView
      device colorImage
      Vulkan.VK_IMAGE_VIEW_TYPE_2D
      colFmt
      Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
  (screenshotImage, screenshotImageMemory) <-
    createImage physicalDevice device
      screenshotImageInfo
      [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
      , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
      ]
  (depthImage, _) <-
    createImage physicalDevice device
      depthImageInfo
      [ ]
  depthImageView <- createImageView device depthImage
    Vulkan.VK_IMAGE_VIEW_TYPE_2D
    depthFmt
    Vulkan.VK_IMAGE_ASPECT_DEPTH_BIT
  
  framebuffer <- createFramebuffer device renderPass extent [colorImageView, depthImageView]


  let clearValues :: [ Vulkan.VkClearValue ] -- in bijection with framebuffer attachments
      clearValues = [ pinkClear
                    , Vulkan.createVk ( Vulkan.set @"depthStencil" depthStencilClear )
                    ]
        where
          pink :: Vulkan.VkClearColorValue
          pink =
            Vulkan.createVk
              (  Vulkan.setAt @"float32" @0 1.0
              &* Vulkan.setAt @"float32" @1 0.5
              &* Vulkan.setAt @"float32" @2 0.7
              &* Vulkan.setAt @"float32" @3 1
              )

          pinkClear :: Vulkan.VkClearValue
          pinkClear = Vulkan.createVk ( Vulkan.set @"color"  pink )

          depthStencilClear :: Vulkan.VkClearDepthStencilValue
          depthStencilClear = Vulkan.createVk
            ( Vulkan.set @"depth" 1 &* Vulkan.set @"stencil" 0 )

  commandPool <- logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex
  queue       <- getQueue device 0

  descriptorPool <- createDescriptorPool device

  descriptorSetLayout <- createDescriptorSetLayout device
  descriptorSet       <- allocateDescriptorSet device descriptorPool descriptorSetLayout

  let pipelineInfo = PipelineInfo extent Vulkan.VK_SAMPLE_COUNT_1_BIT
  ( graphicsPipeline, pipelineLayout )
    <- createGraphicsPipeline device renderPass pipelineInfo descriptorSetLayout shaderPipeline

  let

    phi :: Float
    phi = 0.5 + sqrt 1.25

    icosahedronVerts :: [ Struct VertexInput ]
    icosahedronVerts =
      [ ( V3    0     1    phi  ) :& ( V3 0    1    0    ) :& End
      , ( V3    0   (-1)   phi  ) :& ( V3 0    0.75 0.25 ) :& End
      , ( V3    0     1  (-phi) ) :& ( V3 0    0.25 0.75 ) :& End
      , ( V3    0   (-1) (-phi) ) :& ( V3 0    0    1    ) :& End
      , ( V3    1    phi    0   ) :& ( V3 1    0    0    ) :& End
      , ( V3  (-1)   phi    0   ) :& ( V3 0.75 0.25 0    ) :& End
      , ( V3    1  (-phi)   0   ) :& ( V3 0.25 0.75 0    ) :& End
      , ( V3  (-1) (-phi)   0   ) :& ( V3 0    1    0    ) :& End
      , ( V3   phi    0     1   ) :& ( V3 1    0    0    ) :& End
      , ( V3   phi    0   (-1)  ) :& ( V3 0.75 0    0.25 ) :& End
      , ( V3 (-phi)   0     1   ) :& ( V3 0.25 0    0.75 ) :& End
      , ( V3 (-phi)   0   (-1)  ) :& ( V3 0    0    1    ) :& End
      ]

    icosahedronIndices :: [ Word32 ]
    icosahedronIndices
      = [ 0,  1,  8
        , 0, 10,  1
        , 0,  4,  5
        , 0,  8,  4
        , 0,  5, 10
        , 1,  7,  6
        , 1,  6,  8
        , 1, 10,  7
        , 2,  9,  3
        , 2,  3, 11
        , 2,  5,  4
        , 2,  4,  9
        , 2, 11,  5
        , 3,  6,  7
        , 3,  9,  6
        , 3,  7, 11
        , 4,  8,  9
        , 5, 11, 10
        , 6,  9,  8
        , 7, 10, 11
        ]

  (vertexBuffer, _) <- createVertexBuffer physicalDevice device icosahedronVerts

  (indexBuffer, _) <- createIndexBuffer physicalDevice device icosahedronIndices

  (mvpUniformBuffer, _)
    <- createUniformBuffer
          physicalDevice
          device
          ( modelViewProjection initialObserver Nothing )

  updateDescriptorSet device descriptorSet mvpUniformBuffer

  commandBuffer <- allocateCommandBuffer device commandPool

  beginCommandBuffer commandBuffer

  cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues extent

  liftIO $
    Foreign.Marshal.withArray [ vertexBuffer ] $ \buffers ->
    Foreign.Marshal.withArray [ 0 ] $ \offsets ->
    Vulkan.vkCmdBindVertexBuffers commandBuffer 0 1 buffers offsets

  liftIO $
    Vulkan.vkCmdBindIndexBuffer
      commandBuffer
      indexBuffer
      0
      Vulkan.VK_INDEX_TYPE_UINT32

  liftIO $ do
    Vulkan.vkCmdBindPipeline
      commandBuffer
      Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
      graphicsPipeline

    Foreign.Marshal.withArray [ descriptorSet ] $ \descriptorSetsPtr ->
      Vulkan.vkCmdBindDescriptorSets
        commandBuffer
        Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        pipelineLayout
        0
        1
        descriptorSetsPtr
        0
        Vulkan.vkNullPtr

    Vulkan.vkCmdDrawIndexed
      commandBuffer
      ( fromIntegral ( length icosahedronIndices ) )
      1
      0
      0
      0

  cmdEndRenderPass commandBuffer

  -- image copying
  let noOffset :: Vulkan.VkOffset3D
      noOffset
        = Vulkan.createVk
            (  Vulkan.set @"x" 0
            &* Vulkan.set @"y" 0
            &* Vulkan.set @"z" 0
            )

      layers :: Vulkan.VkImageSubresourceLayers
      layers
        = Vulkan.createVk
          (  Vulkan.set @"aspectMask"     Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
          &* Vulkan.set @"mipLevel"       0
          &* Vulkan.set @"baseArrayLayer" 0
          &* Vulkan.set @"layerCount"     1
          )

      imageCopy :: Vulkan.VkImageCopy
      imageCopy
        = Vulkan.createVk
          (  Vulkan.set @"srcSubresource" layers
          &* Vulkan.set @"srcOffset"      noOffset
          &* Vulkan.set @"dstSubresource" layers
          &* Vulkan.set @"dstOffset"      noOffset
          &* Vulkan.set @"extent"         extent3D
          )

      subresourceRange :: Vulkan.VkImageSubresourceRange
      subresourceRange =
        Vulkan.createVk
          (  Vulkan.set @"aspectMask"     Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
          &* Vulkan.set @"baseMipLevel"   0
          &* Vulkan.set @"levelCount"     1
          &* Vulkan.set @"baseArrayLayer" 0
          &* Vulkan.set @"layerCount"     1
          )

      screenshotImageBarrier :: Vulkan.VkImageMemoryBarrier
      screenshotImageBarrier =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
          &* Vulkan.set @"pNext" Vulkan.vkNullPtr
          &* Vulkan.set @"srcAccessMask" Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          &* Vulkan.set @"dstAccessMask" Vulkan.VK_ZERO_FLAGS
          &* Vulkan.set @"oldLayout"     Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
          &* Vulkan.set @"newLayout"     Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
          &* Vulkan.set @"image"               screenshotImage
          &* Vulkan.set @"subresourceRange"    subresourceRange
          &* Vulkan.set @"srcQueueFamilyIndex" Vulkan.VK_QUEUE_FAMILY_IGNORED
          &* Vulkan.set @"dstQueueFamilyIndex" Vulkan.VK_QUEUE_FAMILY_IGNORED
          )

  cmdPipelineBarrier
    commandBuffer
    Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    []
    []
    [ screenshotImageBarrier ]
  liftIO $ Vulkan.vkCmdCopyImage commandBuffer
    colorImage
    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    screenshotImage
    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    1
    ( Vulkan.unsafePtr imageCopy )

  -- now change image layout to make screenshot image available for memory mapping (to write to disk)

  let
    screenshotImageBarrier2 :: Vulkan.VkImageMemoryBarrier
    screenshotImageBarrier2 =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"srcAccessMask" Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        &* Vulkan.set @"dstAccessMask" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"oldLayout"     Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        &* Vulkan.set @"newLayout"     Vulkan.VK_IMAGE_LAYOUT_GENERAL
        &* Vulkan.set @"image"               screenshotImage
        &* Vulkan.set @"subresourceRange"    subresourceRange
        &* Vulkan.set @"srcQueueFamilyIndex" Vulkan.VK_QUEUE_FAMILY_IGNORED
        &* Vulkan.set @"dstQueueFamilyIndex" Vulkan.VK_QUEUE_FAMILY_IGNORED
        )

  cmdPipelineBarrier
    commandBuffer
    Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    []
    []
    [ screenshotImageBarrier2 ]

  endCommandBuffer commandBuffer

  fence <- createFence device

  submitCommandBuffer
    queue
    commandBuffer
    []
    []
    ( Just fence )

  liftIO $ waitForFences device (WaitAll [fence])

  liftIO do

      memPtr :: Vulkan.Ptr Word8
        <- coerce <$> allocaAndPeek
              ( Vulkan.vkMapMemory device screenshotImageMemory 0 maxBound Vulkan.VK_ZERO_FLAGS
                >=> throwVkResult
              )

      let size = 4 * width * height

          -- image data is stored in BGRA component order,
          -- whether R8G8B8A8 or B8G8R8A8 format is used
          bgraToRgba :: [a] -> [a]
          bgraToRgba ( b : g : r : a : rest )
            = r : g : b : a : bgraToRgba rest
          bgraToRgba l = l

      imageData :: Image PixelRGBA8
        <- Image width height . Vector.fromList . bgraToRgba <$> Foreign.peekArray size memPtr

      writePng "screenshots/offscreen.png" imageData

      Vulkan.vkUnmapMemory device screenshotImageMemory

  liftIO ( Vulkan.vkQueueWaitIdle queue )
    >>= throwVkResult

  pure ()



createRenderPass
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkFormat
  -> Vulkan.VkFormat
  -> m Vulkan.VkRenderPass
createRenderPass dev colorFormat depthFormat =
  let

    colorAttachmentDescription :: Vulkan.VkAttachmentDescription
    colorAttachmentDescription =
      Vulkan.createVk
        (  Vulkan.set @"flags"          Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"format"         colorFormat
        &* Vulkan.set @"samples"        Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"loadOp"         Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
        &* Vulkan.set @"storeOp"        Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        &* Vulkan.set @"stencilLoadOp"  Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* Vulkan.set @"initialLayout"  Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        &* Vulkan.set @"finalLayout"    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        )

    colorAttachmentReference :: Vulkan.VkAttachmentReference
    colorAttachmentReference =
      Vulkan.createVk
        (  Vulkan.set @"attachment" 0
        &* Vulkan.set @"layout"     Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        )

    depthAttachmentDescription :: Vulkan.VkAttachmentDescription
    depthAttachmentDescription =
      Vulkan.createVk
        (  Vulkan.set @"flags"          Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"format"         depthFormat
        &* Vulkan.set @"samples"        Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"loadOp"         Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
        &* Vulkan.set @"storeOp"        Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        &* Vulkan.set @"stencilLoadOp"  Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* Vulkan.set @"initialLayout"  Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        &* Vulkan.set @"finalLayout"    Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        )

    depthAttachmentReference :: Vulkan.VkAttachmentReference
    depthAttachmentReference =
      Vulkan.createVk
        (  Vulkan.set @"attachment" 1
        &* Vulkan.set @"layout"     Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        )

    subpass :: Vulkan.VkSubpassDescription
    subpass =
      Vulkan.createVk
        (  Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"pipelineBindPoint" Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        &* Vulkan.setListCountAndRef
              @"colorAttachmentCount"
              @"pColorAttachments"
              [ colorAttachmentReference ]
        &* Vulkan.setVkRef @"pDepthStencilAttachment" depthAttachmentReference
        &* Vulkan.setListCountAndRef @"inputAttachmentCount"    @"pInputAttachments"    []
        &* Vulkan.setListCountAndRef @"preserveAttachmentCount" @"pPreserveAttachments" []
        &* Vulkan.set @"pResolveAttachments" Vulkan.vkNullPtr

        )

    dependency1 :: Vulkan.VkSubpassDependency
    dependency1 =
      Vulkan.createVk
        (  Vulkan.set @"srcSubpass"    Vulkan.VK_SUBPASS_EXTERNAL
        &* Vulkan.set @"dstSubpass"    Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"srcStageMask"  Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        &* Vulkan.set @"srcAccessMask" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"dstStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"dstAccessMask"
              (    Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
        &* Vulkan.set @"dependencyFlags" Vulkan.VK_DEPENDENCY_BY_REGION_BIT
        )

    dependency2 :: Vulkan.VkSubpassDependency
    dependency2 =
      Vulkan.createVk
        (  Vulkan.set @"srcSubpass"    Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"dstSubpass"    Vulkan.VK_SUBPASS_EXTERNAL
        &* Vulkan.set @"srcStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"srcAccessMask"
              (    Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
        &* Vulkan.set @"dstStageMask"  Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        &* Vulkan.set @"dstAccessMask" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"dependencyFlags" Vulkan.VK_DEPENDENCY_BY_REGION_BIT
        )

    createInfo :: Vulkan.VkRenderPassCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.setListCountAndRef
              @"attachmentCount"
              @"pAttachments"
              [ colorAttachmentDescription, depthAttachmentDescription ]
        &* Vulkan.setListCountAndRef
              @"subpassCount"
              @"pSubpasses"
              [ subpass ]
        &* Vulkan.setListCountAndRef
              @"dependencyCount"
              @"pDependencies"
              [ dependency1, dependency2 ]
        )
  in
    managedVulkanResource
      ( Vulkan.vkCreateRenderPass  dev ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyRenderPass dev )



createDescriptorSetLayout
  :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkDescriptorSetLayout
createDescriptorSetLayout device = do
  let
    binding =
      Vulkan.createVk
        (  Vulkan.set @"binding" 0
        &* Vulkan.set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* Vulkan.set @"descriptorCount" 1
        &* Vulkan.set @"stageFlags" Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        &* Vulkan.set @"pImmutableSamplers" Vulkan.VK_NULL
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.setListCountAndRef @"bindingCount" @"pBindings" [ binding ]
        )

  managedVulkanResource
    ( Vulkan.vkCreateDescriptorSetLayout
        device
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyDescriptorSetLayout device )


createDescriptorPool
  :: MonadManaged m
  => Vulkan.VkDevice -> m Vulkan.VkDescriptorPool
createDescriptorPool device =
  let
    poolSize0 =
      Vulkan.createVk
        (  Vulkan.set @"type" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* Vulkan.set @"descriptorCount" 1
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
        &* Vulkan.setListCountAndRef @"poolSizeCount" @"pPoolSizes" [ poolSize0 ]
        &* Vulkan.set @"maxSets" 1
        )

  in
  managedVulkanResource
    ( Vulkan.vkCreateDescriptorPool device ( Vulkan.unsafePtr createInfo ) )
    ( Vulkan.vkDestroyDescriptorPool device )


allocateDescriptorSet
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorPool
  -> Vulkan.VkDescriptorSetLayout
  -> m Vulkan.VkDescriptorSet
allocateDescriptorSet dev descriptorPool layout0 = do
  let
    allocateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"descriptorPool" descriptorPool
        &* Vulkan.setListCountAndRef @"descriptorSetCount" @"pSetLayouts" [ layout0 ]
        )

  manageBracket
    ( allocaAndPeek
        ( Vulkan.vkAllocateDescriptorSets
            dev
            ( Vulkan.unsafePtr allocateInfo )
            >=> throwVkResult
        )
    )
    ( \a ->
        Foreign.Marshal.withArray [ a ]
          ( Vulkan.vkFreeDescriptorSets dev descriptorPool 1 )
    )


updateDescriptorSet
  :: MonadManaged m
  => Vulkan.VkDevice -> Vulkan.VkDescriptorSet -> Vulkan.VkBuffer -> m ()
updateDescriptorSet device descriptorSet buffer = do
  let
    bufferInfo =
      Vulkan.createVk
        (  Vulkan.set @"buffer" buffer
        &* Vulkan.set @"offset" 0
        &* Vulkan.set @"range" ( fromIntegral Vulkan.VK_WHOLE_SIZE )
        )
    writeUpdate0 =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"dstSet" descriptorSet
        &* Vulkan.set @"dstBinding" 0
        &* Vulkan.set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* Vulkan.set @"pTexelBufferView" Vulkan.VK_NULL
        &* Vulkan.set @"pImageInfo" Vulkan.VK_NULL
        &* Vulkan.setListRef @"pBufferInfo" [ bufferInfo ]
        &* Vulkan.set @"descriptorCount" 1
        &* Vulkan.set @"dstArrayElement" 0
        )

  liftIO $
    Foreign.Marshal.withArray [ writeUpdate0 ] $ \writeUpdatesPtr ->
      Vulkan.vkUpdateDescriptorSets device 1 writeUpdatesPtr 0 Vulkan.vkNullPtr
