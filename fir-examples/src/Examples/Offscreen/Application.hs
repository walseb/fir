{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Examples.Offscreen.Application ( offscreen ) where

-- base
import Data.Bits
import Data.Word
  ( Word32 )
import qualified Foreign.Marshal
import GHC.Generics
  ( Generic )

-- managed
import Control.Monad.Managed
  ( MonadManaged, runManaged )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( index )

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
  ( M, pattern V3 )

-- fir-examples
import Examples.Offscreen.Shaders
import Simulation.Observer
import Vulkan.Backend
import Vulkan.Context
import Vulkan.Features
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.Resource
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- Shaders and resource types.

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Vertex shader"  , compileVertexShader  )
        , ("Fragment shader", compileFragmentShader)
        ]
     )

appName :: String
appName = "fir-examples - Offscreen"
shortName :: String
shortName = "offscreen" -- output filename

type VertexData = Struct VertexInput

data ResourceSet i st
  = ResourceSet
    { uboResource  :: UniformBuffer (M 4 4 Float) i st
    , vertexBuffer :: VertexBuffer  VertexData    i st
    , indexBuffer  :: IndexBuffer   Word32        i st
    }
  deriving Generic

----------------------------------------------------------------------------
-- Application.

offscreen :: IO ()
offscreen = runManaged do

  -------------------------------------------
  -- Obtain requirements from shaders.

  ( reqs :: ModuleRequirements ) <-
    case shaderCompilationResult of
      Left  err  -> error $ "Shader compilation was unsuccessful:\n" <> ShortText.unpack err
      Right reqs -> logMsg ( "Shaders were succesfully compiled." ) *> pure reqs

  -------------------------------------------
  -- Initialise Vulkan context.

  features <- liftIO ( requiredFeatures reqs )
  VulkanContext{..} <-
    initialiseContext @Headless appName []
      RenderInfo
        { features
        , queueType   = Vulkan.VK_QUEUE_GRAPHICS_BIT
        , surfaceInfo = ()
        }

  -------------------------------------------
  -- Create images.

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

    colorImageInfo, depthImageInfo :: ImageInfo
    colorImageInfo =
      Default2DImageInfo extent3D colFmt
        ( Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT )
    depthImageInfo =
      Default2DImageInfo extent3D depthFmt
        Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT

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

  (depthImage, _) <-
    createImage physicalDevice device
      depthImageInfo
      [ ]
  depthImageView <- createImageView device depthImage
    Vulkan.VK_IMAGE_VIEW_TYPE_2D
    depthFmt
    Vulkan.VK_IMAGE_ASPECT_DEPTH_BIT

  (screenshotImage, screenshotImageMemory) <-
    createScreenshotImage physicalDevice device
      ( screenshotImageInfo extent3D colFmt )

  renderPass  <- logMsg "Creating a render pass" *> createRenderPass device colFmt depthFmt
  framebuffer <- createFramebuffer device renderPass extent [colorImageView, depthImageView]

  let
    clearValues :: [ Vulkan.VkClearValue ] -- in bijection with framebuffer attachments
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

    -------------------------------------------
    -- Manage resources.

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

    initialMVP :: M 4 4 Float
    initialMVP = modelViewProjection initialObserver Nothing

    resourceFlags :: ResourceSet 1 Named
    resourceFlags = ResourceSet
      ( StageFlags Vulkan.VK_SHADER_STAGE_VERTEX_BIT )
      InputResource
      InputResource

    initialResourceSet :: ResourceSet 1 Pre
    initialResourceSet = ResourceSet
      ( UniformBuffer initialMVP )
      ( VertexBuffer icosahedronVerts   )
      ( IndexBuffer  icosahedronIndices )

  ( descriptorSetLayout, descriptorSets, resources ) <-
      initialiseResources physicalDevice device resourceFlags initialResourceSet

  -------------------------------------------
  -- Create a command buffer and record the commands into it.


  commandPool <- logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex
  queue       <- getQueue device 0

  let pipelineInfo = VkPipelineInfo extent Vulkan.VK_SAMPLE_COUNT_1_BIT

  ( graphicsPipeline, pipelineLayout )
    <- createGraphicsPipeline device renderPass pipelineInfo descriptorSetLayout shaderPipeline

  commandBuffer <- allocateCommandBuffer device commandPool

  beginCommandBuffer commandBuffer

  cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues extent

  liftIO $
    Foreign.Marshal.withArray [ inputBufferObject $ vertexBuffer resources ] $ \buffers ->
    Foreign.Marshal.withArray [ 0 ] $ \offsets ->
    Vulkan.vkCmdBindVertexBuffers commandBuffer 0 1 buffers offsets

  liftIO $
    Vulkan.vkCmdBindIndexBuffer
      commandBuffer
      ( inputBufferObject $ indexBuffer resources )
      0
      Vulkan.VK_INDEX_TYPE_UINT32

  liftIO $ do
    Vulkan.vkCmdBindPipeline
      commandBuffer
      Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
      graphicsPipeline

    Foreign.Marshal.withArray [ descriptorSets `V.index` 0 ] $ \descriptorSetsPtr ->
      Vulkan.vkCmdBindDescriptorSets
        commandBuffer
        Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        pipelineLayout
        0 -- no offset
        1 -- unique descriptor set
        descriptorSetsPtr
        0 -- no dynamic offset
        Vulkan.vkNullPtr

    Vulkan.vkCmdDrawIndexed
      commandBuffer
      ( fromIntegral ( length icosahedronIndices ) )
      1 -- instance count
      0 -- offset into index buffer
      0 -- offset into vertex buffer
      0 -- first instance ID

  cmdEndRenderPass commandBuffer

  cmdTakeScreenshot
    ( Vulkan.VK_ZERO_FLAGS, Vulkan.VK_ZERO_FLAGS ) -- argument ignored in this situation
    commandBuffer extent3D
    ( colorImage,
      ( Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      , Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      )
    )
    screenshotImage

  endCommandBuffer commandBuffer

  fence <- createFence device

  submitCommandBuffer
    queue
    commandBuffer
    []
    []
    ( Just fence )

  liftIO $ waitForFences device (WaitAll [fence])

  writeScreenshotData shortName device extent screenshotImageMemory

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
