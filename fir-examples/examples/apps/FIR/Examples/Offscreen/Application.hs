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

module FIR.Examples.Offscreen.Application ( offscreen ) where

-- base
import Data.Bits
  ( (.|.) )
import Data.Word
  ( Word32 )
import GHC.Generics
  ( Generic )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack, unpack )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( head )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan.Core_1_0       as Vulkan
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
import FIR.Examples.Common
import FIR.Examples.Offscreen.Shaders
import FIR.Examples.Paths
import FIR.Examples.RenderState
import Vulkan.Attachment
import Vulkan.Backend
import Vulkan.Context
import Vulkan.Features
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.Resource
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- Shaders and resources.

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
    where
      phi :: Float
      phi = 0.5 + sqrt 1.25

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

nbIndices :: Word32
nbIndices = fromIntegral $ length icosahedronIndices

initialResourceSet :: ResourceSet 1 Pre
initialResourceSet = ResourceSet
  ( UniformBuffer initialMVP )
  ( VertexBuffer icosahedronVerts   )
  ( IndexBuffer  icosahedronIndices )
    where
      initialMVP :: M 4 4 Float
      initialMVP = modelViewProjection initialObserver Nothing


----------------------------------------------------------------------------
-- Application.

offscreen :: IO ()
offscreen = runVulkan () do

  -------------------------------------------
  -- Obtain requirements from shaders.

  ( reqs :: ModuleRequirements ) <-
    case shaderCompilationResult of
      Left  err  -> error $ "Shader compilation was unsuccessful:\n" <> ShortText.unpack err
      Right reqs -> do
        logInfo ( "Shaders were successfully compiled.\nShader directory:\n" <> ShortText.pack shaderDir )
        pure reqs

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

  renderPass <- logDebug "Creating a render pass" *>
    simpleRenderPass device
      ( noAttachments
        { colorAttachments = [ presentableColorAttachmentDescription colFmt ]
        , mbDepthStencilAttachment = Just (depthAttachmentDescription depthFmt)
        }
      )
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

    resourceFlags :: ResourceSet 1 Named
    resourceFlags = ResourceSet
      ( StageFlags Vulkan.VK_SHADER_STAGE_VERTEX_BIT )
      InputResource
      InputResource

  PostInitialisationResult
    descriptorSetLayout descriptorSets cmdBindBuffers _
      <- initialiseResources physicalDevice device resourceFlags initialResourceSet

  -------------------------------------------
  -- Create command buffers and record commands into them.


  commandPool <- logDebug "Creating command pool" *> createCommandPool device queueFamilyIndex
  queue       <- getQueue device 0

  pipelineLayout <- createPipelineLayout device descriptorSetLayout
  let pipelineInfo = VkPipelineInfo extent Vulkan.VK_SAMPLE_COUNT_1_BIT pipelineLayout

  shaders <- traverse (loadShader device) shaderPipeline

  ( _, pipeline )
    <- createGraphicsPipeline device renderPass pipelineInfo (fmap snd shaders)

  ( _ , commandBuffer )
    <- recordSimpleIndexedDrawCall
          device commandPool framebuffer (renderPass, clearValues)
          ( V.head descriptorSets ) cmdBindBuffers
          ( colorImage, extent )
          ( Just ( screenshotImage, extent3D ) )
          nbIndices
          pipelineLayout pipeline

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
