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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module FIR.Examples.Texture.Application (texture) where

-- base
import Control.Monad
  ( (>=>), when )
import Data.Bits
  ( (.|.) )
import Data.Coerce
  ( coerce )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import Data.Word
  ( Word8, Word32 )
import qualified Foreign.Marshal
import GHC.Generics
  ( Generic )

-- bytestring
import qualified Data.ByteString as ByteString

-- filepath
import System.FilePath
  ( (</>) )

-- JuicyPixels
import Codec.Picture.Types
  ( Image(..), DynamicImage(ImageRGBA8) )
import Codec.Picture.Png
  ( decodePng )

-- lens
import Control.Lens
  ( use, assign )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- sdl2
import qualified SDL
import qualified SDL.Event

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack, unpack )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector
import qualified Data.Vector.Storable as Vector
  ( unsafeWith )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( fromTuple, zip, zip3, head, index )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan                as Vulkan
import qualified Graphics.Vulkan.Core_1_0       as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct(..)
  , ModuleRequirements
  )
import Math.Linear

-- fir-examples
import FIR.Examples.Common
import FIR.Examples.Paths
import FIR.Examples.Reload
import FIR.Examples.RenderState
import FIR.Examples.Texture.Shaders
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

appName :: IsString a => a
appName = "fir-examples - Texture"
shortName :: String
shortName = "texture" -- name for screenshots

type VertexData = Struct VertexInput

data ResourceSet i st
  = ResourceSet
    { uboResource  :: UniformBuffer (M 4 4 Float) i st
    , sampledImage :: SampledImage                i st
    , vertexBuffer :: VertexBuffer  VertexData    i st
    , indexBuffer  :: IndexBuffer   Word32        i st
    }
  deriving Generic

cubeVerts :: [ Struct VertexInput ]
cubeVerts =
  [ V3 m m m :& V3 1 1 1 :& V2 0 0 :& End
  , V3 m m p :& V3 1 1 0 :& V2 1 0 :& End
  , V3 m p m :& V3 1 0 1 :& V2 0 1 :& End
  , V3 m p p :& V3 1 0 0 :& V2 1 1 :& End
  , V3 p m m :& V3 0 1 1 :& V2 1 0 :& End
  , V3 p m p :& V3 0 1 0 :& V2 0 0 :& End
  , V3 p p m :& V3 0 0 1 :& V2 1 1 :& End
  , V3 p p p :& V3 0 0 0 :& V2 0 1 :& End
  ]
    where
      p, m :: Float
      p = 1
      m = (-1)

cubeIndices :: [ Word32 ]
cubeIndices
  = [ 0, 1, 2
    , 1, 3, 2
    , 0, 4, 1
    , 1, 4, 5
    , 0, 2, 4
    , 2, 6, 4
    , 5, 4, 7
    , 7, 4, 6
    , 7, 2, 3
    , 7, 6, 2
    , 1, 5, 3
    , 3, 5, 7
    ]

nbIndices :: Word32
nbIndices = fromIntegral $ length cubeIndices

initialMVP :: M 4 4 Float
initialMVP = modelViewProjection initialObserver Nothing

clearValues :: [ Vulkan.VkClearValue ]
clearValues =
  [ tealClear
  , Vulkan.createVk ( Vulkan.set @"depthStencil" depthStencilClear )
  ]
  where
    teal :: Vulkan.VkClearColorValue
    teal =
      Vulkan.createVk
        (  Vulkan.setAt @"float32" @0 0.1
        &* Vulkan.setAt @"float32" @1 0.5
        &* Vulkan.setAt @"float32" @2 0.7
        &* Vulkan.setAt @"float32" @3 1
        )

    tealClear :: Vulkan.VkClearValue
    tealClear = Vulkan.createVk ( Vulkan.set @"color" teal )

    depthStencilClear :: Vulkan.VkClearDepthStencilValue
    depthStencilClear = Vulkan.createVk
      ( Vulkan.set @"depth" 1 &* Vulkan.set @"stencil" 0 )

----------------------------------------------------------------------------
-- Application.

texture :: IO ()
texture = runVulkan initialState do

  -------------------------------------------
  -- Obtain requirements from shaders.

  ( reqs :: ModuleRequirements ) <-
    case shaderCompilationResult of
      Left  err  -> error $ "Shader compilation was unsuccessful:\n" <> ShortText.unpack err
      Right reqs -> do
        logInfo ( "Shaders were successfully compiled.\nShader directory:\n" <> ShortText.pack shaderDir )
        pure reqs

  -------------------------------------------
  -- Initialise window and Vulkan context.

  ( window, windowExtensions ) <-
    initialiseWindow
      WindowInfo
        { width      = 1920
        , height     = 1080
        , windowName = appName
        , mouseMode  = SDL.RelativeLocation
        }

  features <- liftIO ( requiredFeatures reqs )
  let
    surfaceInfo =
      SurfaceInfo
        { surfaceWindow = window
        , preferredFormat =
            VkSurfaceFormatKHR
              Vulkan.VK_FORMAT_B8G8R8A8_UNORM
              Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
        , surfaceUsage =
            [ Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT
            , Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            ]
        }

  VulkanContext{..} <-
    initialiseContext @WithSwapchain appName windowExtensions
      RenderInfo
        { features
        , queueType   = Vulkan.VK_QUEUE_GRAPHICS_BIT
        , surfaceInfo = surfaceInfo
        }

  withSwapchainInfo aSwapchainInfo \ ( swapchainInfo@(SwapchainInfo {..}) :: SwapchainInfo numImages ) -> do

    let
      width, height :: Num a => a
      width  = fromIntegral $ Vulkan.getField @"width"  swapchainExtent
      height = fromIntegral $ Vulkan.getField @"height" swapchainExtent

      extent3D :: Vulkan.VkExtent3D
      extent3D
        = Vulkan.createVk
            (  Vulkan.set @"width"  width
            &* Vulkan.set @"height" height
            &* Vulkan.set @"depth"  1
            )

      logoExtent3D :: Vulkan.VkExtent3D
      logoExtent3D
        = Vulkan.createVk
            (  Vulkan.set @"width"  1024
            &* Vulkan.set @"height" 1024
            &* Vulkan.set @"depth"  1
            )

      colFmt, depthFmt :: Vulkan.VkFormat
      colFmt   = Vulkan.getField @"format" surfaceFormat
      depthFmt = Vulkan.VK_FORMAT_D32_SFLOAT

    -------------------------------------------
    -- Create images.

    let

    -- Handle the Haskell logo texture.

      logoStagingImageInfo, logoImageInfo, depthImageInfo :: ImageInfo
      logoStagingImageInfo =
        ( Default2DImageInfo logoExtent3D Vulkan.VK_FORMAT_R8G8B8A8_UNORM
            Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT
        ) { imageLayout = Vulkan.VK_IMAGE_LAYOUT_PREINITIALIZED
          , imageTiling = Vulkan.VK_IMAGE_TILING_LINEAR
          }
      logoImageInfo =
        Default2DImageInfo logoExtent3D Vulkan.VK_FORMAT_R8G8B8A8_UNORM
         (   Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
         .|. Vulkan.VK_IMAGE_USAGE_SAMPLED_BIT
         )
      depthImageInfo =
        Default2DImageInfo extent3D depthFmt
          Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT

    (logoStagingImage, logoStagingImageMemory) <-
      createImage physicalDevice device
        logoStagingImageInfo
        [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
        , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
        ]

    logInfo "Loading logo."

    mbLogo <- liftIO $ decodePng <$> ByteString.readFile ( assetDir </> "haskell_logo.png" )
    let
      logo = case mbLogo of
        Left err -> error $ "Could not load and decode logo: " ++ err
        Right (ImageRGBA8 img) -> img
        Right _ -> error "Logo not in the expected RGBA8 format."

    logoStagingPtr :: Vulkan.Ptr Word8
      <- coerce <$> allocaAndPeek
                  ( Vulkan.vkMapMemory device logoStagingImageMemory
                      0 (4 * 1024 * 1024) Vulkan.VK_ZERO_FLAGS
                    >=> throwVkResult
                  )

    liftIO $ Vector.unsafeWith (imageData logo) \logosrc ->
      Foreign.Marshal.copyBytes logoStagingPtr logosrc (4 * 1024 * 1024)

    liftIO $ Vulkan.vkUnmapMemory device logoStagingImageMemory

    (logoImage, _) <-
      createImage physicalDevice device
        logoImageInfo
        [ ]

    logoImageView <-
      createImageView
        device logoImage
        Vulkan.VK_IMAGE_VIEW_TYPE_2D
        Vulkan.VK_FORMAT_R8G8B8A8_UNORM
        Vulkan.VK_IMAGE_ASPECT_COLOR_BIT

    logoSampler <- createSampler device

    commandPool <- logDebug "Creating command pool" *> createCommandPool device queueFamilyIndex
    queue       <- getQueue device 0

    ( _copyCommandBufferKey, logoCopyCommandBuffer ) <- allocateCommandBuffer device commandPool

    beginCommandBuffer logoCopyCommandBuffer

    cmdTransitionImageLayout logoCopyCommandBuffer logoStagingImage
      Vulkan.VK_IMAGE_LAYOUT_PREINITIALIZED
      Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      ( Vulkan.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.VK_ZERO_FLAGS )
      ( Vulkan.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.VK_ZERO_FLAGS )
    cmdTransitionImageLayout logoCopyCommandBuffer logoImage
      Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
      Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      ( Vulkan.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.VK_ZERO_FLAGS )
      ( Vulkan.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.VK_ZERO_FLAGS )

    let
      noOffset :: Vulkan.VkOffset3D
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
      logoImageCopy :: Vulkan.VkImageCopy
      logoImageCopy
        = Vulkan.createVk
          (  Vulkan.set @"srcSubresource" layers
          &* Vulkan.set @"srcOffset"      noOffset
          &* Vulkan.set @"dstSubresource" layers
          &* Vulkan.set @"dstOffset"      noOffset
          &* Vulkan.set @"extent"         logoExtent3D
          )

    liftIO $ Vulkan.vkCmdCopyImage logoCopyCommandBuffer
      logoStagingImage
      Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      logoImage
      Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      1
      ( Vulkan.unsafePtr logoImageCopy )

    cmdTransitionImageLayout logoCopyCommandBuffer logoImage
      Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      ( Vulkan.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.VK_ZERO_FLAGS )
      ( Vulkan.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.VK_ZERO_FLAGS )

    endCommandBuffer logoCopyCommandBuffer

    submitCommandBuffer
      queue
      logoCopyCommandBuffer
      [] [] Nothing

    --release copyCommandBufferKey

    -- Create the framebuffer attachments.

    renderPass <- logDebug "Creating a render pass" *>
      simpleRenderPass device
        ( noAttachments
          { colorAttachments = [ presentableColorAttachmentDescription colFmt ]
          , mbDepthStencilAttachment = Just (depthAttachmentDescription depthFmt)
          }
        )

    framebuffersWithAttachments
      <- logDebug "Creating frame buffers"
        *> ( for swapchainImages $ \swapchainImage -> do

            colorImageView
              <- createImageView
                    device swapchainImage
                    Vulkan.VK_IMAGE_VIEW_TYPE_2D
                    colFmt
                    Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
            (depthImage, _)
              <- createImage physicalDevice device
                    depthImageInfo
                    [ ]
            depthImageView
              <- createImageView device depthImage
                    Vulkan.VK_IMAGE_VIEW_TYPE_2D
                    depthFmt
                    Vulkan.VK_IMAGE_ASPECT_DEPTH_BIT
            let attachments =
                  V.fromTuple
                    ( (swapchainImage, colorImageView)
                    , (depthImage    , depthImageView)
                    )
            framebuffer <- createFramebuffer device renderPass swapchainExtent (fmap snd attachments)
            pure (framebuffer, attachments)
         )

    screenshotImagesAndMemories <-
      for swapchainImages \ _ ->
        createScreenshotImage physicalDevice device
          ( screenshotImageInfo extent3D colFmt )

    -------------------------------------------
    -- Manage resources.

    let

      resourceFlags :: ResourceSet numImages Named
      resourceFlags = ResourceSet
        ( StageFlags Vulkan.VK_SHADER_STAGE_VERTEX_BIT   )
        ( StageFlags Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT )
        InputResource
        InputResource

      initialResourceSet :: ResourceSet numImages Pre
      initialResourceSet = ResourceSet
        ( UniformBuffer initialMVP )
        ( SampledImage logoSampler logoImageView )
        ( VertexBuffer cubeVerts   )
        ( IndexBuffer  cubeIndices )

    PostInitialisationResult
      descriptorSetLayout descriptorSets cmdBindBuffers resources
       <- initialiseResources physicalDevice device resourceFlags initialResourceSet

    -------------------------------------------
    -- Create command buffers and record commands into them.

    nextImageSem <- createSemaphore device
    submitted    <- createSemaphore device

    pipelineLayout <- logDebug "Creating pipeline layout" *> createPipelineLayout device descriptorSetLayout
    let pipelineInfo = VkPipelineInfo swapchainExtent Vulkan.VK_SAMPLE_COUNT_1_BIT pipelineLayout

    shaders <- logDebug "Loading shaders" *> traverse (\path -> (path, ) <$> loadShader device path) shaderPipeline

    let
      recordCommandBuffers pipe =
        for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( descriptorSet, (framebuffer, attachments ) ) ->
          recordSimpleIndexedDrawCall
            device commandPool framebuffer (renderPass, clearValues)
            descriptorSet cmdBindBuffers
            ( fst $ V.head attachments, swapchainExtent )
            Nothing
            nbIndices
            pipelineLayout pipe
      recordScreenshotCommandBuffers pipe =
        for (V.zip3 descriptorSets framebuffersWithAttachments screenshotImagesAndMemories)
          \ ( descriptorSet, (framebuffer, attachments), (screenshotImage, _) ) ->
            recordSimpleIndexedDrawCall
              device commandPool framebuffer (renderPass, clearValues)
              descriptorSet cmdBindBuffers
              ( fst $ V.head attachments, swapchainExtent )
              ( Just ( screenshotImage, extent3D ) )
              nbIndices
              pipelineLayout pipe

      recordAllCommandsFromShaders = record2CommandBuffersFromShaders
        ( createGraphicsPipeline device renderPass pipelineInfo )
        recordCommandBuffers
        recordScreenshotCommandBuffers

    -- launch shader reload watcher, which writes command buffers to use to a TVar
    resourcesTVar <- statelessly $ shaderReloadWatcher device shaders recordAllCommandsFromShaders

    mainLoop do

      ----------------
      -- shader reloading

      ( updatedCommands, updatedScreenshotCommands )
        <- statelessly ( snd <$> readTVarWithCleanup resourcesTVar )

      ----------------
      -- input

      inputEvents <- map SDL.Event.eventPayload <$> SDL.pollEvents
      prevInput <- use _input
      let newInput = foldl onSDLInput prevInput inputEvents
      let action = interpretInput newInput
      assign _input ( newInput { mouseRel = pure 0, keysPressed = [] } )

      ----------------
      -- simulation

      oldObserver <- use _observer
      let (observer, orientation) = oldObserver `move` action
      assign _observer observer

      let mvp = modelViewProjection observer (Just orientation)

      when ( locate action )
        ( liftIO $ putStrLn ( show observer ) )

      -- update UBO
      let
        BufferResource _ updateUBO = uboResource resources

      liftIO ( updateUBO mvp )

      ----------------
      -- rendering

      nextImageIndex <- acquireNextImage device swapchainInfo nextImageSem

      let
        commandBuffer
          | takeScreenshot action = updatedScreenshotCommands `V.index` nextImageIndex
          | otherwise             = updatedCommands           `V.index` nextImageIndex

      submitCommandBuffer
        queue
        commandBuffer
        [(nextImageSem, Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
        [submitted]
        Nothing

      present queue swapchain nextImageIndex [submitted]

      liftIO ( Vulkan.vkQueueWaitIdle queue )
        >>= throwVkResult

      when ( takeScreenshot action ) $
        writeScreenshotData shortName device swapchainExtent
          ( snd ( screenshotImagesAndMemories `V.index` nextImageIndex ) )

      ----------------

      pure ( shouldQuit action )
