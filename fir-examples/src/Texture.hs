{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Texture (texture) where

-- base
import Control.Monad
  ( (>=>), when )
import Control.Monad.IO.Class
  ( liftIO )
import Data.Bits
import Data.Coerce
  ( coerce )
import Data.Traversable
  ( for )
import Data.Word
  ( Word8, Word32 )
import qualified Foreign
import qualified Foreign.C
import qualified Foreign.Marshal

-- bytestring
import qualified Data.ByteString as ByteString

-- JuicyPixels
import Codec.Picture.Types
  ( Image(..), DynamicImage(ImageRGBA8), PixelRGBA8(..) )
import Codec.Picture.Png
  ( writePng, decodePng )

-- lens
import Control.Lens
  ( use, assign )

-- managed
import Control.Monad.Managed
  ( MonadManaged, runManaged )

-- sdl2
import qualified SDL
import qualified SDL.Event

-- text-utf8
import "text-utf8" Data.Text
  ( Text )
import qualified "text-utf8" Data.Text as Text

-- transformers
import Control.Monad.Trans.State.Lazy
  ( evalStateT )

-- vector
import qualified Data.Vector.Storable as Vector

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct(..)
  , Poke(poke)
  , Layout(Extended)
  )
import Math.Linear

-- fir-examples
import Shaders.Texture
import Vulkan.Backend
import Vulkan.Buffer
import Vulkan.Monad
import Simulation.Observer
import Vulkan.Pipeline
import Vulkan.SDL

----------------------------------------------------------------------------

shaderCompilationResult :: Either Text ()
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Vertex shader"  , compileVertexShader  )
        , ("Fragment shader", compileFragmentShader)
        ]
     )

texture :: IO ()
texture = ( runManaged . ( `evalStateT` initialState ) ) do

  case shaderCompilationResult of
    Left  err -> logMsg ( "Shader compilation was unsuccessful:\n" <> Text.unpack err )
    Right _   -> logMsg ( "Shaders were succesfully compiled." )

  enableSDLLogging
  initializeSDL SDL.RelativeLocation -- relative mouse location
  window           <- logMsg "Creating SDL window"           *> createWindow "fir-examples - Texture"
  setWindowIcon window "assets/fir_logo.png"

  neededExtensions <- logMsg "Loading needed extensions"     *> getNeededExtensions window
  extensionNames <- traverse ( liftIO . Foreign.C.peekCString ) neededExtensions
  logMsg $ "Needed instance extensions are: " ++ show extensionNames

  vulkanInstance   <- logMsg "Creating Vulkan instance"      *> createVulkanInstance neededExtensions
  physicalDevice   <- logMsg "Creating physical device"      *> createPhysicalDevice vulkanInstance
  queueFamilyIndex <- logMsg "Finding suitable queue family"
      *> findQueueFamilyIndex physicalDevice [Vulkan.VK_QUEUE_GRAPHICS_BIT]


  device      <- logMsg "Creating logical device" *> createLogicalDevice  physicalDevice queueFamilyIndex Nothing
  commandPool <- logMsg "Creating command pool"   *> createCommandPool    device queueFamilyIndex
  queue       <- getQueue device 0
  surface     <- logMsg "Creating SDL surface"    *> createSurface window vulkanInstance

  assertSurfacePresentable physicalDevice queueFamilyIndex surface

  let preferredSwapchainFormat :: Vulkan.VkSurfaceFormatKHR
      preferredSwapchainFormat
        = VkSurfaceFormatKHR
            Vulkan.VK_FORMAT_B8G8R8A8_UNORM
            Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR

      depthFmt :: Vulkan.VkFormat
      depthFmt = Vulkan.VK_FORMAT_D32_SFLOAT

  surfaceFormat@(~(VkSurfaceFormatKHR colFmt _)) <-
    logMsg "Choosing swapchain format & color space"
      *> chooseSwapchainFormat preferredSwapchainFormat physicalDevice surface

  ( swapchain, extent ) <-
    logMsg "Creating swapchain"
      *> createSwapchain
            physicalDevice device
            surface surfaceFormat
            ( Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT .|. Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT )

  let width, height :: Num a => a
      width  = fromIntegral $ Vulkan.getField @"width"  extent
      height = fromIntegral $ Vulkan.getField @"height" extent

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

      logoStagingImageInfo, logoImageInfo, screenshotImageInfo, depthImageInfo :: ImageInfo
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
      screenshotImageInfo =
        ( Default2DImageInfo extent3D colFmt
          Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
        ) { imageTiling = Vulkan.VK_IMAGE_TILING_LINEAR }  -- host visible image needs linear tiling
      depthImageInfo =
        Default2DImageInfo extent3D depthFmt
          Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT

  (logoStagingImage, logoStagingImageMemory) <-
    createImage physicalDevice device
      logoStagingImageInfo
      [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
      , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
      ]

  logMsg "Loading logo."

  mbLogo <- liftIO $ decodePng <$> ByteString.readFile "assets/haskell_logo.png"
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

  logoCopyCommandBuffer <- allocateCommandBuffer device commandPool

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


  swapchainImages <- logMsg "Getting swapchain images" *> getSwapchainImages device swapchain
  renderPass      <- logMsg "Creating a render pass"   *> createRenderPass   device colFmt depthFmt

  framebuffersWithAttachments <- logMsg "Creating frame buffers"
    *> ( for swapchainImages $ \swapchainImage -> do

          colorImageView
            <- createImageView
                  device swapchainImage
                  Vulkan.VK_IMAGE_VIEW_TYPE_2D
                  colFmt
                  Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
          (screenshotImage, screenshotImageMemory)
            <- createImage physicalDevice device
                  screenshotImageInfo
                  [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                  , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
                  ]
          (depthImage, _)
            <- createImage physicalDevice device
                  depthImageInfo
                  [ ]
          depthImageView
            <- createImageView device depthImage
                  Vulkan.VK_IMAGE_VIEW_TYPE_2D
                  depthFmt
                  Vulkan.VK_IMAGE_ASPECT_DEPTH_BIT
          let attachments = [ (swapchainImage, colorImageView)
                            , (depthImage    , depthImageView)
                            ]
          framebuffer <- createFramebuffer device renderPass extent (map snd attachments)
          pure (framebuffer, attachments, (screenshotImage, screenshotImageMemory))
       )

  let clearValues :: [ Vulkan.VkClearValue ] -- in bijection with framebuffer attachments
      clearValues = [ tealClear
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
          tealClear = Vulkan.createVk ( Vulkan.set @"color"  teal )

          depthStencilClear :: Vulkan.VkClearDepthStencilValue
          depthStencilClear = Vulkan.createVk
            ( Vulkan.set @"depth" 1 &* Vulkan.set @"stencil" 0 )

  nextImageSem <- createSemaphore device
  submitted    <- createSemaphore device

  descriptorPool      <- createDescriptorPool device
  descriptorSetLayout <- createDescriptorSetLayout device
  descriptorSet       <- allocateDescriptorSet device descriptorPool descriptorSetLayout

  ( graphicsPipeline, pipelineLayout )
    <- createGraphicsPipeline device renderPass extent descriptorSetLayout shaderPipeline

  let

    p, m :: Float
    p = 1
    m = (-1)

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

  (vertexBuffer, _) <- createVertexBuffer physicalDevice device cubeVerts

  (indexBuffer, _) <- createIndexBuffer physicalDevice device cubeIndices

  (ubo, uboPtr)
    <- createUniformBuffer
          physicalDevice
          device
          ( modelViewProjection initialObserver Nothing )

  updateDescriptorSet device descriptorSet ubo (logoImageView, logoSampler)

  let
    mkCommandBuffer
      :: MonadManaged m
      => Vulkan.VkFramebuffer
      -> [(Vulkan.VkImage, Vulkan.VkImageView)]
      -> Maybe (Vulkan.VkImage, Vulkan.VkDeviceMemory)
      -> m Vulkan.VkCommandBuffer
    mkCommandBuffer framebuffer attachments mbScreenshot
      = do

        commandBuffer <-
          allocateCommandBuffer device commandPool

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
            ( fromIntegral ( length cubeIndices ) )
            1
            0
            0
            0

        cmdEndRenderPass commandBuffer

        -- if taking a screenshot, copy swapchain color image onto screenshot image
        case mbScreenshot of
          Nothing -> pure ()
          Just ( screenshotImage, _)
            -> let
                  imageCopy :: Vulkan.VkImageCopy
                  imageCopy
                    = Vulkan.createVk
                      (  Vulkan.set @"srcSubresource" layers
                      &* Vulkan.set @"srcOffset"      noOffset
                      &* Vulkan.set @"dstSubresource" layers
                      &* Vulkan.set @"dstOffset"      noOffset
                      &* Vulkan.set @"extent"         extent3D
                      )
               in
                do -- transition screenshot image layout to TRANSFER_DST so that swapchain image can be copied onto it
                  cmdTransitionImageLayout commandBuffer screenshotImage
                    Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                    ( Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT )
                    ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS)
                  -- perform the copy
                  liftIO $ Vulkan.vkCmdCopyImage commandBuffer
                    ( fst $ attachments !! 0 ) -- (swapchain) color attachment
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                    screenshotImage
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                    1
                    ( Vulkan.unsafePtr imageCopy )

        -- now change image layouts:
        --   - make swapchain image available for presentation
        --   - make screenshot image available for memory mapping (to write to disk)

        let
          subresourceRange :: Vulkan.VkImageSubresourceRange
          subresourceRange =
            Vulkan.createVk
              (  Vulkan.set @"aspectMask"     Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
              &* Vulkan.set @"baseMipLevel"   0
              &* Vulkan.set @"levelCount"     1
              &* Vulkan.set @"baseArrayLayer" 0
              &* Vulkan.set @"layerCount"     1
              )

          swapchainImageBarrier :: Vulkan.VkImageMemoryBarrier
          swapchainImageBarrier =
            Vulkan.createVk
              (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
              &* Vulkan.set @"pNext" Vulkan.vkNullPtr
              &* Vulkan.set @"srcAccessMask" Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              &* Vulkan.set @"dstAccessMask" Vulkan.VK_ZERO_FLAGS
              &* Vulkan.set @"oldLayout"     Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
              &* Vulkan.set @"newLayout"     Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
              &* Vulkan.set @"image"         ( fst $ attachments !! 0 ) -- swapchain image
              &* Vulkan.set @"subresourceRange"    subresourceRange
              &* Vulkan.set @"srcQueueFamilyIndex" Vulkan.VK_QUEUE_FAMILY_IGNORED
              &* Vulkan.set @"dstQueueFamilyIndex" Vulkan.VK_QUEUE_FAMILY_IGNORED
              )

          barriers = case mbScreenshot of
            Nothing -> [ swapchainImageBarrier ]
            Just ( screenshotImage, _ )
              -> let
                    screenshotImageBarrier :: Vulkan.VkImageMemoryBarrier
                    screenshotImageBarrier =
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
                  in [ swapchainImageBarrier, screenshotImageBarrier ]

        cmdPipelineBarrier
          commandBuffer
          Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
          []
          []
          barriers

        endCommandBuffer commandBuffer

        pure commandBuffer


  commandBuffers <-
    for framebuffersWithAttachments $ \(framebuffer, attachments, _ ) ->
      mkCommandBuffer framebuffer attachments Nothing

  screenshotCommandBuffers <-
    for framebuffersWithAttachments $ \(framebuffer, attachments, screenshotImageAndMemory) ->
      mkCommandBuffer framebuffer attachments (Just screenshotImageAndMemory)

  mainLoop do

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

    -- update MVP
    liftIO ( poke @_ @Extended uboPtr mvp )

    ----------------
    -- rendering

    nextImageIndex <- acquireNextImage device swapchain nextImageSem

    let
      commandBuffer
        | takeScreenshot action = screenshotCommandBuffers !! nextImageIndex
        | otherwise             = commandBuffers           !! nextImageIndex

    submitCommandBuffer
      queue
      commandBuffer
      [(nextImageSem, Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
      [submitted]
      Nothing

    present queue swapchain nextImageIndex [submitted]

    -- if taking a screenshot, map the image memory, read from it, and write to disk
    when ( takeScreenshot action ) $ liftIO do

        let screenshotImageMemory :: Vulkan.VkDeviceMemory
            ( _, _, (_, screenshotImageMemory) )
              = framebuffersWithAttachments !! nextImageIndex

        memPtr :: Vulkan.Ptr Word8
          <- coerce <$> allocaAndPeek
                ( Vulkan.vkMapMemory device screenshotImageMemory
                    0 maxBound Vulkan.VK_ZERO_FLAGS
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

        writePng "screenshots/texture.png" imageData

        Vulkan.vkUnmapMemory device screenshotImageMemory

    liftIO ( Vulkan.vkQueueWaitIdle queue )
      >>= throwVkResult

    ----------------

    pure ( shouldQuit action )





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
        &* Vulkan.set @"srcStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"srcAccessMask" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"dstStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"dstAccessMask"
              (    Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
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
        &* Vulkan.set @"stageFlags"
              Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        &* Vulkan.set @"pImmutableSamplers" Vulkan.VK_NULL
        )

    logoBinding =
      Vulkan.createVk
        (  Vulkan.set @"binding" 1
        &* Vulkan.set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        &* Vulkan.set @"descriptorCount" 1
        &* Vulkan.set @"stageFlags"
              Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
        &* Vulkan.set @"pImmutableSamplers" Vulkan.VK_NULL
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.setListCountAndRef @"bindingCount" @"pBindings" [ binding, logoBinding ]
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

    poolSize1 =
      Vulkan.createVk
      (  Vulkan.set @"type" Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      &* Vulkan.set @"descriptorCount" 1
      )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
        &* Vulkan.setListCountAndRef @"poolSizeCount" @"pPoolSizes" [ poolSize0, poolSize1 ]
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
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorSet
  -> Vulkan.VkBuffer
  -> (Vulkan.VkImageView, Vulkan.VkSampler)
  -> m ()
updateDescriptorSet device descriptorSet buffer (imageView, sampler) =
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
    imageInfo =
      Vulkan.createVk
        (  Vulkan.set @"sampler"     sampler
        &* Vulkan.set @"imageView"   imageView
        &* Vulkan.set @"imageLayout" Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        )
    writeUpdate1 =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"dstSet" descriptorSet
        &* Vulkan.set @"dstBinding" 1
        &* Vulkan.set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        &* Vulkan.set @"descriptorCount" 1
        &* Vulkan.set @"pTexelBufferView" Vulkan.VK_NULL
        &* Vulkan.setVkRef @"pImageInfo" imageInfo
        &* Vulkan.set @"dstArrayElement" 0
        &* Vulkan.setListRef @"pBufferInfo" [ ]
        )
  in liftIO $
       Foreign.Marshal.withArray [ writeUpdate0, writeUpdate1 ] $ \writeUpdatesPtr ->
         Vulkan.vkUpdateDescriptorSets device 2 writeUpdatesPtr 0 Vulkan.vkNullPtr
