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

module Example1.Main ( example1 ) where

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
  ( Word8 )
import qualified Foreign
import qualified Foreign.C
import qualified Foreign.Marshal

-- JuicyPixels
import Codec.Picture.Types
  ( Image(..), PixelRGBA8(..) )
import Codec.Picture.Png
  ( writePng )

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
  ( runCompilationsTH )
import Math.Linear
  ( V, pattern V2, pattern V3
  , (^+^)
  )

-- fir-examples
import Example1.Shaders
import Vulkan.Backend
import Vulkan.Buffer
import Vulkan.Monad
import Vulkan.Observer
import Vulkan.SDL


----------------------------------------------------------------------------

shaderCompilationResult :: Either Text Text
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Vertex shader"  , compileVertexShader  )
        , ("Fragment shader", compileFragmentShader)
        ]
     )

example1 :: IO ()
example1 = ( runManaged . ( `evalStateT` initialState ) ) do

  case shaderCompilationResult of
    Left  err -> logMsg ( "Shader compilation was unsuccessful:\n" <> Text.unpack err)
    Right _   -> logMsg ( "Shaders were succesfully compiled." )

  enableSDLLogging
  initializeSDL
  window           <- logMsg "Creating SDL window"           *> createWindow "fir-examples"

  neededExtensions <- logMsg "Loading needed extensions"     *> getNeededExtensions window
  extensionNames <- traverse ( liftIO . Foreign.C.peekCString ) neededExtensions
  logMsg $ "Needed instance extensions are: " ++ show extensionNames

  vulkanInstance   <- logMsg "Creating Vulkan instance"      *> createVulkanInstance neededExtensions
  physicalDevice   <- logMsg "Creating physical device"      *> createPhysicalDevice vulkanInstance
  queueFamilyIndex <- logMsg "Finding suitable queue family" *> findQueueFamilyIndex physicalDevice

  let features :: Vulkan.Ptr Vulkan.VkPhysicalDeviceFeatures
      features = Vulkan.vkNullPtr
  device           <- logMsg "Creating logical device"       *> createLogicalDevice  physicalDevice queueFamilyIndex features
  surface          <- logMsg "Creating SDL surface"          *> createSurface window vulkanInstance

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
                  Vulkan.VK_IMAGE_TYPE_2D
                  extent3D
                  colFmt
                  Vulkan.VK_IMAGE_TILING_LINEAR -- host visible image needs linear tiling
                  (     Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT
                    .|. Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
                  )
                  [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                  , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
                  ]
          (depthImage, _)
            <- createImage physicalDevice device
                  Vulkan.VK_IMAGE_TYPE_2D
                  extent3D
                  depthFmt
                  Vulkan.VK_IMAGE_TILING_OPTIMAL
                  Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
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

  commandPool <- logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex
  queue       <- getQueue device 0

  nextImageSem <- createSemaphore device
  submitted    <- createSemaphore device

  descriptorPool <- createDescriptorPool device

  descriptorSetLayout <- createDescriptorSetLayout device
  descriptorSet       <- allocateDescriptorSet device descriptorPool descriptorSetLayout

  ( graphicsPipeline, pipelineLayout )
    <- createGraphicsPipeline device renderPass extent descriptorSetLayout

  let

    phi :: Foreign.C.CFloat
    phi = 0.5 + sqrt 1.25

    offset :: V 3 Foreign.C.CFloat
    offset = V3 0 0 (-5)

    icosahedronVerts :: [ V 2 ( V 3 Foreign.C.CFloat) ]
    icosahedronVerts = fmap ( \(V2 p c) -> V2 (p ^+^ offset) c )
      [ V2 ( V3    0     1    phi  ) ( V3 0    1    0    )
      , V2 ( V3    0   (-1)   phi  ) ( V3 0    0.75 0.25 )
      , V2 ( V3    0     1  (-phi) ) ( V3 0    0.25 0.75 )
      , V2 ( V3    0   (-1) (-phi) ) ( V3 0    0    1    )
      , V2 ( V3    1    phi    0   ) ( V3 1    0    0    )
      , V2 ( V3  (-1)   phi    0   ) ( V3 0.75 0.25 0    )
      , V2 ( V3    1  (-phi)   0   ) ( V3 0.25 0.75 0    )
      , V2 ( V3  (-1) (-phi)   0   ) ( V3 0    1    0    )
      , V2 ( V3   phi    0     1   ) ( V3 1    0    0    )
      , V2 ( V3   phi    0   (-1)  ) ( V3 0.75 0    0.25 )
      , V2 ( V3 (-phi)   0     1   ) ( V3 0.25 0    0.75 )
      , V2 ( V3 (-phi)   0   (-1)  ) ( V3 0    0    1    )
      ]

    icosahedronIndices :: [ Foreign.Word32 ]
    icosahedronIndices
      = [ 0,  8,  1
        , 0,  1, 10
        , 0,  5,  4
        , 0,  4,  8
        , 0, 10,  5
        , 1,  6,  7
        , 1,  8,  6
        , 1,  7, 10
        , 2,  3,  9
        , 2, 11,  3
        , 2,  4,  5
        , 2,  9,  4
        , 2,  5, 11
        , 3,  7,  6
        , 3,  6,  9
        , 3, 11,  7
        , 4,  9,  8
        , 5, 10, 11
        , 6,  8,  9
        , 7, 11, 10
        ]

  (vertexBuffer, _) <- createVertexBuffer physicalDevice device icosahedronVerts

  (indexBuffer, _) <- createIndexBuffer physicalDevice device icosahedronIndices

  (mvpUniformBuffer, mvpUniformPtr)
    <- createUniformBuffer
          physicalDevice
          device
          ( modelViewProjection initialObserver (pure 0) )

  updateDescriptorSet device descriptorSet mvpUniformBuffer

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

        beginRenderPass commandBuffer renderPass framebuffer clearValues extent

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

        endRenderPass commandBuffer

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

        -- if taking a screenshot, copy swapchain color image onto screenshot image
        case mbScreenshot of
          Nothing -> pure ()
          Just ( screenshotImage, _)
            -> liftIO do
                -- transition screenshot image layout to TRANSFER_DST so that swapchain image can be copied onto it
                let
                  screenshotImageBarrier :: Vulkan.VkImageMemoryBarrier
                  screenshotImageBarrier =
                    Vulkan.createVk
                      (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
                      &* Vulkan.set @"pNext" Vulkan.vkNullPtr
                      &* Vulkan.set @"srcAccessMask" Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                      &* Vulkan.set @"dstAccessMask" 0
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
                -- perform the copy
                Vulkan.vkCmdCopyImage commandBuffer
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

          swapchainImageBarrier :: Vulkan.VkImageMemoryBarrier
          swapchainImageBarrier =
            Vulkan.createVk
              (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
              &* Vulkan.set @"pNext" Vulkan.vkNullPtr
              &* Vulkan.set @"srcAccessMask" Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              &* Vulkan.set @"dstAccessMask" 0
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
                        &* Vulkan.set @"dstAccessMask" 0
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

    let mvp = modelViewProjection observer orientation

    when ( locate action )
      ( liftIO $ putStrLn ( show observer ) )

    -- update MVP
    liftIO ( Foreign.poke mvpUniformPtr mvp )

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
      submitted

    present queue swapchain nextImageIndex [submitted]

    -- if taking a screenshot, map the image memory, read from it, and write to disk
    when ( takeScreenshot action ) $ liftIO do

        let screenshotImageMemory :: Vulkan.VkDeviceMemory
            ( _, _, (_, screenshotImageMemory) )
              = framebuffersWithAttachments !! nextImageIndex

        memPtr :: Vulkan.Ptr Word8
          <- coerce <$> allocaAndPeek
                ( Vulkan.vkMapMemory device screenshotImageMemory 0 maxBound 0
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

        writePng "screenshots/example1.png" imageData

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
        (  Vulkan.set @"flags"          0
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
        (  Vulkan.set @"flags"          0
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
        (  Vulkan.set @"flags" 0
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
        &* Vulkan.set @"dstSubpass"    0
        &* Vulkan.set @"srcStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"srcAccessMask" 0
        &* Vulkan.set @"dstStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"dstAccessMask"
              (    Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
        )

    dependency2 :: Vulkan.VkSubpassDependency
    dependency2 =
      Vulkan.createVk
        (  Vulkan.set @"srcSubpass"    0
        &* Vulkan.set @"dstSubpass"    Vulkan.VK_SUBPASS_EXTERNAL
        &* Vulkan.set @"srcStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"srcAccessMask"
              (    Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
        &* Vulkan.set @"dstStageMask"  Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        &* Vulkan.set @"dstAccessMask" 0
        )

    createInfo :: Vulkan.VkRenderPassCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" 0
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


createGraphicsPipeline
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> Vulkan.VkDescriptorSetLayout
  -> m ( Vulkan.VkPipeline, Vulkan.VkPipelineLayout )
createGraphicsPipeline device renderPass extent layout0 = do
  pipelineLayout <-
    let
      pipelineLayoutCreateInfo :: Vulkan.VkPipelineLayoutCreateInfo
      pipelineLayoutCreateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" 0
          &* Vulkan.setListCountAndRef
                @"setLayoutCount"
                @"pSetLayouts"
                [ layout0 ]
          &* Vulkan.setListCountAndRef
                @"pushConstantRangeCount"
                @"pPushConstantRanges"
                [ ]
          )

    in
    managedVulkanResource
      ( Vulkan.vkCreatePipelineLayout  device ( Vulkan.unsafePtr pipelineLayoutCreateInfo ) )
      ( Vulkan.vkDestroyPipelineLayout device )

  vertexShader   <- loadShader device vertPath

  fragmentShader <- loadShader device fragPath

  let
    rasterizationCreateInfo :: Vulkan.VkPipelineRasterizationStateCreateInfo
    rasterizationCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        &* Vulkan.set @"pNext"                   Vulkan.VK_NULL
        &* Vulkan.set @"depthClampEnable"        Vulkan.VK_FALSE
        &* Vulkan.set @"rasterizerDiscardEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"polygonMode"             Vulkan.VK_POLYGON_MODE_FILL
        &* Vulkan.set @"lineWidth"               1
        &* Vulkan.set @"depthBiasEnable"         Vulkan.VK_FALSE
        &* Vulkan.set @"depthBiasSlopeFactor"    0
        &* Vulkan.set @"depthBiasClamp"          0
        &* Vulkan.set @"depthBiasConstantFactor" 0
        &* Vulkan.set @"frontFace"               Vulkan.VK_FRONT_FACE_COUNTER_CLOCKWISE
        &* Vulkan.set @"cullMode"                Vulkan.VK_CULL_MODE_BACK_BIT
        )

    vertexShaderStage =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* Vulkan.set @"pNext"        Vulkan.VK_NULL
        &* Vulkan.set @"flags"        0
        &* Vulkan.setStrRef @"pName"  "main"
        &* Vulkan.set @"module"       vertexShader
        &* Vulkan.set @"stage"        Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        )

    fragmentShaderStage =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* Vulkan.set @"pNext"        Vulkan.VK_NULL
        &* Vulkan.set @"flags"        0
        &* Vulkan.setStrRef @"pName"  "main"
        &* Vulkan.set @"module"       fragmentShader
        &* Vulkan.set @"stage"        Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
        )

    vertexBindingDescription :: Vulkan.VkVertexInputBindingDescription
    vertexBindingDescription =
      Vulkan.createVk
        (  Vulkan.set @"binding"   0
        &* Vulkan.set @"stride"    ( fromIntegral ( Foreign.sizeOf ( undefined :: V 2 ( V 3 Foreign.C.CFloat ) ) ) )
        &* Vulkan.set @"inputRate" Vulkan.VK_VERTEX_INPUT_RATE_VERTEX
        )

    positionAttributeDescription =
      Vulkan.createVk
        (  Vulkan.set @"location" 0
        &* Vulkan.set @"binding"  0
        &* Vulkan.set @"format"   Vulkan.VK_FORMAT_R32G32B32_SFLOAT
        &* Vulkan.set @"offset"   0
        )

    colorAttributeDescription =
      Vulkan.createVk
        (  Vulkan.set @"location" 1
        &* Vulkan.set @"binding"  0
        &* Vulkan.set @"format"  Vulkan.VK_FORMAT_R32G32B32_SFLOAT
        &* Vulkan.set @"offset"  ( fromIntegral ( Foreign.sizeOf ( undefined :: V 3 Foreign.C.CFloat ) ) )
        )

    vertexInputState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.setListCountAndRef
                @"vertexBindingDescriptionCount"
                @"pVertexBindingDescriptions"
                [ vertexBindingDescription ]
        &* Vulkan.setListCountAndRef
                @"vertexAttributeDescriptionCount"
                @"pVertexAttributeDescriptions"
                [ positionAttributeDescription
                , colorAttributeDescription
                ]
        )

    assemblyStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"    Vulkan.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        &* Vulkan.set @"pNext"    Vulkan.VK_NULL
        &* Vulkan.set @"topology" Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        &* Vulkan.set @"primitiveRestartEnable" Vulkan.VK_FALSE
        )

    viewport =
      Vulkan.createVk
        (  Vulkan.set @"x"        0
        &* Vulkan.set @"y"        0
        &* Vulkan.set @"width"    ( fromIntegral $ Vulkan.getField @"width"  extent )
        &* Vulkan.set @"height"   ( fromIntegral $ Vulkan.getField @"height" extent )
        &* Vulkan.set @"minDepth" 0
        &* Vulkan.set @"maxDepth" 1
        )

    scissor =
      let
        offset =
          Vulkan.createVk
            (  Vulkan.set @"x" 0
            &* Vulkan.set @"y" 0
            )

      in
      Vulkan.createVk
        (  Vulkan.set @"offset" offset
        &* Vulkan.set @"extent" extent
        )

    viewportState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.setListCountAndRef @"viewportCount" @"pViewports" [ viewport ]
        &* Vulkan.setListCountAndRef @"scissorCount"  @"pScissors"  [ scissor  ]
        )

    multisampleState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
        &* Vulkan.set @"minSampleShading"     1
        &* Vulkan.set @"rasterizationSamples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"pNext"                Vulkan.VK_NULL
        )

    attachmentState =
      Vulkan.createVk
        (  Vulkan.set @"blendEnable"         Vulkan.VK_FALSE
        &* Vulkan.set @"alphaBlendOp"        Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set @"colorBlendOp"        Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set @"colorWriteMask"
             (     Vulkan.VK_COLOR_COMPONENT_R_BIT
               .|. Vulkan.VK_COLOR_COMPONENT_G_BIT
               .|. Vulkan.VK_COLOR_COMPONENT_B_BIT
               .|. Vulkan.VK_COLOR_COMPONENT_A_BIT
             )
        )

    colorBlendState =
      Vulkan.createVk
        (  Vulkan.set @"sType"   Vulkan.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
        &* Vulkan.set @"logicOp" Vulkan.VK_LOGIC_OP_COPY
        &* Vulkan.set @"pNext"   Vulkan.VK_NULL
        &* Vulkan.setAt @"blendConstants" @0 0
        &* Vulkan.setAt @"blendConstants" @1 0
        &* Vulkan.setAt @"blendConstants" @2 0
        &* Vulkan.setAt @"blendConstants" @3 0
        &* Vulkan.setListCountAndRef @"attachmentCount" @"pAttachments" [ attachmentState ]
        )

    nullStencilOp :: Vulkan.VkStencilOpState
    nullStencilOp =
      Vulkan.createVk
        (  Vulkan.set @"reference"   0
        &* Vulkan.set @"writeMask"   0
        &* Vulkan.set @"compareMask" 0
        &* Vulkan.set @"compareOp"   Vulkan.VK_COMPARE_OP_EQUAL
        &* Vulkan.set @"depthFailOp" Vulkan.VK_STENCIL_OP_KEEP
        &* Vulkan.set @"passOp"      Vulkan.VK_STENCIL_OP_KEEP
        &* Vulkan.set @"failOp"      Vulkan.VK_STENCIL_OP_KEEP
        )

    depthStencilState :: Vulkan.VkPipelineDepthStencilStateCreateInfo
    depthStencilState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"depthTestEnable"   Vulkan.VK_TRUE
        &* Vulkan.set @"depthWriteEnable"  Vulkan.VK_TRUE
        &* Vulkan.set @"depthCompareOp"    Vulkan.VK_COMPARE_OP_LESS_OR_EQUAL
        &* Vulkan.set @"maxDepthBounds"    1
        &* Vulkan.set @"minDepthBounds"    0
        &* Vulkan.set @"stencilTestEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"front"             nullStencilOp
        &* Vulkan.set @"back"              nullStencilOp
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.setListCountAndRef
                @"stageCount"
                @"pStages"
                [ vertexShaderStage, fragmentShaderStage ]
        &* Vulkan.setVkRef @"pVertexInputState"   vertexInputState
        &* Vulkan.set      @"basePipelineIndex"   0
        &* Vulkan.set      @"subpass"             0
        &* Vulkan.set      @"renderPass"          renderPass
        &* Vulkan.set      @"layout"              pipelineLayout
        &* Vulkan.setVkRef @"pRasterizationState" rasterizationCreateInfo
        &* Vulkan.setVkRef @"pInputAssemblyState" assemblyStateCreateInfo
        &* Vulkan.setVkRef @"pViewportState"      viewportState
        &* Vulkan.setVkRef @"pMultisampleState"   multisampleState
        &* Vulkan.setVkRef @"pColorBlendState"    colorBlendState
        &* Vulkan.setVkRef @"pDepthStencilState"  depthStencilState
        )

  pipeline <-
    managedVulkanResource
      ( Vulkan.vkCreateGraphicsPipelines
          device
          Vulkan.vkNullPtr
          1
          ( Vulkan.unsafePtr createInfo )
      )
      ( Vulkan.vkDestroyPipeline device )

  return ( pipeline, pipelineLayout )



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
        &* Vulkan.set @"flags" 0
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
        &* Vulkan.set @"descriptorSetCount" 1
        &* Vulkan.setListRef @"pSetLayouts" [ layout0 ]
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
