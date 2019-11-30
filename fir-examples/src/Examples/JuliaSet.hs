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

module Examples.JuliaSet ( juliaSet ) where

-- base
import Control.Monad
  ( (>=>), when )
import Control.Monad.IO.Class
  ( liftIO )
import Data.Bits
import Data.Coerce
  ( coerce )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import Data.Word
  ( Word8, Word32 )
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
import qualified SDL.Raw.Event as SDL

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

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
  , Poke(poke)
  , Layout(Extended)
  , Struct((:&),End)
  , ModuleRequirements
  )
import Math.Linear
  ( pattern V2, pattern V3
  , (^+^), (*^)
  )

-- fir-examples
import Examples.JuliaSet.Shaders
import Simulation.Observer
import Vulkan.Backend
import Vulkan.Buffer
import Vulkan.Features
  ( requiredFeatures )
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.SDL

----------------------------------------------------------------------------

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Vertex shader"  , compileVertexShader   )
        , ("Fragment shader", compileFragmentShader )
        ]
     )

appName :: IsString a => a
appName = "fir-examples - Julia set"

juliaSet :: IO ()
juliaSet = ( runManaged . ( `evalStateT` initialState ) ) do

  ( reqs :: ModuleRequirements ) <-
    case shaderCompilationResult of
      Left  err  -> error $ "Shader compilation was unsuccessful:\n" <> ShortText.unpack err
      Right reqs -> logMsg ( "Shaders were succesfully compiled." ) *> pure reqs

  enableSDLLogging
  initializeSDL SDL.AbsoluteLocation
  window           <- logMsg "Creating SDL window"           *> createWindow 1920 1080 appName
  setWindowIcon window "assets/fir_logo.png"

  neededExtensions <- logMsg "Loading needed extensions"     *> getNeededExtensions window
  extensionNames <- traverse ( liftIO . Foreign.C.peekCString ) neededExtensions
  logMsg $ "Needed instance extensions are: " ++ show extensionNames

  vulkanInstance   <- logMsg "Creating Vulkan instance"      *> createVulkanInstance appName neededExtensions
  physicalDevice   <- logMsg "Creating physical device"      *> createPhysicalDevice vulkanInstance
  queueFamilyIndex <- logMsg "Finding suitable queue family"
      *> findQueueFamilyIndex physicalDevice [Vulkan.VK_QUEUE_GRAPHICS_BIT]

  features <- liftIO ( requiredFeatures reqs )
  device   <- logMsg "Creating logical device" *> createLogicalDevice  physicalDevice queueFamilyIndex features
  surface  <- logMsg "Creating SDL surface"    *> createSurface window vulkanInstance

  assertSurfacePresentable physicalDevice queueFamilyIndex surface

  let preferredSwapchainFormat :: Vulkan.VkSurfaceFormatKHR
      preferredSwapchainFormat
        = VkSurfaceFormatKHR
            Vulkan.VK_FORMAT_B8G8R8A8_UNORM
            Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR

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
  renderPass      <- logMsg "Creating a render pass"   *> createRenderPass   device colFmt

  let
    screenshotImageInfo :: ImageInfo
    screenshotImageInfo =
      ( Default2DImageInfo extent3D colFmt
        Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
      ) { imageTiling = Vulkan.VK_IMAGE_TILING_LINEAR } -- host visible image needs linear tiling

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
          let attachments = [ (swapchainImage, colorImageView) ]
          framebuffer <- createFramebuffer device renderPass extent (map snd attachments)
          pure (framebuffer, attachments, (screenshotImage, screenshotImageMemory))
       )

  let clearValues :: [ Vulkan.VkClearValue ] -- in bijection with framebuffer attachments
      clearValues = [ blackClear ]
        where
          transparentBlack :: Vulkan.VkClearColorValue
          transparentBlack =
            Vulkan.createVk
              (  Vulkan.setAt @"float32" @0 0
              &* Vulkan.setAt @"float32" @1 0
              &* Vulkan.setAt @"float32" @2 0
              &* Vulkan.setAt @"float32" @3 0
              )

          blackClear :: Vulkan.VkClearValue
          blackClear = Vulkan.createVk ( Vulkan.set @"color" transparentBlack )

  commandPool <- logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex
  queue       <- getQueue device 0

  nextImageSem <- createSemaphore device
  submitted    <- createSemaphore device

  descriptorPool <- createDescriptorPool device

  descriptorSetLayout <- createDescriptorSetLayout device
  descriptorSet       <- allocateDescriptorSet device descriptorPool descriptorSetLayout

  let pipelineInfo = VkPipelineInfo extent Vulkan.VK_SAMPLE_COUNT_1_BIT
  ( graphicsPipeline, pipelineLayout )
    <- createGraphicsPipeline device renderPass pipelineInfo descriptorSetLayout shaderPipeline

  let

    viewportVertices :: [ Struct VertexInput ]
    viewportVertices =
      [ V3 (-1) (-1) 0 :& End
      , V3 (-1)   1  0 :& End
      , V3   1 (-1)  0 :& End
      , V3   1   1   0 :& End
      ]

    viewportIndices :: [ Word32 ]
    viewportIndices
      = [ 0, 1, 2
        , 2, 1, 3
        ]

  (vertexBuffer, _) <- createVertexBuffer physicalDevice device viewportVertices

  (indexBuffer, _) <- createIndexBuffer physicalDevice device viewportIndices

  (mousePosUniformBuffer, mousePosUniformPtr)
    <- createUniformBuffer
          physicalDevice
          device
          (V2 0 0)

  updateDescriptorSet device descriptorSet mousePosUniformBuffer

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
            ( fromIntegral ( length viewportIndices ) )
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
    let
      prevAction = interpretInput prevInput
      newInput = foldl onSDLInput prevInput inputEvents
      action   = interpretInput newInput
    pos <- 
      if locate action
      then do SDL.setMouseLocationMode SDL.RelativeLocation
              -- precision mode
              pure ( mousePos prevInput ^+^ ( 20 *^ mouseRel newInput ) )
      else do SDL.setMouseLocationMode SDL.AbsoluteLocation
              -- smooth out mouse movement slightly
              let pos@(V2 px py) = 0.5 *^ ( mousePos prevInput ^+^ mousePos newInput )
              when (locate prevAction) do
                ( SDL.warpMouse SDL.WarpCurrentFocus (SDL.P (SDL.V2 (round px) (round py))) )
                _ <- SDL.captureMouse True
                pure ()

              pure pos
    assign _input ( newInput { mousePos = pos, mouseRel = pure 0 } )

    ----------------
    -- simulation

    -- update MVP
    liftIO ( poke @_ @Extended mousePosUniformPtr pos )

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

        writePng "screenshots/julia_set.png" imageData

        Vulkan.vkUnmapMemory device screenshotImageMemory

    liftIO ( Vulkan.vkQueueWaitIdle queue )
      >>= throwVkResult

    ----------------

    pure ( shouldQuit action )



createRenderPass
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkFormat
  -> m Vulkan.VkRenderPass
createRenderPass dev colorFormat =
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


    subpass :: Vulkan.VkSubpassDescription
    subpass =
      Vulkan.createVk
        (  Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"pipelineBindPoint" Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        &* Vulkan.setListCountAndRef
              @"colorAttachmentCount"
              @"pColorAttachments"
              [ colorAttachmentReference ]
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
              [ colorAttachmentDescription ]
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
        &* Vulkan.set @"stageFlags" Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
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
