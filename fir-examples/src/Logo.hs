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

module Logo ( logo ) where

-- base
import Control.Monad
  ( (>=>), when )
import Control.Monad.IO.Class
  ( liftIO )
import Data.Bits
  ( (.|.) )
import Data.Coerce
  ( coerce )
import Data.Foldable
  ( for_ )
import Data.Monoid
  ( Sum(getSum) )
import Data.String
  ( IsString )
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
  , Layout(Extended), Poke(poke)
  , (:->)((:->)), Struct((:&),End)
  )
import qualified FIR
import Math.Linear
  ( V, pattern V2, pattern V3
  , (*^), (^+^)
  , normalise
  )
import Math.Quaternion
  ( rotate, axisAngle )

-- fir-examples
import Shaders.Logo
import Vulkan.Backend
import Vulkan.Buffer
import Vulkan.Monad
import Simulation.Observer
import Vulkan.Pipeline
import Vulkan.SDL

----------------------------------------------------------------------------

shaderCompilationResult :: Either ShortText ()
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Compute shader", compileComputeShader) ]
     )

appName :: IsString a => a
appName = "fir-examples - Logo"

logo :: IO ()
logo = ( runManaged . ( `evalStateT` initialState ) ) do

  case shaderCompilationResult of
    Left  err -> logMsg ( "Shader compilation was unsuccessful:\n" <> ShortText.unpack err )
    Right _   -> logMsg ( "Shaders were succesfully compiled." )

  enableSDLLogging
  initializeSDL SDL.RelativeLocation -- relative mouse location
  window           <- logMsg "Creating SDL window"           *> createWindow appName
  setWindowIcon window "assets/fir_logo.png"

  neededExtensions <- logMsg "Loading needed extensions"     *> getNeededExtensions window
  extensionNames <- traverse ( liftIO . Foreign.C.peekCString ) neededExtensions
  logMsg $ "Needed instance extensions are: " ++ show extensionNames

  vulkanInstance   <- logMsg "Creating Vulkan instance"      *> createVulkanInstance appName neededExtensions
  physicalDevice   <- logMsg "Creating physical device"      *> createPhysicalDevice vulkanInstance
  queueFamilyIndex <- logMsg "Finding suitable queue family"
    *> findQueueFamilyIndex physicalDevice [Vulkan.VK_QUEUE_COMPUTE_BIT]

  device  <- logMsg "Creating logical device" *> createLogicalDevice physicalDevice queueFamilyIndex Nothing
  surface <- logMsg "Creating SDL surface"    *> createSurface window vulkanInstance

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
            (   Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT
            .|. Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
            .|. Vulkan.VK_IMAGE_USAGE_STORAGE_BIT
            )

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

  let
    screenshotImageInfo :: ImageInfo
    screenshotImageInfo =
      ( Default2DImageInfo extent3D colFmt
        Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
      ) { imageTiling = Vulkan.VK_IMAGE_TILING_LINEAR }  -- host visible image needs linear tiling


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
    imageCopy :: Vulkan.VkImageCopy
    imageCopy
      = Vulkan.createVk
        (  Vulkan.set @"srcSubresource" layers
        &* Vulkan.set @"srcOffset"      noOffset
        &* Vulkan.set @"dstSubresource" layers
        &* Vulkan.set @"dstOffset"      noOffset
        &* Vulkan.set @"extent"         extent3D
        )

  images <-
    for swapchainImages $ \swapchainImage -> do

        swapchainImageView
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

        pure
          ( (swapchainImage , swapchainImageView   )
          , (screenshotImage, screenshotImageMemory)
          )

  let
    numImages :: Int
    numImages = length images

  commandPool <- logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex
  queue       <- getQueue device 0

  nextImageSem <- createSemaphore device
  submitted    <- createSemaphore device

  descriptorPool <- createDescriptorPool device numImages

  descriptorSetLayout <- createDescriptorSetLayout device
  descriptorSets      <- allocateDescriptorSets device descriptorPool descriptorSetLayout numImages

  ( computePipeline, pipelineLayout )
    <- createComputePipeline device descriptorSetLayout compPath

  (cameraUniformBuffer, camPtr)
    <- createUniformBuffer
          physicalDevice
          device
          ( camera initialObserver Nothing )

  for_ (zip images descriptorSets) \ ( ( (_, swapchainImageView) , _), descriptorSet) ->
    updateDescriptorSet device descriptorSet cameraUniformBuffer swapchainImageView

  let
    mkCommandBuffer
      :: MonadManaged m
      => Vulkan.VkDescriptorSet
      -> (Vulkan.VkImage, Vulkan.VkImageView)
      -> Maybe (Vulkan.VkImage, Vulkan.VkDeviceMemory)
      -> m Vulkan.VkCommandBuffer
    mkCommandBuffer
      descriptorSet
      ( swapchainImage, _ )
      mbScreenshot = do
        commandBuffer <- allocateCommandBuffer device commandPool


        beginCommandBuffer commandBuffer

        cmdTransitionImageLayout commandBuffer swapchainImage
          Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
          Vulkan.VK_IMAGE_LAYOUT_GENERAL
          ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
          ( Vulkan.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.VK_ACCESS_SHADER_WRITE_BIT )

        liftIO do
          Vulkan.vkCmdBindPipeline
            commandBuffer
            Vulkan.VK_PIPELINE_BIND_POINT_COMPUTE
            computePipeline

          Foreign.Marshal.withArray [ descriptorSet ] $ \descriptorSetsPtr ->
            Vulkan.vkCmdBindDescriptorSets
              commandBuffer
              Vulkan.VK_PIPELINE_BIND_POINT_COMPUTE
              pipelineLayout
              0
              1
              descriptorSetsPtr
              0
              Vulkan.vkNullPtr

          Vulkan.vkCmdDispatch
            commandBuffer
            120 135 1 -- local size 16 8 1

        -- if taking a screenshot, copy swapchain image onto screenshot image
        case mbScreenshot of
          Just ( screenshotImage, _)
            -> do -- transition screenshot image layout to TRANSFER_DST so that storage image can be copied onto it
                  cmdTransitionImageLayout commandBuffer screenshotImage
                    Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                    ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT , Vulkan.VK_ZERO_FLAGS )
                    ( Vulkan.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.VK_ZERO_FLAGS )
                  cmdTransitionImageLayout commandBuffer swapchainImage
                    Vulkan.VK_IMAGE_LAYOUT_GENERAL
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                    ( Vulkan.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.VK_ACCESS_SHADER_WRITE_BIT )
                    ( Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
                  -- perform the copy
                  liftIO $ Vulkan.vkCmdCopyImage commandBuffer
                    swapchainImage
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                    screenshotImage
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                    1
                    ( Vulkan.unsafePtr imageCopy )
                  -- transition screenshot image to general layout so that we can copy it to disk
                  cmdTransitionImageLayout commandBuffer screenshotImage
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                    Vulkan.VK_IMAGE_LAYOUT_GENERAL
                    ( Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
                    ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.VK_ZERO_FLAGS )
                  cmdTransitionImageLayout commandBuffer swapchainImage
                    Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                    Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    ( Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
                    ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.VK_ZERO_FLAGS )
          Nothing
            -> cmdTransitionImageLayout commandBuffer swapchainImage
                  Vulkan.VK_IMAGE_LAYOUT_GENERAL
                  Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                  ( Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
                  ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.VK_ZERO_FLAGS )

        endCommandBuffer commandBuffer

        pure commandBuffer


  commandBuffers <-
    for (zip images descriptorSets) $ \((swapImgAndView, _), descriptorSet) ->
      mkCommandBuffer descriptorSet swapImgAndView Nothing

  screenshotCommandBuffers <-
    for (zip images descriptorSets) $ \((swapImgAndView, screenshotImageAndMemory), descriptorSet) ->
      mkCommandBuffer descriptorSet swapImgAndView (Just screenshotImageAndMemory)

  assign _observer ( Observer { position = V3 0 0 10, angles = V2 (5*pi/4) (pi/5) } )

  mainLoop do

    ----------------
    -- input

    inputEvents <- map SDL.Event.eventPayload <$> SDL.pollEvents
    prevInput <- use _input
    let newInput = foldl onSDLInput prevInput inputEvents
    let action = interpretInput newInput
        angs   = fmap getSum ( look action )
    assign _input ( newInput { mouseRel = pure 0, keysPressed = [] } )

    ----------------
    -- simulation

    oldObserver <- use _observer
    let
      oldAngs = angles oldObserver
      newAngs@(V2 x y) = oldAngs ^+^ angs
      orientation = axisAngle (V3 0 (-1) 0) x FIR.* axisAngle (V3 1 0 0) y

      pos, fwd, up, right :: V 3 Float
      pos   = rotate orientation ( V3 0 0 10 )
      fwd   = normalise ( (-1) *^ pos )
      up    = rotate orientation ( V3 0 (-1) 0 )
      right = rotate orientation ( V3 1   0  0 )

      cam :: Struct
                '[ "position" ':-> V 3 Float
                 , "right"    ':-> V 3 Float
                 , "up"       ':-> V 3 Float
                 , "forward"  ':-> V 3 Float
                 ]
      cam = pos :& right :& up :& fwd :& End

      newObserver = oldObserver { angles = newAngs }

    assign _observer newObserver

    when ( locate action )
      ( liftIO $ putStrLn ( show newObserver ) )

    -- update camera
    liftIO ( poke @_ @Extended camPtr cam )

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
      [(nextImageSem, Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)]
      [submitted]
      Nothing

    present queue swapchain nextImageIndex [submitted]

    -- if taking a screenshot, map the image memory, read from it, and write to disk
    when ( takeScreenshot action ) $ liftIO do

        let screenshotImageMemory :: Vulkan.VkDeviceMemory
            ( _, (_, screenshotImageMemory) )
              = images !! nextImageIndex

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

        writePng "screenshots/logo.png" imageData

        Vulkan.vkUnmapMemory device screenshotImageMemory

    liftIO ( Vulkan.vkQueueWaitIdle queue )
      >>= throwVkResult

    ----------------

    pure ( shouldQuit action )



createDescriptorSetLayout
  :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkDescriptorSetLayout
createDescriptorSetLayout device = do
  let
    uboBinding =
      Vulkan.createVk
        (  Vulkan.set @"binding"            0
        &* Vulkan.set @"descriptorType"     Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* Vulkan.set @"descriptorCount"    1
        &* Vulkan.set @"stageFlags"         Vulkan.VK_SHADER_STAGE_COMPUTE_BIT
        &* Vulkan.set @"pImmutableSamplers" Vulkan.VK_NULL
        )

    imageBinding =
      Vulkan.createVk
        (  Vulkan.set @"binding"            1
        &* Vulkan.set @"descriptorType"     Vulkan.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
        &* Vulkan.set @"descriptorCount"    1
        &* Vulkan.set @"stageFlags"         Vulkan.VK_SHADER_STAGE_COMPUTE_BIT
        &* Vulkan.set @"pImmutableSamplers" Vulkan.VK_NULL
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.setListCountAndRef @"bindingCount" @"pBindings" [ uboBinding, imageBinding ]
        )

  managedVulkanResource
    ( Vulkan.vkCreateDescriptorSetLayout
        device
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyDescriptorSetLayout device )



createDescriptorPool
  :: MonadManaged m
  => Vulkan.VkDevice -> Int -> m Vulkan.VkDescriptorPool
createDescriptorPool device maxSets =
  let
    poolSize0 =
      Vulkan.createVk
        (  Vulkan.set @"type" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
        &* Vulkan.set @"descriptorCount" 1
        )

    poolSize1 =
      Vulkan.createVk
        (  Vulkan.set @"type" Vulkan.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
        &* Vulkan.set @"descriptorCount" 1
        )

    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
        &* Vulkan.setListCountAndRef @"poolSizeCount" @"pPoolSizes" [ poolSize0, poolSize1 ]
        &* Vulkan.set @"maxSets" ( fromIntegral maxSets )
        )

  in
    managedVulkanResource
      ( Vulkan.vkCreateDescriptorPool device ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyDescriptorPool device )


allocateDescriptorSets
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorPool
  -> Vulkan.VkDescriptorSetLayout
  -> Int
  -> m [Vulkan.VkDescriptorSet]
allocateDescriptorSets dev descriptorPool layout0 count = do

  let

    allocateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"descriptorPool" descriptorPool
        &* Vulkan.setListCountAndRef @"descriptorSetCount" @"pSetLayouts"
             ( replicate count layout0 )
        )

  manageBracket
    ( allocaAndPeekArray count
        ( Vulkan.vkAllocateDescriptorSets
            dev
            ( Vulkan.unsafePtr allocateInfo )
            >=> throwVkResult
        )
    )
    ( \descs ->
        Foreign.Marshal.withArray descs
          ( Vulkan.vkFreeDescriptorSets dev descriptorPool (fromIntegral count) )
    )


updateDescriptorSet
  :: MonadManaged m
  => Vulkan.VkDevice -> Vulkan.VkDescriptorSet -> Vulkan.VkBuffer -> Vulkan.VkImageView -> m ()
updateDescriptorSet device descriptorSet buffer imageView =
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
        (  Vulkan.set @"sampler"     Vulkan.VK_NULL
        &* Vulkan.set @"imageView"   imageView
        &* Vulkan.set @"imageLayout" Vulkan.VK_IMAGE_LAYOUT_GENERAL
        )
    writeUpdate1 =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"dstSet" descriptorSet
        &* Vulkan.set @"dstBinding" 1
        &* Vulkan.set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
        &* Vulkan.set @"descriptorCount" 1
        &* Vulkan.set @"pTexelBufferView" Vulkan.VK_NULL
        &* Vulkan.setVkRef @"pImageInfo" imageInfo
        &* Vulkan.set @"dstArrayElement" 0
        &* Vulkan.setListRef @"pBufferInfo" [ ]
        )
  in liftIO $
       Foreign.Marshal.withArray [ writeUpdate0, writeUpdate1 ] $ \writeUpdatesPtr ->
         Vulkan.vkUpdateDescriptorSets device 2 writeUpdatesPtr 0 Vulkan.vkNullPtr
