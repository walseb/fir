{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Vulkan.Screenshot
  ( createScreenshotImage
  , screenshotImageInfo
  , cmdTakeScreenshot
  , writeScreenshotData
  )
  where

-- base
import Control.Monad
  ( (>=>), when )
import Data.Coerce
  ( coerce )
import Data.Word
  ( Word8 )
import qualified Foreign
  ( peekArray )
import Foreign.Ptr
  ( Ptr )

-- filepath
import System.FilePath
  ( (</>), (<.>) )

-- JuicyPixels
import Codec.Picture.Types
  ( Image(..), PixelRGBA8(..) )
import Codec.Picture.Png
  ( writePng )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector
import qualified Data.Vector.Storable as Vector

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan                as Vulkan
import qualified Graphics.Vulkan.Core_1_0       as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir-examples
import Vulkan.Backend
import Vulkan.Monad

----------------------------------------------------------------------------

createScreenshotImage
  :: MonadVulkan m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> ImageInfo
  -> m ( Vulkan.VkImage, Vulkan.VkDeviceMemory )
createScreenshotImage physicalDevice device imageInfo =
  createImage
    physicalDevice device
    imageInfo
    [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
    , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
    ]

screenshotImageInfo :: Vulkan.VkExtent3D -> Vulkan.VkFormat -> ImageInfo
screenshotImageInfo swapchainExtent3D colFmt =
  ( Default2DImageInfo swapchainExtent3D colFmt Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT )
    { imageTiling = Vulkan.VK_IMAGE_TILING_LINEAR } -- host visible image needs linear tiling

cmdTakeScreenshot
  :: MonadVulkan m
  => ( Vulkan.VkPipelineStageFlags, Vulkan.VkAccessFlags )
  -> Vulkan.VkCommandBuffer
  -> Vulkan.VkExtent3D
  -> ( Vulkan.VkImage, (Vulkan.VkImageLayout, Vulkan.VkImageLayout) )
  -> Vulkan.VkImage
  -> m ()
cmdTakeScreenshot screenshotAfterFlags commandBuffer swapchainExtent3D
  (swapchainImage, (swapchainInitialLayout, swapchainFinalLayout) )
  screenshotImage
    = do
      -- transition swapchain image: transfer source
      when ( swapchainInitialLayout /= Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL ) do
        cmdTransitionImageLayout commandBuffer swapchainImage
          swapchainInitialLayout
          Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          screenshotAfterFlags
          ( Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
      -- transition screenshot image: transfer destination
      cmdTransitionImageLayout commandBuffer screenshotImage
        Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT , Vulkan.VK_ZERO_FLAGS )
        ( Vulkan.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.VK_ZERO_FLAGS )
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
      when ( swapchainFinalLayout /= Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL ) do
        cmdTransitionImageLayout commandBuffer swapchainImage
          Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          swapchainFinalLayout
          ( Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
          ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.VK_ZERO_FLAGS )

        where
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
              &* Vulkan.set @"extent"         swapchainExtent3D
              )

writeScreenshotData
  :: MonadVulkan m
  => FilePath
  -> Vulkan.VkDevice
  -> Vulkan.VkExtent2D
  -> Vulkan.VkDeviceMemory
  -> m ()
writeScreenshotData screenshotPath device extent2D screenshotImageMemory = liftIO do

  memPtr :: Ptr Word8
    <- coerce <$> allocaAndPeek
          ( Vulkan.vkMapMemory device screenshotImageMemory
              0 maxBound Vulkan.VK_ZERO_FLAGS
            >=> throwVkResult
          )

  let
    imageWidth, imageHeight :: Num a => a
    imageWidth  = fromIntegral $ Vulkan.getField @"width"  extent2D
    imageHeight = fromIntegral $ Vulkan.getField @"height" extent2D
    size = 4 * imageWidth * imageHeight

    -- image data is stored in BGRA component order,
    -- whether R8G8B8A8 or B8G8R8A8 format is used
    bgraToRgba :: [a] -> [a]
    bgraToRgba ( b : g : r : a : rest )
      = r : g : b : a : bgraToRgba rest
    bgraToRgba l = l

  imageData :: Image PixelRGBA8
    <- Image imageWidth imageHeight . Vector.fromList . bgraToRgba <$> Foreign.peekArray size memPtr

  writePng
    ( "screenshots" </> screenshotPath <.> "png" )
    imageData

  Vulkan.vkUnmapMemory device screenshotImageMemory
