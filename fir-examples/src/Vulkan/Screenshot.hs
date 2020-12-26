{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Vulkan.Screenshot
  ( createScreenshotImage
  , screenshotImageInfo
  , cmdTakeScreenshot
  , writeScreenshotData
  )
  where

-- base
import Control.Monad
  ( when )
import Data.Bits
  ( (.|.) )
import Data.Coerce
  ( coerce )
import Data.Word
  ( Word8, Word32 )
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
import qualified Data.Vector          as Boxed.Vector
  ( singleton )
import qualified Data.Vector.Storable as Storable.Vector
  ( fromList )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Zero as Vulkan

-- fir-examples
import FIR.Examples.Paths
  ( screenshotDir )
import Vulkan.Backend
import Vulkan.Monad

----------------------------------------------------------------------------

createScreenshotImage
  :: MonadVulkan m
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> ImageInfo
  -> m ( Vulkan.Image, Vulkan.DeviceMemory )
createScreenshotImage physicalDevice device imageInfo =
  createImage
    physicalDevice device
    imageInfo
    ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )

screenshotImageInfo :: Vulkan.Extent3D -> Vulkan.Format -> ImageInfo
screenshotImageInfo swapchainExtent3D colFmt =
  ( Default2DImageInfo swapchainExtent3D colFmt Vulkan.IMAGE_USAGE_TRANSFER_DST_BIT )
    { imageTiling = Vulkan.IMAGE_TILING_LINEAR } -- host visible image needs linear tiling

cmdTakeScreenshot
  :: MonadVulkan m
  => ( Vulkan.PipelineStageFlags, Vulkan.AccessFlags )
  -> Vulkan.CommandBuffer
  -> Vulkan.Extent3D
  -> ( Vulkan.Image, (Vulkan.ImageLayout, Vulkan.ImageLayout) )
  -> Vulkan.Image
  -> m ()
cmdTakeScreenshot screenshotAfterFlags commandBuffer swapchainExtent3D
  (swapchainImage, (swapchainInitialLayout, swapchainFinalLayout) )
  screenshotImage
    = do
      -- transition swapchain image: transfer source
      when ( swapchainInitialLayout /= Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL ) do
        cmdTransitionImageLayout commandBuffer swapchainImage
          swapchainInitialLayout
          Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          screenshotAfterFlags
          ( Vulkan.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.zero )
      -- transition screenshot image: transfer destination
      cmdTransitionImageLayout commandBuffer screenshotImage
        Vulkan.IMAGE_LAYOUT_UNDEFINED
        Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT , Vulkan.zero )
        ( Vulkan.PIPELINE_STAGE_ALL_COMMANDS_BIT, Vulkan.zero )
      -- perform the copy
      liftIO $ Vulkan.cmdCopyImage commandBuffer
        swapchainImage
        Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        screenshotImage
        Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        ( Boxed.Vector.singleton imageCopy )
      -- transition screenshot image to general layout so that we can copy it to disk
      cmdTransitionImageLayout commandBuffer screenshotImage
        Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        Vulkan.IMAGE_LAYOUT_GENERAL
        ( Vulkan.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.zero )
        ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.zero )
      when ( swapchainFinalLayout /= Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL ) do
        cmdTransitionImageLayout commandBuffer swapchainImage
          Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          swapchainFinalLayout
          ( Vulkan.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.zero )
          ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.zero )

        where
          noOffset :: Vulkan.Offset3D
          noOffset
            = Vulkan.Offset3D
                { Vulkan.x = 0
                , Vulkan.y = 0
                , Vulkan.z = 0
                }
          layers :: Vulkan.ImageSubresourceLayers
          layers
            = Vulkan.ImageSubresourceLayers
              { Vulkan.aspectMask     = Vulkan.IMAGE_ASPECT_COLOR_BIT
              , Vulkan.mipLevel       = 0
              , Vulkan.baseArrayLayer = 0
              , Vulkan.layerCount     = 1
              }
          imageCopy :: Vulkan.ImageCopy
          imageCopy
            = Vulkan.ImageCopy
              { Vulkan.srcSubresource = layers
              , Vulkan.srcOffset      = noOffset
              , Vulkan.dstSubresource = layers
              , Vulkan.dstOffset      = noOffset
              , Vulkan.extent         = swapchainExtent3D
              }

writeScreenshotData
  :: MonadVulkan m
  => FilePath
  -> Vulkan.Device
  -> Vulkan.Extent2D
  -> Vulkan.DeviceMemory
  -> m ()
writeScreenshotData screenshotPath device extent2D screenshotImageMemory = liftIO do

  memPtr :: Ptr Word8
    <- coerce <$>
        ( Vulkan.mapMemory device screenshotImageMemory
            0 maxBound Vulkan.zero
        )

  let
    imageWidth, imageHeight :: Num a => a
    imageWidth  = fromIntegral $ ( Vulkan.width  :: Vulkan.Extent2D -> Word32 ) extent2D
    imageHeight = fromIntegral $ ( Vulkan.height :: Vulkan.Extent2D -> Word32 ) extent2D
    size = 4 * imageWidth * imageHeight

    -- image data is stored in BGRA component order,
    -- whether R8G8B8A8 or B8G8R8A8 format is used
    bgraToRgba :: [a] -> [a]
    bgraToRgba ( b : g : r : a : rest )
      = r : g : b : a : bgraToRgba rest
    bgraToRgba l = l

  imageData :: Image PixelRGBA8
    <- Image imageWidth imageHeight . Storable.Vector.fromList . bgraToRgba <$> Foreign.peekArray size memPtr

  writePng
    ( screenshotDir </> screenshotPath <.> "png" )
    imageData

  Vulkan.unmapMemory device screenshotImageMemory
