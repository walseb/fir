{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module FIR.Examples.Common where

-- base
import Data.Bits
  ( (.|.) )
import Data.Foldable
  ( traverse_ )
import Data.Word
  ( Word32 )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, allocate, release )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector
import qualified Data.Vector as Boxed.Vector
  ( fromList, singleton )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( Vector, unzip )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Zero as Vulkan

-- fir-examples
import Vulkan.Attachment
import Vulkan.Backend
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- A few simple routines shared by some examples.

recordSimpleIndexedDrawCall
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPool
  -> Vulkan.Framebuffer
  -> (Vulkan.RenderPass, [Vulkan.ClearValue])
  -> Vulkan.DescriptorSet
  -> ( Vulkan.CommandBuffer -> m () )
  -> ( Vulkan.Image, Vulkan.Extent2D)
  -> Maybe (Vulkan.Image, Vulkan.Extent3D)
  -> Word32
  -> Vulkan.PipelineLayout
  -> VkPipeline
  -> m (ReleaseKey, Vulkan.CommandBuffer)
recordSimpleIndexedDrawCall
  dev commandPool framebuffer (renderPass, clearValues)
  descriptorSet cmdBindBuffers
  (swapchainImage, extent2D) mbScreenshotImage
  nbIndices pipelineLayout pipeline
  = do

    res@(_ , commandBuffer) <- allocateCommandBuffer dev commandPool

    beginCommandBuffer commandBuffer

    cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues extent2D

    cmdBindBuffers commandBuffer
    cmdBindPipeline commandBuffer pipeline
    cmdBindDescriptorSets commandBuffer pipelineLayout pipeline [ descriptorSet ]

    liftIO $
      Vulkan.cmdDrawIndexed
        commandBuffer
        nbIndices
        1 -- instance count
        0 -- offset into index buffer
        0 -- offset into vertex buffer
        0 -- first instance ID

    cmdEndRenderPass commandBuffer

    case mbScreenshotImage of
      Just (screenshotImage, extent3D) ->
        cmdTakeScreenshot
          ( Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, Vulkan.ACCESS_COLOR_ATTACHMENT_WRITE_BIT )
          commandBuffer extent3D
          ( swapchainImage,
            ( Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
            , Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
            )
          )
          screenshotImage
      Nothing -> pure () -- no image transitions to do manually, assumed to be taken care of by the render pass

    endCommandBuffer commandBuffer

    pure res

recordSavedDrawCall
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPool
  -> Vulkan.Framebuffer
  -> (Vulkan.RenderPass, [Vulkan.ClearValue])
  -> Vulkan.DescriptorSet
  -> ( Vulkan.CommandBuffer -> m () )
  -> ( Vulkan.Image, Vulkan.Extent2D )
  -> ( Vulkan.Image, Vulkan.Extent3D )
  -> Maybe (Vulkan.Image, Vulkan.Extent3D)
  -> Word32
  -> Vulkan.PipelineLayout
  -> VkPipeline
  -> m (ReleaseKey, Vulkan.CommandBuffer)
recordSavedDrawCall
  dev commandPool framebuffer (renderPass, clearValues)
  descriptorSet cmdBindBuffers
  (swapchainImage, extent2D)
  (saveImage, extent3D) mbScreenshotImage
  nbIndices pipelineLayout pipeline
  = do

    res@(_ , commandBuffer) <- allocateCommandBuffer dev commandPool

    beginCommandBuffer commandBuffer

    case mbScreenshotImage of
      Nothing -> do

        cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues extent2D

        cmdBindBuffers commandBuffer
        cmdBindPipeline commandBuffer pipeline
        cmdBindDescriptorSets commandBuffer pipelineLayout pipeline [ descriptorSet ]

        liftIO $
          Vulkan.cmdDrawIndexed
            commandBuffer
            nbIndices
            1 -- instance count
            0 -- offset into index buffer
            0 -- offset into vertex buffer
            0 -- first instance ID

        cmdEndRenderPass commandBuffer

        cmdImageCopy commandBuffer swapchainImage ImageSave saveImage extent3D

      Just (screenshotImage, extent3D') -> do
        cmdImageCopy commandBuffer saveImage ImageRestore swapchainImage extent3D

        cmdTakeScreenshot
          ( Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, Vulkan.ACCESS_COLOR_ATTACHMENT_WRITE_BIT )
          commandBuffer extent3D'
          ( swapchainImage,
            ( Vulkan.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
            , Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
            )
          )
          screenshotImage

    endCommandBuffer commandBuffer

    pure res

recordRestoreSavedImage
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPool
  -> Vulkan.Image
  -> Vulkan.Image
  -> Vulkan.Extent3D
  -> m (ReleaseKey, Vulkan.CommandBuffer)
recordRestoreSavedImage dev commandPool srcImage dstImage extent3D = do
    res@(_ , commandBuffer) <- allocateCommandBuffer dev commandPool
    beginCommandBuffer commandBuffer

    cmdImageCopy commandBuffer srcImage ImageRestore dstImage extent3D

    endCommandBuffer commandBuffer
    pure res


data CopyMode = ImageSave | ImageRestore

cmdImageCopy
  :: MonadVulkan m
  => Vulkan.CommandBuffer
  -> Vulkan.Image
  -> CopyMode
  -> Vulkan.Image
  -> Vulkan.Extent3D
  -> m ()
cmdImageCopy commandBuffer srcImage imageResult dstImage extent3D = do
    let
      transitionLayout :: MonadVulkan m => Vulkan.Image -> Vulkan.ImageLayout -> Vulkan.ImageLayout -> m ()
      transitionLayout image from to =
        cmdTransitionImageLayout
          commandBuffer image from to
          ( Vulkan.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.zero )
          ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.zero )

    transitionLayout srcImage srcLayout Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    transitionLayout dstImage Vulkan.IMAGE_LAYOUT_UNDEFINED Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

    doCopyImage commandBuffer extent3D srcImage dstImage

    transitionLayout dstImage Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL dstLayout
    transitionLayout srcImage Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL srcLayout

    where
      srcLayout = case imageResult of
        ImageSave -> Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
        ImageRestore -> Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      dstLayout = case imageResult of
        ImageSave -> Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        ImageRestore -> Vulkan.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

doCopyImage
  :: MonadVulkan m
  => Vulkan.CommandBuffer
  -> Vulkan.Extent3D
  -> Vulkan.Image
  -> Vulkan.Image
  -> m ()
doCopyImage commandBuffer extent3D srcImage dstImage =
    Vulkan.cmdCopyImage
      commandBuffer
      srcImage
      Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      dstImage
      Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      ( Boxed.Vector.singleton imageCopy )
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
        , Vulkan.extent         = extent3D
        }



recordSimpleDispatch
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.CommandPool
  -> Vulkan.DescriptorSet
  -> ( Vulkan.Image, Vulkan.Extent2D)
  -> Maybe (Vulkan.Image, Vulkan.Extent3D)
  -> (Word32, Word32, Word32)
  -> Vulkan.PipelineLayout
  -> VkPipeline
  -> m (ReleaseKey, Vulkan.CommandBuffer)
recordSimpleDispatch
  dev commandPool
  descriptorSet
  (swapchainImage, _) mbScreenshotImage
  (globalSize_x, globalSize_y, globalSize_z)
  pipelineLayout pipeline
  = do
    res@(_, commandBuffer) <- allocateCommandBuffer dev commandPool

    beginCommandBuffer commandBuffer

    cmdTransitionImageLayout commandBuffer swapchainImage
      Vulkan.IMAGE_LAYOUT_UNDEFINED
      Vulkan.IMAGE_LAYOUT_GENERAL
      ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.zero )
      ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_WRITE_BIT )

    cmdBindPipeline commandBuffer pipeline
    cmdBindDescriptorSets commandBuffer pipelineLayout pipeline [ descriptorSet ]

    liftIO $
      Vulkan.cmdDispatch
        commandBuffer
        globalSize_x globalSize_y globalSize_z

    case mbScreenshotImage of
      Just (screenshotImage, extent3D)
        -> cmdTakeScreenshot
              ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_WRITE_BIT )
              commandBuffer extent3D
              ( swapchainImage,
                ( Vulkan.IMAGE_LAYOUT_GENERAL
                , Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
                )
              )
              screenshotImage
      Nothing
        -> cmdTransitionImageLayout commandBuffer swapchainImage
              Vulkan.IMAGE_LAYOUT_GENERAL
              Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
              ( Vulkan.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.zero )
              ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.zero )

    endCommandBuffer commandBuffer

    pure res



simpleRenderPass
  :: MonadVulkan m
  => Vulkan.Device
  -> SubpassAttachments (Vulkan.AttachmentDescription, AttachmentType)
  -> m Vulkan.RenderPass
simpleRenderPass dev attachments = snd <$> Vulkan.withRenderPass dev createInfo Nothing allocate
  where

    attachmentReferences   :: SubpassAttachmentReferences
    attachmentDescriptions :: [ Vulkan.AttachmentDescription ]
    ( attachmentReferences, attachmentDescriptions )
      = attachmentReferencesAndDescriptions attachments

    subpass :: Vulkan.SubpassDescription
    subpass = createSubpass attachmentReferences

    dependency1 :: Vulkan.SubpassDependency
    dependency1 =
      Vulkan.SubpassDependency
        { Vulkan.srcSubpass      = Vulkan.SUBPASS_EXTERNAL
        , Vulkan.dstSubpass      = Vulkan.zero
        , Vulkan.srcStageMask    = Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vulkan.srcAccessMask   = Vulkan.zero
        , Vulkan.dstStageMask    = Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vulkan.dstAccessMask   =
              (    Vulkan.ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
        , Vulkan.dependencyFlags = Vulkan.zero
        }

    dependency2 :: Vulkan.SubpassDependency
    dependency2 =
      Vulkan.SubpassDependency
        { Vulkan.srcSubpass      = Vulkan.zero
        , Vulkan.dstSubpass      = Vulkan.SUBPASS_EXTERNAL
        , Vulkan.srcStageMask    = Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vulkan.srcAccessMask   =
              (    Vulkan.ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
        , Vulkan.dstStageMask    = Vulkan.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        , Vulkan.dstAccessMask   = Vulkan.zero
        , Vulkan.dependencyFlags = Vulkan.zero
        }

    createInfo :: Vulkan.RenderPassCreateInfo '[]
    createInfo =
      Vulkan.RenderPassCreateInfo
        { Vulkan.next         = ()
        , Vulkan.flags        = Vulkan.zero
        , Vulkan.attachments  = Boxed.Vector.fromList attachmentDescriptions
        , Vulkan.subpasses    = Boxed.Vector.singleton subpass
        , Vulkan.dependencies = Boxed.Vector.fromList [ dependency1, dependency2 ]
        }

----------------------------------------------------------------------------
-- Specify the resource management that needs to take place
-- when reloading shaders:
--  * create a new pipeline using the new shaders,
--  * record new command buffers using this new pipeline.


record2CommandBuffersFromShaders
  :: ( Traversable t, MonadVulkan m )
  => ( t Vulkan.ShaderModule -> m ( ReleaseKey, pipeline ) )
  -> ( pipeline -> m ( V.Vector i ( ReleaseKey, commands ) ) )
  -> ( pipeline -> m ( V.Vector i ( ReleaseKey, commands ) ) )
  -> t Vulkan.ShaderModule
  -> m ( m (), ( V.Vector i commands, V.Vector i commands) )
record2CommandBuffersFromShaders
  createPipeline
  recordCommandBuffers1
  recordCommandBuffers2
  shaders = do
    ( pipeKey , pipeline  ) <- createPipeline shaders
    ( cmdKeys1, commands1 ) <- V.unzip <$> recordCommandBuffers1 pipeline
    ( cmdKeys2, commands2 ) <- V.unzip <$> recordCommandBuffers2 pipeline
    let releaseResources
          =  release pipeKey
          *> traverse_ release cmdKeys1
          *> traverse_ release cmdKeys2
    pure ( releaseResources, ( commands1, commands2 ) )


record3CommandBuffersFromShaders
  :: ( Traversable t, MonadVulkan m )
  => ( t Vulkan.ShaderModule -> m ( ReleaseKey, pipeline ) )
  -> ( pipeline -> m ( V.Vector i ( ReleaseKey, commands ) ) )
  -> ( pipeline -> m ( V.Vector i ( ReleaseKey, commands ) ) )
  -> ( pipeline -> m ( V.Vector i ( ReleaseKey, commands ) ) )
  -> t Vulkan.ShaderModule
  -> m ( m (), ( V.Vector i commands, V.Vector i commands, V.Vector i commands) )
record3CommandBuffersFromShaders
  createPipeline
  recordCommandBuffers1
  recordCommandBuffers2
  recordCommandBuffers3
  shaders = do
    ( pipeKey , pipeline  ) <- createPipeline shaders
    ( cmdKeys1, commands1 ) <- V.unzip <$> recordCommandBuffers1 pipeline
    ( cmdKeys2, commands2 ) <- V.unzip <$> recordCommandBuffers2 pipeline
    ( cmdKeys3, commands3 ) <- V.unzip <$> recordCommandBuffers3 pipeline
    let releaseResources
          =  release pipeKey
          *> traverse_ release cmdKeys1
          *> traverse_ release cmdKeys2
          *> traverse_ release cmdKeys3
    pure ( releaseResources, ( commands1, commands2, commands3 ) )
