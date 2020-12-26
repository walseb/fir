{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}

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
