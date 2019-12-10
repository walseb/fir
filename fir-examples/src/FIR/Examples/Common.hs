{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

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
  ( ReleaseKey, release )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( Vector, unzip )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

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
  => Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> Vulkan.VkFramebuffer
  -> (Vulkan.VkRenderPass, [Vulkan.VkClearValue])
  -> Vulkan.VkDescriptorSet
  -> ( Vulkan.VkCommandBuffer -> m () )
  -> ( Vulkan.VkImage, Vulkan.VkExtent2D)
  -> Maybe (Vulkan.VkImage, Vulkan.VkExtent3D)
  -> Word32
  -> Vulkan.VkPipelineLayout
  -> VkPipeline
  -> m (ReleaseKey, Vulkan.VkCommandBuffer)
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
      Vulkan.vkCmdDrawIndexed
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
          ( Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT )
          commandBuffer extent3D
          ( swapchainImage,
            ( Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
            , Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
            )
          )
          screenshotImage
      Nothing -> pure () -- no image transitions to do manually, assumed to be taken care of by the render pass

    endCommandBuffer commandBuffer

    pure res


recordSimpleDispatch
  :: MonadVulkan m
  => Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> Vulkan.VkDescriptorSet
  -> ( Vulkan.VkImage, Vulkan.VkExtent2D)
  -> Maybe (Vulkan.VkImage, Vulkan.VkExtent3D)
  -> (Word32, Word32, Word32)
  -> Vulkan.VkPipelineLayout
  -> VkPipeline
  -> m (ReleaseKey, Vulkan.VkCommandBuffer)
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
      Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
      Vulkan.VK_IMAGE_LAYOUT_GENERAL
      ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
      ( Vulkan.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.VK_ACCESS_SHADER_WRITE_BIT )

    cmdBindPipeline commandBuffer pipeline
    cmdBindDescriptorSets commandBuffer pipelineLayout pipeline [ descriptorSet ]

    liftIO $
      Vulkan.vkCmdDispatch
        commandBuffer
        globalSize_x globalSize_y globalSize_z

    case mbScreenshotImage of
      Just (screenshotImage, extent3D)
        -> cmdTakeScreenshot
              ( Vulkan.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.VK_ACCESS_SHADER_WRITE_BIT )
              commandBuffer extent3D
              ( swapchainImage,
                ( Vulkan.VK_IMAGE_LAYOUT_GENERAL
                , Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                )
              )
              screenshotImage
      Nothing
        -> cmdTransitionImageLayout commandBuffer swapchainImage
              Vulkan.VK_IMAGE_LAYOUT_GENERAL
              Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
              ( Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
              ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.VK_ZERO_FLAGS )

    endCommandBuffer commandBuffer

    pure res



simpleRenderPass
  :: MonadVulkan m
  => Vulkan.VkDevice
  -> SubpassAttachments (Vulkan.VkAttachmentDescription, AttachmentType)
  -> m Vulkan.VkRenderPass
simpleRenderPass dev attachments =
  let

    attachmentReferences   :: SubpassAttachmentReferences
    attachmentDescriptions :: [ Vulkan.VkAttachmentDescription ]
    ( attachmentReferences, attachmentDescriptions )
      = attachmentReferencesAndDescriptions attachments

    subpass :: Vulkan.VkSubpassDescription
    subpass = createSubpass attachmentReferences

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
              attachmentDescriptions
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
    managedVulkanResource createInfo
      ( Vulkan.vkCreateRenderPass  dev )
      ( Vulkan.vkDestroyRenderPass dev )


----------------------------------------------------------------------------
-- Specify the resource management that needs to take place
-- when reloading shaders:
--  * create a new pipeline using the new shaders,
--  * record new command buffers using this new pipeline.


record2CommandBuffersFromShaders
  :: ( Traversable t, MonadVulkan m )
  => ( t Vulkan.VkShaderModule -> m ( ReleaseKey, pipeline ) )
  -> ( pipeline -> m ( V.Vector i ( ReleaseKey, commands ) ) )
  -> ( pipeline -> m ( V.Vector i ( ReleaseKey, commands ) ) )
  -> t Vulkan.VkShaderModule
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
