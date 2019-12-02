{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Common where

-- base
import Data.Bits
  ( (.|.) )
import Data.Word
  ( Word32 )

-- managed
import Control.Monad.Managed
  ( MonadManaged )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

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
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> Vulkan.VkFramebuffer
  -> (Vulkan.VkRenderPass, [Vulkan.VkClearValue])
  -> Vulkan.VkDescriptorSet
  -> ( Vulkan.VkCommandBuffer -> m () )
  -> ( Vulkan.VkImage, Vulkan.VkExtent2D)
  -> Maybe (Vulkan.VkImage, Vulkan.VkExtent3D)
  -> Word32
  -> VkPipeline
  -> m Vulkan.VkCommandBuffer
recordSimpleIndexedDrawCall
  dev commandPool framebuffer (renderPass, clearValues)
  descriptorSet cmdBindBuffers
  (swapchainImage, extent2D) mbScreenshotImage
  nbIndices vkPipeline
  = do

    commandBuffer <- allocateCommandBuffer dev commandPool

    beginCommandBuffer commandBuffer

    cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues extent2D

    cmdBindBuffers commandBuffer
    cmdBindPipeline commandBuffer vkPipeline
    cmdBindDescriptorSets commandBuffer vkPipeline [ descriptorSet ]

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
      Nothing -> pure () -- no image transitions to do manually, taken care of by the render pass

    endCommandBuffer commandBuffer

    pure commandBuffer


recordSimpleDispatch
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkCommandPool
  -> Vulkan.VkDescriptorSet
  -> ( Vulkan.VkImage, Vulkan.VkExtent2D)
  -> Maybe (Vulkan.VkImage, Vulkan.VkExtent3D)
  -> (Word32, Word32, Word32)
  -> VkPipeline
  -> m Vulkan.VkCommandBuffer
recordSimpleDispatch
  dev commandPool
  descriptorSet
  (swapchainImage, _) mbScreenshotImage
  (globalSize_x, globalSize_y, globalSize_z) vkPipeline
  = do
    commandBuffer <- allocateCommandBuffer dev commandPool

    beginCommandBuffer commandBuffer

    cmdTransitionImageLayout commandBuffer swapchainImage
      Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
      Vulkan.VK_IMAGE_LAYOUT_GENERAL
      ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
      ( Vulkan.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.VK_ACCESS_SHADER_WRITE_BIT )

    cmdBindPipeline commandBuffer vkPipeline
    cmdBindDescriptorSets commandBuffer vkPipeline [ descriptorSet ]

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

    pure commandBuffer



simpleRenderPass
  :: MonadManaged m
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
    managedVulkanResource
      ( Vulkan.vkCreateRenderPass  dev ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyRenderPass dev )
