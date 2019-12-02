{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Vulkan.Attachment where

-- base
import Data.Word
  ( Word32 )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir-examples
import Data.Traversable.Indexed
  ( iunzipWith )

---------------------------------------------------------------
-- Attachment types and their corresponding image layouts.

data AttachmentAccess
  = ReadAttachment
  | ReadWriteAttachment
  deriving stock ( Eq, Show )

data DepthStencilType =
  DepthStencilType
    { depth   :: Maybe AttachmentAccess
    , stencil :: Maybe AttachmentAccess
    }
  deriving stock ( Eq, Show )

data InputAttachmentType
  = ColorInputAttachment
  | DepthInputAttachment
  | StencilInputAttachment
  | DepthStencilInputAttachment
  deriving stock ( Eq, Show )

data AttachmentType
  = ColorAttachment
  | DepthStencilAttachment DepthStencilType
  | InputAttachment InputAttachmentType
  deriving stock ( Eq, Show )

data AttachmentUsage
  = UseAttachment
  | PreserveAttachment
  | ResolveAttachment
  deriving stock ( Eq, Show )


depthStencilAttachmentLayout :: DepthStencilType -> Vulkan.VkImageLayout
depthStencilAttachmentLayout _ = Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
{-
depthStencilAttachmentLayout
  DepthStencilType Nothing Nothing
    = Vulkan.VK_IMAGE_LAYOUT_GENERAL
depthStencilAttachmentLayout
  DepthStencilType (Just ReadAttachment) Nothing
    = Vulkan.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR
depthStencilAttachmentLayout
  DepthStencilType (Just ReadWriteAttachment) Nothing
    = Vulkan.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR
depthStencilAttachmentLayout
  DepthStencilType Nothing (Just ReadAttachment)
    = Vulkan.VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR
depthStencilAttachmentLayout
  DepthStencilType Nothing (Just ReadWriteAttachment)
    = Vulkan.VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR
depthStencilAttachmentLayout
  DepthStencilType (Just ReadAttachment) (Just ReadAttachment)
    = Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL_KHR
depthStencilAttachmentLayout
  DepthStencilType (Just ReadWriteAttachment) (Just ReadAttachment)
    = Vulkan.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
depthStencilAttachmentLayout
  DepthStencilType (Just ReadAttachment) (Just WriteAttachment)
    = Vulkan.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
depthStencilAttachmentLayout
  DepthStencilType (Just ReadWriteAttachment) (Just WriteAttachment)
    = Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
-}

inputAttachmentLayout :: InputAttachmentType -> Vulkan.VkImageLayout
inputAttachmentLayout ColorInputAttachment
  = Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
inputAttachmentLayout DepthInputAttachment
  = depthStencilAttachmentLayout ( DepthStencilType (Just ReadAttachment) Nothing )
inputAttachmentLayout StencilInputAttachment
  = depthStencilAttachmentLayout ( DepthStencilType Nothing (Just ReadAttachment) )
inputAttachmentLayout DepthStencilInputAttachment
  = depthStencilAttachmentLayout ( DepthStencilType (Just ReadAttachment) (Just ReadAttachment) )

attachmentLayout :: AttachmentType -> Vulkan.VkImageLayout
attachmentLayout ColorAttachment
  = Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
attachmentLayout (DepthStencilAttachment depthStencilType)
  = depthStencilAttachmentLayout depthStencilType
attachmentLayout (InputAttachment inputAttachmentType)
  = inputAttachmentLayout inputAttachmentType

---------------------------------------------------------------
-- Some simple attachment descriptions, for convenience.

presentableColorAttachmentDescription :: Vulkan.VkFormat -> ( Vulkan.VkAttachmentDescription, AttachmentType )
presentableColorAttachmentDescription colorFormat =
  ( description, ColorAttachment )
  where
    description =
      Vulkan.createVk
        (  Vulkan.set @"flags"          Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"format"         colorFormat
        &* Vulkan.set @"samples"        Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"loadOp"         Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
        &* Vulkan.set @"storeOp"        Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        &* Vulkan.set @"stencilLoadOp"  Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* Vulkan.set @"initialLayout"  Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        &* Vulkan.set @"finalLayout"    Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
        )

depthAttachmentDescription :: Vulkan.VkFormat -> ( Vulkan.VkAttachmentDescription, AttachmentType )
depthAttachmentDescription depthFormat =
  ( description, DepthStencilAttachment ( DepthStencilType (Just ReadWriteAttachment) Nothing ) )
    where
      description =
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

msDepthAttachmentDescription
  :: Vulkan.VkSampleCountFlagBits
  -> Vulkan.VkFormat
  -> ( Vulkan.VkAttachmentDescription, AttachmentType )
msDepthAttachmentDescription samples depthFormat =
  ( description, DepthStencilAttachment ( DepthStencilType (Just ReadWriteAttachment) Nothing ) )
    where
      description =
        Vulkan.createVk
          (  Vulkan.set @"flags"          Vulkan.VK_ZERO_FLAGS
          &* Vulkan.set @"format"         depthFormat
          &* Vulkan.set @"samples"        samples
          &* Vulkan.set @"loadOp"         Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
          &* Vulkan.set @"storeOp"        Vulkan.VK_ATTACHMENT_STORE_OP_STORE
          &* Vulkan.set @"stencilLoadOp"  Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
          &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
          &* Vulkan.set @"initialLayout"  Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
          &* Vulkan.set @"finalLayout"    Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          )

msColorAttachmentDescription
  :: Vulkan.VkSampleCountFlagBits
  -> Vulkan.VkFormat
  -> ( Vulkan.VkAttachmentDescription, AttachmentType )
msColorAttachmentDescription samples colorFormat =
  ( description, ColorAttachment )
    where
      description =
        Vulkan.createVk
          (  Vulkan.set @"flags"          Vulkan.VK_ZERO_FLAGS
          &* Vulkan.set @"format"         colorFormat
          &* Vulkan.set @"samples"        samples
          &* Vulkan.set @"loadOp"         Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
          &* Vulkan.set @"storeOp"        Vulkan.VK_ATTACHMENT_STORE_OP_STORE
          &* Vulkan.set @"stencilLoadOp"  Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
          &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
          &* Vulkan.set @"initialLayout"  Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
          &* Vulkan.set @"finalLayout"    Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          )

---------------------------------------------------------------
-- Set the attachments in a subpass.

type SubpassAttachmentInfo
  = '[ "colorAttachmentCount", "pColorAttachments"
     , "pDepthStencilAttachment"
     , "inputAttachmentCount", "pInputAttachments"
     , "preserveAttachmentCount", "pPreserveAttachments"
     , "pResolveAttachments"
     ]

data SubpassAttachments a
  = SubpassAttachments
  { colorAttachments         :: [ a ]
  , mbDepthStencilAttachment :: Maybe a
  , inputAttachments         :: [ a ]
  , preserveAttachments      :: [ a ]
  , resolveAttachments       :: [ a ]
  } deriving stock ( Functor, Foldable, Traversable )

type SubpassAttachmentReferences = SubpassAttachments Vulkan.VkAttachmentReference


noAttachments :: SubpassAttachments a
noAttachments = SubpassAttachments [] Nothing [] [] []


createSubpass
  :: SubpassAttachmentReferences
  -> Vulkan.VkSubpassDescription
createSubpass SubpassAttachments { .. } =
  Vulkan.createVk
    (  Vulkan.set @"flags"
          Vulkan.VK_ZERO_FLAGS
    &* Vulkan.setListCountAndRef @"colorAttachmentCount" @"pColorAttachments"
          colorAttachments
    &* Vulkan.set @"pipelineBindPoint"
          Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
    &* setDepthStencilRef
    &* Vulkan.setListCountAndRef @"inputAttachmentCount"    @"pInputAttachments"
          inputAttachments
    &* Vulkan.setListCountAndRef @"preserveAttachmentCount" @"pPreserveAttachments"
          ( map (Vulkan.getField @"attachment") preserveAttachments )
    &* Vulkan.setListRef @"pResolveAttachments"
          resolveAttachments
    )
      where
        setDepthStencilRef :: Vulkan.CreateVkStruct Vulkan.VkSubpassDescription '[ "pDepthStencilAttachment" ] ()
        setDepthStencilRef = case mbDepthStencilAttachment of
          Nothing              -> Vulkan.set      @"pDepthStencilAttachment" Vulkan.vkNullPtr
          Just depthStencilRef -> Vulkan.setVkRef @"pDepthStencilAttachment" depthStencilRef

attachmentReference :: Word32 -> AttachmentType -> Vulkan.VkAttachmentReference
attachmentReference attachmentNumber attachmentType  =
  Vulkan.createVk
    (  Vulkan.set @"attachment" attachmentNumber
    &* Vulkan.set @"layout"     ( attachmentLayout attachmentType )
    )

attachmentReferencesAndDescriptions
  :: forall t. Traversable t
  => t ( Vulkan.VkAttachmentDescription, AttachmentType )
  -> ( t Vulkan.VkAttachmentReference, [ Vulkan.VkAttachmentDescription ] )
attachmentReferencesAndDescriptions =
  iunzipWith
    ( \ i -> attachmentReference i . snd )
    ( const fst )
