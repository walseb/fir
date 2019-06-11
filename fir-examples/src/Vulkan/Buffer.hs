{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Vulkan.Buffer where

-- base
import Control.Monad
  ( (>=>) )
import Data.Coerce
  ( coerce )

-- managed
import Control.Monad.Managed
  ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir
import FIR
  ( Poke(..), Layout(Base, Extended, Locations), pokeArray )

-- fir-examples
import Vulkan.Memory
import Vulkan.Monad

-----------------------------------------------------------------------------------------------------

createVertexBuffer
  :: forall a m.
     ( MonadManaged m, Poke a Locations )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ a ]
  -> m (Vulkan.VkBuffer, Vulkan.Ptr a)
createVertexBuffer
  = createBufferFromList @a @Locations
      Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT


createIndexBuffer
  :: forall a m.
     ( MonadManaged m, Poke a Base )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ a ]
  -> m (Vulkan.VkBuffer, Vulkan.Ptr a)
createIndexBuffer
  = createBufferFromList @a @Base
      Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT


createBufferFromList
  :: forall a ali m.
     ( MonadManaged m, Poke a ali )
  => Vulkan.VkBufferUsageBitmask Vulkan.FlagMask
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ a ]
  -> m (Vulkan.VkBuffer, Vulkan.Ptr a)
createBufferFromList usage physicalDevice device elems =
  createBuffer
    device
    physicalDevice
    usage
    ( \memPtr -> pokeArray @a @ali memPtr elems )
    ( fromIntegral ( length elems ) * fromIntegral (sizeOf @a @ali) )


createUniformBuffer
  :: forall a m.
     ( MonadManaged m, Poke a Extended )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> a
  -> m (Vulkan.VkBuffer, Vulkan.Ptr a)
createUniformBuffer physicalDevice device bufferData =
  createBuffer
    device
    physicalDevice
    Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
    ( \memPtr -> poke @a @Extended memPtr bufferData )
    ( fromIntegral ( sizeOf @a @Extended ) )


createBuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkBufferUsageBitmask Vulkan.FlagMask
  -> (Vulkan.Ptr a -> IO ())
  -> Vulkan.VkDeviceSize
  -> m (Vulkan.VkBuffer, Vulkan.Ptr a)
createBuffer device physicalDevice usage poking sizeInBytes =
  let
    createInfo :: Vulkan.VkBufferCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"size" sizeInBytes
        &* Vulkan.set @"usage" usage
        &* Vulkan.set @"sharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
        &* Vulkan.set @"queueFamilyIndexCount" 0
        &* Vulkan.set @"pQueueFamilyIndices" Vulkan.VK_NULL
        )
  in do
    buffer :: Vulkan.VkBuffer
      <- managedVulkanResource
          ( Vulkan.vkCreateBuffer device ( Vulkan.unsafePtr createInfo ) )
          ( Vulkan.vkDestroyBuffer device )

    memReqs :: Vulkan.VkMemoryRequirements
      <- allocaAndPeek
          ( Vulkan.vkGetBufferMemoryRequirements device buffer )

    let requiredFlags :: [ Vulkan.VkMemoryPropertyFlags ]
        requiredFlags = [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                        , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
                        ]

    memory :: Vulkan.VkDeviceMemory
      <- allocateMemory physicalDevice device memReqs requiredFlags

    ptr <- manageBracket
      ( do
          Vulkan.vkBindBufferMemory device buffer memory 0
            >>= throwVkResult

          memPtr :: Vulkan.Ptr a
             <- coerce <$> allocaAndPeek
                  ( Vulkan.vkMapMemory device memory 0 sizeInBytes 0 >=> throwVkResult )

          poking memPtr
          pure memPtr
      )
      ( \_ -> Vulkan.vkUnmapMemory device memory )

    pure (buffer, ptr)
