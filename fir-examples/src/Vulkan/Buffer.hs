{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Buffer where

-- base
import Control.Monad
  ( (>=>) )
import Data.Coerce
  ( coerce )
import Foreign.Ptr
  ( Ptr )

-- resourcet
import Control.Monad.Trans.Resource
  ( allocate )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir
import FIR
  ( Poke(..), Layout(Base, Extended, Locations), pokeArray, roundUp )

-- fir-examples
import Vulkan.Memory
import Vulkan.Monad

-----------------------------------------------------------------------------------------------------

createVertexBuffer
  :: forall a m
  .  ( MonadVulkan m, Poke a Locations )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ a ]
  -> m (Vulkan.VkBuffer, [a] -> IO (), Int )
createVertexBuffer
  = createBufferFromList @a @Locations
      Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT

createIndexBuffer
  :: forall a m
  .  ( MonadVulkan m, Poke a Base )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ a ]
  -> m (Vulkan.VkBuffer, [a] -> IO (), Int )
createIndexBuffer
  = createBufferFromList @a @Base
      Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT

createUniformBuffer
  :: forall a m
  .  ( MonadVulkan m, Poke a Extended )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> a
  -> m (Vulkan.VkBuffer, a -> IO () )
createUniformBuffer
  = createBuffer @a @Extended
      Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT

createBuffer
  :: forall a ali m.
     ( MonadVulkan m, Poke a ali )
  => Vulkan.VkBufferUsageFlags
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> a
  -> m (Vulkan.VkBuffer, a -> IO ())
createBuffer usage physicalDevice device elems = do
  ( buf, bufPtr ) <-
    createBufferFromPoke
      usage
      physicalDevice
      device
      ( \memPtr -> pokeFunction memPtr elems )
      ( fromIntegral (sizeOf @a @ali) )
  pure ( buf, pokeFunction bufPtr )
    where
      pokeFunction = poke @a @ali

createBufferFromList
  :: forall a ali m.
     ( MonadVulkan m, Poke a ali )
  => Vulkan.VkBufferUsageFlags
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> [ a ]
  -> m (Vulkan.VkBuffer, [a] -> IO (), Int )
createBufferFromList usage physicalDevice device elems = do
  ( buf, bufPtr ) <-
    createBufferFromPoke
      usage
      physicalDevice
      device
      ( \memPtr -> pokeFunction memPtr elems )
      ( fromIntegral nbElems * fromIntegral (sizeOf @a @ali) )
  pure ( buf, pokeFunction bufPtr, nbElems )
    where
      nbElems = length elems
      pokeFunction = pokeArray @a @ali

createBufferFromPoke
  :: MonadVulkan m
  => Vulkan.VkBufferUsageFlags
  -> Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> (Ptr a -> IO ())
  -> Vulkan.VkDeviceSize
  -> m (Vulkan.VkBuffer, Ptr a)
createBufferFromPoke usage physicalDevice device poking sizeInBytes =
  let

    roundedSize = sizeInBytes `roundUp` 64 -- nonCoherentAtomSize

    bufferCreateInfo :: Vulkan.VkBufferCreateInfo
    bufferCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"size"  roundedSize
        &* Vulkan.set @"usage" usage
        &* Vulkan.set @"sharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
        &* Vulkan.set @"queueFamilyIndexCount" 0
        &* Vulkan.set @"pQueueFamilyIndices" Vulkan.VK_NULL
        )
  in do
    ( _, buffer :: Vulkan.VkBuffer )
      <- allocateVulkanResource bufferCreateInfo
          ( Vulkan.vkCreateBuffer  device )
          ( Vulkan.vkDestroyBuffer device )

    memReqs :: Vulkan.VkMemoryRequirements
      <- allocaAndPeek
          ( Vulkan.vkGetBufferMemoryRequirements device buffer )

    let requiredFlags :: [ Vulkan.VkMemoryPropertyFlags ]
        requiredFlags = [ Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                        , Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
                        ]

    ( _, memory :: Vulkan.VkDeviceMemory )
      <- allocateMemory physicalDevice device memReqs requiredFlags

    (_, ptr) <- allocate
      ( do
          Vulkan.vkBindBufferMemory device buffer memory 0
            >>= throwVkResult

          memPtr :: Ptr a
             <- coerce <$> allocaAndPeek
                  (   Vulkan.vkMapMemory device memory 0 roundedSize Vulkan.VK_ZERO_FLAGS
                  >=> throwVkResult
                  )

          poking memPtr
          pure memPtr
      )
      ( \_ -> Vulkan.vkUnmapMemory device memory )

    pure (buffer, ptr)
