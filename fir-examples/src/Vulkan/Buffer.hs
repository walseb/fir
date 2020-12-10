{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Vulkan.Buffer where

-- base
import Data.Coerce
  ( coerce )
import Foreign.Ptr
  ( Ptr )

-- resourcet
import Control.Monad.Trans.Resource
  ( allocate )

-- vector
import qualified Data.Vector as Boxed.Vector
  ( empty )

-- vulkan
import qualified Vulkan

-- fir
import FIR
  ( Poke(..), Layout(Base, Extended, Locations)
  , pokeArrayOff, roundUp
  )

-- fir-examples
import Vulkan.Memory
import Vulkan.Monad

-----------------------------------------------------------------------------------------------------

createVertexBuffer
  :: forall a m
  .  ( MonadVulkan m, Poke a Locations )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> [ a ]
  -> m (Vulkan.Buffer, Int -> [a] -> IO (), Int)
createVertexBuffer
  = createBufferFromList @a @Locations
      Vulkan.BUFFER_USAGE_VERTEX_BUFFER_BIT

createIndexBuffer
  :: forall a m
  .  ( MonadVulkan m, Poke a Base )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> [ a ]
  -> m (Vulkan.Buffer, Int -> [a] -> IO (), Int)
createIndexBuffer
  = createBufferFromList @a @Base
      Vulkan.BUFFER_USAGE_INDEX_BUFFER_BIT

createUniformBuffer
  :: forall a m
  .  ( MonadVulkan m, Poke a Extended )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> a
  -> m (Vulkan.Buffer, a -> IO ())
createUniformBuffer
  = createBuffer @a @Extended
      Vulkan.BUFFER_USAGE_UNIFORM_BUFFER_BIT

createBuffer
  :: forall a ali m.
     ( MonadVulkan m, Poke a ali )
  => Vulkan.BufferUsageFlags
  -> Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> a
  -> m (Vulkan.Buffer, a -> IO ())
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
  => Vulkan.BufferUsageFlags
  -> Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> [ a ]
  -> m (Vulkan.Buffer, Int -> [a] -> IO (), Int)
createBufferFromList usage physicalDevice device elems = do
  ( buf, bufPtr ) <-
    createBufferFromPoke
      usage
      physicalDevice
      device
      ( \memPtr -> pokeFunction memPtr 0 elems )
      ( fromIntegral nbElems * fromIntegral (sizeOf @a @ali) )
  pure ( buf, pokeFunction bufPtr, nbElems )
    where
      nbElems = length elems
      pokeFunction = pokeArrayOff @a @ali

createBufferFromPoke
  :: MonadVulkan m
  => Vulkan.BufferUsageFlags
  -> Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> (Ptr a -> IO ())
  -> Vulkan.DeviceSize
  -> m (Vulkan.Buffer, Ptr a)
createBufferFromPoke usage physicalDevice device poking sizeInBytes =
  let

    roundedSize = sizeInBytes `roundUp` 64 -- nonCoherentAtomSize

    bufferCreateInfo :: Vulkan.BufferCreateInfo '[]
    bufferCreateInfo =
      Vulkan.BufferCreateInfo
        { Vulkan.next               = ()
        , Vulkan.flags              = Vulkan.zero
        , Vulkan.size               = roundedSize
        , Vulkan.usage              = usage
        , Vulkan.sharingMode        = Vulkan.SHARING_MODE_EXCLUSIVE
        , Vulkan.queueFamilyIndices = Boxed.Vector.empty
        }
  in do
    ( _, buffer :: Vulkan.Buffer )
      <- Vulkan.withBuffer device bufferCreateInfo Nothing allocate

    memReqs :: Vulkan.MemoryRequirements
      <- Vulkan.getBufferMemoryRequirements device buffer

    let requiredFlags :: [ Vulkan.MemoryPropertyFlags ]
        requiredFlags = [ Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT
                        , Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT
                        ]

    ( _, memory :: Vulkan.DeviceMemory )
      <- allocateMemory physicalDevice device memReqs requiredFlags

    (_, ptr) <- allocate
      ( do
          Vulkan.bindBufferMemory device buffer memory 0

          memPtr :: Ptr a
             <- coerce <$> Vulkan.mapMemory device memory 0 roundedSize Vulkan.zero

          poking memPtr
          pure memPtr
      )
      ( \_ -> Vulkan.unmapMemory device memory )

    pure (buffer, ptr)
