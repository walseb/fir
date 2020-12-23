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
import Data.Bits
  ( (.|.) )
import Data.Coerce
  ( coerce )
import Foreign.Ptr
  ( Ptr )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, allocate )

-- vector
import qualified Data.Vector as Boxed.Vector
  ( empty )

-- vulkan
import qualified Vulkan

-- fir
import FIR
  ( Array, Layout(Base, Extended, Locations)
  , PrimTy, Poke(..)
  , nextAligned, pokeArrayOff, roundUp
  )

-- fir-examples
import Vulkan.Memory
import Vulkan.Monad

-----------------------------------------------------------------------------------------------------

createVertexBuffer
  :: forall a m
  .  ( MonadVulkan m, PrimTy a, Poke a Locations )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> [ a ]
  -> m (Vulkan.Buffer, Int -> [a] -> IO (), Int)
createVertexBuffer
  = createBufferFromList @a @Locations
      Vulkan.BUFFER_USAGE_VERTEX_BUFFER_BIT
      ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
      Vulkan.zero

createIndexBuffer
  :: forall a m
  .  ( MonadVulkan m, PrimTy a, Poke a Base, Poke (Array 1 a) Base )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> [ a ]
  -> m (Vulkan.Buffer, Int -> [a] -> IO (), Int)
createIndexBuffer
  = createBufferFromList @a @Base
      Vulkan.BUFFER_USAGE_INDEX_BUFFER_BIT
      ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
      Vulkan.zero

createUniformBuffer
  :: forall a m
  .  ( MonadVulkan m, PrimTy a, Poke a Extended )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> a
  -> m (Vulkan.Buffer, a -> IO ())
createUniformBuffer
  = createBuffer @a @Extended
      Vulkan.BUFFER_USAGE_UNIFORM_BUFFER_BIT
      ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
      Vulkan.zero

createBuffer
  :: forall a ali m.
     ( MonadVulkan m, Poke a ali )
  => Vulkan.BufferUsageFlags
  -> Vulkan.MemoryPropertyFlags
  -> Vulkan.MemoryAllocateFlags
  -> Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> a
  -> m (Vulkan.Buffer, a -> IO ())
createBuffer usage memFlags memAllocReqs physicalDevice device elems = do
  ( buf, bufPtr ) <- snd <$>
    createBufferFromPoke
      usage
      memFlags
      memAllocReqs
      physicalDevice
      device
      ( \memPtr -> pokeFunction memPtr elems )
      ( fromIntegral (sizeOf @a @ali) )
  pure ( buf, pokeFunction bufPtr )
    where
      pokeFunction = poke @a @ali

createBufferFromList
  :: forall a ali m.
     ( MonadVulkan m, Poke a ali, Poke (Array 1 a) ali )
  => Vulkan.BufferUsageFlags
  -> Vulkan.MemoryPropertyFlags
  -> Vulkan.MemoryAllocateFlags
  -> Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> [ a ]
  -> m (Vulkan.Buffer, Int -> [a] -> IO (), Int)
createBufferFromList usage memFlags memAllocReqs physicalDevice device elems = do
  ( buf, bufPtr ) <- snd <$>
    createBufferFromPoke
      usage
      memFlags
      memAllocReqs
      physicalDevice
      device
      ( \memPtr -> pokeFunction memPtr 0 elems )
      ( fromIntegral nbElems * fromIntegral sz )
  pure ( buf, pokeFunction bufPtr, nbElems )
    where
      nbElems = length elems
      pokeFunction = pokeArrayOff @a @ali
      sz = nextAligned ( sizeOf @a @ali ) ( alignment @(Array 1 a) @ali )

createBufferFromPoke
  :: MonadVulkan m
  => Vulkan.BufferUsageFlags
  -> Vulkan.MemoryPropertyFlags
  -> Vulkan.MemoryAllocateFlags
  -> Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> (Ptr a -> IO ())
  -> Vulkan.DeviceSize
  -> m ([ReleaseKey], (Vulkan.Buffer, Ptr a))
createBufferFromPoke usage memFlags memAllocReqs physicalDevice device poking sizeInBytes =
  let

    roundedSize = max 64 ( sizeInBytes `roundUp` 64 ) -- nonCoherentAtomSize

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
    ( releaseBuffer, buffer :: Vulkan.Buffer )
      <- Vulkan.withBuffer device bufferCreateInfo Nothing allocate

    memReqs :: Vulkan.MemoryRequirements
      <- Vulkan.getBufferMemoryRequirements device buffer

    ( releaseMem, memory :: Vulkan.DeviceMemory )
      <- allocateMemory physicalDevice device memReqs memFlags memAllocReqs

    ( releaseMapMem, ptr ) <- allocate
      ( do
          Vulkan.bindBufferMemory device buffer memory 0

          memPtr :: Ptr a
             <- coerce <$> Vulkan.mapMemory device memory 0 roundedSize Vulkan.zero

          poking memPtr
          pure memPtr
      )
      ( \_ -> Vulkan.unmapMemory device memory )

    pure ( [ releaseMapMem, releaseMem, releaseBuffer ], (buffer, ptr) )
