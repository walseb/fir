{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Vulkan.Memory where

-- base
import Control.Monad
  ( guard )
import Data.Bits
  ( (.&.), testBit )
import Data.Word
  ( Word32, Word64 )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, allocate )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( (!?), imapMaybe )

-- vulkan
import qualified Vulkan

-- fir-examples
import Vulkan.Monad

-----------------------------------------------------------------------------------------------------

allocateMemory
  :: MonadVulkan m
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> Vulkan.MemoryRequirements
  -> Vulkan.MemoryPropertyFlags
  -> Vulkan.MemoryAllocateFlags
  -> m ( ReleaseKey, Vulkan.DeviceMemory )
allocateMemory physicalDevice device memReqs memFlags allocateFlags = do

    Vulkan.PhysicalDeviceMemoryProperties
      { Vulkan.memoryTypes
      } <- Vulkan.getPhysicalDeviceMemoryProperties physicalDevice

    let
      possibleMemoryTypeIndices :: Boxed.Vector Word32
      possibleMemoryTypeIndices = ( `Boxed.Vector.imapMaybe` memoryTypes ) \ i_int memoryType -> do
        let
          i :: Word32
          i = fromIntegral i_int
        guard
          ( testBit
              ( ( Vulkan.memoryTypeBits :: Vulkan.MemoryRequirements -> Word32 ) memReqs )
              i_int
          )
        guard ( Vulkan.propertyFlags memoryType .&. memFlags >= memFlags )
        pure i

      memoryTypeIndex :: Word32
      memoryTypeIndex =
        case possibleMemoryTypeIndices Boxed.Vector.!? 0 of
          Nothing ->
            error
              ( "No available memory types with requirements:\n"
              ++ show memReqs
              ++ "\nand with flags:\n"
              ++ show memFlags
              )
          Just i -> i

    let
      allocateFlagsInfo :: Vulkan.MemoryAllocateFlagsInfo
      allocateFlagsInfo =
        Vulkan.MemoryAllocateFlagsInfo
          { Vulkan.flags      = allocateFlags
          , Vulkan.deviceMask = 0
          }
      allocateInfo :: Vulkan.MemoryAllocateInfo '[ Vulkan.MemoryAllocateFlagsInfo ]
      allocateInfo =
        Vulkan.MemoryAllocateInfo
          { Vulkan.next            = ( allocateFlagsInfo, () )
          , Vulkan.allocationSize  = ( Vulkan.size :: Vulkan.MemoryRequirements -> Word64 ) memReqs
          , Vulkan.memoryTypeIndex = memoryTypeIndex
          }

    Vulkan.withMemory device allocateInfo Nothing allocate
