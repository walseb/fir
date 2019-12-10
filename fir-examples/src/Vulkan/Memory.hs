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

module Vulkan.Memory where

-- base
import Control.Monad
  ( guard )
import Data.Bits
import Data.Foldable
  ( for_ )
import Data.Word
  ( Word32 )
import qualified Foreign
import qualified Foreign.Marshal

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir-examples
import Vulkan.Monad

-----------------------------------------------------------------------------------------------------

allocateMemory
  :: MonadVulkan m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkMemoryRequirements
  -> [ Vulkan.VkMemoryPropertyFlags ]
  -> m ( ReleaseKey, Vulkan.VkDeviceMemory )
allocateMemory physicalDevice device memReqs requiredFlags = do

    memoryProperties :: Vulkan.VkPhysicalDeviceMemoryProperties
      <- allocaAndPeek
          ( Vulkan.vkGetPhysicalDeviceMemoryProperties physicalDevice )

    let
      memoryTypeCount :: Word32
      memoryTypeCount = Vulkan.getField @"memoryTypeCount" memoryProperties

    memoryTypes :: [ Vulkan.VkMemoryType ]
      <- liftIO $
        Foreign.Marshal.peekArray @Vulkan.VkMemoryType
          ( fromIntegral memoryTypeCount )
          ( Vulkan.unsafePtr memoryProperties
              `Foreign.plusPtr`
              Vulkan.fieldOffset @"memoryTypes" @Vulkan.VkPhysicalDeviceMemoryProperties
          )

    let
      possibleMemoryTypeIndices :: [ Word32 ]
      possibleMemoryTypeIndices = do

        ( i, memoryType ) <- zip [ 0 .. ] memoryTypes

        guard
          ( testBit
              ( Vulkan.getField @"memoryTypeBits" memReqs )
              ( fromIntegral i )
          )

        for_ requiredFlags
          ( \f ->
              guard ( Vulkan.getField @"propertyFlags" memoryType .&. f > Vulkan.VK_ZERO_FLAGS )
          )

        pure i

    memoryTypeIndex :: Word32
      <- case possibleMemoryTypeIndices of
              [] -> error $  "No available memory types with requirements:\n"
                          ++ show memReqs
                          ++ "\nand with flags:\n"
                          ++ show requiredFlags
              ( i : _ ) -> pure i

    let
      allocateInfo :: Vulkan.VkMemoryAllocateInfo
      allocateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"allocationSize" ( Vulkan.getField @"size" memReqs )
          &* Vulkan.set @"memoryTypeIndex" memoryTypeIndex
          )

    allocateVulkanResource allocateInfo
      ( Vulkan.vkAllocateMemory device )
      ( Vulkan.vkFreeMemory device )
