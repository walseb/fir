{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Context where

-- base
import Data.Bits
  ( (.|.) )
import Data.Maybe
  ( fromMaybe )
import Data.Kind
  ( Type )
import Foreign.C.String
  ( CString )
import Foreign.C.Types
  ( CInt )
import GHC.TypeLits
  ( KnownNat )

-- bytestring
import qualified Data.ByteString.Short as ShortByteString
  ( packCString )

-- filepath
import System.FilePath
  ( (</>) )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- sdl2
import qualified SDL

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( intercalate, fromShortByteString )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( withSizedList )

-- vulkan-api
import qualified Graphics.Vulkan                      as Vulkan
import qualified Graphics.Vulkan.Core_1_0             as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_surface   as Vulkan

-- fir-examples
import FIR.Examples.Paths
  ( assetDir )
import Vulkan.Backend
import Vulkan.Monad
import Vulkan.SDL

----------------------------------------------------------------------------
-- Two different rendering contexts: with or without a swapchain.

data RenderingContext
  = Headless
  | WithSwapchain

data SRenderingContext (ctx :: RenderingContext) where
  SHeadless :: SRenderingContext Headless
  SWithSwapchain :: SRenderingContext WithSwapchain

class KnownRenderingContext ( ctx :: RenderingContext ) where
  renderingContext :: SRenderingContext ctx
instance KnownRenderingContext Headless where
  renderingContext = SHeadless
instance KnownRenderingContext WithSwapchain where
  renderingContext = SWithSwapchain

----------------------------------------------------------------------------

data WindowInfo
  = WindowInfo
  { width      :: CInt
  , height     :: CInt
  , windowName :: ShortText
  , mouseMode  :: SDL.LocationMode
  }

type family ContextSurfaceInfo ( ctx :: RenderingContext ) :: Type where
  ContextSurfaceInfo Headless      = ()
  ContextSurfaceInfo WithSwapchain = SurfaceInfo

data RenderInfo ( ctx :: RenderingContext )
  = RenderInfo
  { features    :: Vulkan.VkPhysicalDeviceFeatures
  , queueType   :: Vulkan.VkQueueFlags
  , surfaceInfo :: ContextSurfaceInfo ctx
  }

data SurfaceInfo
  = SurfaceInfo
  { surfaceWindow   :: SDL.Window
  , preferredFormat :: Vulkan.VkSurfaceFormatKHR
  , surfaceUsage    :: [ Vulkan.VkImageUsageFlags ]
  }

data family ContextSwapchainInfo ( ctx :: RenderingContext ) :: Type
data instance ContextSwapchainInfo Headless     = NoSwapchain
data instance ContextSwapchainInfo WithSwapchain where
  ASwapchainInfo :: KnownNat n => SwapchainInfo n -> ContextSwapchainInfo WithSwapchain

data VulkanContext ( ctx :: RenderingContext )
  = VulkanContext
  { physicalDevice   :: Vulkan.VkPhysicalDevice
  , device           :: Vulkan.VkDevice
  , queueFamilyIndex :: Int
  , aSwapchainInfo   :: ContextSwapchainInfo ctx
  }

type VulkanSwapchainContext = VulkanContext WithSwapchain
type VulkanHeadlessContext  = VulkanContext Headless

withSwapchainInfo
  :: ContextSwapchainInfo WithSwapchain
  -> ( forall n. KnownNat n => SwapchainInfo n -> r )
  -> r
withSwapchainInfo ( ASwapchainInfo swapchain ) f = f swapchain

initialiseWindow :: MonadVulkan m => WindowInfo -> m ( SDL.Window, [ CString ] )
initialiseWindow WindowInfo { .. } = do
  enableSDLLogging
  initializeSDL mouseMode
  window           <- logDebug "Creating SDL window"           *> createWindow width height windowName
  setWindowIcon window ( assetDir </> "fir_logo.png" )
  neededExtensions <- logDebug "Loading needed extensions"     *> getNeededExtensions window
  extensionNames   <- traverse ( liftIO . peekCString ) neededExtensions
  logInfo $ "Needed instance extensions are: " <> ( ShortText.intercalate ", " extensionNames )
  pure ( window, neededExtensions )

peekCString :: CString -> IO ShortText
peekCString = fmap ( fromMaybe "???" . ShortText.fromShortByteString ) . ShortByteString.packCString

initialiseContext
  :: forall ctx m. ( KnownRenderingContext ctx, MonadVulkan m )
  => String -> [ CString ] -> RenderInfo ctx -> m ( VulkanContext ctx )
initialiseContext appName neededExtensions RenderInfo { .. } = do
  vkInstance       <- logDebug "Creating Vulkan instance"      *> createVulkanInstance appName neededExtensions
  physicalDevice   <- logDebug "Creating physical device"      *> createPhysicalDevice vkInstance
  queueFamilyIndex <- logDebug "Finding suitable queue family" *> findQueueFamilyIndex physicalDevice [queueType]
  ( device, aSwapchainInfo ) <- case renderingContext @ctx of
    SHeadless      -> do
      device <- logDebug "Creating logical device" *> createLogicalDevice physicalDevice queueFamilyIndex False features
      pure ( device, NoSwapchain )
    SWithSwapchain -> do
      device <- logDebug "Creating logical device" *> createLogicalDevice physicalDevice queueFamilyIndex True  features
      let SurfaceInfo {..} = surfaceInfo
      surface <- logDebug "Creating SDL surface" *> createSurface surfaceWindow vkInstance
      assertSurfacePresentable physicalDevice queueFamilyIndex surface
      surfaceFormat <- logDebug "Choosing swapchain format & color space"
        *> chooseSwapchainFormat preferredFormat physicalDevice surface
      ( swapchain, swapchainExtent ) <-
        logDebug "Creating swapchain"
          *> createSwapchain
                physicalDevice device
                surface surfaceFormat
                ( foldr (.|.) Vulkan.VK_ZERO_FLAGS surfaceUsage )
      swapchainImageList <- logDebug "Getting swapchain images" *> getSwapchainImages device swapchain
      V.withSizedList swapchainImageList \ swapchainImages -> do
        let swapchainInfo = SwapchainInfo {..}
        pure ( device, ASwapchainInfo swapchainInfo )
  pure ( VulkanContext {..} )
