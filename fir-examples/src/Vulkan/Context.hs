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
import Data.Foldable
  ( toList )
import Data.Maybe
  ( fromMaybe, fromJust )
import Data.Kind
  ( Type )
import Foreign.C.String
  ( CString )
import Foreign.C.Types
  ( CInt )
import GHC.TypeLits
  ( KnownNat )

-- bytestring
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as ByteString
  ( intercalate )
import qualified Data.ByteString.Short as ShortByteString
  ( packCString, fromShort, toShort )

-- containers
import Data.Set
  ( Set )

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
  ( intercalate, fromShortByteString, toShortByteString )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( withSized )

-- vulkan-api
import qualified Vulkan
import qualified Vulkan.CStruct.Extends as Vulkan
  ( Extendss, PokeChain )

-- fir
import qualified FIR as SPIRV
  ( Extension )

-- fir-examples
import FIR.Examples.Paths
  ( assetDir )
import Vulkan.Backend
import Vulkan.Features
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

data RenderInfo ( ctx :: RenderingContext ) where
  RenderInfo
    :: ( Vulkan.PokeChain fs
       , Vulkan.Extendss Vulkan.DeviceCreateInfo fs
       )
    => { features    :: Vulkan.PhysicalDeviceFeatures2 fs
       , queueType   :: Vulkan.QueueFlags
       , surfaceInfo :: ContextSurfaceInfo ctx
       }
    -> RenderInfo ctx

data SurfaceInfo
  = SurfaceInfo
  { surfaceWindow   :: SDL.Window
  , preferredFormat :: Vulkan.SurfaceFormatKHR
  , surfaceUsage    :: [ Vulkan.ImageUsageFlags ]
  }

data family ContextSwapchainInfo ( ctx :: RenderingContext ) :: Type
data instance ContextSwapchainInfo Headless     = NoSwapchain
data instance ContextSwapchainInfo WithSwapchain where
  ASwapchainInfo :: KnownNat n => SwapchainInfo n -> ContextSwapchainInfo WithSwapchain

data VulkanContext ( ctx :: RenderingContext )
  = VulkanContext
  { physicalDevice   :: Vulkan.PhysicalDevice
  , device           :: Vulkan.Device
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

initialiseWindow :: MonadVulkan m => WindowInfo -> m ( SDL.Window, [ ByteString ] )
initialiseWindow WindowInfo { .. } = do
  enableSDLLogging
  initializeSDL mouseMode
  window           <- logDebug "Creating SDL window"           *> createWindow width height windowName
  setWindowIcon window ( assetDir </> "fir_logo.png" )
  neededExtensions <- logDebug "Loading needed extensions"     *> getNeededExtensions window
  extensionNames   <- traverse ( liftIO . peekCString ) neededExtensions
  logInfo $ "Needed instance extensions are: " <> ( ShortText.intercalate ", " extensionNames )
  pure ( window, map ( ShortByteString.fromShort . ShortText.toShortByteString ) extensionNames )

peekCString :: CString -> IO ShortText
peekCString = fmap ( fromMaybe "???" . ShortText.fromShortByteString ) . ShortByteString.packCString

initialiseContext
  :: forall ctx m. ( KnownRenderingContext ctx, MonadVulkan m )
  => ByteString -> [ ByteString ] -> Set SPIRV.Extension -> RenderInfo ctx -> m ( VulkanContext ctx )
initialiseContext appName neededInstanceExtensions neededSPIRVExts RenderInfo { .. } = do
  vkInstance       <- logDebug "Creating Vulkan instance"      *> createVulkanInstance appName neededInstanceExtensions neededSPIRVExts
  physicalDevice   <- logDebug "Creating physical device"      *> createPhysicalDevice vkInstance
  queueFamilyIndex <- logDebug "Finding suitable queue family" *> findQueueFamilyIndex physicalDevice [queueType]
  let
    deviceExtensions :: [ ByteString ]
    deviceExtensions = requiredDeviceExtensions =<< toList neededSPIRVExts
  ( device, aSwapchainInfo ) <- case renderingContext @ctx of
    SHeadless      -> do
      let
        usedDeviceExts :: [ ByteString ]
        usedDeviceExts = deviceExtensions
      logInfo $ "Needed device extensions are: " <>
        ( fromJust . ShortText.fromShortByteString . ShortByteString.toShort $ ByteString.intercalate ", " usedDeviceExts )
      device <- logDebug "Creating logical device" *>
        createLogicalDevice physicalDevice queueFamilyIndex usedDeviceExts features
      pure ( device, NoSwapchain )
    SWithSwapchain -> do
      let
        usedDeviceExts :: [ ByteString ]
        usedDeviceExts = ( Vulkan.KHR_SWAPCHAIN_EXTENSION_NAME : deviceExtensions )
      logInfo $ "Needed device extensions are: " <>
        ( fromJust . ShortText.fromShortByteString . ShortByteString.toShort $ ByteString.intercalate ", " usedDeviceExts )
      device <- logDebug "Creating logical device" *>
        createLogicalDevice physicalDevice queueFamilyIndex usedDeviceExts features
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
                ( foldr (.|.) ( Vulkan.zero :: Vulkan.ImageUsageFlags ) surfaceUsage )
      swapchainImageVec <- logDebug "Getting swapchain images" *> getSwapchainImages device swapchain
      V.withSized swapchainImageVec \ swapchainImages -> do
        let swapchainInfo = SwapchainInfo {..}
        pure ( device, ASwapchainInfo swapchainInfo )
  pure ( VulkanContext {..} )
