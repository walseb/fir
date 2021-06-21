{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Vulkan.Context where

-- base
import Data.Bits
  ( (.|.) )
import Data.Foldable
  ( toList )
import Data.Kind
  ( Type )
import Data.Maybe
  ( fromMaybe, mapMaybe )
import Foreign.C.String
  ( CString )
import Foreign.C.Types
  ( CInt )
import GHC.TypeLits
  ( KnownNat )

-- bytestring
import Data.ByteString
  ( ByteString )
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
  ( intercalate, fromShortByteString, toByteString )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector
import qualified Data.Vector as Boxed.Vector
  ( singleton )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( withSized )

-- vulkan
import qualified Vulkan
import qualified Vulkan.CStruct.Extends as Vulkan
  ( SomeStruct(..) )
import qualified Vulkan.Requirement as Vulkan
import qualified Vulkan.Zero as Vulkan

-- vulkan-utils
import qualified Vulkan.Utils.Initialization as Vulkan.Utils
  ( createInstanceFromRequirements, createDebugInstanceFromRequirements
  , createDeviceFromRequirements
  )

-- fir
import FIR
  ( ModuleRequirements(..), Extension(..), showCapability )

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

data InstanceType
  = Normal
  | Debug
  deriving stock Show

data VulkanRequirements =
  VulkanRequirements
    { instanceRequirements :: [ Vulkan.InstanceRequirement ]
    , deviceRequirements   :: [ Vulkan.DeviceRequirement   ]
    }

ignoreMinVersion :: VulkanRequirements -> VulkanRequirements
ignoreMinVersion ( VulkanRequirements instReqs devReqs ) =
  VulkanRequirements ( mapMaybe noMinInstVer instReqs ) ( mapMaybe noMinDevVer devReqs )
    where
      noMinInstVer :: Vulkan.InstanceRequirement -> Maybe Vulkan.InstanceRequirement
      noMinInstVer ( Vulkan.RequireInstanceVersion {} ) = Nothing
      noMinInstVer lay@( Vulkan.RequireInstanceLayer {} ) = Just $ lay { Vulkan.instanceLayerMinVersion = 0 }
      noMinInstVer instExt@( Vulkan.RequireInstanceExtension {} ) = Just $ instExt { Vulkan.instanceExtensionMinVersion = 0 }
      noMinDevVer :: Vulkan.DeviceRequirement -> Maybe Vulkan.DeviceRequirement
      noMinDevVer ( Vulkan.RequireDeviceVersion {} ) = Nothing
      noMinDevVer devExt@( Vulkan.RequireDeviceExtension {} ) = Just $ devExt { Vulkan.deviceExtensionMinVersion = 0 }
      noMinDevVer devReq = Just devReq

vulkanRequirements :: ModuleRequirements -> VulkanRequirements
vulkanRequirements ( ModuleRequirements { requiredCapabilities, requiredExtensions } ) =
  VulkanRequirements
    { instanceRequirements = instReqs2
    , deviceRequirements   = devReqs2
    }
  where
    ( instReqs1, devReqs1 ) = goCaps ( toList requiredCapabilities ) []        []
    ( instReqs2, devReqs2 ) = goExts ( toList requiredExtensions   ) instReqs1 devReqs1
    goCaps []             instReqs devReqs = ( instReqs, devReqs )
    goCaps ( cap : caps ) instReqs devReqs = ( inst <> instReqs', dev <> devReqs' )
      where
        ( inst, dev ) = Vulkan.spirvCapabilityRequirements ( showCapability cap )
        ( instReqs', devReqs' ) = goCaps caps instReqs devReqs
    goExts []                       instReqs devReqs = ( instReqs, devReqs )
    goExts ( Extension ext : exts ) instReqs devReqs = ( inst <> instReqs', dev <> devReqs' )
      where
        ( inst, dev ) = Vulkan.spirvExtensionRequirements ( ShortText.toByteString ext )
        ( instReqs', devReqs' ) = goExts exts instReqs devReqs

addInstanceExtensions :: [ ByteString ] -> VulkanRequirements -> VulkanRequirements
addInstanceExtensions extNames ( VulkanRequirements instReqs devReqs ) =
  VulkanRequirements ( map mkExtensionRequirement extNames <> instReqs ) devReqs
    where
      mkExtensionRequirement :: ByteString -> Vulkan.InstanceRequirement
      mkExtensionRequirement extName =
        Vulkan.RequireInstanceExtension
          { Vulkan.instanceExtensionLayerName  = Nothing
          , Vulkan.instanceExtensionName       = extName
          , Vulkan.instanceExtensionMinVersion = 0
          }

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
    :: { queueType   :: Vulkan.QueueFlags
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
  { vkInstance       :: Vulkan.Instance
  , physicalDevice   :: Vulkan.PhysicalDevice
  , device           :: Vulkan.Device
  , queueFamilyIndex :: Int
  , queue            :: Vulkan.Queue
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
initialiseWindow ( WindowInfo { height, width, windowName, mouseMode } ) = do
  enableSDLLogging
  initializeSDL mouseMode
  window           <- logDebug "Creating SDL window"           *> createWindow width height windowName
  setWindowIcon window ( assetDir </> "fir_logo.png" )
  neededExtensions <- logDebug "Loading needed extensions"     *> getNeededExtensions window
  extensionNames   <- traverse ( liftIO . peekCString ) neededExtensions
  logInfo $ "Needed instance extensions are: " <> ( ShortText.intercalate ", " extensionNames )
  pure ( window, map ShortText.toByteString extensionNames )

peekCString :: CString -> IO ShortText
peekCString = fmap ( fromMaybe "???" . ShortText.fromShortByteString ) . ShortByteString.packCString

initialiseContext
  :: forall ctx m. ( KnownRenderingContext ctx, MonadVulkan m )
  => InstanceType -> ByteString -> VulkanRequirements -> RenderInfo ctx -> m ( VulkanContext ctx )
initialiseContext instanceType appName ( VulkanRequirements { instanceRequirements, deviceRequirements } ) ( RenderInfo { queueType, surfaceInfo } ) = do
  logDebug "Creating Vulkan instance"
  vkInstanceInfo   <- vulkanInstanceInfo appName
  vkInstance       <- case instanceType of
    Normal -> Vulkan.Utils.createInstanceFromRequirements      instanceRequirements [] vkInstanceInfo
    Debug  -> Vulkan.Utils.createDebugInstanceFromRequirements instanceRequirements [] vkInstanceInfo
  physicalDevice   <- logDebug "Creating physical device"      *> createPhysicalDevice vkInstance
  queueFamilyIndex <- logDebug "Finding suitable queue family" *> findQueueFamilyIndex physicalDevice [queueType]
  let
    queueCreateInfo :: Vulkan.DeviceQueueCreateInfo '[]
    queueCreateInfo = Vulkan.zero
      { Vulkan.queueFamilyIndex = fromIntegral queueFamilyIndex
      , Vulkan.queuePriorities  = Boxed.Vector.singleton ( 1.0 :: Float )
      }
    logicalDeviceCreateInfo :: Vulkan.DeviceCreateInfo '[]
    logicalDeviceCreateInfo = Vulkan.zero { Vulkan.queueCreateInfos = Boxed.Vector.singleton ( Vulkan.SomeStruct queueCreateInfo ) }
  ( device, aSwapchainInfo ) <- case renderingContext @ctx of
    SHeadless      -> do
      device <- logDebug "Creating logical device" *>
        Vulkan.Utils.createDeviceFromRequirements deviceRequirements [] physicalDevice logicalDeviceCreateInfo
      pure ( device, NoSwapchain )
    SWithSwapchain -> do
      let
        swapchainDeviceRequirements :: [ Vulkan.DeviceRequirement ]
        swapchainDeviceRequirements = Vulkan.RequireDeviceExtension Nothing Vulkan.KHR_SWAPCHAIN_EXTENSION_NAME 0 : deviceRequirements
      device <- logDebug "Creating logical device" *>
        Vulkan.Utils.createDeviceFromRequirements swapchainDeviceRequirements [] physicalDevice logicalDeviceCreateInfo
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
  queue  <- Vulkan.getDeviceQueue device ( fromIntegral queueFamilyIndex ) 0
  pure ( VulkanContext {..} )
