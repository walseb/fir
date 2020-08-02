{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.SDL where

-- base
import Control.Monad
  ( void )
import Data.String
  ( fromString )
import qualified Foreign
import qualified Foreign.C as Foreign
import Foreign.C.String
  ( CString )
import Foreign.Ptr
  ( castPtr )

-- JuicyPixels
import qualified Codec.Picture as JP
  ( readImage, convertRGBA8
  , imageWidth, imageHeight, imageData
  )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logError )

-- resourcet
import Control.Monad.Trans.Resource
  ( allocate )

-- sdl2
import qualified SDL
import qualified SDL.Internal.Types as SDL
import qualified SDL.Raw
import qualified SDL.Video.Vulkan

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack, unpack )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- vector
import qualified Data.Vector.Storable as Vector

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vulkan

-- fir-examples
import Vulkan.Monad

-----------------------------------------------------------------------------------------------------

enableSDLLogging :: MonadVulkan m => m ()
enableSDLLogging =
  SDL.Raw.logSetAllPriority SDL.Raw.SDL_LOG_PRIORITY_VERBOSE


initializeSDL :: MonadVulkan m => SDL.LocationMode -> m ()
initializeSDL mouseLocationMode = do
  logDebug "Initializing SDL" *> SDL.initialize [ SDL.InitVideo ]
  void ( SDL.setMouseLocationMode mouseLocationMode )


createWindow :: MonadVulkan m => Foreign.CInt -> Foreign.CInt -> ShortText -> m SDL.Window
createWindow x y title =
  snd <$> allocate
    ( SDL.createWindow
              ( fromString ( ShortText.unpack title ) )
              SDL.defaultWindow
                { SDL.windowGraphicsContext = SDL.VulkanContext
                , SDL.windowInitialSize = SDL.V2 x y
                }
    )
    SDL.destroyWindow

setWindowIcon :: MonadVulkan m => SDL.Window -> FilePath -> m ()
setWindowIcon (SDL.Window window) iconPath = do
  imgData <- liftIO $ fmap JP.convertRGBA8 <$> JP.readImage iconPath
  case imgData of
    Left _
      -> logError ( "Could not load icon from filepath \"" <> ShortText.pack iconPath <> "\"." )
    Right icon
      -> do
            let
              iconWidth  = JP.imageWidth  icon
              iconHeight = JP.imageHeight icon
              iconData   = JP.imageData   icon
            iconSurface <-
              liftIO
                ( Vector.unsafeWith iconData \ iconDataPtr ->
                  SDL.Raw.createRGBSurfaceFrom
                    ( castPtr iconDataPtr )
                    ( fromIntegral iconWidth  )
                    ( fromIntegral iconHeight )
                    32
                    ( 4 * fromIntegral iconWidth )
                    0x000000ff 0x0000ff00 0x00ff0000 0xff000000
                )
            liftIO $ SDL.Raw.setWindowIcon window iconSurface
            logDebug ( "Set window icon using \"" <> ShortText.pack iconPath <> "\"." )

createSurface
  :: MonadVulkan m
  => SDL.Window
  -> Vulkan.VkInstance
  -> m SDL.Video.Vulkan.VkSurfaceKHR
createSurface window vulkanInstance
  = snd <$> allocate
    ( SDL.Video.Vulkan.vkCreateSurface window ( Foreign.castPtr vulkanInstance ) )
    ( \ surf -> Vulkan.vkDestroySurfaceKHR vulkanInstance ( Vulkan.VkPtr surf ) Vulkan.VK_NULL_HANDLE )

getNeededExtensions :: MonadIO m => SDL.Window -> m [CString]
getNeededExtensions = SDL.Video.Vulkan.vkGetInstanceExtensions
