{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
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

-- managed
import Control.Monad.Managed
  ( MonadManaged )

-- sdl2
import qualified SDL
import qualified SDL.Internal.Types as SDL
import qualified SDL.Raw
import qualified SDL.Video.Vulkan

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- vector
import qualified Data.Vector.Storable as Vector

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan

-- fir-examples
import Vulkan.Monad
  ( logMsg, manageBracket )

-----------------------------------------------------------------------------------------------------

enableSDLLogging :: MonadIO m => m ()
enableSDLLogging =
  SDL.Raw.logSetAllPriority SDL.Raw.SDL_LOG_PRIORITY_VERBOSE


initializeSDL :: MonadIO m => SDL.LocationMode -> m ()
initializeSDL mouseLocationMode = do
  logMsg "Initializing SDL" *> SDL.initialize [ SDL.InitVideo ]
  void ( SDL.setMouseLocationMode mouseLocationMode )


createWindow :: MonadManaged m => Foreign.CInt -> Foreign.CInt -> ShortText -> m SDL.Window
createWindow x y title =
  manageBracket
    ( SDL.createWindow
              ( fromString ( ShortText.unpack title ) )
              SDL.defaultWindow
                { SDL.windowGraphicsContext = SDL.VulkanContext
                , SDL.windowInitialSize = SDL.V2 x y
                }
    )
    SDL.destroyWindow

setWindowIcon :: MonadIO m => SDL.Window -> FilePath -> m ()
setWindowIcon (SDL.Window window) iconPath = liftIO $
  fmap JP.convertRGBA8 <$> JP.readImage iconPath
  >>= \case
    Left _
      -> putStrLn $ "Could not load icon from filepath \"" <> iconPath <> "\"."
    Right icon
      -> let
           iconWidth  = JP.imageWidth  icon
           iconHeight = JP.imageHeight icon
           iconData   = JP.imageData   icon
         in do
          SDL.Raw.setWindowIcon window
            =<<
            ( Vector.unsafeWith iconData \ iconDataPtr ->
              SDL.Raw.createRGBSurfaceFrom
                ( castPtr iconDataPtr )
                ( fromIntegral iconWidth  )
                ( fromIntegral iconHeight )
                32
                ( 4 * fromIntegral iconWidth )
                0x000000ff 0x0000ff00 0x00ff0000 0xff000000
            )
          putStrLn $ "Set window icon using \"" <> iconPath <> "\"."

createSurface
  :: MonadIO m
  => SDL.Window
  -> Vulkan.VkInstance
  -> m SDL.Video.Vulkan.VkSurfaceKHR
createSurface window vulkanInstance
  = SDL.Video.Vulkan.vkCreateSurface window $ Foreign.castPtr vulkanInstance

getNeededExtensions :: MonadIO m => SDL.Window -> m [CString]
getNeededExtensions = SDL.Video.Vulkan.vkGetInstanceExtensions
