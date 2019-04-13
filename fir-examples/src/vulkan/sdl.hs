{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.SDL where

-- base
import Control.Monad.IO.Class
  ( MonadIO )
import qualified Foreign

-- managed
import Control.Monad.Managed
  ( MonadManaged )

-- sdl2
import qualified SDL
import qualified SDL.Raw
import qualified SDL.Video.Vulkan

-- text
import "text" Data.Text
  ( Text )

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan

-- fir-examples
import Vulkan.Monad
  ( logMsg, manageBracket )

-----------------------------------------------------------------------------------------------------

enableSDLLogging :: MonadIO m => m ()
enableSDLLogging =
  SDL.Raw.logSetAllPriority SDL.Raw.SDL_LOG_PRIORITY_VERBOSE


initializeSDL :: MonadIO m => m ()
initializeSDL = do
  logMsg "Initializing SDL" *> SDL.initialize [ SDL.InitVideo ]
  logMsg "SDL: setting relative mouse location" <* SDL.setMouseLocationMode SDL.RelativeLocation


createWindow :: MonadManaged m => Text -> m SDL.Window
createWindow title =
  manageBracket
    ( SDL.createWindow
        title
        SDL.defaultWindow
          { SDL.windowGraphicsContext = SDL.VulkanContext
          , SDL.windowInitialSize = SDL.V2 1920 1080
          }
    )
    SDL.destroyWindow

createSurface
  :: MonadIO m
  => SDL.Window
  -> Vulkan.VkInstance
  -> m SDL.Video.Vulkan.VkSurfaceKHR
createSurface window vulkanInstance
  = SDL.Video.Vulkan.vkCreateSurface window $ Foreign.castPtr vulkanInstance

getNeededExtensions :: MonadIO m => SDL.Window -> m [Vulkan.CString]
getNeededExtensions = SDL.Video.Vulkan.vkGetInstanceExtensions
