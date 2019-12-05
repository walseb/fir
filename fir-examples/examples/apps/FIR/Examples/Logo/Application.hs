{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module FIR.Examples.Logo.Application ( logo ) where

-- base
import Control.Monad
  ( when )
import Data.Functor.Identity
  ( Identity(..) )
import Data.Monoid
  ( Sum(getSum) )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import Data.Word
  ( Word32 )
import GHC.Generics
  ( Generic )

-- lens
import Control.Lens
  ( use, assign )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- sdl2
import qualified SDL
import qualified SDL.Event

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack, unpack )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( zip, zip3, index )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan                as Vulkan
import qualified Graphics.Vulkan.Core_1_0       as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , (:->)((:->)), Struct((:&),End)
  , ModuleRequirements
  )
import qualified FIR
import Math.Linear
  ( V, pattern V2, pattern V3
  , (*^), (^+^)
  , normalise
  )
import Math.Quaternion
  ( rotate, axisAngle )

-- fir-examples
import FIR.Examples.Common
import FIR.Examples.Logo.Shaders
import FIR.Examples.Paths
import FIR.Examples.Reload
import FIR.Examples.RenderState
import Vulkan.Backend
import Vulkan.Context
import Vulkan.Features
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.Resource
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- Compute shader, resources.

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Compute shader", compileComputeShader) ]
     )

appName :: IsString a => a
appName = "fir-examples - Logo"
shortName :: String
shortName = "logo" -- name for screenshots

type Camera =
  ( Struct
     '[ "position" ':-> V 3 Float
      , "right"    ':-> V 3 Float
      , "up"       ':-> V 3 Float
      , "forward"  ':-> V 3 Float
      ]
  )

data ResourceSet i st
  = ResourceSet
    { cameraResource   :: UniformBuffer Camera i st
    , storageResources :: StorageImages        i st
    }
  deriving ( Generic )

initialObserverLogo :: Observer
initialObserverLogo =
  Observer
    { position = V3 0 0 10
    , angles   = V2 (5*pi/4) (pi/5)
    , clock    = 0
    }

initialStateLogo :: RenderState
initialStateLogo = initialState { observer = initialObserverLogo }

globalSizes :: ( Word32, Word32, Word32 )
globalSizes = (120, 135, 1) -- local size 16 8 1

----------------------------------------------------------------------------
-- Application.

logo :: IO ()
logo = runVulkan initialStateLogo do

  -------------------------------------------
  -- Obtain requirements from shaders.

  ( reqs :: ModuleRequirements ) <-
    case shaderCompilationResult of
      Left  err  -> error $ "Shader compilation was unsuccessful:\n" <> ShortText.unpack err
      Right reqs -> do
        logInfo ( "Shaders were successfully compiled.\nShader directory:\n" <> ShortText.pack shaderDir )
        pure reqs

  -------------------------------------------
  -- Initialise window and Vulkan context.

  ( window, windowExtensions ) <-
    initialiseWindow
      WindowInfo
        { width      = 1920
        , height     = 1080
        , windowName = appName
        , mouseMode  = SDL.RelativeLocation
        }

  features <- liftIO ( requiredFeatures reqs )
  let
    surfaceInfo =
      SurfaceInfo
        { surfaceWindow = window
        , preferredFormat =
            VkSurfaceFormatKHR
              Vulkan.VK_FORMAT_B8G8R8A8_UNORM
              Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
        , surfaceUsage =
            [ Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT
            , Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
            , Vulkan.VK_IMAGE_USAGE_STORAGE_BIT
            ]
        }

  VulkanContext{..} <-
    initialiseContext @WithSwapchain appName windowExtensions
      RenderInfo
        { features
        , queueType   = Vulkan.VK_QUEUE_COMPUTE_BIT
        , surfaceInfo = surfaceInfo
        }

  withSwapchainInfo aSwapchainInfo \ ( swapchainInfo@(SwapchainInfo {..}) :: SwapchainInfo numImages ) -> do

  -------------------------------------------
  -- Create images.

    let

      width, height :: Num a => a
      width  = fromIntegral $ Vulkan.getField @"width"  swapchainExtent
      height = fromIntegral $ Vulkan.getField @"height" swapchainExtent

      extent3D :: Vulkan.VkExtent3D
      extent3D
        = Vulkan.createVk
            (  Vulkan.set @"width"  width
            &* Vulkan.set @"height" height
            &* Vulkan.set @"depth"  1
            )

      colFmt :: Vulkan.VkFormat
      colFmt = Vulkan.getField @"format" surfaceFormat

    swapchainImagesAndViews <-
      for swapchainImages \swapchainImage -> do
        swapchainImageView
          <- createImageView
                device swapchainImage
                Vulkan.VK_IMAGE_VIEW_TYPE_2D
                colFmt
                Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
        pure ( swapchainImage, swapchainImageView )

    screenshotImagesAndMemories <-
      for swapchainImages $ \ _ ->
        createScreenshotImage physicalDevice device
          ( screenshotImageInfo extent3D colFmt )

    -------------------------------------------
    -- Manage resources: uniform buffers, storage images.

    let

      resourceFlags :: ResourceSet numImages Named
      resourceFlags = ResourceSet
        ( StageFlags Vulkan.VK_SHADER_STAGE_COMPUTE_BIT )
        ( StageFlags Vulkan.VK_SHADER_STAGE_COMPUTE_BIT )

      initialCamera :: Camera
      initialCamera = camera initialObserverLogo Nothing

      initialResourceSet :: ResourceSet numImages Pre
      initialResourceSet =
        ResourceSet
          { cameraResource   = UniformBuffer initialCamera
          , storageResources = Ixed $
              ( fmap ( StorageImage . snd ) swapchainImagesAndViews )
          }

    PostInitialisationResult
      descriptorSetLayout descriptorSets _ resources
        <- initialiseResources physicalDevice device resourceFlags initialResourceSet

    -------------------------------------------
    -- Create a command buffer and record the commands into it.

    commandPool <- logDebug "Creating command pool" *> createCommandPool device queueFamilyIndex
    queue       <- getQueue device 0

    nextImageSem <- createSemaphore device
    submitted    <- createSemaphore device

    pipelineLayout <- createPipelineLayout device descriptorSetLayout

    shader <- loadShader device compPath

    let
      recordCommandBuffers pipe =
        for (V.zip descriptorSets swapchainImagesAndViews) $ \ ( descriptorSet, (swapchainImage, _ ) ) ->
          recordSimpleDispatch
            device commandPool
            descriptorSet
            ( swapchainImage, swapchainExtent )
            Nothing
            globalSizes
            pipelineLayout pipe
      recordScreenshotCommandBuffers pipe =
        for (V.zip3 descriptorSets swapchainImagesAndViews screenshotImagesAndMemories)
        \ ( descriptorSet, (swapchainImage, _ ), (screenshotImage, _) ) ->
          recordSimpleDispatch
            device commandPool
            descriptorSet
            ( swapchainImage, swapchainExtent )
            ( Just ( screenshotImage, extent3D ) )
            globalSizes
            pipelineLayout pipe

      recordAllCommandsFromShaders = record2CommandBuffersFromShaders
        ( createComputePipeline device pipelineLayout . runIdentity )
        recordCommandBuffers
        recordScreenshotCommandBuffers

    -- launch shader reload watcher, which writes command buffers to use to a TVar
    resourcesTVar <-
      statelessly $ shaderReloadWatcher device
        ( Identity (compPath, shader) )
        recordAllCommandsFromShaders

    mainLoop do

      ----------------
      -- shader reloading

      ( updatedCommands, updatedScreenshotCommands )
        <- statelessly ( snd <$> readTVarWithCleanup resourcesTVar )

      ----------------
      -- input

      inputEvents <- map SDL.Event.eventPayload <$> SDL.pollEvents
      prevInput <- use _input
      let newInput = foldl onSDLInput prevInput inputEvents
      let action = interpretInput newInput
          angs   = fmap getSum ( look action )
      assign _input ( newInput { mouseRel = pure 0, keysPressed = [] } )

      ----------------
      -- simulation

      oldObserver <- use _observer
      let
        oldAngs = angles oldObserver
        newAngs@(V2 x y) = oldAngs ^+^ angs
        orientation = axisAngle (V3 0 (-1) 0) x FIR.* axisAngle (V3 1 0 0) y

        pos, fwd, up, right :: V 3 Float
        pos   = rotate orientation ( V3 0 0 10 )
        fwd   = normalise ( (-1) *^ pos )
        up    = rotate orientation ( V3 0 (-1) 0 )
        right = rotate orientation ( V3 1   0  0 )

        cam :: Camera
        cam = pos :& right :& up :& fwd :& End

        newObserver = oldObserver { angles = newAngs }

      assign _observer newObserver

      when ( locate action )
        ( liftIO $ putStrLn ( show newObserver ) )

     -- update camera
      let
        BufferResource _ updateCameraBuffer = cameraResource resources

      liftIO (updateCameraBuffer cam)

      ----------------
      -- rendering

      nextImageIndex <- acquireNextImage device swapchainInfo nextImageSem

      let
        commandBuffer
          | takeScreenshot action = updatedScreenshotCommands `V.index` nextImageIndex
          | otherwise             = updatedCommands           `V.index` nextImageIndex

      submitCommandBuffer
        queue
        commandBuffer
        [(nextImageSem, Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
        [submitted]
        Nothing

      present queue swapchain nextImageIndex [submitted]

      when ( takeScreenshot action ) $
        writeScreenshotData shortName device swapchainExtent
          ( snd ( screenshotImagesAndMemories `V.index` nextImageIndex ) )

      liftIO ( Vulkan.vkQueueWaitIdle queue )
        >>= throwVkResult

      ----------------

      pure ( shouldQuit action )
