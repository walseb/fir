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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Examples.Logo ( logo ) where

-- base
import Control.Monad
  ( when, replicateM )
import Data.Maybe
  ( fromJust )

import Data.Monoid
  ( Sum(getSum) )
import Data.Proxy
  ( Proxy )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import qualified Foreign.Marshal
import GHC.Generics
  ( Generic )
import GHC.TypeNats
  ( SomeNat(SomeNat), someNatVal )

-- lens
import Control.Lens
  ( use, assign )

-- managed
import Control.Monad.Managed
  ( MonadManaged, runManaged )

-- sdl2
import qualified SDL
import qualified SDL.Event

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )
import Control.Monad.Trans.State.Lazy
  ( evalStateT )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( fromList, unsafeIndex )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vulkan
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
import Examples.Logo.Shaders
import Simulation.Observer
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

----------------------------------------------------------------------------
-- Application.

logo :: IO ()
logo = ( runManaged . ( `evalStateT` initialState { observer = initialObserverLogo } ) ) do

  -------------------------------------------
  -- Obtain requirements from shaders.

  ( reqs :: ModuleRequirements ) <-
    case shaderCompilationResult of
      Left  err  -> error $ "Shader compilation was unsuccessful:\n" <> ShortText.unpack err
      Right reqs -> logMsg ( "Shaders were succesfully compiled." ) *> pure reqs

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
  let

    SwapchainInfo { .. } = swapchainInfo

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

  -------------------------------------------
  -- Create images.

  let
    numImages :: Int
    numImages = length swapchainImages

  case someNatVal ( fromIntegral numImages ) of
    SomeNat ( _ :: Proxy numImages ) -> do

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
        replicateM numImages $
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
            , storageResources = Ixed . fromJust . V.fromList $
                ( map ( StorageImage . snd ) swapchainImagesAndViews )
            }

      ( descriptorSetLayout, descriptorSets, resources ) <-
        initialiseResources physicalDevice device resourceFlags initialResourceSet

      -------------------------------------------
      -- Create a command buffer and record the commands into it.

      ( computePipeline, pipelineLayout )
        <- createComputePipeline device descriptorSetLayout compPath

      commandPool <- logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex
      queue       <- getQueue device 0

      nextImageSem <- createSemaphore device
      submitted    <- createSemaphore device

      let
        recordCommandBuffer
          :: MonadManaged m
          => Vulkan.VkDescriptorSet
          -> Vulkan.VkImage
          -> Maybe Vulkan.VkImage
          -> m Vulkan.VkCommandBuffer
        recordCommandBuffer descriptorSet swapchainImage mbScreenshotImage = do
          commandBuffer <- allocateCommandBuffer device commandPool

          beginCommandBuffer commandBuffer

          cmdTransitionImageLayout commandBuffer swapchainImage
            Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
            Vulkan.VK_IMAGE_LAYOUT_GENERAL
            ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
            ( Vulkan.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.VK_ACCESS_SHADER_WRITE_BIT )

          liftIO do
            Vulkan.vkCmdBindPipeline
              commandBuffer
              Vulkan.VK_PIPELINE_BIND_POINT_COMPUTE
              computePipeline

            Foreign.Marshal.withArray [ descriptorSet ] $ \descriptorSetsPtr ->
              Vulkan.vkCmdBindDescriptorSets
                commandBuffer
                Vulkan.VK_PIPELINE_BIND_POINT_COMPUTE
                pipelineLayout
                0 -- no offset
                1 -- unique descriptor set
                descriptorSetsPtr
                0 -- no dynamic offset
                Vulkan.vkNullPtr

            Vulkan.vkCmdDispatch
              commandBuffer
              120 135 1 -- local size 16 8 1

          case mbScreenshotImage of
            Just screenshotImage
              -> cmdTakeScreenshot
                    ( Vulkan.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.VK_ACCESS_SHADER_WRITE_BIT )
                    commandBuffer extent3D
                    ( swapchainImage,
                      ( Vulkan.VK_IMAGE_LAYOUT_GENERAL
                      , Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                      )
                    )
                    screenshotImage
            Nothing
              -> cmdTransitionImageLayout commandBuffer swapchainImage
                    Vulkan.VK_IMAGE_LAYOUT_GENERAL
                    Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    ( Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.VK_ZERO_FLAGS )
                    ( Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.VK_ZERO_FLAGS )

          endCommandBuffer commandBuffer

          pure commandBuffer

      commandBuffers <-
        for (zip [0..] swapchainImagesAndViews) \ ( i, ( swapImg, _ ) ) ->
          recordCommandBuffer ( descriptorSets `V.unsafeIndex` i ) swapImg Nothing

      screenshotCommandBuffers <-
        for (zip3 [0..] swapchainImagesAndViews screenshotImagesAndMemories)
          \ ( i, ( swapImg, _ ), ( screenshotImg, _ ) ) ->
              recordCommandBuffer ( descriptorSets `V.unsafeIndex` i )  swapImg (Just screenshotImg)


      mainLoop do

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

        nextImageIndex <- acquireNextImage device swapchain nextImageSem

        let
          commandBuffer
            | takeScreenshot action = screenshotCommandBuffers !! nextImageIndex
            | otherwise             = commandBuffers           !! nextImageIndex

        submitCommandBuffer
          queue
          commandBuffer
          [(nextImageSem, Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)]
          [submitted]
          Nothing

        present queue swapchain nextImageIndex [submitted]

        when ( takeScreenshot action ) $
          writeScreenshotData shortName device swapchainExtent
            ( snd ( screenshotImagesAndMemories !! nextImageIndex ) )

        liftIO ( Vulkan.vkQueueWaitIdle queue )
          >>= throwVkResult

        ----------------

        pure ( shouldQuit action )
