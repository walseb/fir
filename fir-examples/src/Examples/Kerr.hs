{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
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

module Examples.Kerr ( kerr ) where

-- base
import Control.Monad
  ( when, replicateM )
import Data.Maybe
  ( fromJust )
import Data.Monoid
  ( Sum(..) )
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
  , Struct((:&),End)
  , ModuleRequirements
  )
import qualified FIR
import Math.Linear
  ( pattern V2, pattern V3, pattern V4
  , (^+^), (^-^)
  )
import Math.Quaternion
  ( rotate, axisAngle )

-- fir-examples
import Examples.Kerr.Coordinates
  ( boyerLindquistPosition, boyerLindquistTangent
  , proj, normalise, gramSchmidt
  )
import Examples.Kerr.Shaders
  ( compPath, compileComputeShader
  , Camera
  )
import qualified Examples.Kerr.Shaders as Kerr
import Examples.Kerr.Info
  ( KerrInfo, defaultKerrInfo
  , DiskInfo, defaultDiskInfo
  )
import Simulation.Observer
import Vulkan.Backend
import Vulkan.Context
import Vulkan.Features
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.Resource
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- Compute shader, resources, and observer movement logic.

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Compute shader", compileComputeShader) ]
     )

appName :: IsString a => a
appName = "fir-examples - Kerr space-time"
shortName :: String
shortName = "kerr" -- name for screenshots

initialObserverKerr :: Observer
initialObserverKerr =
  Observer
    { position = V3 23 0 3.1
    , angles   = V2 (pi/2) (-0.14)
    , clock    = 0
    }

initialStateKerr :: RenderState
initialStateKerr =
  RenderState
    { observer   = initialObserverKerr
    , input      = nullInput
    }

boyerLindquistCamera :: Observer -> Camera
boyerLindquistCamera Observer { position = pos, angles = V2 x y, clock = clockTime } =
  case spacelike_vecs of
    [fwd_bl, up_bl, right_bl] -> pos_bl :& time_bl :& right_bl :& up_bl :& fwd_bl :& clockTime :& End
    _ -> error "impossible"
  where
    ori     = axisAngle (V3 0 0 1) x FIR.* axisAngle (V3 1 0 0) y
    pos_bl  = boyerLindquistPosition defaultKerrInfo pos
    time_bl = normalise defaultKerrInfo pos_bl (V4 1 0 0 0)
    spacelike_vecs = gramSchmidt defaultKerrInfo pos_bl $
      map
        ( ( \v -> v ^-^ proj defaultKerrInfo pos_bl v time_bl )
        . boyerLindquistTangent defaultKerrInfo pos_bl
        . rotate ori
        )
        [ V3 0 1 0, V3 0 0 1, V3 1 0 0 ]

data ResourceSet i st
  = ResourceSet
    { cameraResource   :: UniformBuffer Camera   i st
    , kerrInfoResource :: UniformBuffer KerrInfo i st
    , diskInfoResource :: UniformBuffer DiskInfo i st
    , storageResources :: StorageImages          i st
    }
  deriving ( Generic )

----------------------------------------------------------------------------
-- Application.

kerr :: IO ()
kerr = ( runManaged . ( `evalStateT` initialStateKerr ) ) do

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
        { width      = Kerr.width
        , height     = Kerr.height
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
          ( StageFlags Vulkan.VK_SHADER_STAGE_COMPUTE_BIT )
          ( StageFlags Vulkan.VK_SHADER_STAGE_COMPUTE_BIT )

        initialCamera :: Camera
        initialCamera = boyerLindquistCamera initialObserverKerr

        initialResourceSet :: ResourceSet numImages Pre
        initialResourceSet =
          ResourceSet
            { cameraResource   = UniformBuffer initialCamera
            , kerrInfoResource = UniformBuffer defaultKerrInfo
            , diskInfoResource = UniformBuffer defaultDiskInfo
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
              20 30 1 -- global sizes

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

      -------------------------------------------
      -- Main loop:
      --  * update game state,
      --  * submit the command buffer,
      --  * present result to the screen.

      mainLoop do

        ----------------
        -- input

        inputEvents <- map SDL.Event.eventPayload <$> SDL.pollEvents
        prevInput <- use _input
        let newInput = foldl onSDLInput prevInput inputEvents
        let action = interpretInput newInput
        assign _input ( newInput { mouseRel = pure 0, keysPressed = [] } )

        ----------------
        -- simulation

        Observer { position = oldPos, angles = oldAngs, clock = oldClock } <- use _observer
        let
          newAngs@(V2 x y) = oldAngs ^+^ fmap getSum (look action)
          mov = case fmap getSum (movement action) of
            V3 mx mz my -> V3 mx my (-mz)
          ori    = axisAngle (V3 0 0 1) x FIR.* axisAngle (V3 1 0 0) y
          newPos = oldPos ^+^ rotate ori mov
          newClock = oldClock + 1
          newObs = Observer { position = newPos, angles = newAngs, clock = newClock }

        assign _observer newObs
        let cam = boyerLindquistCamera newObs

        when ( locate action ) do
          liftIO $ putStrLn ( show newObs )
          liftIO $ putStrLn "Boyer-Lindquist coordinates follow"
          liftIO $ putStrLn ( show cam )

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
