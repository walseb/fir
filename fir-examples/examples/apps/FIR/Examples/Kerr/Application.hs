{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
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

module FIR.Examples.Kerr.Application ( kerr ) where

-- base
import Control.Monad
  ( when )
import Data.Functor.Identity
  ( Identity(..) )
import Data.Monoid
  ( Sum(..) )
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

-- vulkan
import qualified Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct((:&),End)
  , ModuleRequirements(..)
  )
import qualified FIR
import Math.Linear
  ( pattern V2, pattern V3, pattern V4
  , (^+^), (^-^)
  )
import Math.Quaternion
  ( rotate, axisAngle )

-- fir-examples
import FIR.Examples.Common
import FIR.Examples.Kerr.Coordinates
  ( boyerLindquistPosition, boyerLindquistTangent
  , proj, normalise, gramSchmidt
  )
import FIR.Examples.Kerr.Shaders
  ( compPath, compileComputeShader
  , Camera
  )
import qualified FIR.Examples.Kerr.Shaders as Kerr
import FIR.Examples.Kerr.Info
  ( KerrInfo, defaultKerrInfo
  , DiskInfo, defaultDiskInfo
  )
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
initialStateKerr = initialState { observer = initialObserverKerr }

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

globalSizes :: ( Word32, Word32, Word32 )
globalSizes = ( 20, 30, 1 )

----------------------------------------------------------------------------
-- Application.

kerr :: IO ()
kerr = runVulkan initialStateKerr do

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
        { width      = Kerr.width
        , height     = Kerr.height
        , windowName = appName
        , mouseMode  = SDL.RelativeLocation
        }

  let
    features = requiredFeatures reqs
    surfaceInfo =
      SurfaceInfo
        { surfaceWindow = window
        , preferredFormat =
            Vulkan.SurfaceFormatKHR
              Vulkan.FORMAT_B8G8R8A8_UNORM
              Vulkan.COLOR_SPACE_SRGB_NONLINEAR_KHR
        , surfaceUsage =
            [ Vulkan.IMAGE_USAGE_TRANSFER_SRC_BIT
            , Vulkan.IMAGE_USAGE_TRANSFER_DST_BIT
            , Vulkan.IMAGE_USAGE_STORAGE_BIT
            ]
        }

  VulkanContext{..} <-
    initialiseContext @WithSwapchain appName windowExtensions ( requiredExtensions reqs )
      RenderInfo
        { features
        , queueType   = Vulkan.QUEUE_COMPUTE_BIT
        , surfaceInfo = surfaceInfo
        }


  withSwapchainInfo aSwapchainInfo \ ( swapchainInfo@(SwapchainInfo {..}) :: SwapchainInfo numImages ) -> do

  -------------------------------------------
  -- Create images.

    let

      width, height :: Num a => a
      width  = fromIntegral $ ( Vulkan.width  :: Vulkan.Extent2D -> Word32 ) swapchainExtent
      height = fromIntegral $ ( Vulkan.height :: Vulkan.Extent2D -> Word32 ) swapchainExtent

      extent3D :: Vulkan.Extent3D
      extent3D
        = Vulkan.Extent3D
            { Vulkan.width  = width
            , Vulkan.height = height
            , Vulkan.depth  = 1
            }

      colFmt :: Vulkan.Format
      colFmt = ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) surfaceFormat

    swapchainImagesAndViews <-
      for swapchainImages \swapchainImage -> do
        swapchainImageView
          <- createImageView
                device swapchainImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                colFmt
                Vulkan.IMAGE_ASPECT_COLOR_BIT
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
        ( StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT )
        ( StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT )
        ( StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT )
        ( StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT )

      initialCamera :: Camera
      initialCamera = boyerLindquistCamera initialObserverKerr

      initialResourceSet :: ResourceSet numImages Pre
      initialResourceSet =
        ResourceSet
          { cameraResource   = BufferData initialCamera
          , kerrInfoResource = BufferData defaultKerrInfo
          , diskInfoResource = BufferData defaultDiskInfo
          , storageResources = Ixed $
              ( fmap ( StorageImage . snd ) swapchainImagesAndViews )
          }

    PostInitialisationResult
      descriptorSetLayout descriptorSets _ resources
       <- initialiseResources physicalDevice device resourceFlags initialResourceSet


    -------------------------------------------
    -- Create command buffers and record commands into them.

    commandPool <- logDebug "Creating command pool" *> ( snd <$> createCommandPool device queueFamilyIndex )
    queue       <- getQueue device 0

    nextImageSem <- createSemaphore device
    submitted    <- createSemaphore device

    pipelineLayout <- logDebug "Creating pipeline layout" *> createPipelineLayout device [descriptorSetLayout]

    shader <- logDebug "Loading shader" *> loadShader device compPath

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

      nextImageIndex <- acquireNextImage device swapchainInfo nextImageSem

      let
        commandBuffer
          | takeScreenshot action = updatedScreenshotCommands `V.index` nextImageIndex
          | otherwise             = updatedCommands           `V.index` nextImageIndex

      submitCommandBuffer
        queue
        commandBuffer
        [(nextImageSem, Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
        [submitted]
        Nothing

      present queue swapchain nextImageIndex [submitted]

      Vulkan.queueWaitIdle queue

      when ( takeScreenshot action ) $
        writeScreenshotData shortName device swapchainExtent
          ( snd ( screenshotImagesAndMemories `V.index` nextImageIndex ) )

      ----------------

      pure ( shouldQuit action )
