{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
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

module FIR.Examples.Ising.Application ( ising ) where

-- base
import Control.Monad
  ( when )
import Data.Bits
  ( (.|.) )
import Data.Foldable
  ( for_, traverse_ )
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
  ( assign, use )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- resourcet
import Control.Monad.Trans.Resource
  ( release )

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
  ( imapM, index, unzip )

-- vulkan
import qualified Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct((:&),End)
  , ModuleRequirements(..)
  )
import Math.Linear
  ( pattern V2 )

-- fir-examples
import FIR.Examples.Ising.Shaders
import qualified FIR.Examples.Ising.Shaders as Ising
import FIR.Examples.Paths
import FIR.Examples.Reload
import FIR.Examples.RenderState
import Vulkan.Backend
import Vulkan.Context
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.Resource
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- Compute shaders, resources.

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ( "Even simulation step shader", compileEvenStepShader )
        , ( "Odd simulation step shader" , compileOddStepShader  )
        , ( "Resolve shader"             , compileResolveShader  )
        ]
     )

appName :: IsString a => a
appName = "fir-examples - Ising model"
shortName :: String
shortName = "ising" -- name for screenshots

data ResourceSet i st
  = ResourceSet
    { stateResource    :: UniformBuffer IsingParameters i st
    , outputImage      :: StorageImages                 i st
    , evenCheckerboard :: StorageImages                 i st
    ,  oddCheckerboard :: StorageImages                 i st
    }
  deriving ( Generic )

data Triple a = Triple a a a
  deriving stock ( Show, Functor, Foldable, Traversable )
instance Applicative Triple where
  pure a = Triple a a a
  Triple f1 f2 f3 <*> Triple a1 a2 a3 = Triple ( f1 a1 ) ( f2 a2 ) ( f3 a3 )

----------------------------------------------------------------------------
-- Application.

ising :: IO ()
ising = runVulkan initialState do

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
        { width      = Ising.width
        , height     = Ising.height
        , windowName = appName
        , mouseMode  = SDL.AbsoluteLocation
        }

  let
    vulkanReqs = addInstanceExtensions windowExtensions $ vulkanRequirements reqs
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
    initialiseContext @WithSwapchain Normal appName vulkanReqs
      RenderInfo
        { queueType   = Vulkan.QUEUE_COMPUTE_BIT
        , surfaceInfo = surfaceInfo
        }

  withSwapchainInfo aSwapchainInfo \ ( swapchainInfo@(SwapchainInfo {..}) :: SwapchainInfo numImages ) -> do

    commandPool <- logDebug "Creating command pool" *> ( snd <$> createCommandPool device queueFamilyIndex )
    queue       <- getQueue device 0

  -------------------------------------------
  -- Create images.

    let

      sc_width, sc_height :: Num a => a
      sc_width  = fromIntegral $ ( Vulkan.width  :: Vulkan.Extent2D -> Word32 ) swapchainExtent
      sc_height = fromIntegral $ ( Vulkan.height :: Vulkan.Extent2D -> Word32 ) swapchainExtent

      extent3D, checkerboardExtent3D :: Vulkan.Extent3D
      extent3D
        = Vulkan.Extent3D
            { Vulkan.width  = sc_width
            , Vulkan.height = sc_height
            , Vulkan.depth  = 1
            }
      checkerboardExtent3D
        = Vulkan.Extent3D
            { Vulkan.width  = ( Ising.ss * Ising.width ) `div` 2
            , Vulkan.height = Ising.ss * Ising.height
            , Vulkan.depth  = 1
            }

      colFmt, checkerboardFormat :: Vulkan.Format
      colFmt = ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) surfaceFormat
      checkerboardFormat = Vulkan.FORMAT_R32_SFLOAT



    swapchainImagesAndViews <-
      for swapchainImages \ swapchainImage -> do
        swapchainImageView
          <- createImageView
                device swapchainImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                colFmt
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        pure ( swapchainImage, swapchainImageView )

    screenshotImagesAndMemories <-
      for swapchainImages \ _ ->
        createScreenshotImage physicalDevice device
          ( screenshotImageInfo extent3D colFmt )
    
    let
      checkerboardImageInfo :: ImageInfo
      checkerboardImageInfo =
        Default2DImageInfo
          checkerboardExtent3D
          checkerboardFormat
          Vulkan.IMAGE_USAGE_STORAGE_BIT

    checkerboardImages <-
      for swapchainImages \ _ -> do
        -- Create images.
        ( evenImage, evenImageMemory ) <-
          createImage
            physicalDevice device checkerboardImageInfo
            Vulkan.zero
        evenImageView
          <- createImageView
                device evenImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                checkerboardFormat
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        ( oddImage, oddImageMemory ) <-
          createImage
            physicalDevice device checkerboardImageInfo
            Vulkan.zero
        oddImageView
          <- createImageView
                device oddImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                checkerboardFormat
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        -- Not initializing images manually (too annoying).
        pure
          ( ( evenImage, evenImageMemory, evenImageView )
          , (  oddImage,  oddImageMemory,  oddImageView )
          )

    ( _, imageInitCommandBuffer ) <- allocateCommandBuffer device commandPool
    beginCommandBuffer imageInitCommandBuffer
    for_ checkerboardImages \ ( ( evenImage, _, _ ), ( oddImage, _, _ ) ) ->
      for_ [ evenImage, oddImage ] \ image ->
        cmdTransitionImageLayout imageInitCommandBuffer image
          Vulkan.IMAGE_LAYOUT_UNDEFINED
          Vulkan.IMAGE_LAYOUT_GENERAL
          ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.zero )
          ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT
          , Vulkan.ACCESS_SHADER_READ_BIT .|. Vulkan.ACCESS_SHADER_WRITE_BIT
          )
    endCommandBuffer imageInitCommandBuffer
    submitCommandBuffer queue imageInitCommandBuffer [] [] Nothing

    -------------------------------------------
    -- Manage resources: uniform buffers, storage images.

    startTime <- SDL.time

    let

      resourceFlags :: ResourceSet numImages Named
      resourceFlags = ResourceSet
        ( StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT )
        ( StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT )
        ( StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT )
        ( StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT )


      initialResourceSet :: ResourceSet numImages Pre
      initialResourceSet =
        ResourceSet
          { stateResource    = BufferData ( 1 :& 1 :& 0 :& startTime :& 1 :& End )
          , outputImage      = Ixed $ fmap ( StorageImage . snd ) swapchainImagesAndViews
          , evenCheckerboard = Ixed $
              fmap
                ( StorageImage . \ ( ( _, _, evenImageView ), _ ) -> evenImageView )
                checkerboardImages
          ,  oddCheckerboard = Ixed $
              fmap
                ( StorageImage . \ ( _, ( _, _, oddImageView ) ) -> oddImageView )
                checkerboardImages
          }

    PostInitialisationResult
      descriptorSetLayout descriptorSets _ resources
        <- initialiseResources physicalDevice device resourceFlags initialResourceSet

    -------------------------------------------
    -- Create a command buffer and record the commands into it.

    (_, nextImageSem ) <- createSemaphore device
    (_, submitted    ) <- createSemaphore device

    pipelineLayout <- logDebug "Creating pipeline layout" *> createPipelineLayout device [descriptorSetLayout,descriptorSetLayout]

    evenStepShader <- logDebug "Loading even step shader" *> loadShader device evenStepPath
    oddStepShader  <- logDebug "Loading odd step shader"  *> loadShader device oddStepPath
    resolShader    <- logDebug "Loading resolve shader"   *> loadShader device resolvePath

    let
      recordCommandBuffers takeScreenshot ( Triple evenPipeline oddPipeline resolvePipeline ) =
        ( `V.imapM` swapchainImagesAndViews ) \ i ( swapchainImage, _ ) -> do
          let
            descriptorSet, prevDescriptorSet :: Vulkan.DescriptorSet
            descriptorSet     = descriptorSets `V.index` i
            prevDescriptorSet = descriptorSets `V.index` ( i - 1 )
            ( ( evenImage, _, _ ), ( oddImage, _, _ ) ) = checkerboardImages `V.index` i
          res@(_, commandBuffer) <- allocateCommandBuffer device commandPool
          beginCommandBuffer commandBuffer

          cmdBindPipeline commandBuffer evenPipeline
          cmdBindDescriptorSets commandBuffer pipelineLayout evenPipeline [ descriptorSet, prevDescriptorSet ]

          liftIO $
            Vulkan.cmdDispatch
              commandBuffer
              ( ( Ising.ss * Ising.width  ) `div` ( 2 * Ising.localSizeX ) )
              ( ( Ising.ss * Ising.height ) `div`       Ising.localSizeY   )
              1

          -- image memory barrier: finish writing to the "even" checkerboard
          cmdTransitionImageLayout commandBuffer evenImage
            Vulkan.IMAGE_LAYOUT_GENERAL
            Vulkan.IMAGE_LAYOUT_GENERAL
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_WRITE_BIT )
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_READ_BIT  )

          cmdBindPipeline commandBuffer oddPipeline
          cmdBindDescriptorSets commandBuffer pipelineLayout oddPipeline [ descriptorSet, prevDescriptorSet ]

          liftIO $
            Vulkan.cmdDispatch
              commandBuffer
              ( ( Ising.ss * Ising.width  ) `div` ( 2 * Ising.localSizeX ) )
              ( ( Ising.ss * Ising.height ) `div`       Ising.localSizeY   )
              1

          -- image memory barrier: finish writing to the "odd" checkerboard
          cmdTransitionImageLayout commandBuffer oddImage
            Vulkan.IMAGE_LAYOUT_GENERAL
            Vulkan.IMAGE_LAYOUT_GENERAL
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_WRITE_BIT )
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_READ_BIT  )
          -- ensure the swapchain image is in the correct layout for writing to it
          cmdTransitionImageLayout commandBuffer swapchainImage
            Vulkan.IMAGE_LAYOUT_UNDEFINED
            Vulkan.IMAGE_LAYOUT_GENERAL
            ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.zero )
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_WRITE_BIT )

          cmdBindPipeline commandBuffer resolvePipeline
          cmdBindDescriptorSets commandBuffer pipelineLayout resolvePipeline [ descriptorSet ]

          liftIO $
            Vulkan.cmdDispatch
              commandBuffer
              ( Ising.width  `div` Ising.localSizeX )
              ( Ising.height `div` Ising.localSizeY )
              1

          if takeScreenshot
          then do
            let screenshotImage = fst ( screenshotImagesAndMemories `V.index` i )
            cmdTakeScreenshot
              ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_WRITE_BIT )
              commandBuffer extent3D
              ( swapchainImage,
                ( Vulkan.IMAGE_LAYOUT_GENERAL
                , Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
                )
              )
              screenshotImage
          else
            cmdTransitionImageLayout commandBuffer swapchainImage
              Vulkan.IMAGE_LAYOUT_GENERAL
              Vulkan.IMAGE_LAYOUT_PRESENT_SRC_KHR
              ( Vulkan.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, Vulkan.zero )
              ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT   , Vulkan.zero )
          endCommandBuffer commandBuffer
          pure res

      recordAllCommandsFromShaders ( Triple shaderEven shaderOdd shaderResolve ) = do
        ( keyEven, pipeEven ) <- createComputePipeline device pipelineLayout shaderEven
        ( keyOdd , pipeOdd  ) <- createComputePipeline device pipelineLayout shaderOdd
        ( keyRes , pipeRes  ) <- createComputePipeline device pipelineLayout shaderResolve
        ( cmdKeys1, commands1 ) <- V.unzip <$> recordCommandBuffers False ( Triple pipeEven pipeOdd pipeRes )
        ( cmdKeys2, commands2 ) <- V.unzip <$> recordCommandBuffers True  ( Triple pipeEven pipeOdd pipeRes )
        let
          releaseAll = do
            release keyEven
            release keyOdd
            release keyRes
            traverse_ release cmdKeys1
            traverse_ release cmdKeys2
        pure ( releaseAll, ( commands1, commands2 ) )

    -- launch shader reload watcher, which writes command buffers to use to a TVar
    resourcesTVar <-
      statelessly $ shaderReloadWatcher device
        ( Triple
          ( evenStepPath, evenStepShader )
          ( oddStepPath , oddStepShader  )
          ( resolvePath , resolShader    )
        )
        recordAllCommandsFromShaders

    mainLoop do

      Vulkan.deviceWaitIdle device

      ----------------
      -- shader reloading

      ( updatedCommands, updatedScreenshotCommands )
        <- statelessly ( snd <$> readTVarWithCleanup resourcesTVar )

      ----------------
      -- input

      inputEvents <- map SDL.Event.eventPayload <$> SDL.pollEvents
      prevInput <- use _input
      timeNow <- SDL.time
      let
        newInput = foldl onSDLInput prevInput inputEvents
        action   = interpretInput newInput
        pos@(V2 px py) = mousePos newInput
        temperature = max 1e-4 $ px / ( 0.3 * Ising.width )
        interactionStrength = 0.7
        magneticField = ( py - 0.5 * Ising.height ) / ( 3 * Ising.height )
        reset :: Word32
        reset 
          |  SDL.ScancodeR `elem` ( keysPressed newInput ) 
          || timeNow - startTime < 0.1 -- just started, randomize spins
          = 1
          | otherwise
          = 0
        newParameters :: IsingParameters
        newParameters
          =  temperature
          :& interactionStrength
          :& magneticField
          :& timeNow
          :& reset
          :& End

      assign _input ( newInput { mousePos = pos, mouseRel = pure 0, keysPressed = [] } )

      when ( locate action )
        ( liftIO $ putStrLn ( show newParameters ) )

      -- update UBO
      let
        BufferResource _ updateStateBuffer = stateResource resources

      liftIO (updateStateBuffer newParameters)

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
