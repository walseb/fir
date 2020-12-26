{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module FIR.Examples.Hopf.Application ( hopf ) where

-- base
import Control.Monad
  ( when )
import Data.Bits
  ( (.|.) )
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

-- vector
import qualified Data.Vector as Boxed.Vector
  ( singleton )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( fromTuple, zip, zip3, index )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Zero as Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct(..)
  , (:->)((:->))
  , ModuleRequirements(..)
  )
import Math.Linear

-- fir-examples
import FIR.Examples.Common
import FIR.Examples.Hopf.Shaders
import FIR.Examples.Hopf.Villarceau
import FIR.Examples.Paths
import FIR.Examples.Reload
import FIR.Examples.RenderState
import Vulkan.Attachment
import Vulkan.Backend
import Vulkan.Context
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.Resource
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- Shaders and resources.

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Vertex shader"                 , compileVertexShader                 )
        , ("Tessellation control shader"   , compileTessellationControlShader    )
        , ("Tessellation evaluation shader", compileTessellationEvaluationShader )
        , ("Fragment shader"               , compileFragmentShader               )
        ]
     )

appName :: IsString a => a
appName = "fir-examples - Hopf fibration"
shortName :: String
shortName = "hopf" -- name for screenshots

type UBO =
  Struct
    '[ "mvp"    ':-> M 4 4 Float
     , "origin" ':-> V 4 Float
     ]

type VertexData = Struct VertexInput

data ResourceSet i st
  = ResourceSet
    { uboResource  :: UniformBuffer UBO        i st
    , vertexBuffer :: VertexBuffer  VertexData i st
    , indexBuffer  :: IndexBuffer   Word32     i st
    }
  deriving Generic

toriVerts :: [ Struct VertexInput ]
toriVerts
  =  villarceauCircles c1 a1 circleThickening nbCircles blues  0
  <> villarceauCircles c2 a2 circleThickening nbCircles greens ( 2 * pi / (3 * fromIntegral nbCircles) )
  <> villarceauCircles c3 a3 circleThickening nbCircles sunset ( 4 * pi / (3 * fromIntegral nbCircles) )
      where
        nbCircles = 10
        circleThickening = 0.025
        (a1, c1) = villarceauRadii 0.7
        (a2, c2) = villarceauRadii 1.1
        (a3, c3) = villarceauRadii 1.5

toriIndices :: [ Word32 ]
toriIndices = [ 0 .. nbIndices - 1 ]

nbIndices :: Word32
nbIndices = fromIntegral (length toriVerts)

initialResourceSet :: ResourceSet numImages Pre
initialResourceSet = ResourceSet
  ( BufferData ( initialMVP :& initialOrig :& End ) )
  ( BufferData toriVerts   )
  ( BufferData toriIndices )
    where
      initialMVP :: M 4 4 Float
      initialMVP = modelViewProjection initialObserver Nothing
      initialOrig :: V 4 Float
      initialOrig = V4 0 0 0 1 ^*! initialMVP

clearValues :: [ Vulkan.ClearValue ] -- in bijection with framebuffer attachments
clearValues =
  [ blackClear
  , Vulkan.DepthStencil depthStencilClear
  , blackClear
  ]
  where
    black :: Vulkan.ClearColorValue
    black = Vulkan.Float32 0 0 0 0
    blackClear :: Vulkan.ClearValue
    blackClear = Vulkan.Color black
    depthStencilClear :: Vulkan.ClearDepthStencilValue
    depthStencilClear = Vulkan.ClearDepthStencilValue { Vulkan.depth = 1, Vulkan.stencil = 0 }

----------------------------------------------------------------------------
-- Application.

hopf :: IO ()
hopf = runVulkan initialState do

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
            , Vulkan.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            ]
        }

  VulkanContext{..} <-
    initialiseContext @WithSwapchain Normal appName vulkanReqs
      RenderInfo
        { queueType   = Vulkan.QUEUE_GRAPHICS_BIT
        , surfaceInfo = surfaceInfo
        }

  withSwapchainInfo aSwapchainInfo \ ( swapchainInfo@(SwapchainInfo {..}) :: SwapchainInfo numImages ) -> do

  -------------------------------------------
  -- Create framebuffer attachments.

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

      colFmt, depthFmt :: Vulkan.Format
      colFmt   = ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) surfaceFormat
      depthFmt = Vulkan.FORMAT_D32_SFLOAT


      msImageInfo, depthImageInfo :: ImageInfo
      msImageInfo =
        ( Default2DImageInfo extent3D colFmt
          Vulkan.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        ) { imageSamples = Vulkan.SAMPLE_COUNT_8_BIT }
      depthImageInfo =
        ( Default2DImageInfo extent3D depthFmt
          Vulkan.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
        ) { imageSamples = Vulkan.SAMPLE_COUNT_8_BIT }


    renderPass <- logDebug "Creating a render pass" *>
      simpleRenderPass device
        ( noAttachments
          { colorAttachments = Boxed.Vector.singleton $ msColorAttachmentDescription Vulkan.SAMPLE_COUNT_8_BIT colFmt
          , mbDepthStencilAttachment = Just ( msDepthAttachmentDescription Vulkan.SAMPLE_COUNT_8_BIT depthFmt )
          , resolveAttachments = Boxed.Vector.singleton $ presentableColorAttachmentDescription colFmt
          }
        )

    framebuffersWithAttachments <- logDebug "Creating frame buffers"
      *> ( for swapchainImages $ \swapchainImage -> do

        colorImageView
          <- createImageView
                device swapchainImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                colFmt
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        (msImage, _)
          <- createImage physicalDevice device msImageInfo Vulkan.zero
        msImageView
          <- createImageView device msImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                colFmt
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        (depthImage, _)
          <- createImage physicalDevice device
                depthImageInfo
                Vulkan.zero
        depthImageView
          <- createImageView device depthImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                depthFmt
                Vulkan.IMAGE_ASPECT_DEPTH_BIT
        let attachments =
              V.fromTuple
                ( (msImage       , msImageView   )
                , (depthImage    , depthImageView)
                , (swapchainImage, colorImageView)
                )
        framebuffer <- createFramebuffer device renderPass swapchainExtent (fmap snd attachments)
        pure (framebuffer, attachments)
      )

    screenshotImagesAndMemories <-
      for swapchainImages $ \ _ ->
        createScreenshotImage physicalDevice device
          ( screenshotImageInfo extent3D colFmt )

    -------------------------------------------
    -- Manage resources.

    let

      resourceFlags :: ResourceSet numImages Named
      resourceFlags = ResourceSet
        ( StageFlags
          (   Vulkan.SHADER_STAGE_VERTEX_BIT
          .|. Vulkan.SHADER_STAGE_TESSELLATION_EVALUATION_BIT
          )
        )
        GeneralResource
        GeneralResource

    PostInitialisationResult
      descriptorSetLayout descriptorSets cmdBindBuffers resources
       <- initialiseResources physicalDevice device resourceFlags initialResourceSet

    -------------------------------------------
    -- Create command buffers and record commands into them.

    commandPool <- logDebug "Creating command pool" *> ( snd <$> createCommandPool device queueFamilyIndex )
    queue       <- getQueue device 0

    (_, nextImageSem ) <- createSemaphore device
    (_, submitted    ) <- createSemaphore device

    pipelineLayout <- logDebug "Creating pipeline layout" *> createPipelineLayout device [descriptorSetLayout]
    let pipelineInfo = VkPipelineInfo swapchainExtent Vulkan.SAMPLE_COUNT_8_BIT pipelineLayout

    shaders <- logDebug "Loading shaders" *> traverse (\path -> (path, ) <$> loadShader device path) shaderPipeline

    let
      recordCommandBuffers pipe =
        for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( descriptorSet, (framebuffer, attachments ) ) ->
          recordSimpleIndexedDrawCall
            device commandPool framebuffer (renderPass, clearValues)
            descriptorSet cmdBindBuffers
            ( fst $ attachments `V.index` 2, swapchainExtent )
            Nothing
            nbIndices
            pipelineLayout pipe
      recordScreenshotCommandBuffers pipe =
        for (V.zip3 descriptorSets framebuffersWithAttachments screenshotImagesAndMemories)
          \ ( descriptorSet, (framebuffer, attachments), (screenshotImage, _) ) ->
            recordSimpleIndexedDrawCall
              device commandPool framebuffer (renderPass, clearValues)
              descriptorSet cmdBindBuffers
              ( fst $ attachments `V.index` 2, swapchainExtent )
              ( Just ( screenshotImage, extent3D ) )
              nbIndices
              pipelineLayout pipe

      recordAllCommandsFromShaders = record2CommandBuffersFromShaders
        ( createGraphicsPipeline device renderPass pipelineInfo )
        recordCommandBuffers
        recordScreenshotCommandBuffers

    -- launch shader reload watcher, which writes command buffers to use to a TVar
    resourcesTVar <- statelessly $ shaderReloadWatcher device shaders recordAllCommandsFromShaders

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

      oldObserver <- use _observer
      let (observer, orientation) = oldObserver `move` action
      assign _observer observer

      let mvp = modelViewProjection observer (Just orientation)
          origin = mvp !*^ V4 0 0 0 1

      when ( locate action )
        ( liftIO $ putStrLn ( show observer ) )

      -- update UBO
      let
        BufferResource _ updateUBO = uboResource resources

      liftIO ( updateUBO ( mvp :& origin :& End ) )

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
