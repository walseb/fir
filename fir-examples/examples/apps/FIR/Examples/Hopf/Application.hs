{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
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

-- vector-sized
import qualified Data.Vector.Sized as V
  ( fromTuple, zip, zip3, index )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan                as Vulkan
import qualified Graphics.Vulkan.Core_1_0       as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct(..)
  , (:->)((:->))
  , ModuleRequirements
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
import Vulkan.Features
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
  ( UniformBuffer ( initialMVP :& initialOrig :& End ) )
  ( VertexBuffer toriVerts   )
  ( IndexBuffer  toriIndices )
    where
      initialMVP :: M 4 4 Float
      initialMVP = modelViewProjection initialObserver Nothing
      initialOrig :: V 4 Float
      initialOrig = V4 0 0 0 1 ^*! initialMVP

clearValues :: [ Vulkan.VkClearValue ] -- in bijection with framebuffer attachments
clearValues =
  [ blackClear
  , Vulkan.createVk ( Vulkan.set @"depthStencil" depthStencilClear )
  , blackClear
  ]
  where
    black :: Vulkan.VkClearColorValue
    black =
      Vulkan.createVk
        (  Vulkan.setAt @"float32" @0 0
        &* Vulkan.setAt @"float32" @1 0
        &* Vulkan.setAt @"float32" @2 0
        &* Vulkan.setAt @"float32" @3 1
        )

    blackClear :: Vulkan.VkClearValue
    blackClear = Vulkan.createVk ( Vulkan.set @"color" black )

    depthStencilClear :: Vulkan.VkClearDepthStencilValue
    depthStencilClear = Vulkan.createVk
      ( Vulkan.set @"depth" 1 &* Vulkan.set @"stencil" 0 )

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
            , Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            ]
        }

  VulkanContext{..} <-
    initialiseContext @WithSwapchain appName windowExtensions
      RenderInfo
        { features
        , queueType   = Vulkan.VK_QUEUE_GRAPHICS_BIT
        , surfaceInfo = surfaceInfo
        }

  withSwapchainInfo aSwapchainInfo \ ( swapchainInfo@(SwapchainInfo {..}) :: SwapchainInfo numImages ) -> do

  -------------------------------------------
  -- Create framebuffer attachments.

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

      colFmt, depthFmt :: Vulkan.VkFormat
      colFmt   = Vulkan.getField @"format" surfaceFormat
      depthFmt = Vulkan.VK_FORMAT_D32_SFLOAT


      msImageInfo, depthImageInfo :: ImageInfo
      msImageInfo =
        ( Default2DImageInfo extent3D colFmt
          Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        ) { imageSamples = Vulkan.VK_SAMPLE_COUNT_8_BIT }
      depthImageInfo =
        ( Default2DImageInfo extent3D depthFmt
          Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
        ) { imageSamples = Vulkan.VK_SAMPLE_COUNT_8_BIT }


    renderPass <- logDebug "Creating a render pass" *>
      simpleRenderPass device
        ( noAttachments
          { colorAttachments = [ msColorAttachmentDescription Vulkan.VK_SAMPLE_COUNT_8_BIT colFmt ]
          , mbDepthStencilAttachment = Just (msDepthAttachmentDescription Vulkan.VK_SAMPLE_COUNT_8_BIT depthFmt)
          , resolveAttachments = [ presentableColorAttachmentDescription colFmt ]
          }
        )

    framebuffersWithAttachments <- logDebug "Creating frame buffers"
      *> ( for swapchainImages $ \swapchainImage -> do

        colorImageView
          <- createImageView
                device swapchainImage
                Vulkan.VK_IMAGE_VIEW_TYPE_2D
                colFmt
                Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
        (msImage, _)
          <- createImage physicalDevice device msImageInfo []
        msImageView
          <- createImageView device msImage
                Vulkan.VK_IMAGE_VIEW_TYPE_2D
                colFmt
                Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
        (depthImage, _)
          <- createImage physicalDevice device
                depthImageInfo
                [ ]
        depthImageView
          <- createImageView device depthImage
                Vulkan.VK_IMAGE_VIEW_TYPE_2D
                depthFmt
                Vulkan.VK_IMAGE_ASPECT_DEPTH_BIT
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
          (   Vulkan.VK_SHADER_STAGE_VERTEX_BIT
          .|. Vulkan.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
          )
        )
        InputResource
        InputResource

    PostInitialisationResult
      descriptorSetLayout descriptorSets cmdBindBuffers resources
       <- initialiseResources physicalDevice device resourceFlags initialResourceSet

    -------------------------------------------
    -- Create command buffers and record commands into them.

    commandPool <- logDebug "Creating command pool" *> createCommandPool device queueFamilyIndex
    queue       <- getQueue device 0

    nextImageSem <- createSemaphore device
    submitted    <- createSemaphore device

    pipelineLayout <- logDebug "Creating pipeline layout" *> createPipelineLayout device [descriptorSetLayout]
    let pipelineInfo = VkPipelineInfo swapchainExtent Vulkan.VK_SAMPLE_COUNT_8_BIT pipelineLayout

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
        [(nextImageSem, Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
        [submitted]
        Nothing

      present queue swapchain nextImageIndex [submitted]

      liftIO ( Vulkan.vkQueueWaitIdle queue )
        >>= throwVkResult

      when ( takeScreenshot action ) $
        writeScreenshotData shortName device swapchainExtent
          ( snd ( screenshotImagesAndMemories `V.index` nextImageIndex ) )

      ----------------

      pure ( shouldQuit action )
