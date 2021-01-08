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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module FIR.Examples.JuliaSet.Application ( juliaSet ) where

-- base
import Control.Monad
  ( when, void )
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
import qualified SDL.Raw.Event as SDL

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
  ( zip, zip3, index )

-- vulkan
import qualified Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct((:&),End)
  , ModuleRequirements(..)
  )
import Math.Linear
  ( V
  , pattern V2, pattern V3
  , (^+^), (*^)
  )

-- fir-examples
import FIR.Examples.Common
import FIR.Examples.JuliaSet.Shaders
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
        [ ("Vertex shader"  , compileVertexShader   )
        , ("Fragment shader", compileFragmentShader )
        ]
     )

appName :: IsString a => a
appName = "fir-examples - Julia set"
shortName :: String
shortName = "julia" -- name for screenshots

type VertexData = Struct VertexInput

data ResourceSet i st
  = ResourceSet
    { mousePosUBO  :: UniformBuffer (V 2 Float) i st
    , vertexBuffer :: VertexBuffer  VertexData  i st
    , indexBuffer  :: IndexBuffer   Word32      i st
    }
  deriving Generic

viewportVertices :: [ Struct VertexInput ]
viewportVertices =
  [ V3 (-1) (-1) 0 :& End
  , V3 (-1)   1  0 :& End
  , V3   1 (-1)  0 :& End
  , V3   1   1   0 :& End
  ]

viewportIndices :: [ Word32 ]
viewportIndices
  = [ 0, 1, 2
    , 2, 1, 3
    ]

nbIndices :: Word32
nbIndices = 6

initialResourceSet :: ResourceSet numImages Pre
initialResourceSet = ResourceSet
  ( BufferData ( V2 0 0 ) )
  ( BufferData viewportVertices  )
  ( BufferData viewportIndices   )

clearValue :: Vulkan.ClearValue
clearValue = Vulkan.Color black
  where
    black :: Vulkan.ClearColorValue
    black = Vulkan.Float32 0 0 0 0

----------------------------------------------------------------------------
-- Application.

juliaSet :: IO ()
juliaSet = runVulkan initialState do

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
        , mouseMode  = SDL.AbsoluteLocation
        }
  let
    vulkanReqs = ignoreMinVersion . addInstanceExtensions windowExtensions $ vulkanRequirements reqs
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

      colFmt :: Vulkan.Format
      colFmt = ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) surfaceFormat

    renderPass <- logDebug "Creating a render pass" *>
      simpleRenderPass device
        ( noAttachments
          { colorAttachments = Boxed.Vector.singleton $ presentableColorAttachmentDescription colFmt }
        )

    framebuffersWithAttachments
      <- logDebug "Creating frame buffers"
        *> ( for swapchainImages $ \swapchainImage -> do

            colorImageView
              <- createImageView
                    device swapchainImage
                    Vulkan.IMAGE_VIEW_TYPE_2D
                    colFmt
                    Vulkan.IMAGE_ASPECT_COLOR_BIT
            let attachment = (swapchainImage, colorImageView)
            framebuffer <- createFramebuffer device renderPass swapchainExtent [colorImageView]
            pure (framebuffer, attachment)
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
        ( StageFlags Vulkan.SHADER_STAGE_FRAGMENT_BIT )
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
    let pipelineInfo = VkPipelineInfo swapchainExtent Vulkan.SAMPLE_COUNT_1_BIT pipelineLayout

    shaders <- logDebug "Loading shaders" *> traverse (\path -> (path, ) <$> loadShader device path) shaderPipeline

    let
      recordCommandBuffers pipe =
        for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( descriptorSet, (framebuffer, attachment ) ) ->
          recordSimpleIndexedDrawCall
            device commandPool framebuffer (renderPass, [clearValue])
            descriptorSet cmdBindBuffers
            ( fst attachment, swapchainExtent )
            Nothing
            nbIndices
            pipelineLayout pipe
      recordScreenshotCommandBuffers pipe =
        for (V.zip3 descriptorSets framebuffersWithAttachments screenshotImagesAndMemories)
          \ ( descriptorSet, (framebuffer, attachment), (screenshotImage, _) ) ->
            recordSimpleIndexedDrawCall
              device commandPool framebuffer (renderPass, [clearValue])
              descriptorSet cmdBindBuffers
              ( fst attachment, swapchainExtent )
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
      let
        prevAction = interpretInput 1 prevInput
        newInput = foldl onSDLInput prevInput inputEvents
        action   = interpretInput 1 newInput

      pos <-
        if locate action
        then do void $ SDL.setMouseLocationMode SDL.RelativeLocation
                -- precision mode
                pure ( mousePos prevInput ^+^ ( 20 *^ mouseRel newInput ) )
        else do void $ SDL.setMouseLocationMode SDL.AbsoluteLocation
                -- smooth out mouse movement slightly
                let pos@(V2 px py) = 0.5 *^ ( mousePos prevInput ^+^ mousePos newInput )
                when (locate prevAction) do
                  ( SDL.warpMouse SDL.WarpCurrentFocus (SDL.P (SDL.V2 (round px) (round py))) )
                  _ <- SDL.captureMouse True
                  pure ()

                pure pos
      assign _input ( newInput { mousePos = pos, mouseRel = pure 0 } )

      ----------------
      -- simulation

      -- update UBO
      let
        BufferResource _ updateMousePos = mousePosUBO resources

      liftIO ( updateMousePos pos )


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
