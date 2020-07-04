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

module FIR.Examples.FullPipeline.Application ( fullPipeline ) where

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
  ( fromTuple, zip, zip3, head, index )

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
import FIR.Examples.FullPipeline.Shaders
import FIR.Examples.Paths
import FIR.Examples.RenderState
import FIR.Examples.Reload
import Vulkan.Attachment
import Vulkan.Backend
import Vulkan.Context
import Vulkan.Features
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.Resource
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- Shaders and resource types.

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Vertex shader"                 , compileVertexShader                 )
        , ("Tessellation control shader"   , compileTessellationControlShader    )
        , ("Tessellation evaluation shader", compileTessellationEvaluationShader )
        , ("Geometry shader"               , compileGeometryShader               )
        , ("Fragment shader"               , compileFragmentShader               )
        ]
     )

appName :: IsString a => a
appName = "fir-examples - Full pipeline"
shortName :: String
shortName = "fullpipeline" -- name for screenshots

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

icosahedronVerts :: [ Struct VertexInput ]
icosahedronVerts =
  [ ( V3    0     1    phi  ) :& ( V3 0    1    0    ) :& End
  , ( V3    0   (-1)   phi  ) :& ( V3 0    0.75 0.25 ) :& End
  , ( V3    0     1  (-phi) ) :& ( V3 0    0.25 0.75 ) :& End
  , ( V3    0   (-1) (-phi) ) :& ( V3 0    0    1    ) :& End
  , ( V3    1    phi    0   ) :& ( V3 1    0    0    ) :& End
  , ( V3  (-1)   phi    0   ) :& ( V3 0.75 0.25 0    ) :& End
  , ( V3    1  (-phi)   0   ) :& ( V3 0.25 0.75 0    ) :& End
  , ( V3  (-1) (-phi)   0   ) :& ( V3 0    1    0    ) :& End
  , ( V3   phi    0     1   ) :& ( V3 1    0    0    ) :& End
  , ( V3   phi    0   (-1)  ) :& ( V3 0.75 0    0.25 ) :& End
  , ( V3 (-phi)   0     1   ) :& ( V3 0.25 0    0.75 ) :& End
  , ( V3 (-phi)   0   (-1)  ) :& ( V3 0    0    1    ) :& End
  ]
    where
      phi = 0.5 + sqrt 1.25

icosahedronIndices :: [ Word32 ]
icosahedronIndices
  = [ 0,  1,  8
    , 0, 10,  1
    , 0,  4,  5
    , 0,  8,  4
    , 0,  5, 10
    , 1,  7,  6
    , 1,  6,  8
    , 1, 10,  7
    , 2,  9,  3
    , 2,  3, 11
    , 2,  5,  4
    , 2,  4,  9
    , 2, 11,  5
    , 3,  6,  7
    , 3,  9,  6
    , 3,  7, 11
    , 4,  8,  9
    , 5, 11, 10
    , 6,  9,  8
    , 7, 10, 11
    ]

nbIndices :: Word32
nbIndices = fromIntegral ( length icosahedronIndices )

initialResourceSet :: ResourceSet numImages Pre
initialResourceSet = ResourceSet
  ( UniformBuffer ( initialMVP :& initialOrig :& End ) )
  ( VertexBuffer icosahedronVerts   )
  ( IndexBuffer  icosahedronIndices )
      where
        initialMVP :: M 4 4 Float
        initialMVP = modelViewProjection initialObserver Nothing
        initialOrig :: V 4 Float
        initialOrig = V4 0 0 0 1 ^*! initialMVP

clearValues :: [ Vulkan.VkClearValue ] -- in bijection with framebuffer attachments
clearValues =
  [ tealClear
  , Vulkan.createVk ( Vulkan.set @"depthStencil" depthStencilClear )
  ]
  where
    teal :: Vulkan.VkClearColorValue
    teal =
      Vulkan.createVk
        (  Vulkan.setAt @"float32" @0 0.1
        &* Vulkan.setAt @"float32" @1 0.5
        &* Vulkan.setAt @"float32" @2 0.7
        &* Vulkan.setAt @"float32" @3 1
        )
    tealClear :: Vulkan.VkClearValue
    tealClear = Vulkan.createVk ( Vulkan.set @"color" teal )
    depthStencilClear :: Vulkan.VkClearDepthStencilValue
    depthStencilClear = Vulkan.createVk
      ( Vulkan.set @"depth" 1 &* Vulkan.set @"stencil" 0 )

----------------------------------------------------------------------------
-- Application.

fullPipeline :: IO ()
fullPipeline = runVulkan initialState do

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

      depthImageInfo :: ImageInfo
      depthImageInfo =
        Default2DImageInfo extent3D depthFmt
          Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT

    renderPass <- logDebug "Creating a render pass" *>
      simpleRenderPass device
        ( noAttachments
          { colorAttachments = [ presentableColorAttachmentDescription colFmt ]
          , mbDepthStencilAttachment = Just (depthAttachmentDescription depthFmt)
          }
        )

    framebuffersWithAttachments
      <- logDebug "Creating frame buffers"
        *> ( for swapchainImages $ \swapchainImage -> do

          colorImageView
            <- createImageView
                  device swapchainImage
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
                  ( (swapchainImage, colorImageView)
                  , (depthImage    , depthImageView)
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

    pipelineLayout <- logDebug "Creating pipeline layout" *> createPipelineLayout device descriptorSetLayout
    let pipelineInfo = VkPipelineInfo swapchainExtent Vulkan.VK_SAMPLE_COUNT_1_BIT pipelineLayout

    shaders <- logDebug "Loading shaders" *> traverse (\path -> (path, ) <$> loadShader device path) shaderPipeline

    let
      recordCommandBuffers pipe =
        for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( descriptorSet, (framebuffer, attachments ) ) ->
          recordSimpleIndexedDrawCall
            device commandPool framebuffer (renderPass, clearValues)
            descriptorSet cmdBindBuffers
            ( fst $ V.head attachments, swapchainExtent )
            Nothing
            nbIndices
            pipelineLayout pipe
      recordScreenshotCommandBuffers pipe =
        for (V.zip3 descriptorSets framebuffersWithAttachments screenshotImagesAndMemories)
          \ ( descriptorSet, (framebuffer, attachments), (screenshotImage, _) ) ->
            recordSimpleIndexedDrawCall
              device commandPool framebuffer (renderPass, clearValues)
              descriptorSet cmdBindBuffers
              ( fst $ V.head attachments, swapchainExtent )
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
