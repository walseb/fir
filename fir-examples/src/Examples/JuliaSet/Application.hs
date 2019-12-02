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

module Examples.JuliaSet.Application ( juliaSet ) where

-- base
import Control.Monad
  ( when )
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

-- managed
import Control.Monad.Managed
  ( runManaged )

-- sdl2
import qualified SDL
import qualified SDL.Event
import qualified SDL.Raw.Event as SDL

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
  , Struct((:&),End)
  , ModuleRequirements
  )
import Math.Linear
  ( V
  , pattern V2, pattern V3
  , (^+^), (*^)
  )

-- fir-examples
import Examples.Common
import Examples.JuliaSet.Shaders
import Simulation.Observer
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
  ( UniformBuffer ( V2 0 0 ) )
  ( VertexBuffer viewportVertices  )
  ( IndexBuffer  viewportIndices   )

clearValue :: Vulkan.VkClearValue
clearValue = Vulkan.createVk ( Vulkan.set @"color" black )
  where
    black :: Vulkan.VkClearColorValue
    black =
      Vulkan.createVk
        (  Vulkan.setAt @"float32" @0 0
        &* Vulkan.setAt @"float32" @1 0
        &* Vulkan.setAt @"float32" @2 0
        &* Vulkan.setAt @"float32" @3 1
        )

----------------------------------------------------------------------------
-- Application.

juliaSet :: IO ()
juliaSet = ( runManaged . ( `evalStateT` initialState ) ) do

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
        , mouseMode  = SDL.AbsoluteLocation
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

      colFmt :: Vulkan.VkFormat
      colFmt = Vulkan.getField @"format" surfaceFormat

    renderPass <- logMsg "Creating a render pass" *>
      simpleRenderPass device
        ( noAttachments
          { colorAttachments = [ presentableColorAttachmentDescription colFmt ] }
        )

    framebuffersWithAttachments
      <- logMsg "Creating frame buffers"
        *> ( for swapchainImages $ \swapchainImage -> do

            colorImageView
              <- createImageView
                    device swapchainImage
                    Vulkan.VK_IMAGE_VIEW_TYPE_2D
                    colFmt
                    Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
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
        ( StageFlags Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT )
        InputResource
        InputResource

    PostInitialisationResult
      descriptorSetLayout descriptorSets cmdBindBuffers resources
       <- initialiseResources physicalDevice device resourceFlags initialResourceSet

    -------------------------------------------
    -- Create command buffers and record commands into them.

    commandPool <- logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex
    queue       <- getQueue device 0

    nextImageSem <- createSemaphore device
    submitted    <- createSemaphore device

    let pipelineInfo = VkPipelineInfo swapchainExtent Vulkan.VK_SAMPLE_COUNT_1_BIT

    vkPipeline
      <- createGraphicsPipeline device renderPass pipelineInfo descriptorSetLayout shaderPipeline


    commandBuffers <-
      for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( descriptorSet, (framebuffer, attachment ) ) ->
        recordSimpleIndexedDrawCall
          device commandPool framebuffer (renderPass, [clearValue])
          descriptorSet cmdBindBuffers
          ( fst attachment, swapchainExtent )
          Nothing
          nbIndices
          vkPipeline

    screenshotCommandBuffers <-
      for (V.zip3 descriptorSets framebuffersWithAttachments screenshotImagesAndMemories)
        \ ( descriptorSet, (framebuffer, attachment), (screenshotImage, _) ) ->
          recordSimpleIndexedDrawCall
            device commandPool framebuffer (renderPass, [clearValue])
            descriptorSet cmdBindBuffers
            ( fst attachment, swapchainExtent )
            ( Just ( screenshotImage, extent3D ) )
            nbIndices
            vkPipeline

    mainLoop do

      ----------------
      -- input

      inputEvents <- map SDL.Event.eventPayload <$> SDL.pollEvents
      prevInput <- use _input
      let
        prevAction = interpretInput prevInput
        newInput = foldl onSDLInput prevInput inputEvents
        action   = interpretInput newInput

      pos <-
        if locate action
        then do SDL.setMouseLocationMode SDL.RelativeLocation
                -- precision mode
                pure ( mousePos prevInput ^+^ ( 20 *^ mouseRel newInput ) )
        else do SDL.setMouseLocationMode SDL.AbsoluteLocation
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
          | takeScreenshot action = screenshotCommandBuffers `V.index` nextImageIndex
          | otherwise             = commandBuffers           `V.index` nextImageIndex

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
