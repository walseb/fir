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

module Examples.Bezier.Application ( bezier ) where

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

-- managed
import Control.Monad.Managed
  ( runManaged )

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
import Examples.Bezier.Shaders
import Examples.Common
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
        [ ("Vertex shader"                 , compileVertexShader                 )
        , ("Tessellation control shader"   , compileTessellationControlShader    )
        , ("Tessellation evaluation shader", compileTessellationEvaluationShader )
        , ("Geometry shader"               , compileGeometryShader               )
        , ("Fragment shader"               , compileFragmentShader               )
        ]
     )

appName :: IsString a => a
appName = "fir-examples - Bézier curves"
shortName :: String
shortName = "bezier" -- name for screenshots

type UBO =
  Struct
    '[ "mvp"      ':-> M 4 4 Float
     , "binormal" ':-> V 4 Float
     , "widths"   ':-> V 3 Float
     ]

type VertexData = Struct VertexInput

data ResourceSet i st
  = ResourceSet
    { uboResource  :: UniformBuffer UBO        i st
    , vertexBuffer :: VertexBuffer  VertexData i st
    , indexBuffer  :: IndexBuffer   Word32     i st
    }
  deriving Generic

letterT :: [ Struct VertexInput ]
letterT
  = [ V3 (-3) (-6) 0 :& V3 1 0 0 :& End
    , V3   0  (-8) 0 :& V3 1 0 0 :& End
    , V3   3  (-6) 0 :& V3 1 0 0 :& End
    , V3   3  (-5) 0 :& V3 1 0 0 :& End
    , V3   3  (-4) 0 :& V3 1 0 1 :& End
    , V3   2  (-4) 0 :& V3 1 0 1 :& End
    , V3   1  (-4) 0 :& V3 1 0 1 :& End
    , V3   1  (-2) 0 :& V3 1 0 1 :& End
    , V3   1    0  0 :& V3 0 0 1 :& End
    , V3   0    0  0 :& V3 0 0 1 :& End
    , V3 (-1)   0  0 :& V3 0 0 1 :& End
    , V3 (-1) (-2) 0 :& V3 0 0 1 :& End
    , V3 (-1) (-4) 0 :& V3 0 0 0 :& End
    , V3 (-2) (-4) 0 :& V3 0 0 0 :& End
    , V3 (-3) (-4) 0 :& V3 0 0 0 :& End
    , V3 (-3) (-5) 0 :& V3 0 0 0 :& End
    ]

letterT_indices :: [ Word32 ]
letterT_indices
  = concat do
      i <- 15 : [1,3..13]
      pure ( map (`mod` 16) [i..i+4] )

nbIndices :: Word32
nbIndices = fromIntegral ( length letterT_indices )

bezierInitialObserver :: Observer
bezierInitialObserver = initialObserver { position = V3 0 (-3.5) (-6) }

binormal :: V 4 Float
binormal = V4 0 0 (-1) 0

lineWidths :: V 3 Float
lineWidths = V3 0.11 0.1 (recip 0.005)

initialResourceSet :: ResourceSet numImages Pre
initialResourceSet = ResourceSet
  ( UniformBuffer ( initialMVP :& binormal :& lineWidths :& End ) )
  ( VertexBuffer letterT )
  ( IndexBuffer  letterT_indices )
    where
      initialMVP = modelViewProjection bezierInitialObserver Nothing

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

bezier :: IO ()
bezier = ( runManaged . ( `evalStateT` ( initialState { observer = bezierInitialObserver } ) ) ) do

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

    renderPass <- logMsg "Creating a render pass" *>
      simpleRenderPass device
        ( noAttachments
          { colorAttachments = [ presentableColorAttachmentDescription colFmt ]
          , mbDepthStencilAttachment = Just (depthAttachmentDescription depthFmt)
          }
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
          (   Vulkan.VK_SHADER_STAGE_GEOMETRY_BIT
          .|. Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
          )
        )
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
      for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( descriptorSet, (framebuffer, attachments ) ) ->
        recordSimpleIndexedDrawCall
          device commandPool framebuffer (renderPass, clearValues)
          descriptorSet cmdBindBuffers
          ( fst $ V.head attachments, swapchainExtent )
          Nothing
          nbIndices
          vkPipeline

    screenshotCommandBuffers <-
      for (V.zip3 descriptorSets framebuffersWithAttachments screenshotImagesAndMemories)
        \ ( descriptorSet, (framebuffer, attachments), (screenshotImage, _) ) ->
          recordSimpleIndexedDrawCall
            device commandPool framebuffer (renderPass, clearValues)
            descriptorSet cmdBindBuffers
            ( fst $ V.head attachments, swapchainExtent )
            ( Just ( screenshotImage, extent3D ) )
            nbIndices
            vkPipeline

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

      oldObserver <- use _observer
      let (observer, orientation) = oldObserver `move` action
      assign _observer observer

      let mvp = modelViewProjection observer (Just orientation)

      when ( locate action )
        ( liftIO $ putStrLn ( show observer ) )

      -- update UBO
      let
        BufferResource _ updateUBO = uboResource resources

      liftIO ( updateUBO ( mvp :& binormal :& lineWidths :& End ) )

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
