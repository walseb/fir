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

module Examples.FullPipeline ( fullPipeline ) where

-- base
import Control.Monad
  ( when, replicateM )
import Data.Bits
import Data.Proxy
  ( Proxy )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import Data.Word
  ( Word32 )
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
  ( unsafeIndex )

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
  , Struct(..)
  , (:->)((:->))
  , ModuleRequirements
  )
import Math.Linear

-- fir-examples
import Examples.FullPipeline.Shaders
import Simulation.Observer
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

----------------------------------------------------------------------------
-- Application.

fullPipeline :: IO ()
fullPipeline = ( runManaged . ( `evalStateT` initialState ) ) do

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

    colFmt, depthFmt :: Vulkan.VkFormat
    colFmt   = Vulkan.getField @"format" surfaceFormat
    depthFmt = Vulkan.VK_FORMAT_D32_SFLOAT

  -------------------------------------------
  -- Create images.

  let

    numImages :: Int
    numImages = length swapchainImages

    depthImageInfo :: ImageInfo
    depthImageInfo =
      Default2DImageInfo extent3D depthFmt
        Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT

  renderPass <- logMsg "Creating a render pass" *> createRenderPass device colFmt depthFmt

  case someNatVal ( fromIntegral numImages ) of
    SomeNat ( _ :: Proxy numImages ) -> do

      framebuffersWithAttachments <- logMsg "Creating frame buffers"
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
              let attachments = [ (swapchainImage, colorImageView)
                                , (depthImage    , depthImageView)
                                ]
              framebuffer <- createFramebuffer device renderPass swapchainExtent (map snd attachments)
              pure (framebuffer, attachments)
           )

      screenshotImagesAndMemories <-
        replicateM numImages $
          createScreenshotImage physicalDevice device
            ( screenshotImageInfo extent3D colFmt )

      let clearValues :: [ Vulkan.VkClearValue ] -- in bijection with framebuffer attachments
          clearValues = [ tealClear
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
              tealClear = Vulkan.createVk ( Vulkan.set @"color"  teal )

              depthStencilClear :: Vulkan.VkClearDepthStencilValue
              depthStencilClear = Vulkan.createVk
                ( Vulkan.set @"depth" 1 &* Vulkan.set @"stencil" 0 )

      -------------------------------------------
      -- Manage resources.

      let

        phi :: Float
        phi = 0.5 + sqrt 1.25

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

        initialMVP :: M 4 4 Float
        initialMVP = modelViewProjection initialObserver Nothing
        initialOrig :: V 4 Float
        initialOrig = V4 0 0 0 1 ^*! initialMVP

        resourceFlags :: ResourceSet numImages Named
        resourceFlags = ResourceSet
          ( StageFlags
            (   Vulkan.VK_SHADER_STAGE_VERTEX_BIT
            .|. Vulkan.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
            )
          )
          InputResource
          InputResource

        initialResourceSet :: ResourceSet numImages Pre
        initialResourceSet = ResourceSet
          ( UniformBuffer ( initialMVP :& initialOrig :& End ) )
          ( VertexBuffer icosahedronVerts   )
          ( IndexBuffer  icosahedronIndices )

      ( descriptorSetLayout, descriptorSets, resources ) <-
        initialiseResources physicalDevice device resourceFlags initialResourceSet

      -------------------------------------------
      -- Create a command buffer and record the commands into it.

      commandPool <- logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex
      queue       <- getQueue device 0

      nextImageSem <- createSemaphore device
      submitted    <- createSemaphore device

      let pipelineInfo = VkPipelineInfo swapchainExtent Vulkan.VK_SAMPLE_COUNT_1_BIT

      ( graphicsPipeline, pipelineLayout )
        <- createGraphicsPipeline device renderPass pipelineInfo descriptorSetLayout shaderPipeline

      let
        mkCommandBuffer
          :: MonadManaged m
          => Vulkan.VkFramebuffer
          -> Vulkan.VkImage
          -> Vulkan.VkDescriptorSet
          -> Maybe Vulkan.VkImage
          -> m Vulkan.VkCommandBuffer
        mkCommandBuffer framebuffer swapchainImage descriptorSet mbScreenshotImage
          = do

            commandBuffer <-
              allocateCommandBuffer device commandPool

            beginCommandBuffer commandBuffer

            cmdBeginRenderPass commandBuffer renderPass framebuffer clearValues swapchainExtent

            liftIO $
              Foreign.Marshal.withArray [ inputBufferObject $ vertexBuffer resources ] $ \buffers ->
              Foreign.Marshal.withArray [ 0 ] $ \offsets ->
              Vulkan.vkCmdBindVertexBuffers commandBuffer 0 1 buffers offsets

            liftIO $
              Vulkan.vkCmdBindIndexBuffer
                commandBuffer
                ( inputBufferObject $ indexBuffer resources )
                0
                Vulkan.VK_INDEX_TYPE_UINT32

            liftIO $ do
              Vulkan.vkCmdBindPipeline
                commandBuffer
                Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
                graphicsPipeline

              Foreign.Marshal.withArray [ descriptorSet ] $ \descriptorSetsPtr ->
                Vulkan.vkCmdBindDescriptorSets
                  commandBuffer
                  Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
                  pipelineLayout
                  0 -- no offset
                  1 -- unique descriptor set
                  descriptorSetsPtr
                  0 -- no dynamic offset
                  Vulkan.vkNullPtr

              Vulkan.vkCmdDrawIndexed
                commandBuffer
                ( fromIntegral ( length icosahedronIndices ) )
                1 -- instance count
                0 -- offset into index buffer
                0 -- offset into vertex buffer
                0 -- first instance ID

            cmdEndRenderPass commandBuffer

            case mbScreenshotImage of
              Just screenshotImage ->
                cmdTakeScreenshot
                  ( Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT )
                  commandBuffer extent3D
                  ( swapchainImage,
                    ( Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    , Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                    )
                  )
                  screenshotImage
              Nothing -> pure () -- no image transitions to do manually, taken care of by the render pass

            endCommandBuffer commandBuffer

            pure commandBuffer


      commandBuffers <-
        for (zip [0..] framebuffersWithAttachments) $ \ ( i, (framebuffer, attachments ) ) ->
          mkCommandBuffer framebuffer ( fst $ head attachments ) ( descriptorSets `V.unsafeIndex` i ) Nothing

      screenshotCommandBuffers <-
        for (zip3 [0..] framebuffersWithAttachments screenshotImagesAndMemories)
          \ ( i, (framebuffer, attachments), (screenshotImage, _) ) ->
        mkCommandBuffer framebuffer ( fst $ head attachments ) ( descriptorSets `V.unsafeIndex` i ) (Just screenshotImage)


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
            origin = mvp !*^ V4 0 0 0 1

        when ( locate action )
          ( liftIO $ putStrLn ( show observer ) )

        -- update UBO
        let
          BufferResource _ updateUBO = uboResource resources

        liftIO ( updateUBO ( mvp :& origin :& End ) )

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
          [(nextImageSem, Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
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



createRenderPass
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkFormat
  -> Vulkan.VkFormat
  -> m Vulkan.VkRenderPass
createRenderPass dev colorFormat depthFormat =
  let

    colorAttachmentDescription :: Vulkan.VkAttachmentDescription
    colorAttachmentDescription =
      Vulkan.createVk
        (  Vulkan.set @"flags"          Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"format"         colorFormat
        &* Vulkan.set @"samples"        Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"loadOp"         Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
        &* Vulkan.set @"storeOp"        Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        &* Vulkan.set @"stencilLoadOp"  Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* Vulkan.set @"initialLayout"  Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        &* Vulkan.set @"finalLayout"    Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
        )

    colorAttachmentReference :: Vulkan.VkAttachmentReference
    colorAttachmentReference =
      Vulkan.createVk
        (  Vulkan.set @"attachment" 0
        &* Vulkan.set @"layout"     Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        )

    depthAttachmentDescription :: Vulkan.VkAttachmentDescription
    depthAttachmentDescription =
      Vulkan.createVk
        (  Vulkan.set @"flags"          Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"format"         depthFormat
        &* Vulkan.set @"samples"        Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"loadOp"         Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
        &* Vulkan.set @"storeOp"        Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        &* Vulkan.set @"stencilLoadOp"  Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* Vulkan.set @"initialLayout"  Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
        &* Vulkan.set @"finalLayout"    Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        )

    depthAttachmentReference :: Vulkan.VkAttachmentReference
    depthAttachmentReference =
      Vulkan.createVk
        (  Vulkan.set @"attachment" 1
        &* Vulkan.set @"layout"     Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        )

    subpass :: Vulkan.VkSubpassDescription
    subpass =
      Vulkan.createVk
        (  Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"pipelineBindPoint" Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        &* Vulkan.setListCountAndRef
              @"colorAttachmentCount"
              @"pColorAttachments"
              [ colorAttachmentReference ]
        &* Vulkan.setVkRef @"pDepthStencilAttachment" depthAttachmentReference
        &* Vulkan.setListCountAndRef @"inputAttachmentCount"    @"pInputAttachments"    []
        &* Vulkan.setListCountAndRef @"preserveAttachmentCount" @"pPreserveAttachments" []
        &* Vulkan.set @"pResolveAttachments" Vulkan.vkNullPtr

        )

    dependency1 :: Vulkan.VkSubpassDependency
    dependency1 =
      Vulkan.createVk
        (  Vulkan.set @"srcSubpass"    Vulkan.VK_SUBPASS_EXTERNAL
        &* Vulkan.set @"dstSubpass"    Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"srcStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"srcAccessMask" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"dstStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"dstAccessMask"
              (    Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
        )

    dependency2 :: Vulkan.VkSubpassDependency
    dependency2 =
      Vulkan.createVk
        (  Vulkan.set @"srcSubpass"    Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"dstSubpass"    Vulkan.VK_SUBPASS_EXTERNAL
        &* Vulkan.set @"srcStageMask"  Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* Vulkan.set @"srcAccessMask"
              (    Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
               .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              )
        &* Vulkan.set @"dstStageMask"  Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        &* Vulkan.set @"dstAccessMask" Vulkan.VK_ZERO_FLAGS
        )

    createInfo :: Vulkan.VkRenderPassCreateInfo
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.setListCountAndRef
              @"attachmentCount"
              @"pAttachments"
              [ colorAttachmentDescription, depthAttachmentDescription ]
        &* Vulkan.setListCountAndRef
              @"subpassCount"
              @"pSubpasses"
              [ subpass ]
        &* Vulkan.setListCountAndRef
              @"dependencyCount"
              @"pDependencies"
              [ dependency1, dependency2 ]
        )
  in
    managedVulkanResource
      ( Vulkan.vkCreateRenderPass  dev ( Vulkan.unsafePtr createInfo ) )
      ( Vulkan.vkDestroyRenderPass dev )
