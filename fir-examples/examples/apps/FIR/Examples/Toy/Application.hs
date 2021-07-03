{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
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

module FIR.Examples.Toy.Application ( toy ) where

-- base
import Control.Exception
  ( throw )
import Control.Monad
  ( when, unless, void )
import Data.Foldable
  ( traverse_ )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import Data.Word
  ( Word32 )
import GHC.Generics
  ( Generic )
import Foreign.C.Types
  ( CInt (..))
import Data.IORef
  ( newIORef, readIORef, writeIORef)

-- dear-imgui
import qualified DearImGui            as ImGui
import qualified DearImGui.Vulkan     as ImGui.Vulkan
import DearImGui.Vulkan
  ( InitInfo(..) )
import qualified DearImGui.SDL        as ImGui.SDL
import qualified DearImGui.SDL.Vulkan as ImGui.SDL.Vulkan

-- lens
import Control.Lens
  ( use, assign )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- resourcet
import qualified Control.Monad.Trans.Resource as ResourceT

-- sdl2
import qualified SDL
import qualified SDL.Raw.Event as SDL
import qualified SDL.Raw.Timer as SDL hiding (delay)

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
  ( (!), singleton )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( zip, zip3, index )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Exception as Vulkan
import qualified Vulkan.Zero      as Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct((:&),End)
  , ModuleRequirements(..)
  )
import Math.Linear
  ( pattern V2, pattern V3
  , (^+^), (*^)
  )

-- fir-examples
import FIR.Examples.Common
import FIR.Examples.Toy.Shaders
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

-- fir-examples-dear-imgui
import FIR.Examples.DearImGui
  ( ControllerRef(Value)
  , createControllers, createControllerRefs, readControllers
  )

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
appName = "fir-examples - Shader toy"
shortName :: String
shortName = "toy" -- name for screenshots

type VertexData = Struct VertexInput

data ResourceSet i st
  = ResourceSet
    { inputDataUBO :: UniformBuffer ( InputData Value ) i st
    , vertexBuffer :: VertexBuffer  VertexData          i st
    , indexBuffer  :: IndexBuffer   Word32              i st
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
  ( BufferData initInputData )
  ( BufferData viewportVertices )
  ( BufferData viewportIndices  )

clearValue1, clearValue2 :: Vulkan.ClearValue
clearValue1 = Vulkan.Color black
  where
    black :: Vulkan.ClearColorValue
    black = Vulkan.Float32 0 0 0 0
clearValue2 = Vulkan.Color yellow
  where
    yellow :: Vulkan.ClearColorValue
    yellow = Vulkan.Float32 1 1 0 1

----------------------------------------------------------------------------
-- Application.

toy :: IO ()
toy = runVulkan initialState do

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
        { width      = CInt (fromIntegral screenX)
        , height     = CInt (fromIntegral screenY)
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

  _ <- ResourceT.allocate ImGui.createContext ImGui.destroyContext

  let
    imGuiDescriptorTypes :: [ ( Vulkan.DescriptorType, Int ) ]
    imGuiDescriptorTypes = map (, 1000)
      [ Vulkan.DESCRIPTOR_TYPE_SAMPLER
      , Vulkan.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , Vulkan.DESCRIPTOR_TYPE_SAMPLED_IMAGE
      , Vulkan.DESCRIPTOR_TYPE_STORAGE_IMAGE
      , Vulkan.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
      , Vulkan.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
      , Vulkan.DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , Vulkan.DESCRIPTOR_TYPE_STORAGE_BUFFER
      , Vulkan.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
      , Vulkan.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
      , Vulkan.DESCRIPTOR_TYPE_INPUT_ATTACHMENT
      ]
  imGuiCommandPool <- createCommandPool device Vulkan.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ( fromIntegral queueFamilyIndex )
  ( _imGuiPoolKey, imGuiDescriptorPool ) <- createDescriptorPool device 1000 imGuiDescriptorTypes

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

    imGuiRenderPass <-
      simpleRenderPass device
        ( noAttachments
          { colorAttachments = Boxed.Vector.singleton $ preservedColorAttachmentDescription colFmt }
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
    -- Initialise Dear ImGui

    let
      imageCount :: Word32
      imageCount = fromIntegral $ length swapchainImages
      initInfo :: ImGui.Vulkan.InitInfo
      initInfo = ImGui.Vulkan.InitInfo
        { instance'      = vkInstance
        , physicalDevice
        , device
        , queueFamily    = fromIntegral queueFamilyIndex
        , queue
        , pipelineCache  = Vulkan.NULL_HANDLE
        , descriptorPool = imGuiDescriptorPool
        , subpass        = 0
        , minImageCount  = max 1 (imageCount - 1)
        , imageCount     = imageCount
        , msaaSamples    = Vulkan.SAMPLE_COUNT_1_BIT
        , mbAllocator    = Nothing
        , checkResult    = \case { Vulkan.SUCCESS -> pure (); e -> throw $ Vulkan.VulkanException e }
        }

    logDebug "Allocating Dear ImGui command buffers"
    imGuiCommandBuffers <- snd <$> allocatePrimaryCommandBuffers device imGuiCommandPool imageCount

    logDebug "Initialising ImGui SDL2 for Vulkan"
    _ <- ResourceT.allocate
          ( ImGui.SDL.Vulkan.sdl2InitForVulkan window )
          ( const ImGui.SDL.sdl2Shutdown )

    _ <- ResourceT.allocate
          ( ImGui.Vulkan.vulkanInit initInfo imGuiRenderPass )
          ( ImGui.Vulkan.vulkanShutdown )

    logDebug "Running one-shot commands to upload ImGui textures"
    logDebug "Creating fence"
    ( fenceKey, fence ) <- createFence device
    logDebug "Allocating one-shot command buffer"
    ( fontUploadCommandBufferKey, fontUploadCommandBuffer ) <-
      allocateCommandBuffer device imGuiCommandPool

    logDebug "Recording one-shot commands"
    beginCommandBuffer fontUploadCommandBuffer
    _ <- ImGui.Vulkan.vulkanCreateFontsTexture fontUploadCommandBuffer
    endCommandBuffer fontUploadCommandBuffer

    logDebug "Submitting one-shot commands"
    submitCommandBuffer queue fontUploadCommandBuffer [] [] ( Just fence )
    waitForFences device ( WaitAll [ fence ] )

    logDebug "Finished uploading font objects"
    logDebug "Cleaning up one-shot commands"
    ImGui.Vulkan.vulkanDestroyFontUploadObjects
    traverse_ ResourceT.release [ fenceKey, fontUploadCommandBufferKey ]

    -------------------------------------------
    -- Manage resources.

    let

      resourceFlags :: ResourceSet numImages Named
      resourceFlags = ResourceSet
        ( StageFlags Vulkan.SHADER_STAGE_FRAGMENT_BIT )
        GeneralResource
        GeneralResource

    imGuiControllerRefs <- liftIO $ createControllerRefs initImGuiData

    PostInitialisationResult
      descriptorSetLayout descriptorSets cmdBindBuffers resources
       <- initialiseResources physicalDevice device resourceFlags initialResourceSet

    -------------------------------------------
    -- Create command buffers and record commands into them.

    commandPool <- logDebug "Creating command pool" *> createCommandPool device Vulkan.zero ( fromIntegral queueFamilyIndex )

    (_, nextImageSem ) <- createSemaphore device
    (_, submitted    ) <- createSemaphore device

    pipelineLayout <- logDebug "Creating pipeline layout" *> createPipelineLayout device [descriptorSetLayout]
    let pipelineInfo = VkPipelineInfo swapchainExtent Vulkan.SAMPLE_COUNT_1_BIT pipelineLayout

    shaders <- logDebug "Loading shaders" *> traverse (\path -> (path, ) <$> loadShader device path) shaderPipeline

    let
      recordCommandBuffers pipe =
        for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( descriptorSet, (framebuffer, attachment ) ) ->
          recordSimpleIndexedDrawCall
            device commandPool framebuffer (renderPass, [clearValue1])
            descriptorSet cmdBindBuffers
            ( fst attachment, swapchainExtent )
            Nothing
            nbIndices
            pipelineLayout pipe
      recordScreenshotCommandBuffers pipe =
        for (V.zip3 descriptorSets framebuffersWithAttachments screenshotImagesAndMemories)
          \ ( descriptorSet, (framebuffer, attachment), (screenshotImage, _) ) ->
            recordSimpleIndexedDrawCall
              device commandPool framebuffer (renderPass, [clearValue1])
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

    -- keep track of plane position and zoom
    zoomRef <- liftIO $ newIORef 3.7238941
    scrollRef <- liftIO $ newIORef 0
    originRef <- liftIO $ newIORef (V2 (-0.33162025) (-1.6875764))
    mouseDownRef <- liftIO $ newIORef False

    let
      renderFrame paused fps = do

        ----------------
        -- shader reloading

        ( updatedCommands, updatedScreenshotCommands )
          <- statelessly ( snd <$> readTVarWithCleanup resourcesTVar )

        ----------------
        -- input

        imguiWantMouse <- ImGui.wantCaptureMouse
        imguiWantKeyboard <- ImGui.wantCaptureKeyboard
        inputEvents' <- map SDL.eventPayload <$> pollEventsWithImGui
        prevInput <- use _input
        let
          imguiWantInput = imguiWantMouse || imguiWantKeyboard
          inputEvents =
            if imguiWantInput
              then []
              else inputEvents'
          prevAction = interpretInput 1 prevInput
          newInput = foldl onSDLInput prevInput inputEvents
          action   = interpretInput 1 newInput

        mouseClicked <- liftIO do
          mouseDown <- readIORef mouseDownRef
          if SDL.ButtonLeft `elem` mouseButtonsDown newInput
            then
              if mouseDown
                then
                  -- The mouse is still down, and we already handle the event
                  pure False
                else do
                  -- It is the first event
                  writeIORef mouseDownRef True
                  pure True
            else do
              -- The mouse has been released
              writeIORef mouseDownRef False
              pure False

        scroll <- liftIO do
          prevScroll <- readIORef scrollRef
          let currentScroll = mouseWheel newInput
          writeIORef scrollRef currentScroll
          pure $ currentScroll - prevScroll

        zoom <- liftIO do
          currentZoom <- readIORef zoomRef
          if scroll /= 0
            then do
              let
                offset = if scroll < 0 then 0.1 else -0.1
                newZoom = currentZoom + offset * currentZoom
              liftIO $ writeIORef zoomRef newZoom
              pure newZoom
            else
              pure currentZoom

        origin <- liftIO do
          currentOrigin <- readIORef originRef
          currentZoom <- readIORef zoomRef
          if scroll /= 0 || mouseClicked
            then do
              let
                mousePos' = mousePos newInput
                newOrigin = pos2Coord (V2 screenX screenY) currentOrigin currentZoom mousePos'
              -- TODO: adjust newOrigin progressively to avoid jumping too far on scroll.
              writeIORef originRef newOrigin
              putStrLn $ "Coordinate changed to: " <> show zoom <> " @ " <> show newOrigin
              pure newOrigin
            else pure currentOrigin

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

        -- TODO: unpause for one-frame when the shader is reloaded
        isPaused <- liftIO $ readIORef paused

        -- update UBO
        controllerValues <- readControllers imGuiControllerRefs

        let
          BufferResource _ updateInputData = inputDataUBO resources
          currentInput :: InputData Value
          currentInput = pos :& zoom :& origin :& Prelude.pure 0 :& controllerValues :& End

        unless isPaused $ liftIO ( updateInputData currentInput )

        ----------------
        -- rendering

        nextImageIndex <- acquireNextImage device swapchainInfo nextImageSem

        ImGui.Vulkan.vulkanNewFrame
        ImGui.SDL.sdl2NewFrame window
        ImGui.newFrame
        began <- ImGui.begin "Shader toy!"
        when began do
          -- TODO: hide fps when isPaused
          ImGui.text $ "FPS: " <> show fps
          ImGui.button (if isPaused then "Play" else "Pause") >>= \case
            False -> return ()
            True  -> liftIO $ writeIORef paused (not isPaused)
          createControllers imGuiControllerRefs
        ImGui.end
        ImGui.render
        drawData <- ImGui.getDrawData
        let
          imGuiCommandBuffer :: Vulkan.CommandBuffer
          imGuiCommandBuffer = imGuiCommandBuffers Boxed.Vector.! fromIntegral nextImageIndex
          framebuffer :: Vulkan.Framebuffer
          framebuffer = fst $ framebuffersWithAttachments `V.index` nextImageIndex

        let
          commandBuffer
            | takeScreenshot action = updatedScreenshotCommands `V.index` nextImageIndex
            | otherwise             = updatedCommands           `V.index` nextImageIndex

        unless isPaused $ submitCommandBuffer
          queue
          commandBuffer
          []
          []
          Nothing

        beginCommandBuffer imGuiCommandBuffer
        cmdBeginRenderPass imGuiCommandBuffer imGuiRenderPass framebuffer [clearValue2] swapchainExtent
        ImGui.Vulkan.vulkanRenderDrawData drawData imGuiCommandBuffer Nothing
        cmdEndRenderPass imGuiCommandBuffer
        endCommandBuffer imGuiCommandBuffer
        submitCommandBuffer
          queue
          imGuiCommandBuffer
          [ ( nextImageSem, Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ) ]
          [ submitted ]
          Nothing

        present queue swapchain nextImageIndex [submitted]

        Vulkan.queueWaitIdle queue

        when ( takeScreenshot action ) $
          writeScreenshotData shortName device swapchainExtent
            ( snd ( screenshotImagesAndMemories `V.index` nextImageIndex ) )

        ----------------

        pure ( shouldQuit action )

    paused <- liftIO $ newIORef False
    fpsRef <- liftIO $ newIORef 0

    mainLoop do
      frameStart <- SDL.getTicks

      prevFps <- liftIO $ readIORef fpsRef
      result <- renderFrame paused prevFps

      frameEnd <- SDL.getTicks

      let
        elapsedMS = max 1 $ frameEnd - frameStart
        fps = 1000 `div` elapsedMS
        elapsedCap = 17 -- about 60 fps
        waitMS = elapsedCap - min elapsedCap elapsedMS

      liftIO $ writeIORef fpsRef fps

      -- cap fps
      SDL.delay $ fromInteger $ toInteger waitMS

      pure result

pollEventsWithImGui :: MonadVulkan m => m [ SDL.Event ]
pollEventsWithImGui = do
  e <- ImGui.SDL.pollEventWithImGui
  case e of
    Nothing -> pure []
    Just e' -> ( e' : ) <$> pollEventsWithImGui
