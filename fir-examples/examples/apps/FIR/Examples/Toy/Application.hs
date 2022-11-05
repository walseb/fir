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
import Control.Arrow
  ( first )
import Control.Exception
  ( throw )
import Control.Monad
  ( when )
import Control.Monad.IO.Class
  ( MonadIO )
import Data.Bits
  ( (.|.) )
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
  ( IORef, newIORef, readIORef, writeIORef)

-- dear-imgui
import qualified DearImGui            as ImGui
import qualified DearImGui.Vulkan     as ImGui.Vulkan
import DearImGui.Vulkan
  ( InitInfo(..) )
import qualified DearImGui.SDL        as ImGui.SDL
import qualified DearImGui.SDL.Vulkan as ImGui.SDL.Vulkan

-- lens
import Control.Lens
  ( Lens', lens, use, assign )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- resourcet
import qualified Control.Monad.Trans.Resource as ResourceT

-- sdl2
import qualified SDL
import qualified SDL.Raw.Timer as SDL hiding (delay)
import qualified SDL.Raw.Video as SDL (getWindowDisplayIndex)
import qualified SDL.Internal.Types (Window(..))

-- text
import qualified Data.Text as Text
  ( pack )

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
import qualified Vulkan as Vulkan.Extent2D
  ( Extent2D(..) )
import qualified Vulkan as Vulkan.Surface
  ( SurfaceFormatKHR(..) )
import qualified Vulkan.Exception as Vulkan
import qualified Vulkan.Zero      as Vulkan

-- fir
import FIR
  ( runCompilationsTH
  , Struct((:&),End)
  , ModuleRequirements(..)
  )
import Math.Linear
  ( V, pattern V2, pattern V3 )

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
-- Render state

data ToyRenderState
  = ToyRenderState
    { inputJulia  :: Input
    , inputMap    :: Input
    }

_inputJulia :: Lens' ToyRenderState Input
_inputJulia = lens inputJulia ( \s v -> s { inputJulia = v } )

_inputMap :: Lens' ToyRenderState Input
_inputMap = lens inputMap ( \s v -> s { inputMap = v } )


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

initialResourceSetMap :: ResourceSet numImagesMap Pre
initialResourceSetMap = ResourceSet
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

{-| 'moveWindowsSideBySide' positions both windows, side by side,
     possibly in the center of the screen, like so:

  -----------------
  |  ----- -----  |
  |  | 1 | | 2 |  |
  |  ----- -----  |
  -----------------
-}
moveWindowsSideBySide :: SDL.Window -> SDL.Window -> IO ()
moveWindowsSideBySide win1 win2 = do
  -- assume both window are on the same display
  (displayX, displayY) <- getDisplaySize win1
  (win1X, win1Y) <- getWinSize win1
  (win2X, win2Y) <- getWinSize win2

  let
    win1PosX = max 0 $ (displayX - win1X - win2X) `div` 2
    win1PosY = max 0 $ (displayY - win1Y) `div` 2
    win2PosX = win1PosX + win1X
    win2PosY = max 0 $ (displayY - win2Y) `div` 2

  setPosition win1 (win1PosX, win1PosY)
  setPosition win2 (win2PosX, win2PosY)

  where
    getDisplaySize :: SDL.Window -> IO (CInt, CInt)
    getDisplaySize (SDL.Internal.Types.Window win) = do
      displayIndex <- SDL.getWindowDisplayIndex win
      displays <- SDL.getDisplays
      let
        display = displays !! fromInteger (toInteger displayIndex)
        SDL.V2 displayX displayY = SDL.displayBoundsSize display
      pure (displayX, displayY)

    getWinSize :: SDL.Window -> IO (CInt, CInt)
    getWinSize win = do
      -- TODO: take border into account with https://github.com/haskell-game/sdl2/pull/231
      --       We'll have to do the moveWindowsSideBySide call after the first render
      -- SDL.V4 winT winL winB winR <- SDL.getWindowBordersSize win
      let
        borderWidth  = 0 -- winL + winR
        borderHeight = 0 -- winT + winB
      SDL.V2 winX winY <- SDL.get (SDL.windowSize win)
      pure (winX + borderWidth, winY + borderHeight)

    setPosition :: SDL.Window -> (CInt, CInt) -> IO ()
    setPosition win (x, y) = do
      SDL.setWindowPosition win (SDL.Absolute (SDL.P (SDL.V2 x y)))

----------------------------------------------------------------------------
-- Application.

toy :: IO ()
toy = runVulkan (ToyRenderState nullInput nullInput) do

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

  let screenX = 800
      screenY = 600
      screen :: V 2 Float
      screen  = V2 (fromIntegral screenX) (fromIntegral screenY)

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
            , Vulkan.IMAGE_USAGE_TRANSFER_DST_BIT
            , Vulkan.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            ]
        }

  vkContext <-
    initialiseContext @WithSwapchain Normal appName vulkanReqs
      RenderInfo
        { queueType   = Vulkan.QUEUE_GRAPHICS_BIT
        , surfaceInfo = surfaceInfo
        }

  let
    queueJulia            = Vulkan.Context.queue vkContext
    deviceJulia           = Vulkan.Context.device vkContext
    physicalDeviceJulia   = Vulkan.Context.physicalDevice vkContext
    queueFamilyIndexJulia = Vulkan.Context.queueFamilyIndex vkContext
    vkInstanceJulia       = Vulkan.Context.vkInstance vkContext
    swapchainInfoJulia'    = Vulkan.Context.aSwapchainInfo vkContext

  _ <- ResourceT.allocate ImGui.createContext ImGui.destroyContext

  -------------------------------------------
  -- Initialise map window and Vulkan context.

  ( windowMap, windowMapExtensions ) <-
    initialiseWindow
      WindowInfo
        { width      = CInt (fromIntegral screenX)
        , height     = CInt (fromIntegral screenY)
        , windowName = (appName <> "Map")
        , mouseMode  = SDL.AbsoluteLocation
        }

  let
    vulkanReqsMap = ignoreMinVersion . addInstanceExtensions windowMapExtensions $ vulkanRequirements reqs
    surfaceInfoMap =
      SurfaceInfo
        { surfaceWindow = windowMap
        , preferredFormat =
            Vulkan.SurfaceFormatKHR
              Vulkan.FORMAT_B8G8R8A8_UNORM
              Vulkan.COLOR_SPACE_SRGB_NONLINEAR_KHR
        , surfaceUsage =
            [ Vulkan.IMAGE_USAGE_TRANSFER_SRC_BIT
            , Vulkan.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            ]
        }

  vkContextMap <-
    initialiseContext @WithSwapchain Normal (appName <> "Map") vulkanReqsMap
      RenderInfo
        { queueType   = Vulkan.QUEUE_GRAPHICS_BIT
        , surfaceInfo = surfaceInfoMap
        }

  let
    queueMap            = Vulkan.Context.queue vkContextMap
    deviceMap           = Vulkan.Context.device vkContextMap
    physicalDeviceMap   = Vulkan.Context.physicalDevice vkContextMap
    queueFamilyIndexMap = Vulkan.Context.queueFamilyIndex vkContextMap
    swapchainInfoMap'    = Vulkan.Context.aSwapchainInfo vkContextMap

  liftIO $ moveWindowsSideBySide window windowMap

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
  imGuiCommandPool <- createCommandPool deviceJulia Vulkan.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ( fromIntegral queueFamilyIndexJulia )
  ( _imGuiPoolKey, imGuiDescriptorPool ) <- createDescriptorPool deviceJulia 1000 imGuiDescriptorTypes

  withSwapchainInfo swapchainInfoJulia' \ ( swapchainInfoJulia :: SwapchainInfo numImages ) ->
   withSwapchainInfo swapchainInfoMap' \ ( swapchainInfoMap :: SwapchainInfo numImagesMap ) -> do

  -------------------------------------------
  -- Create framebuffer attachments.

    let
      swapchainExtentJulia = swapchainExtent swapchainInfoJulia
      swapchainJulia       = swapchain swapchainInfoJulia
      surfaceFormatJulia   = surfaceFormat swapchainInfoJulia
      swapchainImagesJulia = swapchainImages swapchainInfoJulia

      width, height :: Num a => a
      width  = fromIntegral $ Vulkan.Extent2D.width  swapchainExtentJulia
      height = fromIntegral $ Vulkan.Extent2D.height swapchainExtentJulia

      extent3D :: Vulkan.Extent3D
      extent3D
        = Vulkan.Extent3D
            { Vulkan.width  = width
            , Vulkan.height = height
            , Vulkan.depth  = 1
            }

      colFmt :: Vulkan.Format
      colFmt = Vulkan.Surface.format surfaceFormatJulia

    renderPass <- logDebug "Creating a render pass" *>
      simpleRenderPass deviceJulia
        ( noAttachments
          { colorAttachments = Boxed.Vector.singleton $ presentableColorAttachmentDescription colFmt }
        )

    imGuiRenderPass <-
      simpleRenderPass deviceJulia
        ( noAttachments
          { colorAttachments = Boxed.Vector.singleton $ preservedColorAttachmentDescription colFmt }
        )

    framebuffersWithAttachments
      <- logDebug "Creating frame buffers"
        *> ( for swapchainImagesJulia $ \swapchainImage -> do

            colorImageView
              <- createImageView
                    deviceJulia swapchainImage
                    Vulkan.IMAGE_VIEW_TYPE_2D
                    colFmt
                    Vulkan.IMAGE_ASPECT_COLOR_BIT
            let attachment = (swapchainImage, colorImageView)
            framebuffer <- createFramebuffer deviceJulia renderPass swapchainExtentJulia [colorImageView]
            pure (framebuffer, attachment)
         )

    screenshotImagesAndMemories <-
      for swapchainImagesJulia $ \ _ ->
        createScreenshotImage physicalDeviceJulia deviceJulia
          ( screenshotImageInfo extent3D colFmt )

    let
      imageInfo =
        Default2DImageInfo extent3D colFmt
          (   Vulkan.IMAGE_USAGE_TRANSFER_SRC_BIT
          .|. Vulkan.IMAGE_USAGE_TRANSFER_DST_BIT
          .|. Vulkan.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
          )

    (imageJulia, _)
      <- createImage
            physicalDeviceJulia deviceJulia
            imageInfo
            Vulkan.zero

  -------------------------------------------
  -- Create framebuffer attachments for the map.

    let
      swapchainExtentMap = swapchainExtent swapchainInfoMap
      swapchainMap       = swapchain swapchainInfoMap
      surfaceFormatMap   = surfaceFormat swapchainInfoMap
      swapchainImagesMap = swapchainImages swapchainInfoMap

      colFmtMap :: Vulkan.Format
      colFmtMap = ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) surfaceFormatMap

    renderPassMap <- logDebug "Creating a render pass for map" *>
      simpleRenderPass deviceMap
        ( noAttachments
          { colorAttachments = Boxed.Vector.singleton $ presentableColorAttachmentDescription colFmtMap }
        )

    framebuffersWithAttachmentsMap
      <- logDebug "Creating frame buffers for map"
        *> ( for swapchainImagesMap $ \swapchainImage -> do

            colorImageView
              <- createImageView
                    deviceMap swapchainImage
                    Vulkan.IMAGE_VIEW_TYPE_2D
                    colFmtMap
                    Vulkan.IMAGE_ASPECT_COLOR_BIT
            let attachment = (swapchainImage, colorImageView)
            framebuffer <- createFramebuffer deviceMap renderPassMap swapchainExtentMap [colorImageView]
            pure (framebuffer, attachment)
         )

    screenshotImagesAndMemoriesMap <-
      for swapchainImagesMap $ \ _ ->
        createScreenshotImage physicalDeviceMap deviceMap
          ( screenshotImageInfo extent3D colFmtMap )


    -------------------------------------------
    -- Initialise Dear ImGui

    let
      imageCount :: Word32
      imageCount = fromIntegral $ length swapchainImagesJulia
      initInfo :: ImGui.Vulkan.InitInfo
      initInfo = ImGui.Vulkan.InitInfo
        { instance'      = vkInstanceJulia
        , physicalDevice = physicalDeviceJulia
        , device         = deviceJulia
        , queueFamily    = fromIntegral queueFamilyIndexJulia
        , queue          = queueJulia
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
    imGuiCommandBuffers <- snd <$> allocatePrimaryCommandBuffers deviceJulia imGuiCommandPool imageCount

    logDebug "Initialising ImGui SDL2 for Vulkan"
    _ <- ResourceT.allocate
          ( ImGui.SDL.Vulkan.sdl2InitForVulkan window )
          ( const ImGui.SDL.sdl2Shutdown )

    _ <- ResourceT.allocate
          ( ImGui.Vulkan.vulkanInit initInfo imGuiRenderPass )
          ( ImGui.Vulkan.vulkanShutdown )

    logDebug "Running one-shot commands to upload ImGui textures"
    logDebug "Creating fence"
    ( fenceKey, fence ) <- createFence deviceJulia
    logDebug "Allocating one-shot command buffer"
    ( fontUploadCommandBufferKey, fontUploadCommandBuffer ) <-
      allocateCommandBuffer deviceJulia imGuiCommandPool

    logDebug "Recording one-shot commands"
    beginCommandBuffer fontUploadCommandBuffer
    _ <- ImGui.Vulkan.vulkanCreateFontsTexture fontUploadCommandBuffer
    endCommandBuffer fontUploadCommandBuffer

    logDebug "Submitting one-shot commands"
    submitCommandBuffer queueJulia fontUploadCommandBuffer [] [] ( Just fence )
    waitForFences deviceJulia ( WaitAll [ fence ] )

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

      resourceFlagsMap :: ResourceSet numImagesMap Named
      resourceFlagsMap = ResourceSet
        ( StageFlags Vulkan.SHADER_STAGE_FRAGMENT_BIT )
        GeneralResource
        GeneralResource

    imGuiControllerRefs <- liftIO $ createControllerRefs initImGuiData

    PostInitialisationResult
      descriptorSetLayout descriptorSets cmdBindBuffers resources
       <- initialiseResources physicalDeviceJulia deviceJulia resourceFlags initialResourceSet

    PostInitialisationResult
      descriptorSetLayoutMap descriptorSetsMap cmdBindBuffersMap resourcesMap
       <- initialiseResources physicalDeviceMap deviceMap resourceFlagsMap initialResourceSetMap


    -------------------------------------------
    -- Create command buffers and record commands into them.

    commandPool <- logDebug "Creating command pool" *> createCommandPool deviceJulia Vulkan.zero ( fromIntegral queueFamilyIndexJulia )

    (_, nextImageSem ) <- createSemaphore deviceJulia
    (_, submitted    ) <- createSemaphore deviceJulia

    pipelineLayout <- logDebug "Creating pipeline layout" *> createPipelineLayout deviceJulia [descriptorSetLayout]
    let pipelineInfo = VkPipelineInfo swapchainExtentJulia Vulkan.SAMPLE_COUNT_1_BIT pipelineLayout

    shaders <- logDebug "Loading shaders" *> traverse (\path -> (path, ) <$> loadShader deviceJulia path) shaderPipeline

    let
      recordCommandBuffers pipe =
        for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( descriptorSet, (framebuffer, attachment) ) ->
          recordSavedDrawCall
            deviceJulia commandPool framebuffer (renderPass, [clearValue1])
            descriptorSet cmdBindBuffers
            ( fst attachment, swapchainExtentJulia )
            ( imageJulia, extent3D )
            Nothing
            nbIndices
            pipelineLayout pipe
      recordRestoreCommandBuffers _pipe =
        for (V.zip descriptorSets framebuffersWithAttachments) $ \ ( _, (_, attachment) ) ->
          recordRestoreSavedImage
            deviceJulia commandPool imageJulia ( fst attachment ) extent3D
      recordScreenshotCommandBuffers pipe =
        for (V.zip3 descriptorSets framebuffersWithAttachments screenshotImagesAndMemories)
          \ ( descriptorSet, (framebuffer, attachment), (screenshotImage, _) ) ->
            recordSavedDrawCall
              deviceJulia commandPool framebuffer (renderPass, [clearValue1])
              descriptorSet cmdBindBuffers
              ( fst attachment, swapchainExtentJulia )
              ( imageJulia, extent3D )
              ( Just ( screenshotImage, extent3D ) )
              nbIndices
              pipelineLayout pipe

      recordAllCommandsFromShaders = record3CommandBuffersFromShaders
        ( createGraphicsPipeline deviceJulia renderPass pipelineInfo )
        recordCommandBuffers
        recordRestoreCommandBuffers
        recordScreenshotCommandBuffers

    -------------------------------------------
    -- Create command buffers and record commands into them for map.

    commandPoolMap <- logDebug "Creating command pool for map" *> createCommandPool deviceMap Vulkan.zero ( fromIntegral queueFamilyIndexMap )

    (_, nextImageSemMap ) <- createSemaphore deviceMap
    (_, submittedMap    ) <- createSemaphore deviceMap

    pipelineLayoutMap <- logDebug "Creating pipeline layout for map" *> createPipelineLayout deviceMap [descriptorSetLayoutMap]
    let pipelineInfoMap = VkPipelineInfo swapchainExtentMap Vulkan.SAMPLE_COUNT_1_BIT pipelineLayoutMap

    shadersMap <- logDebug "Loading shaders for map" *> traverse (\path -> (path, ) <$> loadShader deviceMap path) shaderPipeline

    let
      recordCommandBuffersMap pipe =
        for (V.zip descriptorSetsMap framebuffersWithAttachmentsMap) $ \ ( descriptorSetMap, (framebufferMap, attachmentMap ) ) ->
          recordSimpleIndexedDrawCall
            deviceMap commandPoolMap framebufferMap (renderPassMap, [clearValue1])
            descriptorSetMap cmdBindBuffersMap
            ( fst attachmentMap, swapchainExtentMap )
            Nothing
            nbIndices
            pipelineLayoutMap pipe
      recordScreenshotCommandBuffersMap pipe =
        for (V.zip3 descriptorSetsMap framebuffersWithAttachmentsMap screenshotImagesAndMemoriesMap)
          \ ( descriptorSetMap, (framebufferMap, attachmentMap), (screenshotImageMap, _) ) ->
            recordSimpleIndexedDrawCall
              deviceMap commandPoolMap framebufferMap (renderPassMap, [clearValue1])
              descriptorSetMap cmdBindBuffersMap
              ( fst attachmentMap, swapchainExtentMap )
              ( Just ( screenshotImageMap, extent3D ) )
              nbIndices
              pipelineLayoutMap pipe
      recordAllCommandsFromShadersMap = record2CommandBuffersFromShaders
        ( createGraphicsPipeline deviceMap renderPassMap pipelineInfoMap )
        recordCommandBuffersMap
        recordScreenshotCommandBuffersMap

    -- launch shader reload watcher, which writes command buffers to use to a TVar
    resourcesTVar <- statelessly $ shaderReloadWatcher deviceJulia shaders recordAllCommandsFromShaders
    resourcesMapTVar <- statelessly $ shaderReloadWatcher deviceMap shadersMap recordAllCommandsFromShadersMap

    -- keep track of plane position and zoom
    juliaObserverRef <- liftIO $ newIORef $
      initialObserver2D
        { zoom = 3.8356593,
          origin = V2 1.39 0
        }
    juliaInputDataRef <- liftIO $ newIORef Nothing

    mapObserverRef <- liftIO $ newIORef $
      initialObserver2D
        { zoom = 3.7238941,
          origin = V2 (-0.33162025) (-1.6875764)
        }
    mapInputDataRef <- liftIO $ newIORef Nothing

    juliaSeedRef <- liftIO $ newIORef $ V2 (-0.7477055835083013) (-2.692868835794263)

    let
      isUpdated :: MonadIO m => IORef (Maybe (InputData Value)) -> InputData Value -> m Bool
      isUpdated inputDataRef inputData = liftIO do
        -- Check if a given inputData has been updated
        prevInputData <- readIORef inputDataRef
        writeIORef inputDataRef (Just inputData)
        pure $ prevInputData /= Just inputData

      isMapUpdated = isUpdated mapInputDataRef
      isJuliaUpdated = isUpdated juliaInputDataRef

      renderFrame paused fps = do

        ----------------
        -- shader reloading

        ( ( updatedCommands, restoreCommands, updatedScreenshotCommands )
          , juliaReloaded )
          <- statelessly ( first snd <$> readDynResources resourcesTVar )

        ( ( updatedCommandsMap, updatedScreenshotCommandsMap )
          , mapReloaded )
          <- statelessly ( first snd <$> readDynResources resourcesMapTVar )

        --------------------
        -- controller values

        controllerValues <- readControllers imGuiControllerRefs
        let
          inversed' :& _ = controllerValues
          inversed = inversed' /= 0

        ----------------
        -- input

        imguiWantMouse <- ImGui.wantCaptureMouse
        imguiWantKeyboard <- ImGui.wantCaptureKeyboard
        inputEvents' <- map SDL.eventPayload <$> pollEventsWithImGui
        prevInput <- use _inputJulia
        prevInputMap <- use _inputMap
        prevJuliaObserver <- liftIO $ readIORef juliaObserverRef
        prevMapObserver <- liftIO $ readIORef mapObserverRef
        let
          imguiWantInput = imguiWantMouse || imguiWantKeyboard
          inputEvents =
            if imguiWantInput
              then []
              else inputEvents'
          newInput = foldl (onSDLInput window) prevInput inputEvents
          newInputMap = foldl (onSDLInput windowMap) prevInputMap inputEvents
          action   = interpretInput 1 newInput

          newJuliaObserver@(Observer2D juliaZoom juliaOrigin _ _ _ _ _ _ ) =
            updateObserver2D inversed prevJuliaObserver (V2 screenX screenY) newInput

          newMapObserver@(Observer2D mapZoom mapOrigin _ mapPos _ _ _ mapRightClicked ) =
            updateObserver2D False prevMapObserver (V2 screenX screenY) newInputMap

        liftIO $ writeIORef juliaObserverRef newJuliaObserver
        liftIO $ writeIORef mapObserverRef newMapObserver

        seed <- liftIO
          if mapRightClicked
            then do
              writeIORef juliaSeedRef mapPos
              pure mapPos
            else readIORef juliaSeedRef

        assign _inputJulia newInput
        assign _inputMap newInputMap

        ----------------
        -- simulation

        -- TODO: unpause for one-frame when the shader is reloaded
        _isPaused <- liftIO $ readIORef paused

        -- update UBO
        let
          BufferResource _ updateInputData = inputDataUBO resources
          BufferResource _ updateInputMapData = inputDataUBO resourcesMap
          currentInput :: InputData Value
          currentInput = 0 :& screen :& juliaZoom :& juliaOrigin :& seed :& Prelude.pure 0 :& controllerValues :& End
          currentInputMap :: InputData Value
          currentInputMap = 1 :& screen :& mapZoom :& mapOrigin :& seed :& Prelude.pure 0 :& controllerValues :& End

        juliaUpdated <- isJuliaUpdated currentInput
        when juliaUpdated $ liftIO ( updateInputData currentInput )

        mapUpdated <- isMapUpdated currentInputMap
        when mapUpdated $ liftIO ( updateInputMapData currentInputMap )

        ----------------
        -- rendering
        nextImageIndex <- acquireNextImage deviceJulia swapchainInfoJulia nextImageSem

        ImGui.Vulkan.vulkanNewFrame
        ImGui.SDL.sdl2NewFrame
        ImGui.newFrame
        began <- ImGui.begin "Shader toy!"
        when began do
          -- TODO: hide fps when isPaused
          ImGui.text $ "FPS: " <> Text.pack ( show fps )
          -- TODO: enable pause button with https://gitlab.com/sheaf/fir/-/merge_requests/21
          -- ImGui.button (if isPaused then "Play" else "Pause") >>= \case
          --  False -> return ()
          --  True  -> liftIO $ writeIORef paused (not isPaused)
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
            | takeScreenshot action         = updatedScreenshotCommands `V.index` nextImageIndex
            | juliaUpdated || juliaReloaded = updatedCommands           `V.index` nextImageIndex
            | otherwise                     = restoreCommands           `V.index` nextImageIndex

        submitCommandBuffer
          queueJulia
          commandBuffer
          [ ( nextImageSem, Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ) ]
          []
          Nothing

        beginCommandBuffer imGuiCommandBuffer
        cmdBeginRenderPass imGuiCommandBuffer imGuiRenderPass framebuffer [clearValue2] swapchainExtentJulia
        ImGui.Vulkan.vulkanRenderDrawData drawData imGuiCommandBuffer Nothing
        cmdEndRenderPass imGuiCommandBuffer
        endCommandBuffer imGuiCommandBuffer
        submitCommandBuffer
          queueJulia
          imGuiCommandBuffer
          []
          [ submitted ]
          Nothing

        present queueJulia swapchainJulia nextImageIndex [submitted]

        ----------------
        -- rendering map
        when (mapUpdated || mapReloaded) do
          nextImageIndexMap <- acquireNextImage deviceMap swapchainInfoMap nextImageSemMap
          let
            commandBufferMap
              | takeScreenshot action = updatedScreenshotCommandsMap `V.index` nextImageIndexMap
              | otherwise             = updatedCommandsMap `V.index` nextImageIndexMap

          submitCommandBuffer
            queueMap
            commandBufferMap
            [ ( nextImageSemMap, Vulkan.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ) ]
            [ submittedMap ]
            Nothing

          present queueMap swapchainMap nextImageIndexMap [submittedMap]

        Vulkan.queueWaitIdle queueJulia

        when ( takeScreenshot action ) $
          writeScreenshotData shortName deviceJulia swapchainExtentJulia
            ( snd ( screenshotImagesAndMemories `V.index` nextImageIndex ) )

        Vulkan.queueWaitIdle queueMap

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
