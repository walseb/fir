{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NegativeLiterals           #-}
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

module FIR.Examples.RayTracing.Application
  ( rayTracing )
  where

-- base
import Control.Monad
  ( when )
import Data.Bits
  ( (.|.) )
import Data.Coerce
  ( coerce )
import Data.Foldable
  ( for_, toList, traverse_ )
import Data.Functor.Compose
  ( Compose(..) )
import Data.Monoid
  ( Sum(..) )
import Data.Proxy
  ( Proxy(..) )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import Data.Word
  ( Word32, Word64 )
import GHC.Generics
  ( Generic )

-- bytestring
import Data.ByteString
  ( ByteString )

-- containers
import qualified Data.Map.Strict as Map
  ( fromList )

-- dependent-map
import qualified Data.Dependent.Map as DMap
  ( lookup )

-- generic-lens
import Data.Generics.Product.Typed
  ( HasType(typed) )

-- lens
import Control.Lens
  ( assign, modifying, use )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, release )

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
  ( length, singleton )

-- vector-sized
import qualified Data.Vector.Sized as Sized
  ( Vector )
import qualified Data.Vector.Sized as V
  ( imapM, index, unzip )

-- vulkan
import qualified Vulkan
import qualified Vulkan.CStruct.Extends as Vulkan
import qualified Vulkan.Requirement     as Vulkan
import qualified Vulkan.Zero            as Vulkan

-- fir
import FIR
  ( ModuleRequirements(..)
  , ShaderGroup(..)
  , Struct((:&),End)
  , Name, Swizzle, type (:.:)
  , view
  , runCompilationsTH
  )
import Math.Linear
  ( V, pattern V2, pattern V3, pattern V4
  , norm
  )

-- fir-examples
import FIR.Examples.RayTracing.BuildScene
  ( TagGeometryData(..), TagLuminaireProperties(..), TagMaterialProperties(..)
  , SceneData(..), ShaderIndices(..)
  , buildScene
  )
import FIR.Examples.RayTracing.Camera
  ( CameraCoordinates )
import FIR.Examples.RayTracing.Geometry
  ( Geometry(GeometryData) )
import FIR.Examples.RayTracing.Luminaire
  ( Luminaire(LuminaireProperties), LightSamplingMethod(SurfaceArea) )
import FIR.Examples.RayTracing.Material
  ( Material(MaterialProperties) )
import FIR.Examples.RayTracing.Scene
  ( Scene(sceneCamera) )
import FIR.Examples.RayTracing.Scenes
  ( chooseScene )
import FIR.Examples.RayTracing.Shaders
  ( Shaders(..), ShaderGroups(..), allShaderCompilations )
import qualified FIR.Examples.RayTracing.Shaders as RayTracing
  ( shaders )
import FIR.Examples.RayTracing.Types
  ( UBO, STriangleQ(..)
  , GeometryKind
      ( Triangle
      , Sphere
      )
  , LuminaireKind
      ( Blackbody )
  , MaterialKind
      ( Lambertian )
  , LuminaireID
  )
import qualified FIR.Examples.RayTracing.Types as RayTracing
  ( width, height, localSizeX, localSizeY, mipWidth, mipHeight )
import FIR.Examples.Paths
import FIR.Examples.Reload
import FIR.Examples.RenderState
import Vulkan.Backend
import Vulkan.Context
import Vulkan.Monad
import Vulkan.Pipeline
import Vulkan.RayTracing
import Vulkan.Resource
import Vulkan.Screenshot

----------------------------------------------------------------------------
-- Ray-tracing shaders, resources.

shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult = $( runCompilationsTH allShaderCompilations )

appName :: IsString a => a
appName = "fir-examples - Ray-tracing"
shortName :: String
shortName = "ray-tracing" -- name for screenshots

-- Ray-tracing descriptor set 0
data UBOResourceSet i st
  = UBOResourceSet
    { uboResource      :: UniformBuffer UBO                               i st
    , accelResource    :: TopLevelAS                                      i st
    , luminaireIDs     :: StorageBuffer LuminaireID                       i st
    , indexBuffer      :: StorageBuffer Word32                            i st
    , trianglesArray   :: StorageBuffer ( GeometryData Triangle )         i st
    , spheresArray     :: StorageBuffer ( GeometryData Sphere   )         i st
    , blackbodiesArray :: StorageBuffer ( LuminaireProperties Blackbody ) i st
    , lambertiansArray :: StorageBuffer ( MaterialProperties Lambertian ) i st
    }
  deriving Generic

-- Ray-tracing descriptor sets 1 and 2 (input and output)
-- Compute shader descriptor set 0
data DataResourceSet i st
  = DataResourceSet
    { dataImages    :: StorageImages i st
    , logLumiImages :: StorageImages i st
    }
  deriving Generic

-- Compute shader descriptor set 1
data PresentResourceSet i st
  = PresentResourceSet
    { presentImages :: StorageImages i st
    , mipImages     :: SampledImages i st
    }
  deriving Generic

data ImageResource a
  = ImageResource
  { dataImage    :: a
  , logLumiImage :: a
  , mipImage     :: a
  }
  deriving stock Functor

-- TODO: this shouldn't be baked in
shaderIndices :: ShaderIndices 2
shaderIndices = ShaderIndices
  { emitterCallableRelativeIndices     = Map.fromList [ ( Blackbody , 0 ) ]
  , lightSampleCallableRelativeIndices = Map.fromList [ ( ( Triangle, SurfaceArea ), 1 ), ( ( Sphere, SurfaceArea ), 2 ) ]
  , materialCallableRelativeIndices    = Map.fromList [ ( Lambertian, 3 :& 4 :& End ) ]
  , hitGroupAbsoluteIndices            = Map.fromList [ ( Triangle, V2 8 9 ), ( Sphere, V2 10 11 ) ]
  }

newtype CameraLock = CameraIsLocked { cameraIsLocked :: Bool }

----------------------------------------------------------------------------
-- Application.

rayTracing :: IO ()
rayTracing = runVulkan ( initialState, CameraIsLocked False ) do

  -------------------------------------------
  -- Obtain requirements from shaders.

  ( reqs :: ModuleRequirements ) <-
    case shaderCompilationResult of
      Left  err  -> error $ "Shader compilation was unsuccessful:\n" <> ShortText.unpack err
      Right reqs -> do
        logInfo ( "Shaders were successfully compiled.\nShader directory:\n" <> ShortText.pack shaderDir )
        pure reqs

  -------------------------------------------
  -- Choose scene

  scene <- liftIO chooseScene
  let
    initialUBO :: UBO
    initialUBO = sceneCamera scene :& 0 :& End

  modifying ( typed @RenderState . _observer )
    ( \ obs -> obs { position = view @( Name "position" :.: Swizzle "xyz" ) ( sceneCamera scene ) } )

  -------------------------------------------
  -- Initialise window and Vulkan context.

  ( window, windowExtensions ) <-
    initialiseWindow
      WindowInfo
        { width      = RayTracing.width
        , height     = RayTracing.height
        , windowName = appName
        , mouseMode  = SDL.RelativeLocation
        }

  let
    vulkanReqs = addInstanceExtensions windowExtensions $ vulkanRequirements reqs
    rtDevReqs  = map mkExtensionRequirement
        [ Vulkan.KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
        , Vulkan.KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
        , Vulkan.KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
        ]
      where
        mkExtensionRequirement :: ByteString -> Vulkan.DeviceRequirement
        mkExtensionRequirement extName =
          Vulkan.RequireDeviceExtension
            { Vulkan.deviceExtensionLayerName  = Nothing
            , Vulkan.deviceExtensionName       = extName
            , Vulkan.deviceExtensionMinVersion = 0
            }
    rtDevFeats =
      [ Vulkan.RequireDeviceFeature
          { Vulkan.featureName = "accelerationStructure"
          , Vulkan.checkFeature =
              ( Vulkan.accelerationStructure
                  :: Vulkan.PhysicalDeviceAccelerationStructureFeaturesKHR -> Bool
              )
          , Vulkan.enableFeature = \ accelFeats ->
             accelFeats
               { Vulkan.accelerationStructure = True }
                 :: Vulkan.PhysicalDeviceAccelerationStructureFeaturesKHR 
          }
      , Vulkan.RequireDeviceFeature
          { Vulkan.featureName = "bufferDeviceAddress"
          , Vulkan.checkFeature =
              ( Vulkan.bufferDeviceAddress
                  :: Vulkan.PhysicalDeviceVulkan12Features -> Bool
              )
          , Vulkan.enableFeature = \ feats ->
             feats
               { Vulkan.bufferDeviceAddress = True }
                 :: Vulkan.PhysicalDeviceVulkan12Features
          }
      , Vulkan.RequireDeviceFeature
          { Vulkan.featureName = "descriptorIndexing"
          , Vulkan.checkFeature =
              ( Vulkan.descriptorIndexing
                  :: Vulkan.PhysicalDeviceVulkan12Features -> Bool
              )
          , Vulkan.enableFeature = \ feats ->
             feats
               { Vulkan.descriptorIndexing = True }
                 :: Vulkan.PhysicalDeviceVulkan12Features
          }
      ]
    rtReqs = case vulkanReqs of
      VulkanRequirements instReqs devReqs ->
        VulkanRequirements instReqs ( rtDevFeats <> rtDevReqs <> devReqs )
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
    initialiseContext @WithSwapchain Debug appName rtReqs
      RenderInfo
        { queueType   = Vulkan.QUEUE_GRAPHICS_BIT .|. Vulkan.QUEUE_COMPUTE_BIT .|. Vulkan.QUEUE_TRANSFER_BIT
        , surfaceInfo = surfaceInfo
        }

  withSwapchainInfo aSwapchainInfo \ ( swapchainInfo@(SwapchainInfo {..}) :: SwapchainInfo numImages ) -> do

    commandPool <- logDebug "Creating command pool" *> ( snd <$> createCommandPool device queueFamilyIndex )
    queue       <- getQueue device 0

    ( topLevelAS
      , SceneData
        { luminaireIDs
        , triangleIndices
        , geometryData
        , luminaireProperties
        , materialProperties
        , hitGroupRecordVector
        }
      )
      <- statelessly $ buildScene physicalDevice device commandPool queue shaderIndices scene

    let
      sbtSizes :: SBTSizes
      sbtSizes@( SBTSizes {..})
        = ( \ ( Sum rs, Sum ms, Sum cs, Sum hs ) -> SBTSizes { numRaygens = rs, numMiss = ms, numCallables = cs, numHits = hs } )
        $ foldMap groupSizes ( shaderGroups RayTracing.shaders )
          where
            groupSizes :: Either a ( ShaderGroup a ) -> ( Sum Word64, Sum Word64, Sum Word64, Sum Word64 )
            groupSizes ( Left _ ) = mempty
            groupSizes ( Right gp ) = case gp of
              RaygenGroup   {} -> ( 1, 0, 0, 0 )
              MissGroup     {} -> ( 0, 1, 0, 0 )
              CallableGroup {} -> ( 0, 0, 1, 0 )
              HitGroup      {} -> ( 0, 0, 0, 1 )

  -------------------------------------------
  -- Create images.

    let

      width, height :: Num a => a
      width  = fromIntegral $ ( Vulkan.width  :: Vulkan.Extent2D -> Word32 ) swapchainExtent
      height = fromIntegral $ ( Vulkan.height :: Vulkan.Extent2D -> Word32 ) swapchainExtent

      extent3D, mipExtent3D :: Vulkan.Extent3D
      extent3D
        = Vulkan.Extent3D
            { Vulkan.width  = width
            , Vulkan.height = height
            , Vulkan.depth  = 1
            }
      mipExtent3D
        = Vulkan.Extent3D
            { Vulkan.width  = RayTracing.mipWidth
            , Vulkan.height = RayTracing.mipHeight
            , Vulkan.depth  = 1
            }

      colFmt :: Vulkan.Format
      colFmt = ( Vulkan.format :: Vulkan.SurfaceFormatKHR -> Vulkan.Format ) surfaceFormat

    swapchainImagesAndViews <-
      for swapchainImages \swapchainImage -> do
        swapchainImageView
          <- createImageView
                device swapchainImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                colFmt
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        pure ( swapchainImage, swapchainImageView )

    screenshotImagesAndMemories <-
      for swapchainImages $ \ _ ->
        createScreenshotImage physicalDevice device
          ( screenshotImageInfo extent3D colFmt )

    let
      dataFormat :: Vulkan.Format
      dataFormat = Vulkan.FORMAT_R32G32B32A32_SFLOAT
      dataImageInfo, logLumiImageInfo, mipImageInfo :: ImageInfo
      dataImageInfo =
        Default2DImageInfo
          extent3D
          dataFormat
          Vulkan.IMAGE_USAGE_STORAGE_BIT
      logLumiImageInfo =
        Default2DImageInfo
          extent3D
          dataFormat
          ( Vulkan.IMAGE_USAGE_STORAGE_BIT .|. Vulkan.IMAGE_USAGE_TRANSFER_SRC_BIT )
      mipImageInfo =
        Default2DImageInfo
          mipExtent3D
          dataFormat
          ( Vulkan.IMAGE_USAGE_SAMPLED_BIT .|. Vulkan.IMAGE_USAGE_TRANSFER_DST_BIT )

    imageResources <-
      for swapchainImages \ _ -> do
        ( dataImage, dataImageMemory ) <-
          createImage
            physicalDevice device dataImageInfo
            Vulkan.zero
        dataImageView
          <- createImageView
                device dataImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                dataFormat
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        ( logLumiImage, logLumiImageMemory ) <-
          createImage
            physicalDevice device logLumiImageInfo
            Vulkan.zero
        logLumiImageView
          <- createImageView
                device logLumiImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                dataFormat
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        ( mipImage, mipImageMemory ) <-
          createImage
            physicalDevice device mipImageInfo
            Vulkan.zero
        mipImageView
          <- createImageView
                device mipImage
                Vulkan.IMAGE_VIEW_TYPE_2D
                dataFormat
                Vulkan.IMAGE_ASPECT_COLOR_BIT
        pure $ ImageResource
          { dataImage    = (    dataImage,    dataImageMemory,    dataImageView )
          , logLumiImage = ( logLumiImage, logLumiImageMemory, logLumiImageView )
          , mipImage     = (     mipImage,     mipImageMemory,     mipImageView )
          }

    -- Initialize data images.
    ( _, imageInitCommandBuffer ) <- allocateCommandBuffer device commandPool
    beginCommandBuffer imageInitCommandBuffer
    for_ imageResources \ ( ImageResource ( dataImg, _, _ ) ( logLumiImg, _, _ ) _ ) ->
      for_ [ dataImg, logLumiImg ] \ img ->
        cmdTransitionImageLayout imageInitCommandBuffer img
          Vulkan.IMAGE_LAYOUT_UNDEFINED
          Vulkan.IMAGE_LAYOUT_GENERAL
          ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.zero )
          ( Vulkan.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
          , Vulkan.ACCESS_SHADER_READ_BIT .|. Vulkan.ACCESS_SHADER_WRITE_BIT
          )
    endCommandBuffer imageInitCommandBuffer
    submitCommandBuffer queue imageInitCommandBuffer [] [] Nothing

    -------------------------------------------
    -- Manage resources: uniform buffer, acceleration structure, storage buffers, images.

    mipSampler <- createSampler device

    let

      uboResourceFlags :: UBOResourceSet numImages Named
      uboResourceFlags = UBOResourceSet
        { uboResource      = StageFlags
                             Vulkan.SHADER_STAGE_RAYGEN_BIT_KHR
        , accelResource    = StageFlags $
                             Vulkan.SHADER_STAGE_RAYGEN_BIT_KHR
                         .|. Vulkan.SHADER_STAGE_CLOSEST_HIT_BIT_KHR
        , luminaireIDs     = StageFlags 
                             Vulkan.SHADER_STAGE_CLOSEST_HIT_BIT_KHR
        , indexBuffer      = StageFlags $
                             Vulkan.SHADER_STAGE_CALLABLE_BIT_KHR
                         .|. Vulkan.SHADER_STAGE_CLOSEST_HIT_BIT_KHR
        , trianglesArray   = StageFlags $
                             Vulkan.SHADER_STAGE_CALLABLE_BIT_KHR
                         .|. Vulkan.SHADER_STAGE_CLOSEST_HIT_BIT_KHR
        , spheresArray     = StageFlags $
                             Vulkan.SHADER_STAGE_CALLABLE_BIT_KHR
                         .|. Vulkan.SHADER_STAGE_CLOSEST_HIT_BIT_KHR
                         .|. Vulkan.SHADER_STAGE_INTERSECTION_BIT_KHR
        , blackbodiesArray = StageFlags Vulkan.SHADER_STAGE_CALLABLE_BIT_KHR
        , lambertiansArray = StageFlags Vulkan.SHADER_STAGE_CALLABLE_BIT_KHR
        }

      dataResourceFlags :: DataResourceSet numImages Named
      dataResourceFlags = DataResourceSet
        { dataImages    = StageFlags $
                          Vulkan.SHADER_STAGE_RAYGEN_BIT_KHR
                      .|. Vulkan.SHADER_STAGE_COMPUTE_BIT
        , logLumiImages = StageFlags Vulkan.SHADER_STAGE_RAYGEN_BIT_KHR
        }

      presentResourceFlags :: PresentResourceSet numImages Named
      presentResourceFlags = PresentResourceSet
        { presentImages = StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT
        , mipImages     = StageFlags Vulkan.SHADER_STAGE_COMPUTE_BIT
        }

      initialUBOResourceSet :: UBOResourceSet numImages Pre
      initialUBOResourceSet = UBOResourceSet
        { uboResource      = BufferData initialUBO
        , accelResource    = TopLevelAS topLevelAS
        , luminaireIDs     = BufferData ( toList luminaireIDs )
        , indexBuffer      = BufferData ( toList triangleIndices )
        , trianglesArray   = BufferData
          ( maybe [] toList $ DMap.lookup ( TagGeometryData     $ STriangle            ) geometryData )
        , spheresArray     = BufferData
          ( maybe [] toList $ DMap.lookup ( TagGeometryData     $ SNotTriangle @Sphere ) geometryData )
        , blackbodiesArray = BufferData
          ( maybe [] toList $ DMap.lookup ( TagLuminaireProperties $ Proxy @Blackbody  ) luminaireProperties )
        , lambertiansArray = BufferData
          ( maybe [] toList $ DMap.lookup ( TagMaterialProperties  $ Proxy @Lambertian ) materialProperties )
        }

      initialDataResourceSet :: DataResourceSet numImages Pre
      initialDataResourceSet = DataResourceSet
        { dataImages    = Ixed $
              fmap
                ( StorageImage . ( \ (_,_,imgView) -> imgView ) . dataImage )
                imageResources
        , logLumiImages = Ixed $
              fmap
                ( StorageImage . ( \ (_,_,imgView) -> imgView ) . logLumiImage )
                imageResources
        }

      initialPresentResourceSet :: PresentResourceSet numImages Pre
      initialPresentResourceSet = PresentResourceSet
        { mipImages     = Ixed $
              fmap
                ( SampledImage mipSampler . ( \ (_,_,imgView) -> imgView ) . mipImage )
                imageResources
        , presentImages = Ixed $ fmap ( StorageImage . snd ) swapchainImagesAndViews
        }

    logDebug "Initialising UBO"
    PostInitialisationResult
      uboDescriptorSetLayout uboDescriptorSets _ uboResources
        <- initialiseResources physicalDevice device uboResourceFlags initialUBOResourceSet
    logDebug "Initialising data images"
    PostInitialisationResult
      dataDescriptorSetLayout dataDescriptorSets _ _
        <- initialiseResources physicalDevice device dataResourceFlags initialDataResourceSet
    logDebug "Initialising compute shader images"
    PostInitialisationResult
      presentDescriptorSetLayout presentDescriptorSets _ _
        <- initialiseResources physicalDevice device presentResourceFlags initialPresentResourceSet

    -------------------------------------------
    -- Create a command buffer and record the commands into it.

    (_, nextImageSem ) <- createSemaphore device
    (_, submitted    ) <- createSemaphore device

    rtPipelineLayout <-
      logDebug "Creating ray-tracing pipeline layout" *>
        createPipelineLayout device
          [ uboDescriptorSetLayout
          , dataDescriptorSetLayout
          , dataDescriptorSetLayout
          ]

    tonemapPipelineLayout <-
      logDebug "Creating tone-mapping pipeline layout" *>
        createPipelineLayout device
          [ dataDescriptorSetLayout
          , presentDescriptorSetLayout
          ]

    initialShaderModules <- traverse
      ( \ ( shaderPath, _ ) -> do
        logDebug ( "Loading shader from " <> ShortText.pack shaderPath )
        releaseKeyAndModule <- loadShader device shaderPath
        pure ( shaderPath, releaseKeyAndModule )
      )
      RayTracing.shaders

    -- Ray-tracing setup
    ( _ Vulkan.::& rtProps@( Vulkan.PhysicalDeviceRayTracingPipelinePropertiesKHR {} ) Vulkan.:& () ) <-
      Vulkan.getPhysicalDeviceProperties2KHR physicalDevice

    let
      recordCommandBuffers
        :: MonadVulkan m
        => Bool
        -> ( VkPipeline, ( Vulkan.Buffer, Word64 ), VkPipeline )
        -> m ( Sized.Vector numImages (ReleaseKey, Vulkan.CommandBuffer) )
      recordCommandBuffers takeScreenshot ( rtPipeline, ( sbtBuffer, sbtStride ), tonemapPipeline ) =
        ( `V.imapM` swapchainImagesAndViews ) \ i ( swapchainImage, _ ) -> do

          sbtBaseAddress <-
            Vulkan.getBufferDeviceAddress device ( Vulkan.BufferDeviceAddressInfo sbtBuffer )

          let
            raygenRegion, missRegion, callableRegion, hitRegion :: Vulkan.StridedDeviceAddressRegionKHR
            raygenRegion = Vulkan.StridedDeviceAddressRegionKHR
              { Vulkan.deviceAddress = sbtBaseAddress
              , Vulkan.stride        = sbtStride
              , Vulkan.size          = numRaygens * sbtStride
              }
            missRegion = Vulkan.StridedDeviceAddressRegionKHR
              { Vulkan.deviceAddress = sbtBaseAddress + numRaygens * sbtStride
              , Vulkan.stride        = sbtStride
              , Vulkan.size          = numMiss * sbtStride
              }
            callableRegion = Vulkan.StridedDeviceAddressRegionKHR
              { Vulkan.deviceAddress = sbtBaseAddress + ( numRaygens + numMiss ) * sbtStride
              , Vulkan.stride        = sbtStride
              , Vulkan.size          = numCallables * sbtStride
              }
            hitRegion = Vulkan.StridedDeviceAddressRegionKHR
              { Vulkan.deviceAddress = sbtBaseAddress + ( numRaygens + numMiss + numCallables ) * sbtStride
              , Vulkan.stride        = sbtStride
              , Vulkan.size          = fromIntegral ( Boxed.Vector.length hitGroupRecordVector ) * sbtStride
              }
            dataImage, logLumiImage, mipImage :: Vulkan.Image
            ImageResource { dataImage, logLumiImage, mipImage } = fmap ( \ (img,_,_) -> img ) $ imageResources `V.index` i

          res@(_, commandBuffer) <- allocateCommandBuffer device commandPool
          beginCommandBuffer commandBuffer

          cmdTransitionImageLayout commandBuffer swapchainImage
            Vulkan.IMAGE_LAYOUT_UNDEFINED
            Vulkan.IMAGE_LAYOUT_GENERAL
            ( Vulkan.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vulkan.zero )
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_WRITE_BIT )

          -- Ray-tracing.

          cmdBindPipeline commandBuffer rtPipeline
          cmdBindDescriptorSets commandBuffer rtPipelineLayout rtPipeline
            [  uboDescriptorSets `V.index`  i
            , dataDescriptorSets `V.index` (i - 1)
            , dataDescriptorSets `V.index`  i
            ]

          Vulkan.cmdTraceRaysKHR commandBuffer
            raygenRegion missRegion hitRegion callableRegion
            RayTracing.width
            RayTracing.height
            1

          -- image memory barrier: let the ray-tracing pipeline finish writing to the data images
          -- before the tone-mapping pipeline tries to use them
          cmdTransitionImageLayout commandBuffer dataImage
            Vulkan.IMAGE_LAYOUT_GENERAL
            Vulkan.IMAGE_LAYOUT_GENERAL
            ( Vulkan.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR, Vulkan.ACCESS_SHADER_WRITE_BIT )
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT        , Vulkan.ACCESS_SHADER_READ_BIT  )
          cmdTransitionImageLayout commandBuffer logLumiImage
            Vulkan.IMAGE_LAYOUT_GENERAL
            Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            ( Vulkan.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR, Vulkan.ACCESS_SHADER_WRITE_BIT  )
            ( Vulkan.PIPELINE_STAGE_TRANSFER_BIT              , Vulkan.ACCESS_TRANSFER_READ_BIT )

          let
            layers :: Vulkan.ImageSubresourceLayers
            layers
              = Vulkan.ImageSubresourceLayers
                { Vulkan.aspectMask     = Vulkan.IMAGE_ASPECT_COLOR_BIT
                , Vulkan.mipLevel       = 0
                , Vulkan.baseArrayLayer = 0
                , Vulkan.layerCount     = 1
                }
            blitRegion :: Vulkan.ImageBlit
            blitRegion = Vulkan.ImageBlit
              { Vulkan.srcSubresource = layers
              , Vulkan.srcOffsets     = ( Vulkan.Offset3D 0 0 0
                                        , Vulkan.Offset3D width height 1
                                        )
              , Vulkan.dstSubresource = layers
              , Vulkan.dstOffsets     = ( Vulkan.Offset3D 0 0 0
                                        , Vulkan.Offset3D RayTracing.mipWidth RayTracing.mipHeight 1
                                        )
              }
          cmdTransitionImageLayout commandBuffer mipImage
            Vulkan.IMAGE_LAYOUT_UNDEFINED
            Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_READ_BIT    )
            ( Vulkan.PIPELINE_STAGE_TRANSFER_BIT      , Vulkan.ACCESS_TRANSFER_WRITE_BIT )
          Vulkan.cmdBlitImage commandBuffer
            logLumiImage Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            mipImage     Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            ( Boxed.Vector.singleton blitRegion )
            Vulkan.FILTER_LINEAR
          cmdTransitionImageLayout commandBuffer mipImage
            Vulkan.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            Vulkan.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            ( Vulkan.PIPELINE_STAGE_TRANSFER_BIT      , Vulkan.ACCESS_TRANSFER_WRITE_BIT )
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_READ_BIT    )

          -- Tone-mapping.
          cmdBindPipeline commandBuffer tonemapPipeline
          cmdBindDescriptorSets commandBuffer tonemapPipelineLayout tonemapPipeline
            [    dataDescriptorSets `V.index` i
            , presentDescriptorSets `V.index` i
            ]

          liftIO $
            Vulkan.cmdDispatch
              commandBuffer
              ( RayTracing.width  `div` RayTracing.localSizeX )
              ( RayTracing.height `div` RayTracing.localSizeY )
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
          cmdTransitionImageLayout commandBuffer logLumiImage
            Vulkan.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            Vulkan.IMAGE_LAYOUT_GENERAL
            ( Vulkan.PIPELINE_STAGE_TRANSFER_BIT      , Vulkan.ACCESS_TRANSFER_READ_BIT )
            ( Vulkan.PIPELINE_STAGE_COMPUTE_SHADER_BIT, Vulkan.ACCESS_SHADER_READ_BIT .|. Vulkan.ACCESS_SHADER_WRITE_BIT )
          endCommandBuffer commandBuffer
          pure res

      recordAllCommandsFromShaders ( ShaderGroups allShaders ) = do
        let
          rtShaderGroups :: Compose Shaders Maybe ( ShaderGroup Vulkan.ShaderModule )
          rtShaderGroups = Compose $ fmap ( \case { Right a -> Just a; _ -> Nothing } ) allShaders
          tonemapShader :: Vulkan.ShaderModule
          tonemapShader = case tonemapCompute allShaders of
            Left shaderModule -> shaderModule
            _ -> error "internal error: can't handle this tonemap shader"
        ( keyRT  , pipeRT, _ ) <- createRayTracingPipeline device rtPipelineLayout      rtShaderGroups 1
        ( keyTone, pipeTone  ) <- createComputePipeline    device tonemapPipelineLayout tonemapShader
        ( bufMemKeys, ( sbtBuffer, _, sbtStride ) ) <-
          createShaderBindingTableBuffer physicalDevice device pipeRT rtProps sbtSizes hitGroupRecordVector
        ( cmdKeys1  , commands1 ) <- V.unzip <$> recordCommandBuffers False ( pipeRT, ( sbtBuffer, sbtStride ), pipeTone )
        ( cmdKeys2  , commands2 ) <- V.unzip <$> recordCommandBuffers True  ( pipeRT, ( sbtBuffer, sbtStride ), pipeTone )
        let
          releaseAll = do
            traverse_ release cmdKeys2
            traverse_ release cmdKeys1
            release keyTone
            traverse_ release bufMemKeys
            release keyRT
        pure ( releaseAll, ( commands1, commands2 ) )

    -- launch shader reload watcher, which writes command buffers to use to a TVar
    resourcesTVar <-
      statelessly $ shaderReloadWatcher device initialShaderModules
        recordAllCommandsFromShaders

    mainLoop do

      Vulkan.deviceWaitIdle device

      ----------------
      -- shader reloading

      ( updatedCommands, updatedScreenshotCommands )
        <- statelessly ( snd <$> readTVarWithCleanup resourcesTVar )

      ----------------
      -- input

      inputEvents   <- map SDL.Event.eventPayload <$> SDL.pollEvents
      prevCamLocked <- cameraIsLocked <$> use ( typed @CameraLock )
      prevInput     <- use ( typed @RenderState . _input )
      let
        newInput = foldl onSDLInput prevInput inputEvents
        cameraIsLocked =
          if   SDL.ScancodeL `elem` ( keysPressed newInput )
          then not prevCamLocked
          else prevCamLocked
        action =
          if cameraIsLocked
          then ( interpretInput newInput ) { movement = mempty, look = mempty }
          else   interpretInput newInput

        reset    = not
          ( (  norm ( coerce ( movement action ) :: V 3 Float ) < 1e-7
            && norm ( coerce ( look     action ) :: V 2 Float ) < 1e-7
            )
          )
      assign ( typed @CameraLock )
        ( CameraIsLocked { cameraIsLocked } )
      assign ( typed @RenderState . _input )
        ( newInput { mouseRel = pure 0, keysPressed = [] } )

      ----------------
      -- simulation

      oldObserver <- use ( typed @RenderState . _observer )

      let (observer, orientation) = oldObserver `move` action
      assign ( typed @RenderState . _observer ) observer

      when ( locate action )
        ( liftIO $ putStrLn ( show observer ) )

      let
        ( V3 pos_x pos_y pos_z :& right :& up :& V3 fwd_x fwd_y fwd_z :& End )
         = camera observer ( Just orientation )
        currentUBO :: UBO
        currentUBO = currentCamera :& ( if reset then 1 else 0 ) :& End
        currentCamera :: CameraCoordinates
        currentCamera
          =  V4 pos_x pos_y pos_z ( fromIntegral $ frame observer )
          :& right
          :& up
          :& V4 fwd_x fwd_y fwd_z 0
          :& End
        BufferResource _ updateUBO = uboResource uboResources

      liftIO ( updateUBO currentUBO )

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
