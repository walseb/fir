{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Vulkan.RayTracing
  ( SBTSizes(..)
  , createRayTracingPipeline, rayStageFlag
  , createShaderBindingTableBuffer
  , mkVkTransform, identityVkTransform
  , buildAccelerationStructuresDevice
  )
  where

-- base
import Control.Arrow
  ( first, second )
import Data.Bits
  ( (.|.) )
import Data.Foldable
  ( for_, toList, traverse_ )
import Data.Maybe
  ( catMaybes )
import Data.Traversable
  ( for )
import Data.Word
  ( Word32, Word64 )
import qualified Foreign.Marshal.Alloc as Foreign.Marshal
  ( allocaBytes )
import qualified Foreign.Marshal.Utils as Foreign.Marshal
  ( copyBytes )
import Foreign.Ptr
  ( Ptr )
import qualified Foreign.Ptr
  ( castPtr, nullPtr, plusPtr )

-- containers
import Data.Sequence
  ( Seq )
import qualified Data.Sequence as Seq
  ( Seq(..), fromList )

-- logging-effect
import Control.Monad.Log
  ( logDebug )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, allocate, release )

-- text-short
import qualified Data.Text.Short as ShortText
  ( pack )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )
import Control.Monad.Trans.State.Strict
  ( State, runState, get, put )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( fromList, head, imapM_, length, singleton, unzip )

-- vector-sized
import qualified Data.Vector.Sized as Boxed.Sized
  ( Vector )
import qualified Data.Vector.Sized as Boxed.SizedVector
  ( fromSized, unzip4 )

-- vulkan
import qualified Vulkan
import qualified Vulkan.CStruct.Extends as Vulkan
  ( SomeStruct(SomeStruct) )
import qualified Vulkan.Zero as Vulkan

-- fir
import FIR
  ( RayShader(..), ShaderGroup(..)
  , Layout(Base), Poke(alignment, poke, sizeOf)
  , nextAligned
  )
import Math.Linear
  ( M(..), pattern V3, pattern V4 )

-- fir-examples
import Vulkan.Backend
import Vulkan.Buffer
  ( createBufferFromPoke )
import Vulkan.Monad
import Vulkan.Pipeline
  ( VkPipeline(..) )

-------------------------------------------------------------------------
-- Handling creation of ray-tracing pipelines.

-- | Create a ray-tracing pipeline from a collection of shader groups.
--
-- The provided order of shader groups will correspond to
-- the order of the shader group handles that are obtained from querying
-- the resulting ray-tracing pipeline with 'getRayTracingShaderGroupHandlesKHR'.
createRayTracingPipeline
  :: forall t m
  .  ( MonadVulkan m, Traversable t )
  => Vulkan.Device
  -> Vulkan.PipelineLayout
  -> t ( ShaderGroup Vulkan.ShaderModule )
  -> Word32 -- ^ Maximum ray recursion depth.
  -> m ( ReleaseKey, VkPipeline, t ( Word32, ShaderGroup Word32 ) )
createRayTracingPipeline device pipelineLayout shaders maxRayRecursionDepth = do

  logDebug "Creating ray-tracing pipeline"
  ( releaseKey, pipeline ) <-
    second ( Boxed.Vector.head . snd ) <$>
      Vulkan.withRayTracingPipelinesKHR
        device
        ( Vulkan.zero :: Vulkan.DeferredOperationKHR )
        ( Vulkan.NULL_HANDLE :: Vulkan.PipelineCache )
        ( Boxed.Vector.singleton $ Vulkan.SomeStruct pipelineCreateInfo )
        Nothing
        allocate

  pure ( releaseKey, RayTracingPipeline pipeline, shaderIndices )

  where

    shaderIndices :: t ( Word32, ShaderGroup Word32 )
    shaderInfos   :: Boxed.Vector ( Vulkan.SomeStruct Vulkan.PipelineShaderStageCreateInfo )
    groupInfos    :: Boxed.Vector ( Vulkan.RayTracingShaderGroupCreateInfoKHR )
    ( shaderIndices, shaderInfos, groupInfos ) = collectRayShaders shaders

    pipelineCreateInfo :: Vulkan.RayTracingPipelineCreateInfoKHR '[]
    pipelineCreateInfo =
      Vulkan.RayTracingPipelineCreateInfoKHR
        { Vulkan.next                         = ()
        , Vulkan.flags                        = Vulkan.zero
        , Vulkan.stages                       = shaderInfos
        , Vulkan.groups                       = groupInfos
        , Vulkan.maxPipelineRayRecursionDepth = maxRayRecursionDepth
        , Vulkan.libraryInfo                  = Nothing
        , Vulkan.libraryInterface             = Nothing
        , Vulkan.dynamicState                 = Nothing
        , Vulkan.layout                       = pipelineLayout
        , Vulkan.basePipelineIndex            = 0
        , Vulkan.basePipelineHandle           = Vulkan.NULL_HANDLE
        }


rayStageFlag :: RayShader -> Vulkan.ShaderStageFlagBits
rayStageFlag RayGenerationShader = Vulkan.SHADER_STAGE_RAYGEN_BIT_KHR
rayStageFlag IntersectionShader  = Vulkan.SHADER_STAGE_INTERSECTION_BIT_KHR
rayStageFlag AnyHitShader        = Vulkan.SHADER_STAGE_ANY_HIT_BIT_KHR
rayStageFlag ClosestHitShader    = Vulkan.SHADER_STAGE_CLOSEST_HIT_BIT_KHR
rayStageFlag MissShader          = Vulkan.SHADER_STAGE_MISS_BIT_KHR
rayStageFlag CallableShader      = Vulkan.SHADER_STAGE_CALLABLE_BIT_KHR

rayShaderInfo
  :: RayShader
  -> Vulkan.ShaderModule
  -> Vulkan.PipelineShaderStageCreateInfo '[]
rayShaderInfo shaderStage shaderModule =
  Vulkan.PipelineShaderStageCreateInfo
    { Vulkan.next               = ()
    , Vulkan.flags              = Vulkan.zero
    , Vulkan.name               = "main"
    , Vulkan.module'            = shaderModule
    , Vulkan.stage              = rayStageFlag shaderStage
    , Vulkan.specializationInfo = Nothing
    }

-- | Take a traversable collection of ray-tracing shader groups,
-- and creates the appropriate vectors of information
-- to be used in creating a ray-tracing pipeline.
collectRayShaders
  :: ( Traversable t )
  => t ( ShaderGroup Vulkan.ShaderModule )
  -> ( t ( Word32, ShaderGroup Word32 )
     , Boxed.Vector ( Vulkan.SomeStruct Vulkan.PipelineShaderStageCreateInfo )
     , Boxed.Vector ( Vulkan.RayTracingShaderGroupCreateInfoKHR )
     )
collectRayShaders = ( \ ( indices, ( _, stages, _, groups ) ) ->
                        ( indices
                        , Boxed.Vector.fromList ( toList stages )
                        , Boxed.Vector.fromList ( toList groups )
                        ) 
                    )
                  . ( `runState` ( 0, Seq.Empty, 0, Seq.Empty ) )
                  . traverse go
  where
    go
      :: ShaderGroup Vulkan.ShaderModule
      -> State
           ( Word32
           , Seq ( Vulkan.SomeStruct Vulkan.PipelineShaderStageCreateInfo )
           , Word32
           , Seq Vulkan.RayTracingShaderGroupCreateInfoKHR
           )
           ( Word32, ShaderGroup Word32 )
    go group = do
      ( shaderNo, shaderInfos, groupNo, groupInfos ) <- get
      let
        thisGroupShaderInfos :: Seq ( Vulkan.SomeStruct Vulkan.PipelineShaderStageCreateInfo )
        thisGroupShaderInfos = Seq.fromList ( groupShaderInfos group )
        thisGroupIndices :: ShaderGroup Word32
        thisGroupInfo :: Vulkan.RayTracingShaderGroupCreateInfoKHR
        newShaderNo :: Word32
        ( thisGroupIndices, thisGroupInfo, newShaderNo ) = groupInfo shaderNo group
      put ( newShaderNo, shaderInfos <> thisGroupShaderInfos, groupNo + 1, groupInfos Seq.:|> thisGroupInfo )
      pure ( groupNo, thisGroupIndices )

groupShaderInfos :: ShaderGroup Vulkan.ShaderModule -> [ Vulkan.SomeStruct Vulkan.PipelineShaderStageCreateInfo ]
groupShaderInfos ( RaygenGroup   sh ) = [ Vulkan.SomeStruct $ rayShaderInfo RayGenerationShader sh ]
groupShaderInfos ( MissGroup     sh ) = [ Vulkan.SomeStruct $ rayShaderInfo MissShader          sh ]
groupShaderInfos ( CallableGroup sh ) = [ Vulkan.SomeStruct $ rayShaderInfo CallableShader      sh ]
groupShaderInfos ( HitGroup { closestHit, anyHit, intersection } ) =
  catMaybes
    [ Vulkan.SomeStruct . rayShaderInfo ClosestHitShader   <$> closestHit
    , Vulkan.SomeStruct . rayShaderInfo AnyHitShader       <$> anyHit
    , Vulkan.SomeStruct . rayShaderInfo IntersectionShader <$> intersection
    ]

groupInfo :: Word32 -> ShaderGroup Vulkan.ShaderModule -> ( ShaderGroup Word32, Vulkan.RayTracingShaderGroupCreateInfoKHR, Word32 )
groupInfo i ( HitGroup { intersection, closestHit, anyHit } ) =
  ( HitGroup
      { closestHit   = fmap ( const i1 ) closestHit
      , anyHit       = fmap ( const i2 ) anyHit
      , intersection = fmap ( const i3 ) intersection
      }
  , Vulkan.RayTracingShaderGroupCreateInfoKHR
    { Vulkan.type'                          = case intersection of
                                   Nothing -> Vulkan.RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR
                                   Just _  -> Vulkan.RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR
    , Vulkan.generalShader                  = Vulkan.SHADER_UNUSED_KHR
    , Vulkan.closestHitShader               = i1
    , Vulkan.anyHitShader                   = i2
    , Vulkan.intersectionShader             = i3
    , Vulkan.shaderGroupCaptureReplayHandle = Foreign.Ptr.nullPtr
    }
  , n
  )
  where
    i1, i2, i3, n  :: Word32
    ( (i1,i2,i3), n ) = case justIndices i [ closestHit, anyHit, intersection ] of
      ( [j1,j2,j3], m ) -> ( (j1,j2,j3), m )
      _                 -> error "impossible"
groupInfo i gp =
  ( fmap ( const i ) gp
  , Vulkan.RayTracingShaderGroupCreateInfoKHR
    { Vulkan.type'                          = Vulkan.RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
    , Vulkan.generalShader                  = i
    , Vulkan.intersectionShader             = Vulkan.SHADER_UNUSED_KHR
    , Vulkan.closestHitShader               = Vulkan.SHADER_UNUSED_KHR
    , Vulkan.anyHitShader                   = Vulkan.SHADER_UNUSED_KHR
    , Vulkan.shaderGroupCaptureReplayHandle = Foreign.Ptr.nullPtr
    }
  , i+1
  )

justIndices :: Word32 -> [ Maybe a ] -> ( [ Word32 ], Word32 )
justIndices i [] = ( [], i )
justIndices i ( Just _  : next ) = first ( i : )                        $ justIndices (i+1) next
justIndices i ( Nothing : next ) = first ( Vulkan.SHADER_UNUSED_KHR : ) $ justIndices i     next

-------------------------------------------------------------------------
-- Shader binding table creation.

data SBTSizes
  = SBTSizes
  { numRaygens   :: !Word64
  , numMiss      :: !Word64
  , numCallables :: !Word64
  , numHits      :: !Word64
  }

-- | Create a shader binding table.
--
-- Assumes that the ray-tracing pipeline which we are querying for the shader group handles
-- has been constructed with the shader groups in the following order:
--
--  - ray-generation shaders,
--  - miss shaders,
--  - callable shaders,
--  - hit shader groups.
createShaderBindingTableBuffer
  :: forall shaderRecordData m
  .  ( MonadVulkan m, Poke shaderRecordData Base )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> VkPipeline
  -> Vulkan.PhysicalDeviceRayTracingPipelinePropertiesKHR
  -> SBTSizes
  -> Boxed.Vector ( shaderRecordData, Word32 )
  -> m ( [ ReleaseKey ], ( Vulkan.Buffer, Ptr (), Word64 ) )
createShaderBindingTableBuffer physicalDevice device ( vkPipeline -> pipeline )
  ( Vulkan.PhysicalDeviceRayTracingPipelinePropertiesKHR
      { shaderGroupHandleSize, shaderGroupHandleAlignment }
  )
  ( SBTSizes
      { numRaygens, numMiss, numCallables, numHits }
  )
  hitGroupRecordVector
  = do

    let
      -- Compute size and alignment requirements.
      numShaderGroups, numSBTHitGroups, numSBTGroups :: Int
      numShaderGroups = fromIntegral numRaygens + fromIntegral numMiss + fromIntegral numCallables + fromIntegral numHits
      numSBTHitGroups = Boxed.Vector.length hitGroupRecordVector
      numSBTGroups    = fromIntegral numRaygens + fromIntegral numMiss + fromIntegral numCallables + numSBTHitGroups
      recordOffset, sbtStride, sbtSize :: Word64
      recordOffset = nextAligned ( fromIntegral shaderGroupHandleSize ) ( fromIntegral $ alignment @shaderRecordData @Base )
      sbtStride    = nextAligned ( recordOffset + fromIntegral ( sizeOf @shaderRecordData @Base ) ) ( fromIntegral shaderGroupHandleAlignment )
      sbtSize      = fromIntegral numSBTGroups * sbtStride

    logDebug "Creating shader binding table buffer"

    ( bufKeys, ( sbtBuffer, sbtPtr ) ) <-
      createBufferFromPoke
        (   Vulkan.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR
        .|. Vulkan.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
        )
        ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
        Vulkan.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
        physicalDevice device ( const $ pure () ) sbtSize

    logDebug ( "Querying " <> ShortText.pack ( show numShaderGroups ) <> " shader group handles from ray-tracing pipeline" )

    Vulkan.getRayTracingShaderGroupHandlesKHR
      device
      pipeline
      0
      ( fromIntegral numShaderGroups )
      sbtSize
      sbtPtr

    logDebug ( "Writing " <> ShortText.pack ( show numSBTGroups ) <> " shader group handles and shader records to shader binding table" )

    liftIO do
      Foreign.Marshal.allocaBytes ( numShaderGroups * fromIntegral shaderGroupHandleSize ) \ handlesPtr -> do

        -- Copy the shader handles to a temporary memory location.
        Foreign.Marshal.copyBytes
          handlesPtr -- dst
          sbtPtr     -- src
          ( numShaderGroups * fromIntegral shaderGroupHandleSize )

        -- Write the ray-generation, miss and callable shaders to their correct (spaced out) locations.
        for_ [ 0 .. fromIntegral ( numRaygens + numMiss + numCallables ) - 1 ] \ ( i :: Int ) -> do
          let
            srcLoc, dstLoc :: Int
            srcLoc = i * fromIntegral shaderGroupHandleSize
            dstLoc = i * fromIntegral sbtStride
          Foreign.Marshal.copyBytes
            ( sbtPtr     `Foreign.Ptr.plusPtr` dstLoc )
            ( handlesPtr `Foreign.Ptr.plusPtr` srcLoc )
            ( fromIntegral shaderGroupHandleSize )

        -- Write the hit group shaders and associated hit group records.
        flip Boxed.Vector.imapM_ hitGroupRecordVector \ i ( recordData, hitGroupNb ) -> do
          let
            srcLoc, dstLoc :: Int
            srcLoc = fromIntegral hitGroupNb * fromIntegral shaderGroupHandleSize
            dstLoc = ( fromIntegral ( numRaygens + numMiss + numCallables ) + i ) * fromIntegral sbtStride
          Foreign.Marshal.copyBytes
            ( sbtPtr     `Foreign.Ptr.plusPtr` dstLoc )
            ( handlesPtr `Foreign.Ptr.plusPtr` srcLoc )
            ( fromIntegral shaderGroupHandleSize )
          poke @shaderRecordData @Base
            ( Foreign.Ptr.castPtr $ sbtPtr `Foreign.Ptr.plusPtr` ( dstLoc + fromIntegral recordOffset ) )
            recordData

    pure ( bufKeys, ( sbtBuffer, sbtPtr, sbtStride ) )

-------------------------------------------------------------------------
-- Acceleration structure creation.

mkVkTransform :: M 3 4 Float -> Vulkan.TransformMatrixKHR
mkVkTransform
  ( M ( V4 ( V3 a00 a10 a20 )
           ( V3 a01 a11 a21 )
           ( V3 a02 a12 a22 )
           ( V3 a03 a13 a23 )
      )
  ) =
  Vulkan.TransformMatrixKHR
    ( a00, a01, a02, a03 )
    ( a10, a11, a12, a13 )
    ( a20, a21, a22, a23 )

identityVkTransform :: Vulkan.TransformMatrixKHR
identityVkTransform =
  Vulkan.TransformMatrixKHR
    ( 1, 0, 0, 0 )
    ( 0, 1, 0, 0 )
    ( 0, 0, 1, 0 )

buildAccelerationStructuresDevice
  :: MonadVulkan m
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> Vulkan.CommandPool
  -> Vulkan.Queue
  -> Vulkan.AccelerationStructureTypeKHR
  -> Boxed.Sized.Vector n
      ( [ ( Vulkan.AccelerationStructureGeometryKHR, Word32 ) ]
      , [ Vulkan.AccelerationStructureBuildRangeInfoKHR ]
      ) 
  -> m ( Boxed.Sized.Vector n ( [ReleaseKey], Vulkan.AccelerationStructureKHR ) )
buildAccelerationStructuresDevice physicalDevice device commandPool queue asType manyGeomsAndBuildRanges = do
  accelsData <- for manyGeomsAndBuildRanges \ ( geoms, buildRanges ) -> do
    -- Vulkan will need a buffer to hold the acceleration structure,
    -- as well as scratch buffers that it uses
    -- while building the acceleration structure from the geometry data.
    -- This means we first need to query for the required sizes of these buffers.
    let
      geomsVector :: Boxed.Vector Vulkan.AccelerationStructureGeometryKHR
      maxPrimsVector :: Boxed.Vector Word32
      ( geomsVector, maxPrimsVector ) = Boxed.Vector.unzip $ Boxed.Vector.fromList geoms
      nbGeoms :: Int
      nbGeoms = Boxed.Vector.length geomsVector
      buildGeometryInfo
        :: Vulkan.AccelerationStructureKHR
        -> Vulkan.DeviceOrHostAddressKHR
        -> Vulkan.AccelerationStructureBuildGeometryInfoKHR
      buildGeometryInfo someAS someScratchAddress = Vulkan.AccelerationStructureBuildGeometryInfoKHR
        { Vulkan.type'                    = asType
        , Vulkan.flags                    = Vulkan.BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
        , Vulkan.mode                     = Vulkan.BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR
        , Vulkan.srcAccelerationStructure = Vulkan.zero
        , Vulkan.dstAccelerationStructure = someAS
        , Vulkan.geometryCount            = fromIntegral nbGeoms
        , Vulkan.geometries               = geomsVector
        , Vulkan.scratchData              = someScratchAddress
        }
    logDebug
      (  "Starting creation of an acceleration structure.\n"
      <> "Number of primitives per geometry: " <> ShortText.pack ( show maxPrimsVector )
      )
    buildSizes <-
      Vulkan.getAccelerationStructureBuildSizesKHR device Vulkan.ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR
        ( buildGeometryInfo Vulkan.zero Vulkan.zero ) maxPrimsVector

    -- We've got the sizes: now create the required buffers.
    logDebug "Allocating acceleration structure buffers."
    ( accelKeys, ( accelBuffer, _ ) )
      <- createBufferFromPoke
            ( Vulkan.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT .|. Vulkan.BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR )
            ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
            Vulkan.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
            physicalDevice device
            ( const ( pure () ) )
            ( Vulkan.accelerationStructureSize buildSizes )
    ( scratchKeys, ( scratchBuffer, _ ) )
      <- createBufferFromPoke
            ( Vulkan.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT .|. Vulkan.BUFFER_USAGE_STORAGE_BUFFER_BIT )
            ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
            Vulkan.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
            physicalDevice device
            ( const ( pure () ) )
            ( Vulkan.buildScratchSize buildSizes )
    scratchBufferAddress <- Vulkan.getBufferDeviceAddress device ( Vulkan.BufferDeviceAddressInfo scratchBuffer )
    logDebug ( "Scratch buffer device address is " <> ShortText.pack ( show scratchBufferAddress ) )
    -- Now create the acceleration structure.
    let
      createInfo :: Vulkan.AccelerationStructureCreateInfoKHR
      createInfo = Vulkan.AccelerationStructureCreateInfoKHR
        { Vulkan.type'         = asType
        , Vulkan.createFlags   = Vulkan.zero
        , Vulkan.buffer        = accelBuffer
        , Vulkan.offset        = 0
        , Vulkan.size          = Vulkan.accelerationStructureSize buildSizes
        , Vulkan.deviceAddress = Vulkan.zero -- no device address
        }
    logDebug "Creating acceleration structure."
    ( _, as ) <- Vulkan.withAccelerationStructureKHR device createInfo Nothing allocate

    pure ( take 1 accelKeys <> scratchKeys
         , ( drop 1 accelKeys, as )
         , buildGeometryInfo as ( Vulkan.DeviceAddress scratchBufferAddress )
         , Boxed.Vector.fromList buildRanges
         )

  -- We've now created all the individual acceleration structures.
  -- We can now build them all in one go.
  let
    ( manyTempKeys, accels, buildInfos, manyBuildRanges ) = Boxed.SizedVector.unzip4 accelsData

  logDebug "Building acceleration structure(s)."
  ( commandBufferKey, commandBuffer ) <- allocateCommandBuffer device commandPool
  ( fenceKey, fence ) <- createFence device
  beginCommandBuffer commandBuffer
  _ <- Vulkan.cmdBuildAccelerationStructuresKHR commandBuffer
          ( Boxed.SizedVector.fromSized buildInfos      )
          ( Boxed.SizedVector.fromSized manyBuildRanges )
  endCommandBuffer commandBuffer
  submitCommandBuffer queue commandBuffer
    [] [] ( Just fence )

  waitForFences device ( WaitAll [ fence ] )
  logDebug "Acceleration structure build complete."
  traverse_ release [ fenceKey, commandBufferKey ]
  traverse_ ( traverse_ release ) manyTempKeys

  pure accels
