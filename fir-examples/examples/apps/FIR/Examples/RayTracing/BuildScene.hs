{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module FIR.Examples.RayTracing.BuildScene where

-- base
import Control.Arrow
  ( second )
import Control.Monad
  ( void )
import Data.Bits
  ( (.|.) )
import Data.Coerce
  ( coerce )
import Data.Foldable
  ( toList, traverse_ )
import Data.Kind
  ( Type )
import Data.Maybe
  ( catMaybes )
import Data.Proxy
  ( Proxy(..) )
import Data.Traversable
  ( for )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Typeable
  ( eqT )
import Data.Word
  ( Word32, Word64 )
import qualified Foreign.Marshal.Array as Foreign.Marshal
  ( pokeArray )
import qualified Foreign.Ptr
  ( castPtr )
import qualified Foreign.Storable as Foreign
  ( Storable )
import qualified Foreign.Storable
  ( Storable(..) )
import GHC.Generics
  ( Generic )
import GHC.TypeNats
  ( Nat )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( lookup )

-- dependent-map
import Data.Dependent.Map
  ( DMap )
import qualified Data.Dependent.Map as DMap
  ( empty, keys, map )
import qualified Data.Dependent.Map.Lens as DMap
  ( dmat )

-- generic-lens
import Data.Generics.Product.Fields
  ( field' )

-- lens
import Control.Lens
  ( Lens', assign, modifying, use )
import Control.Lens.Indexed
  ( ifor )

-- logging-effect
import Control.Monad.Log
  ( logDebug )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, release )

-- some
import Data.GADT.Compare
  ( GCompare(..), GEq(..), GOrdering(..) )
import Data.GADT.Show
  ( GShow(..) )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack, unpack )

-- transformers
import Control.Monad.Trans.State
  ( StateT(runStateT) )

-- unordered-containers
import Data.HashMap.Strict
  ( HashMap )
import qualified Data.HashMap.Strict as HashMap
  ( lookup )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( fromList )

-- vector-builder
import qualified VectorBuilder.Builder as Vector
  ( Builder )
import qualified VectorBuilder.Builder as VectorBuilder
  ( empty, foldable, singleton, size )
import qualified VectorBuilder.Vector as VectorBuilder
  ( build )

-- vector-sized
import qualified Data.Vector.Sized as Boxed.Sized
  ( Vector )
import qualified Data.Vector.Sized as Boxed.Sized.Vector
  ( head, length, singleton, withSized, zipWithM )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Zero as Vulkan

-- fir
import FIR
  ( Struct(..), (:->)((:->)) )
import Math.Linear
  ( V, pattern V2, pattern V3 )

-- fir-examples
import FIR.Examples.RayTracing.Geometry
  ( Geometry(GeometryData, geometryKind), HittableGeometry(aabb) )
import FIR.Examples.RayTracing.Luminaire
  ( Luminaire(LuminaireProperties, luminaireKind), LightSamplingMethod(..) )
import FIR.Examples.RayTracing.Material
  ( Material(MaterialProperties, materialKind) )
import FIR.Examples.RayTracing.Scene
  ( InstanceType(..), Scene(..), GeometryObject(..), SomeMaterialProperties(..), EmitterObject(..) )
import FIR.Examples.RayTracing.Types
  ( GeometryKind(..), LuminaireKind, MaterialKind
  , LuminaireID, ShaderRecord, STriangleQ(..)
  )
import Vulkan.Buffer
  ( createBufferFromPoke )
import Vulkan.Monad
  ( MonadVulkan )
import Vulkan.RayTracing
  ( buildAccelerationStructuresDevice
  , mkVkTransform, identityVkTransform
  )

--------------------------------------------------------------------------

data SceneData f
  = SceneData
  { luminaireIDs         :: f LuminaireID
  , geometryData         :: DMap TagGeometryData        f
  , luminaireProperties  :: DMap TagLuminaireProperties f
  , materialProperties   :: DMap TagMaterialProperties  f
  , hitGroupRecordVector :: f ( ShaderRecord, Word32 )
  , temporaryData        :: [ReleaseKey]
  }
  deriving stock Generic
instance ( forall a. Show a => Show (f a) ) => Show (SceneData f) where
  show (SceneData a b c d e _) =
    "SceneData{\n" <> show a <> "\n" <> show (DMap.keys b) <> "\n" <> show (DMap.keys c) <> "\n" <> show (DMap.keys d) <> "\n" <> show e <> "\n}"

data TagGeometryData ( dat :: Type ) where
  TagGeometryData
    :: HittableGeometry geom => STriangleQ geom -> TagGeometryData ( GeometryData geom )
data TagLuminaireProperties ( props :: Type ) where
  TagLuminaireProperties
    :: Luminaire lum => Proxy lum -> TagLuminaireProperties ( LuminaireProperties lum )
data TagMaterialProperties ( props :: Type ) where
  TagMaterialProperties
    :: Material mat => Proxy mat -> TagMaterialProperties ( MaterialProperties mat )

data ShaderIndices ( nbRayTypes :: Nat ) =
  ShaderIndices
    { emitterCallableRelativeIndices     :: Map LuminaireKind Word32
    , lightSampleCallableRelativeIndices :: Map (GeometryKind, LightSamplingMethod) Word32
    , materialCallableRelativeIndices    :: Map MaterialKind ( Struct '[ "sample" ':-> Word32, "query" ':-> Word32 ] )
    , hitGroupAbsoluteIndices            :: Map GeometryKind ( V nbRayTypes Word32 )
    }

data BLASInfo =
  BLASInfo
    { transform   :: Vulkan.TransformMatrixKHR
    , geometries  :: [ ( Vulkan.AccelerationStructureGeometryKHR, Word32 ) ]
    , buildRanges :: [ Vulkan.AccelerationStructureBuildRangeInfoKHR ]
    , customIndex :: Word32
    , sbtIndex    :: Word32
    }

-- Workaround to enforce stronger alignment requirements.
newtype AlignedAccelerationStructureInstance =
  AlignedAccelerationStructureInstance Vulkan.AccelerationStructureInstanceKHR
  deriving stock Show
instance Foreign.Storable AlignedAccelerationStructureInstance where
  sizeOf _ = Foreign.Storable.sizeOf ( undefined :: Vulkan.AccelerationStructureInstanceKHR )
  alignment _ = 64 -- Vulkan expects a 16 byte alignment when passing a vector of acceleration structure instances
  peek = coerce ( Foreign.Storable.peek @Vulkan.AccelerationStructureInstanceKHR )
  poke = coerce ( Foreign.Storable.poke @Vulkan.AccelerationStructureInstanceKHR )

--------------------------------------------------------------------------

buildScene
  :: ( MonadVulkan m )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> Vulkan.CommandPool
  -> Vulkan.Queue
  -> ShaderIndices nbRayTypes
  -> Scene
  -> m ( Vulkan.AccelerationStructureKHR, SceneData Boxed.Vector )
buildScene physicalDevice device commandPool queue
  ( ShaderIndices
    { emitterCallableRelativeIndices
    , lightSampleCallableRelativeIndices
    , materialCallableRelativeIndices
    , hitGroupAbsoluteIndices
    }
  )
  ( Scene
    { sceneEmitters
    , sceneTriangleGeometries
    , sceneProceduralGeometries
    , sceneInstances
    }
  ) =
    fmap ( second ( morphSceneData VectorBuilder.build ) ) . ( `runStateT` initialBuildSceneData ) $ do

      -- Get enough info to create a BLAS for each emitter.
      logDebug "Gathering scene emitters."
      emitterBLASInfos <-
        ifor sceneEmitters
          \ i ( EmitterObject samplingMethod ( sGeom :: STriangleQ geom ) geomData ( pLum :: Proxy lum ) lumWeight lumProps ( pMat :: Proxy mat ) matProps ) -> do
    
            ( ( ( emitterGeom, nbGeoms :: Int ), ( _ :: GeometryKind ), geomIndex ), buildRange ) <-
              case sGeom of
                STriangle    -> buildTriangleGeometry   physicalDevice device Nothing [geomData]
                SNotTriangle -> buildProceduralGeometry physicalDevice device ( GeometryObject ( Proxy :: Proxy geom ) [geomData] )
            lumIndex  <- appendData pLum ( VectorBuilder.singleton lumProps )
            matIndex  <- appendData pMat ( VectorBuilder.singleton matProps )

            emitterCallable <- case Map.lookup ( luminaireKind @lum ) emitterCallableRelativeIndices of
              Just call -> pure call
              Nothing   ->
                error ( "buildScene: no callable shader index for emitter of kind " <> show ( luminaireKind @lum ) )
            lightSampleCallable <- case Map.lookup ( geometryKind @geom, samplingMethod ) lightSampleCallableRelativeIndices of
              Just call -> pure call
              Nothing   ->
                error ( "buildScene: no callable shader index for light sampling " <> show ( geometryKind @geom, samplingMethod ) )
            ( matSampleCallable, matQueryCallable ) <-
              case Map.lookup ( materialKind  @mat ) materialCallableRelativeIndices of
                Just ( matSampleCallable :& matQueryCallable :& End ) -> pure ( matSampleCallable, matQueryCallable )
                Nothing ->
                  error ( "buildScene: no callable shader indices for material of kind " <> show ( materialKind @mat ) )
            let
              shaderRecord :: ShaderRecord
              shaderRecord
                =  geomIndex
                :& fromIntegral emitterCallable
                :& lumIndex
                :& matSampleCallable
                :& matQueryCallable
                :& matIndex
                :& End
              luminaireID :: LuminaireID
              luminaireID
                =  lumWeight
                :& lumIndex
                :& geomIndex
                :& 0 -- single geometry primitive per instance at the moment
                :& fromIntegral i
                :& emitterCallable
                :& lightSampleCallable
                :& End
            modifying ( field' @"luminaireIDs" ) ( <> VectorBuilder.singleton luminaireID )
            sbtIndex <-
              case Map.lookup ( geometryKind @geom ) hitGroupAbsoluteIndices of
                Nothing -> error ( "buildScene: no shader hit group indices for " <> show ( geometryKind @geom ) <> " emitter geometry" )
                Just is ->
                  appendShaderRecordData
                    ( VectorBuilder.foldable ( fmap ( shaderRecord, ) is ) )
            pure $
              BLASInfo
                { transform   = identityVkTransform
                , geometries  = [ ( emitterGeom, fromIntegral nbGeoms ) ]
                , buildRanges = [ buildRange ]
                , customIndex = fromIntegral i
                , sbtIndex    = fromIntegral sbtIndex
                }

      -- Create acceleration structure geometry data and build ranges for each geometry.
      logDebug "Gathering scene geometries."
      nonEmitterTriangleGeometries   <- for sceneTriangleGeometries   ( uncurry $ buildTriangleGeometry physicalDevice device )
      nonEmitterProceduralGeometries <- for sceneProceduralGeometries ( buildProceduralGeometry physicalDevice device )

      -- Get enough info to create a BLAS for each scene instance.
      logDebug "Gathering scene instances."
      nonEmitterBLASInfos <- ifor sceneInstances \ i ( instanceType, transformMat, geomsAndProps ) -> do
        -- Start off by getting the starting shader binding table index (for the 0th geometry).
        zerothGeomSBTIndex <- VectorBuilder.size <$> use ( field' @"hitGroupRecordVector" )

        -- Next, gather up all the geometries that we are going to add to this BLAS.
        let
          relevantGeometries
            :: HashMap ShortText
                ( ( ( Vulkan.AccelerationStructureGeometryKHR, Int ), GeometryKind, Word32 )
                , Vulkan.AccelerationStructureBuildRangeInfoKHR
                )
          geometryType :: String
          ( relevantGeometries, geometryType )
            | TrianglesInstance <- instanceType
            = ( nonEmitterTriangleGeometries, "Triangle" )
            | otherwise
            = ( nonEmitterProceduralGeometries, "Procedural" )
        ( geoms, buildRanges ) <-
          ( unzip . catMaybes ) <$>
            for geomsAndProps \ ( geomName, SomeMaterialProperties ( pMat :: Proxy mat ) matProps ) -> do

              case HashMap.lookup geomName relevantGeometries of
                Nothing ->
                  error ( "'buildScene': no " <> geometryType <> " geometry named \"" <> ShortText.unpack geomName <> "\"." )
                Just ( ( ( geometry, nbGeoms ), geomKind, geometryDataIndex ), buildRange ) -> do
                  -- Add to the shader record vector for each geometry.
                  matIndex <- appendData pMat ( VectorBuilder.singleton matProps )
                  ( matSampleCallable, matQueryCallable ) <-
                    case Map.lookup ( materialKind @mat ) materialCallableRelativeIndices of
                      Just ( matSampleCallable :& matQueryCallable :& End ) -> pure ( matSampleCallable, matQueryCallable )
                      Nothing ->
                        error
                          ( "'buildScene': no callable shader indices for material of kind " <> show ( materialKind @mat )
                          <> ",\nfor " <> show geomKind <> " geometry named \"" <> ShortText.unpack geomName <> "\"."
                          )
                  let
                    shaderRecord :: ShaderRecord
                    shaderRecord
                      =  geometryDataIndex
                      :& (-1)
                      :& 0
                      :& matSampleCallable
                      :& matQueryCallable
                      :& matIndex
                      :& End
                  case Map.lookup geomKind hitGroupAbsoluteIndices of
                    Nothing -> error ( "'buildScene': no shader hit group indices for " <> show geomKind <> " geometry." )
                    Just is ->
                      void $
                        appendShaderRecordData
                          ( VectorBuilder.foldable ( fmap ( shaderRecord, ) is ) )
                  pure ( Just ( ( geometry, fromIntegral nbGeoms :: Word32 ), buildRange ) )
        pure $
          BLASInfo
            { transform   = mkVkTransform transformMat
            , geometries  = geoms
            , buildRanges = buildRanges
            , customIndex = fromIntegral ( length emitterBLASInfos + i )
            , sbtIndex    = fromIntegral zerothGeomSBTIndex
            }

      -- We now have enough information to create and build all the bottom-level accelerations structures.
      Boxed.Sized.Vector.withSized ( Boxed.Vector.fromList ( emitterBLASInfos <> nonEmitterBLASInfos ) ) \ allBLASInfos -> do

        logDebug "Building bottom-level acceleration structures."

        allBLASes <-
          buildAccelerationStructuresDevice physicalDevice device commandPool queue
            Vulkan.ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
            ( fmap ( \ blasInfo -> ( geometries blasInfo, buildRanges blasInfo ) ) allBLASInfos )

        -- Now that we have all the BLAS, gather them in instances
        -- so that we can build the TLAS.
        let
          mkInstanceFromBLAS ( BLASInfo { transform, customIndex, sbtIndex } ) ( _, blas ) = do
            blasAddress <- Vulkan.getAccelerationStructureDeviceAddressKHR device ( Vulkan.AccelerationStructureDeviceAddressInfoKHR blas )
            logDebug ( "BLAS device address is " <> ShortText.pack ( show blasAddress ) )
            pure . AlignedAccelerationStructureInstance $
              Vulkan.AccelerationStructureInstanceKHR
                { Vulkan.transform                              = transform
                , Vulkan.instanceCustomIndex                    = customIndex
                , Vulkan.mask                                   = 0xff
                , Vulkan.instanceShaderBindingTableRecordOffset = sbtIndex
                , Vulkan.flags                                  = Vulkan.GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
                , Vulkan.accelerationStructureReference         = blasAddress
                }
        ( allInstances :: Boxed.Sized.Vector nbInstances AlignedAccelerationStructureInstance )
          <-  Boxed.Sized.Vector.zipWithM mkInstanceFromBLAS allBLASInfos allBLASes
        let
          nbInstances :: Integral a => a
          nbInstances = fromIntegral $ Boxed.Sized.Vector.length allInstances
          instBufferSize :: Word64
          instBufferSize = nbInstances
                         * fromIntegral ( Foreign.Storable.sizeOf ( undefined :: AlignedAccelerationStructureInstance ) )
        logDebug "Allocating a buffer to hold BLAS instances."
        ( instBufferKeys, ( instBuffer, _ ) ) <-
          createBufferFromPoke
            ( Vulkan.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR .|. Vulkan.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT )
            ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
            Vulkan.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
            physicalDevice
            device
            ( \ instBufferPtr ->
              Foreign.Marshal.pokeArray
                ( Foreign.Ptr.castPtr instBufferPtr )
                ( toList allInstances )
            )
            instBufferSize
        instBufferAddress <- Vulkan.getBufferDeviceAddress device ( Vulkan.BufferDeviceAddressInfo instBuffer )
        logDebug ( "Instance buffer device address is " <> ShortText.pack ( show instBufferAddress ) )
        traverse_ release ( take 1 instBufferKeys )
        let
          allInstancesGeometry :: Vulkan.AccelerationStructureGeometryKHR
          allInstancesGeometry = Vulkan.AccelerationStructureGeometryKHR
            { Vulkan.geometryType = Vulkan.GEOMETRY_TYPE_INSTANCES_KHR
            , Vulkan.flags        = Vulkan.GEOMETRY_OPAQUE_BIT_KHR
            , Vulkan.geometry     = Vulkan.Instances $ Vulkan.AccelerationStructureGeometryInstancesDataKHR
              { Vulkan.arrayOfPointers = False
              , Vulkan.data'           = Vulkan.DeviceAddressConst instBufferAddress
              }
            }
          buildRange :: Vulkan.AccelerationStructureBuildRangeInfoKHR
          buildRange = Vulkan.AccelerationStructureBuildRangeInfoKHR
            { Vulkan.primitiveCount  = nbInstances
            , Vulkan.primitiveOffset = 0
            , Vulkan.firstVertex     = 0
            , Vulkan.transformOffset = 0
            }
        logDebug "Building top-level acceleration structure."
        ( _, tlas ) <- Boxed.Sized.Vector.head <$>
          buildAccelerationStructuresDevice physicalDevice device commandPool queue
            Vulkan.ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
            ( Boxed.Sized.Vector.singleton ( [ ( allInstancesGeometry, nbInstances ) ], [ buildRange ] ) )
        logDebug "Finished building top-level acceleration structure."
        tempKeys <- use ( field' @"temporaryData" )
        traverse_ release tempKeys
        pure tlas

-- | Build an acceleration structure geometry corresponding to given user triangle geometry.
buildTriangleGeometry
  :: MonadVulkan m
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> Maybe [ Word32 ]
  -> [ GeometryData Triangle ]
  -> StateT (SceneData Vector.Builder) m
      ( ( ( Vulkan.AccelerationStructureGeometryKHR, Int ), GeometryKind, Word32 )
      , Vulkan.AccelerationStructureBuildRangeInfoKHR
      )
buildTriangleGeometry physicalDevice device mbIndices geomData = do
  geomIndex <- appendData STriangle ( VectorBuilder.foldable geomData )
  let
    nbGeoms :: Int
    nbGeoms = length geomData
  ( _, ( trisBuffer, _ ) ) <-
    createBufferFromPoke
      ( Vulkan.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR .|. Vulkan.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT )
      ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
      Vulkan.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
      physicalDevice
      device
      ( \ trisBufferPtr ->
        Foreign.Marshal.pokeArray
          trisBufferPtr
          ( fmap ( \ ( p0 :& p1 :& p2 :& _ ) -> V3 p0 p1 p2 ) geomData )
      )
      ( fromIntegral $ nbGeoms * 36 )
  trisBufferAddress <- Vulkan.getBufferDeviceAddress device ( Vulkan.BufferDeviceAddressInfo trisBuffer )
  ( indexType, indexData ) <- case mbIndices of
    Nothing -> pure ( Vulkan.INDEX_TYPE_NONE_KHR, Vulkan.zero )
    Just indices -> do
      let
        nbIndices = length indices
      ( _, ( indsBuffer, _ ) ) <-
        createBufferFromPoke
          ( Vulkan.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR .|. Vulkan.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT )
          ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
          Vulkan.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
          physicalDevice
          device
          ( \ indsPtr ->
            Foreign.Marshal.pokeArray
              indsPtr
              indices
          )
          ( fromIntegral $ 4 * nbIndices )
      indsBufferAddress <- Vulkan.getBufferDeviceAddress device ( Vulkan.BufferDeviceAddressInfo indsBuffer )
      pure ( Vulkan.INDEX_TYPE_UINT32, Vulkan.DeviceAddressConst indsBufferAddress )
  let
    trianglesData :: Vulkan.AccelerationStructureGeometryTrianglesDataKHR
    trianglesData = Vulkan.AccelerationStructureGeometryTrianglesDataKHR
      { Vulkan.vertexFormat  = Vulkan.FORMAT_R32G32B32_SFLOAT
      , Vulkan.vertexData    = Vulkan.DeviceAddressConst trisBufferAddress
      , Vulkan.vertexStride  = 12
      , Vulkan.maxVertex     = 3 * fromIntegral nbGeoms - 1
      , Vulkan.indexType     = indexType
      , Vulkan.indexData     = indexData
      , Vulkan.transformData = Vulkan.zero
      }
    geometry :: Vulkan.AccelerationStructureGeometryKHR
    geometry = Vulkan.AccelerationStructureGeometryKHR
      { Vulkan.geometryType = Vulkan.GEOMETRY_TYPE_TRIANGLES_KHR
      , Vulkan.geometry     = Vulkan.Triangles trianglesData
      , Vulkan.flags        = Vulkan.GEOMETRY_OPAQUE_BIT_KHR
      }
    buildRangeInfo :: Vulkan.AccelerationStructureBuildRangeInfoKHR
    buildRangeInfo = Vulkan.AccelerationStructureBuildRangeInfoKHR
      { Vulkan.primitiveCount  = fromIntegral nbGeoms
      , Vulkan.primitiveOffset = 0
      , Vulkan.firstVertex     = 0
      , Vulkan.transformOffset = 0 
      }
  pure ( ( ( geometry, nbGeoms ), Triangle, geomIndex ), buildRangeInfo )

-- | Build an acceleration structure geometry corresponding to given user procedural geometry
-- (i.e. represented as an AABB in the acceleration structure).
buildProceduralGeometry
  :: MonadVulkan m
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> GeometryObject
  -> StateT (SceneData Vector.Builder) m
      ( ( ( Vulkan.AccelerationStructureGeometryKHR, Int ), GeometryKind, Word32 )
      , Vulkan.AccelerationStructureBuildRangeInfoKHR
      )
buildProceduralGeometry physicalDevice device ( GeometryObject ( _ :: Proxy geom ) geomData ) = do
  geomIndex <- appendData ( SNotTriangle @geom ) ( VectorBuilder.foldable geomData )
  let
    nbGeoms :: Int
    nbGeoms = length geomData
  ( _, ( aabbsBuffer, _ ) ) <-
    createBufferFromPoke
      ( Vulkan.BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR .|. Vulkan.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT )
      ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
      Vulkan.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
      physicalDevice
      device
      ( \ aabbsBufferPtr ->
        Foreign.Marshal.pokeArray
          aabbsBufferPtr
          ( fmap ( uncurry V2 . aabb @geom ) geomData )
      )
      ( fromIntegral $ nbGeoms * 24 )
  aabbsBufferAddress <- Vulkan.getBufferDeviceAddress device ( Vulkan.BufferDeviceAddressInfo aabbsBuffer )
  let
    aabbsData :: Vulkan.AccelerationStructureGeometryAabbsDataKHR
    aabbsData = Vulkan.AccelerationStructureGeometryAabbsDataKHR
      { Vulkan.data'   = Vulkan.DeviceAddressConst aabbsBufferAddress
      , Vulkan.stride  = 24
      }
    geometry :: Vulkan.AccelerationStructureGeometryKHR
    geometry = Vulkan.AccelerationStructureGeometryKHR
      { Vulkan.geometryType = Vulkan.GEOMETRY_TYPE_AABBS_KHR
      , Vulkan.geometry     = Vulkan.Aabbs aabbsData
      , Vulkan.flags        = Vulkan.GEOMETRY_OPAQUE_BIT_KHR
      }
    buildRangeInfo :: Vulkan.AccelerationStructureBuildRangeInfoKHR
    buildRangeInfo = Vulkan.AccelerationStructureBuildRangeInfoKHR
      { Vulkan.primitiveCount  = fromIntegral nbGeoms
      , Vulkan.primitiveOffset = 0
      , Vulkan.firstVertex     = 0
      , Vulkan.transformOffset = 0 
      }
  pure ( ( ( geometry, nbGeoms ), geometryKind @geom, geomIndex ), buildRangeInfo )

--------------------------------------------------------------------------
-- Various utilities.

morphSceneData :: forall f g. ( forall a. f a -> g a ) -> SceneData f -> SceneData g
morphSceneData f ( SceneData i g l m s t ) =
  SceneData ( f i ) ( DMap.map f g ) ( DMap.map f l ) ( DMap.map f m ) ( f s ) t

initialBuildSceneData :: SceneData Vector.Builder
initialBuildSceneData =
  SceneData
    VectorBuilder.empty
    DMap.empty DMap.empty DMap.empty
    VectorBuilder.empty
    []

instance GEq TagGeometryData where
  geq ( TagGeometryData ( _ :: STriangleQ geom1 ) ) ( TagGeometryData ( _ :: STriangleQ geom2 ) ) =
    case eqT @geom1 @geom2 of
      Just Refl -> Just Refl
      _         -> Nothing
instance GEq TagLuminaireProperties where
  geq ( TagLuminaireProperties ( _ :: Proxy lum1 ) ) ( TagLuminaireProperties ( _ :: Proxy lum2 ) ) =
    case eqT @lum1 @lum2 of
      Just Refl -> Just Refl
      _         -> Nothing
instance GEq TagMaterialProperties where
  geq ( TagMaterialProperties ( _ :: Proxy mat1 ) ) ( TagMaterialProperties ( _ :: Proxy mat2 ) ) =
    case eqT @mat1 @mat2 of
      Just Refl -> Just Refl
      _         -> Nothing
instance GCompare TagGeometryData where
  gcompare ( TagGeometryData ( _ :: STriangleQ geom1 ) ) ( TagGeometryData ( _ :: STriangleQ geom2 ) ) =
    case eqT @geom1 @geom2 of
      Just Refl -> GEQ
      _         -> if geometryKind @geom1 < geometryKind @geom2
                   then GLT
                   else GGT
instance GCompare TagLuminaireProperties where
  gcompare ( TagLuminaireProperties ( _ :: Proxy lum1 ) ) ( TagLuminaireProperties ( _ :: Proxy lum2 ) ) =
    case eqT @lum1 @lum2 of
      Just Refl -> GEQ
      _         -> if luminaireKind @lum1 < luminaireKind @lum2
                   then GLT
                   else GGT
instance GCompare TagMaterialProperties where
  gcompare ( TagMaterialProperties ( _ :: Proxy mat1 ) ) ( TagMaterialProperties ( _ :: Proxy mat2 ) ) =
    case eqT @mat1 @mat2 of
      Just Refl -> GEQ
      _         -> if materialKind @mat1 < materialKind @mat2
                   then GLT
                   else GGT
instance GShow TagGeometryData where
  gshowsPrec p ( TagGeometryData ( _ :: STriangleQ geom ) ) = showsPrec p ( geometryKind @geom )
instance GShow TagLuminaireProperties where
  gshowsPrec p ( TagLuminaireProperties ( _ :: Proxy lum ) ) = showsPrec p ( luminaireKind @lum )
instance GShow TagMaterialProperties where
  gshowsPrec p ( TagMaterialProperties ( _ :: Proxy mat ) ) = showsPrec p ( materialKind @mat )

class AppendData datType dat where
  dataLens :: datType -> Lens' (SceneData f) (Maybe (f dat))
instance ( HittableGeometry geom, dat ~ GeometryData geom ) => AppendData (STriangleQ geom) dat where
  dataLens sTriQ = field' @"geometryData"        . DMap.dmat ( TagGeometryData        sTriQ )
instance ( Luminaire lum, dat ~ LuminaireProperties lum ) => AppendData (Proxy lum) dat where
  dataLens pLum  = field' @"luminaireProperties" . DMap.dmat ( TagLuminaireProperties pLum  )
instance ( Material mat, dat ~ MaterialProperties mat ) => AppendData (Proxy mat) dat where
  dataLens pMat  = field' @"materialProperties"  . DMap.dmat ( TagMaterialProperties  pMat  )

appendData
  :: forall datType dat m
  .  Monad m
  => AppendData datType dat => datType -> Vector.Builder dat -> StateT (SceneData Vector.Builder) m Word32
appendData datType dat = do
  mbPrevDat <- use ( dataLens @_ @dat datType )
  let
    ( i, totalDat ) = case mbPrevDat of
      Nothing      -> ( 0                         , dat            )
      Just prevDat -> ( VectorBuilder.size prevDat, prevDat <> dat )
  assign ( dataLens @_ @dat datType ) ( Just totalDat )
  pure ( fromIntegral i )

appendShaderRecordData
  :: Monad m => Vector.Builder ( ShaderRecord, Word32 ) -> StateT ( SceneData Vector.Builder ) m Word32
appendShaderRecordData newDat = do
  prevDat <- use ( field' @"hitGroupRecordVector" )
  assign ( field' @"hitGroupRecordVector" ) ( prevDat <> newDat )
  pure ( fromIntegral $ VectorBuilder.size prevDat )
