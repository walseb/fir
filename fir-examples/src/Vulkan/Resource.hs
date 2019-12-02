{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Vulkan.Resource
  ( ResourceStatus(..)
  , ResourceInfo(..), Pre, Post
  , ResourceUsage(..)
  , ResourceType(..)
  , Resource(..)
  , PostInitialisationResult(..)
  , Descriptor, Descriptors
  , IndexBuffer, VertexBuffer
  , UniformBuffer, UniformBuffers
  , StorageImage, StorageImages
  , SampledImage, SampledImages
  , initialiseResources
  ) where

-- base
import Control.Monad
  ( (>=>) )
import Data.Coerce
  ( coerce )
import Data.Function
  ( (&) )
import Data.Functor
  ( (<&>) )
import Data.Functor.Const
  ( Const(Const) )
import Data.Kind
  ( Type )
import Data.Maybe
  ( fromJust, listToMaybe )
import Data.Monoid
  ( Endo(Endo) )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word16, Word32 )
import qualified Foreign.Marshal
  ( withArray )
import GHC.TypeNats
  ( Nat, KnownNat, natVal )

-- containers
import qualified Data.Map.Strict as Map
  ( empty, insertWith, toList )

-- finite-typelits
import Data.Finite
  ( Finite )

-- generic-lens
import Data.Generics.Product.Constraints
  ( HasConstraints(constraints)
  , HasConstraints'(constraints')
  )

-- managed
import Control.Monad.Managed
  ( MonadManaged )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )
import Control.Monad.Trans.Class
  ( lift )
import Control.Monad.Trans.State.Strict
  ( evalStateT, get, put )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( Vector, fromList, index, ifoldl )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan                      as Vulkan
import qualified Graphics.Vulkan.Core_1_0             as Vulkan
import qualified Graphics.Vulkan.Marshal.Create       as Vulkan

-- fir
import FIR
  ( Poke, Layout(Base, Extended, Locations) )

-- fir-examples
import Vulkan.Buffer
import Vulkan.Monad

----------------------------------------------------------------------------
-- Higher-kinded data approach for generic resource sets.

data ResourceStatus
  = PreInitialised
  | Initialised

data ResourceInfo
  = Named
  | Specified ResourceStatus

type Pre  = Specified PreInitialised
type Post = Specified Initialised

data DescriptorCount (n :: Nat)
  = Single
  | Multiple

data ResourceUsage (n :: Nat)
  = DescriptorUse ( DescriptorCount n )
  | InputUse

data BufferType
  = Vertex
  | Index
  | Uniform

data ImageType
  = Store
  | Sample

data ResourceType t where
  Buffer :: BufferType -> t -> ResourceType t
  Image  :: ImageType -> ResourceType ()

data family Resource ( res :: ResourceType t ) ( use :: ResourceUsage n ) ( st :: ResourceInfo )

data instance Resource res (DescriptorUse n) Named where
  StageFlags :: KnownResourceType res => Vulkan.VkShaderStageFlags -> Resource res (DescriptorUse n) Named
data instance Resource res InputUse Named = InputResource

data instance Resource (Buffer Vertex  a) InputUse               (Specified PreInitialised) = VertexBuffer  [a]
data instance Resource (Buffer Index   a) InputUse               (Specified PreInitialised) = IndexBuffer   [a]
data instance Resource (Buffer Uniform a) (DescriptorUse Single) (Specified PreInitialised) = UniformBuffer  a

data instance Resource (Buffer bufType a) (DescriptorUse Single) (Specified Initialised) where
  BufferResource ::
    { bufferObject :: Vulkan.VkBuffer
    , pokeBuffer   :: a -> IO ()
    }
    -> Resource (Buffer bufType a) (DescriptorUse Single) (Specified Initialised)
data instance Resource (Buffer bufType a) InputUse (Specified Initialised)
  = InputBuffer { inputBufferObject :: Vulkan.VkBuffer, nbBufferElems :: Int }

data instance Resource ( Image Store  ) (DescriptorUse Single) (Specified st) = StorageImage Vulkan.VkImageView
data instance Resource ( Image Sample ) (DescriptorUse Single) (Specified st) = SampledImage Vulkan.VkSampler Vulkan.VkImageView

data instance Resource res (DescriptorUse Multiple :: ResourceUsage n) (Specified st)
  = Ixed ( V.Vector n ( Resource res (DescriptorUse Single :: ResourceUsage n) (Specified st) ) )

type Descriptor   res i st = Resource res ( DescriptorUse Single   :: ResourceUsage i ) st
type Descriptors  res i st = Resource res ( DescriptorUse Multiple :: ResourceUsage i ) st
type UniformBuffer  a i st = Descriptor  ( Buffer Uniform a ) i st
type UniformBuffers a i st = Descriptors ( Buffer Uniform a ) i st
type VertexBuffer   a i st = Resource ( Buffer Vertex a ) ( InputUse :: ResourceUsage i ) st
type IndexBuffer    a i st = Resource ( Buffer Index  a ) ( InputUse :: ResourceUsage i ) st
type StorageImage     i st = Descriptor  ( Image Store  ) i st
type StorageImages    i st = Descriptors ( Image Store  ) i st
type SampledImage     i st = Descriptor  ( Image Sample ) i st
type SampledImages    i st = Descriptors ( Image Sample ) i st

----------------------------------------------------------------------------

data PostInitialisationResult m resources n
  = PostInitialisationResult
      { resourceLayout       :: Vulkan.VkDescriptorSetLayout
      , resourceDescriptors  :: V.Vector n Vulkan.VkDescriptorSet
      , bindBuffersCommand   :: MonadManaged m => Vulkan.VkCommandBuffer -> m ()
      , initialisedResources :: resources n Post
      }

initialiseResources
  :: forall
      ( n  :: Nat )
      ( m  :: Type -> Type )
      ( resources :: Nat -> ResourceInfo -> Type )
  . ( KnownNat n
    , MonadManaged m
    , HasConstraints' HasDescriptorTypeAndFlags ( resources n Named )
    , HasConstraints CanInitialiseResource
         ( resources n Pre  )
         ( resources n Post )
    , HasConstraints' ( CanUpdateResource n ) ( resources n Post )
    )
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> resources n Named
  -> resources n Pre
  -> m (PostInitialisationResult m resources n)
initialiseResources physicalDevice device resourceFlags resourcesPre = do
  let
    descriptorTypesAndFlags :: [ (Vulkan.VkDescriptorType, Vulkan.VkShaderStageFlags) ]
    descriptorTypesAndFlags =
      foldrC @HasDescriptorTypeAndFlags
        ( \ res flags -> case descriptorTypeAndFlags res of
          Just flag -> flag : flags
          Nothing   -> flags
        )
        resourceFlags
        []
    descriptorTypes = map fst descriptorTypesAndFlags
    n = fromIntegral ( natVal ( Proxy @n ) )

  descriptorPool      <- createDescriptorPool device n descriptorTypes
  descriptorSetLayout <- createDescriptorSetLayout device descriptorTypesAndFlags
  descriptorSets      <- allocateDescriptorSets device descriptorPool descriptorSetLayout n

  ( resourcesPost :: resources n Post )
     <- ( constraints @CanInitialiseResource
            ( initialiseResource physicalDevice device )
        ) resourcesPre

  ( descriptors :: V.Vector n Vulkan.VkDescriptorSet ) <-
    ( `evalStateT` descriptorSets ) $ do
      ( theseDescriptorSets, nextDescriptorSets ) <- splitAt n <$> get
      put nextDescriptorSets
      let
        descriptors :: V.Vector n Vulkan.VkDescriptorSet
        descriptors = fromJust $ V.fromList theseDescriptorSets
      lift ( updateDescriptorSets device descriptors resourcesPost )
      pure descriptors

  let
    indexBuffers  :: [ ( Vulkan.VkBuffer, Vulkan.VkIndexType ) ]
    vertexBuffers :: [ Vulkan.VkBuffer ]
    (indexBuffers, vertexBuffers) =
      foldrC @(CanUpdateResource n)
        ( \ res ( indexBinds, vertexBinds) -> case bindingInformation @n res of
          Just (BindIndexBuffer  buf _ ty) -> ( ( buf, ty ) : indexBinds, vertexBinds )
          Just (BindVertexBuffer buf     ) -> ( indexBinds, buf : vertexBinds )
          Nothing                          -> ( indexBinds, vertexBinds)
        )
        resourcesPost
        ( [], [] )

    bindingCommand :: Vulkan.VkCommandBuffer -> m ()
    bindingCommand = bufferBindingCommand ( listToMaybe indexBuffers ) vertexBuffers

  pure
    PostInitialisationResult
      { resourceLayout       = descriptorSetLayout
      , resourceDescriptors  = descriptors
      , bindBuffersCommand   = bindingCommand
      , initialisedResources = resourcesPost
      }

----------------------------------------------------------------------------

createDescriptorSetLayout
  :: MonadManaged m
  => Vulkan.VkDevice
  -> [ ( Vulkan.VkDescriptorType, Vulkan.VkShaderStageFlags ) ]
  -> m Vulkan.VkDescriptorSetLayout
createDescriptorSetLayout device descriptorTypes =
  managedVulkanResource
    ( Vulkan.vkCreateDescriptorSetLayout
        device
        ( Vulkan.unsafePtr createInfo )
    )
    ( Vulkan.vkDestroyDescriptorSetLayout device )

      where
        bindings :: [ Vulkan.VkDescriptorSetLayoutBinding ]
        bindings = zip descriptorTypes [0..] <&> \ ( ( descType, descStageFlags ), i ) ->
          Vulkan.createVk
            (  Vulkan.set @"binding"            i
            &* Vulkan.set @"descriptorType"     descType
            &* Vulkan.set @"descriptorCount"    1
            &* Vulkan.set @"stageFlags"         descStageFlags
            &* Vulkan.set @"pImmutableSamplers" Vulkan.VK_NULL
            )
        createInfo :: Vulkan.VkDescriptorSetLayoutCreateInfo
        createInfo =
          Vulkan.createVk
            (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
            &* Vulkan.set @"pNext" Vulkan.VK_NULL
            &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
            &* Vulkan.setListCountAndRef @"bindingCount" @"pBindings" bindings
            )

createDescriptorPool
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Int
  -> [ Vulkan.VkDescriptorType ]
  -> m Vulkan.VkDescriptorPool
createDescriptorPool device maxSets descTypes =
  managedVulkanResource
  ( Vulkan.vkCreateDescriptorPool device ( Vulkan.unsafePtr createInfo ) )
  ( Vulkan.vkDestroyDescriptorPool device )

    where
      poolSizes :: [ Vulkan.VkDescriptorPoolSize ]
      poolSizes =
        counts descTypes <&> \ ( descType, descCount ) ->
          Vulkan.createVk
          (  Vulkan.set @"type" descType
          &* Vulkan.set @"descriptorCount" descCount
          )
      createInfo :: Vulkan.VkDescriptorPoolCreateInfo
      createInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" Vulkan.VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
          &* Vulkan.setListCountAndRef @"poolSizeCount" @"pPoolSizes" poolSizes
          &* Vulkan.set @"maxSets" ( fromIntegral maxSets )
          )

allocateDescriptorSets
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorPool
  -> Vulkan.VkDescriptorSetLayout
  -> Int
  -> m [Vulkan.VkDescriptorSet]
allocateDescriptorSets dev descriptorPool layout0 count =
  manageBracket
  ( allocaAndPeekArray count
      ( Vulkan.vkAllocateDescriptorSets
          dev
          ( Vulkan.unsafePtr allocateInfo )
          >=> throwVkResult
      )
  )
  ( \descs ->
      Foreign.Marshal.withArray descs
        ( Vulkan.vkFreeDescriptorSets dev descriptorPool (fromIntegral count) )
  )
    where
      allocateInfo :: Vulkan.VkDescriptorSetAllocateInfo
      allocateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"descriptorPool" descriptorPool
          &* Vulkan.setListCountAndRef @"descriptorSetCount" @"pSetLayouts"
               ( replicate count layout0 )
          )

counts :: (Ord a, Num i) => [ a ] -> [ (a, i) ]
counts = Map.toList . foldr ( \ a -> Map.insertWith (+) a 1 ) Map.empty


updateDescriptorSets
  :: forall m n resources
  .  ( MonadManaged m
     , HasConstraints' ( CanUpdateResource n ) ( resources n Post )
     )
  => Vulkan.VkDevice
  -> V.Vector n Vulkan.VkDescriptorSet
  -> resources n Post
  -> m ()
updateDescriptorSets device descriptorSets resourceSet =
  liftIO $
    Foreign.Marshal.withArray
      writes $ \writesPtr ->
        Vulkan.vkUpdateDescriptorSets device
          ( fromIntegral $ length writes )
          writesPtr
          0 -- no descriptor copies
          Vulkan.vkNullPtr
    where
      writes :: [ Vulkan.VkWriteDescriptorSet ]
      writes = V.ifoldl descriptorSetWrites [] descriptorSets
      descriptorSetWrites :: [ Vulkan.VkWriteDescriptorSet ] -> Finite n -> Vulkan.VkDescriptorSet -> [ Vulkan.VkWriteDescriptorSet ]
      descriptorSetWrites writesAcc i descriptorSet
        = writesAcc ++
        ( zipWith (&) [0..] $
          foldrC @(CanUpdateResource n)
            ( \ res descs -> descriptorWrites descriptorSet i res ++ descs )
            resourceSet
            []
        )

bufferBindingCommand
  :: MonadManaged m
  => Maybe ( Vulkan.VkBuffer, Vulkan.VkIndexType )
  -> [ Vulkan.VkBuffer ]
  -> Vulkan.VkCommandBuffer
  -> m ()
bufferBindingCommand mbIndexBuffer vertexBuffers commandBuffer = do

  let
    nbVertexBuffers :: Int
    nbVertexBuffers = length vertexBuffers

  liftIO $
    Foreign.Marshal.withArray vertexBuffers $ \buffers ->
      Foreign.Marshal.withArray ( replicate nbVertexBuffers 0 ) $ \offsets ->
        Vulkan.vkCmdBindVertexBuffers
          commandBuffer
          0
          ( fromIntegral nbVertexBuffers )
          buffers
          offsets

  case mbIndexBuffer of
    Nothing                 -> pure ()
    Just ( buff, ixType ) ->
      liftIO $
        Vulkan.vkCmdBindIndexBuffer
          commandBuffer
          buff
          0 -- no offset
          ixType

-------------------------------------------------------------------------------------------------
-- Auxiliary classes defined for folds and traversals with generic-lens.

foldrC :: forall c b s. HasConstraints' c s
       => ( forall a. c a => a -> b -> b ) -> s -> b -> b
foldrC f s = coerce foldF
    where
      foldF :: Const (Endo b) s
      foldF = constraints' @c ( \ a -> Const ( Endo ( f a ) ) ) s

-- Constraints used with a generic fold to obtain the resource types and flags.
class KnownResourceType (res :: ResourceType t) where
  descriptorType :: Vulkan.VkDescriptorType
instance KnownResourceType (Buffer Uniform a) where
  descriptorType = Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
instance KnownResourceType (Image Store) where
  descriptorType = Vulkan.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
instance KnownResourceType (Image Sample) where
  descriptorType = Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER

class HasDescriptorTypeAndFlags a where
  descriptorTypeAndFlags :: a -> Maybe ( Vulkan.VkDescriptorType, Vulkan.VkShaderStageFlags )
instance ( KnownResourceType res )
  => HasDescriptorTypeAndFlags ( Resource res (DescriptorUse n) Named )
  where
  descriptorTypeAndFlags (StageFlags flags) = Just ( descriptorType @_ @res , flags )
instance HasDescriptorTypeAndFlags ( Resource res InputUse Named ) where
  descriptorTypeAndFlags _ = Nothing

-- Constraint used to initialise a set of resources.
class CanInitialiseResource a b | a -> b where
  initialiseResource
    :: MonadManaged m
    => Vulkan.VkPhysicalDevice
    -> Vulkan.VkDevice
    -> a
    -> m b

instance Poke a Base =>
  CanInitialiseResource
    ( Resource (Buffer Index a) ( InputUse :: ResourceUsage ix ) (Specified PreInitialised) )
    ( Resource (Buffer Index a) ( InputUse :: ResourceUsage ix ) (Specified Initialised   ) )
  where
  initialiseResource physicalDevice device ( IndexBuffer bufferData ) = do
    ( buf, _, nbElems ) <- createIndexBuffer physicalDevice device bufferData
    pure (InputBuffer buf nbElems)
instance Poke a Extended =>
  CanInitialiseResource
    ( Resource (Buffer Uniform a) ( DescriptorUse Single :: ResourceUsage ix ) (Specified PreInitialised) )
    ( Resource (Buffer Uniform a) ( DescriptorUse Single :: ResourceUsage ix ) (Specified Initialised   ) )
  where
  initialiseResource physicalDevice device ( UniformBuffer bufferData ) = do
    uncurry BufferResource <$> createUniformBuffer physicalDevice device bufferData
instance Poke a Locations =>
  CanInitialiseResource
    ( Resource (Buffer Vertex a) ( InputUse :: ResourceUsage ix ) (Specified PreInitialised) )
    ( Resource (Buffer Vertex a) ( InputUse :: ResourceUsage ix ) (Specified Initialised   ) )
  where
  initialiseResource physicalDevice device ( VertexBuffer bufferData ) = do
    ( buf, _, nbElems ) <- createVertexBuffer physicalDevice device bufferData
    pure (InputBuffer buf nbElems)

instance
  CanInitialiseResource
    ( Resource ( Image Store ) ( DescriptorUse Single :: ResourceUsage ix ) ( Specified PreInitialised ) )
    ( Resource ( Image Store ) ( DescriptorUse Single :: ResourceUsage ix ) ( Specified Initialised    ) )
  where
  initialiseResource _ _ ( StorageImage imgView ) = pure ( StorageImage imgView )

instance
  CanInitialiseResource
    ( Resource ( Image Sample ) ( DescriptorUse Single :: ResourceUsage ix ) ( Specified PreInitialised ) )
    ( Resource ( Image Sample ) ( DescriptorUse Single :: ResourceUsage ix ) ( Specified Initialised    ) )
  where
  initialiseResource _ _ ( SampledImage sampler imgView ) = pure ( SampledImage sampler imgView )

instance
  ( CanInitialiseResource
      ( Resource res ( DescriptorUse Single :: ResourceUsage n ) ( Specified PreInitialised ) )
      ( Resource res ( DescriptorUse Single :: ResourceUsage n ) ( Specified Initialised    ) )
  ) => CanInitialiseResource
        ( Resource res ( DescriptorUse Multiple :: ResourceUsage n ) ( Specified PreInitialised ) )
        ( Resource res ( DescriptorUse Multiple :: ResourceUsage n ) ( Specified Initialised    ) )
  where
  initialiseResource physicalDevice device (Ixed ixData) =
    Ixed <$> traverse ( initialiseResource physicalDevice device ) ixData

data BindingInformation
  = BindIndexBuffer  Vulkan.VkBuffer Word32 Vulkan.VkIndexType
  | BindVertexBuffer Vulkan.VkBuffer

-- Constraint used to turn resource into updating information:
--    * update the descriptor set with descriptor write
--    * keep track of how to bind resources.
class CanUpdateResource n a where
  descriptorWrites :: Vulkan.VkDescriptorSet -> Finite n -> a -> [ Word32 -> Vulkan.VkWriteDescriptorSet ]
  bindingInformation :: a -> Maybe BindingInformation


instance CanUpdateResource n ( Resource (Buffer Vertex a) InputUse ( Specified Initialised ) ) where
  descriptorWrites _ _ _ = []
  bindingInformation InputBuffer { inputBufferObject }
    = Just (BindVertexBuffer inputBufferObject)
instance ValidIndexingType a => CanUpdateResource n ( Resource (Buffer Index a) InputUse ( Specified Initialised ) ) where
  descriptorWrites _ _ _ = []
  bindingInformation InputBuffer { inputBufferObject, nbBufferElems }
    = Just (BindIndexBuffer inputBufferObject (fromIntegral nbBufferElems) ( indexType @a ) )

class ValidIndexingType (i :: Type) where
  indexType :: Vulkan.VkIndexType
instance ValidIndexingType Word32 where
  indexType = Vulkan.VK_INDEX_TYPE_UINT32
instance ValidIndexingType Word16 where
  indexType = Vulkan.VK_INDEX_TYPE_UINT16
{-
instance ValidIndexingType Word8 where
  indexType = Vulkan.VK_INDEX_TYPE_UINT8_EXT
-}

instance
  KnownResourceType (Buffer bufType a)
  => CanUpdateResource n
      ( Resource (Buffer bufType a) ( DescriptorUse Single :: ResourceUsage n ) (Specified Initialised) )
 where
  descriptorWrites descriptorSet _ ( BufferResource buffer _ ) = (:[]) $ \bindingNumber ->
    mkDescriptorWrite descriptorSet ( descriptorType @_ @(Buffer bufType a) ) bindingNumber [bufferDesc] setImageInfo
      where
        setImageInfo :: Vulkan.CreateVkStruct Vulkan.VkWriteDescriptorSet '["pImageInfo"] ()
        setImageInfo = Vulkan.set @"pImageInfo" Vulkan.VK_NULL
        bufferDesc :: Vulkan.VkDescriptorBufferInfo
        bufferDesc =
          Vulkan.createVk
            (  Vulkan.set @"buffer" buffer
            &* Vulkan.set @"offset" 0
            &* Vulkan.set @"range" ( fromIntegral Vulkan.VK_WHOLE_SIZE )
            )
  bindingInformation _ = Nothing


instance
  CanUpdateResource n
      ( Resource ( Image Store ) ( DescriptorUse Single :: ResourceUsage n ) (Specified Initialised) )
  where
  descriptorWrites descriptorSet _ ( StorageImage imgView ) = (:[]) $ \bindingNumber ->
    mkDescriptorWrite descriptorSet ( descriptorType @_ @(Image Store) ) bindingNumber [] setImageInfo
      where
        imageInfo =
          Vulkan.createVk
            (  Vulkan.set @"sampler"     Vulkan.VK_NULL
            &* Vulkan.set @"imageView"   imgView
            &* Vulkan.set @"imageLayout" Vulkan.VK_IMAGE_LAYOUT_GENERAL
            )
        setImageInfo :: Vulkan.CreateVkStruct Vulkan.VkWriteDescriptorSet '["pImageInfo"] ()
        setImageInfo = Vulkan.setVkRef @"pImageInfo" imageInfo
  bindingInformation _ = Nothing

instance
  CanUpdateResource n
      ( Resource ( Image Sample ) ( DescriptorUse Single :: ResourceUsage n ) (Specified Initialised) )
  where
  descriptorWrites descriptorSet _ ( SampledImage sampler imgView ) = (:[]) $ \bindingNumber ->
    mkDescriptorWrite descriptorSet ( descriptorType @_ @(Image Sample) ) bindingNumber [] setImageInfo
      where
        imageInfo =
          Vulkan.createVk
            (  Vulkan.set @"sampler"     sampler
            &* Vulkan.set @"imageView"   imgView
            &* Vulkan.set @"imageLayout" Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            )
        setImageInfo :: Vulkan.CreateVkStruct Vulkan.VkWriteDescriptorSet '["pImageInfo"] ()
        setImageInfo = Vulkan.setVkRef @"pImageInfo" imageInfo
  bindingInformation _ = Nothing

instance
     ( CanUpdateResource n
        ( Resource res ( DescriptorUse Single :: ResourceUsage n ) ( Specified Initialised ) )
     )
  => CanUpdateResource n
        ( Resource res ( DescriptorUse Multiple :: ResourceUsage n ) ( Specified Initialised) )
  where
  descriptorWrites descriptorSet i ( Ixed res ) =
    descriptorWrites descriptorSet i (res `V.index` i)
  bindingInformation _ = Nothing

mkDescriptorWrite
  :: Vulkan.VkDescriptorSet
  -> Vulkan.VkDescriptorType
  -> Word32
  -> [Vulkan.VkDescriptorBufferInfo]
  -> Vulkan.CreateVkStruct Vulkan.VkWriteDescriptorSet '["pImageInfo"] ()
  -> Vulkan.VkWriteDescriptorSet
mkDescriptorWrite descSet descType bindingNumber bufferInfo setImageInfo =
  Vulkan.createVk
    (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
    &* Vulkan.set @"pNext" Vulkan.VK_NULL
    &* Vulkan.set @"dstSet"             descSet
    &* Vulkan.set @"dstBinding"         bindingNumber
    &* Vulkan.set @"descriptorType"     descType
    &* Vulkan.set @"pTexelBufferView"   Vulkan.VK_NULL
    &* setImageInfo
    &* Vulkan.setListRef @"pBufferInfo" bufferInfo
    &* Vulkan.set @"descriptorCount"    1
    &* Vulkan.set @"dstArrayElement"    0
    )
