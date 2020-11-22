{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
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
  ( fromJust, listToMaybe, maybeToList )
import Data.Monoid
  ( Endo(Endo) )
import Data.Word
  ( Word16, Word32 )
import GHC.Exts
  ( proxy# )
import GHC.TypeNats
  ( Nat, KnownNat, natVal' )

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

-- logging-effect
import Control.Monad.Log
  ( logDebug )

-- resourcet
import Control.Monad.Trans.Resource
  ( allocate )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )
import Control.Monad.Trans.Class
  ( lift )
import Control.Monad.Trans.State.Strict
  ( evalStateT, get, put )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( empty, fromList, imap, replicate, toList )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( Vector, fromList, index, ifoldl )

-- vulkan-api
import qualified Vulkan
import qualified Vulkan.CStruct.Extends as Vulkan
  ( SomeStruct(SomeStruct) )

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

data ResourceType where
  Buffer :: BufferType -> Type -> ResourceType
  Image  :: ImageType -> ResourceType

data family Resource ( res :: ResourceType ) ( use :: ResourceUsage n ) ( st :: ResourceInfo )

data instance Resource res (DescriptorUse n) Named where
  StageFlags :: KnownResourceType res => Vulkan.ShaderStageFlags -> Resource res (DescriptorUse n) Named
data instance Resource res InputUse Named = InputResource

data instance Resource (Buffer Vertex  a) InputUse               (Specified PreInitialised) = VertexBuffer  [a]
data instance Resource (Buffer Index   a) InputUse               (Specified PreInitialised) = IndexBuffer   [a]
data instance Resource (Buffer Uniform a) (DescriptorUse Single) (Specified PreInitialised) = UniformBuffer  a

data instance Resource (Buffer bufType a) (DescriptorUse Single) (Specified Initialised)
  = BufferResource
    { bufferObject :: Vulkan.Buffer
    , pokeBuffer   :: a -> IO ()
    }
data instance Resource (Buffer bufType a) InputUse (Specified Initialised)
  = InputBuffer
    { inputBufferObject  :: Vulkan.Buffer
    , pokeInputBufferOff :: Int -> [a] -> IO ()
    , nbBufferElems      :: Int
    }

data instance Resource ( Image Store  ) (DescriptorUse Single) (Specified st) = StorageImage Vulkan.ImageView
data instance Resource ( Image Sample ) (DescriptorUse Single) (Specified st) = SampledImage Vulkan.Sampler Vulkan.ImageView

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

data PostInitialisationResult resources n
  = PostInitialisationResult
      { resourceLayout       :: Vulkan.DescriptorSetLayout
      , resourceDescriptors  :: V.Vector n Vulkan.DescriptorSet
      , bindBuffersCommand   :: forall v. MonadVulkan v => Vulkan.CommandBuffer -> v ()
      , initialisedResources :: resources n Post
      }

initialiseResources
  :: forall
      ( n  :: Nat )
      ( m  :: Type -> Type )
      ( resources :: Nat -> ResourceInfo -> Type )
  . ( KnownNat n
    , MonadVulkan m
    , HasConstraints' HasDescriptorTypeAndFlags ( resources n Named )
    , HasConstraints CanInitialiseResource
         ( resources n Pre  )
         ( resources n Post )
    , HasConstraints' ( CanUpdateResource n ) ( resources n Post )
    )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> resources n Named
  -> resources n Pre
  -> m (PostInitialisationResult resources n)
initialiseResources physicalDevice device resourceFlags resourcesPre = do
  let
    descriptorTypesAndFlags :: [ (Vulkan.DescriptorType, Vulkan.ShaderStageFlags) ]
    descriptorTypesAndFlags =
      foldrC @HasDescriptorTypeAndFlags
        ( \ res flags -> case descriptorTypeAndFlags res of
          Just flag -> flag : flags
          Nothing   -> flags
        )
        resourceFlags
        []

    descriptorTypes :: [ Vulkan.DescriptorType ]
    descriptorTypes = map fst descriptorTypesAndFlags
    n :: Int
    n = fromIntegral ( natVal' @n proxy# )
    nb :: ShortText
    nb = ShortText.pack ( show n )

  descriptorPool      <-
    logDebug ( "Creating descriptor pool for " <> nb <> " sets of descriptors, each with types:\n" <> ShortText.pack ( show descriptorTypes ) )
      *> createDescriptorPool device n descriptorTypes
  descriptorSetLayout <-
    logDebug "Creating descriptor set layout"
      *> createDescriptorSetLayout device descriptorTypesAndFlags
  descriptorSets      <-
    logDebug ( "Allocating " <> nb <> " descriptor sets" )
      *> allocateDescriptorSets @n device descriptorPool descriptorSetLayout

  ( resourcesPost :: resources n Post )
     <- logDebug "Initialising resources" *>
        ( constraints @CanInitialiseResource
            ( initialiseResource physicalDevice device )
        ) resourcesPre

  ( descriptors :: V.Vector n Vulkan.DescriptorSet ) <-
    ( logDebug "Updating descriptor sets" *> ) . ( `evalStateT` descriptorSets ) $ do
        ( theseDescriptorSets, nextDescriptorSets ) <- splitAt n <$> get
        put nextDescriptorSets
        let
          descriptors :: V.Vector n Vulkan.DescriptorSet
          descriptors = fromJust $ V.fromList theseDescriptorSets
        lift ( updateDescriptorSets device descriptors resourcesPost )
        pure descriptors

  let
    indexBuffers  :: [ ( Vulkan.Buffer, Vulkan.IndexType ) ]
    vertexBuffers :: [ Vulkan.Buffer ]
    (indexBuffers, vertexBuffers) =
      foldrC @(CanUpdateResource n)
        ( \ res ( indexBinds, vertexBinds) -> case bindingInformation @n res of
          Just (BindIndexBuffer  buf _ ty) -> ( ( buf, ty ) : indexBinds, vertexBinds )
          Just (BindVertexBuffer buf     ) -> ( indexBinds, buf : vertexBinds )
          Nothing                          -> ( indexBinds, vertexBinds)
        )
        resourcesPost
        ( [], [] )

    bindingCommand :: forall v. MonadVulkan v => Vulkan.CommandBuffer -> v ()
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
  :: MonadVulkan m
  => Vulkan.Device
  -> [ ( Vulkan.DescriptorType, Vulkan.ShaderStageFlags ) ]
  -> m Vulkan.DescriptorSetLayout
createDescriptorSetLayout device descriptorTypes = snd <$> Vulkan.withDescriptorSetLayout device createInfo Nothing allocate

      where
        bindings :: Boxed.Vector Vulkan.DescriptorSetLayoutBinding
        bindings = ( `Boxed.Vector.imap` ( Boxed.Vector.fromList descriptorTypes ) ) \ i ( descType, descStageFlags ) ->
          Vulkan.DescriptorSetLayoutBinding
            { Vulkan.binding           = fromIntegral i
            , Vulkan.descriptorType    = descType
            , Vulkan.descriptorCount   = 1
            , Vulkan.stageFlags        = descStageFlags
            , Vulkan.immutableSamplers = Boxed.Vector.empty
            }
        createInfo :: Vulkan.DescriptorSetLayoutCreateInfo '[]
        createInfo =
          Vulkan.DescriptorSetLayoutCreateInfo
            { Vulkan.next     = ()
            , Vulkan.flags    = Vulkan.zero
            , Vulkan.bindings = bindings
            }

createDescriptorPool
  :: MonadVulkan m
  => Vulkan.Device
  -> Int
  -> [ Vulkan.DescriptorType ]
  -> m Vulkan.DescriptorPool
createDescriptorPool device maxSets descTypes = snd <$> Vulkan.withDescriptorPool device createInfo Nothing allocate

    where
      poolSizes :: [ Vulkan.DescriptorPoolSize ]
      poolSizes =
        counts descTypes <&> \ ( descType, descCount ) ->
          Vulkan.DescriptorPoolSize
          { Vulkan.type'           = descType
          , Vulkan.descriptorCount = fromIntegral maxSets * descCount
          }
      createInfo :: Vulkan.DescriptorPoolCreateInfo '[]
      createInfo =
        Vulkan.DescriptorPoolCreateInfo
          { Vulkan.next      = ()
          , Vulkan.flags     = Vulkan.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
          , Vulkan.poolSizes = Boxed.Vector.fromList poolSizes
          , Vulkan.maxSets   = fromIntegral maxSets
          }

allocateDescriptorSets
  :: forall n m
  .  ( KnownNat n, MonadVulkan m )
  => Vulkan.Device
  -> Vulkan.DescriptorPool
  -> Vulkan.DescriptorSetLayout
  -> m [ Vulkan.DescriptorSet ]
allocateDescriptorSets dev descriptorPool layout0 = Boxed.Vector.toList . snd <$> Vulkan.withDescriptorSets dev allocateInfo allocate
  where
    count :: Int
    count = fromIntegral ( natVal' @n proxy# )
    allocateInfo :: Vulkan.DescriptorSetAllocateInfo '[]
    allocateInfo =
      Vulkan.DescriptorSetAllocateInfo
        { Vulkan.next           = ()
        , Vulkan.descriptorPool = descriptorPool
        , Vulkan.setLayouts     = Boxed.Vector.replicate count layout0
        }

counts :: (Ord a, Num i) => [ a ] -> [ (a, i) ]
counts = Map.toList . foldr ( \ a -> Map.insertWith (+) a 1 ) Map.empty


updateDescriptorSets
  :: forall m n resources
  .  ( MonadVulkan m
     , HasConstraints' ( CanUpdateResource n ) ( resources n Post )
     )
  => Vulkan.Device
  -> V.Vector n Vulkan.DescriptorSet
  -> resources n Post
  -> m ()
updateDescriptorSets device descriptorSets resourceSet =
  Vulkan.updateDescriptorSets device
    writes
    Boxed.Vector.empty -- no descriptor copies
    where
      writes :: Boxed.Vector ( Vulkan.SomeStruct Vulkan.WriteDescriptorSet )
      writes = Boxed.Vector.fromList $ V.ifoldl descriptorSetWrites [] descriptorSets
      descriptorSetWrites :: [ Vulkan.SomeStruct Vulkan.WriteDescriptorSet ] -> Finite n -> Vulkan.DescriptorSet -> [ Vulkan.SomeStruct Vulkan.WriteDescriptorSet ]
      descriptorSetWrites writesAcc i descriptorSet
        = writesAcc ++
        ( zipWith (&) [0..] $
          foldrC @(CanUpdateResource n)
            ( \ res descs -> descriptorWrites descriptorSet i res ++ descs )
            resourceSet
            []
        )

bufferBindingCommand
  :: MonadVulkan m
  => Maybe ( Vulkan.Buffer, Vulkan.IndexType )
  -> [ Vulkan.Buffer ]
  -> Vulkan.CommandBuffer
  -> m ()
bufferBindingCommand mbIndexBuffer vertexBuffers commandBuffer = do

  Vulkan.cmdBindVertexBuffers
    commandBuffer
    0
    ( Boxed.Vector.fromList vertexBuffers )
    ( pure 0 )

  case mbIndexBuffer of
    Nothing               -> pure ()
    Just ( buff, ixType ) ->
      liftIO $
        Vulkan.cmdBindIndexBuffer
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
class KnownResourceType (res :: ResourceType) where
  descriptorType :: Vulkan.DescriptorType
instance KnownResourceType (Buffer Uniform a) where
  descriptorType = Vulkan.DESCRIPTOR_TYPE_UNIFORM_BUFFER
instance KnownResourceType (Image Store) where
  descriptorType = Vulkan.DESCRIPTOR_TYPE_STORAGE_IMAGE
instance KnownResourceType (Image Sample) where
  descriptorType = Vulkan.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER

class HasDescriptorTypeAndFlags a where
  descriptorTypeAndFlags :: a -> Maybe ( Vulkan.DescriptorType, Vulkan.ShaderStageFlags )
instance ( KnownResourceType res )
  => HasDescriptorTypeAndFlags ( Resource res (DescriptorUse n) Named )
  where
  descriptorTypeAndFlags (StageFlags flags) = Just ( descriptorType @res , flags )
instance HasDescriptorTypeAndFlags ( Resource res InputUse Named ) where
  descriptorTypeAndFlags _ = Nothing

-- Constraint used to initialise a set of resources.
class CanInitialiseResource a b | a -> b where
  initialiseResource
    :: MonadVulkan m
    => Vulkan.PhysicalDevice
    -> Vulkan.Device
    -> a
    -> m b

instance Poke a Base =>
  CanInitialiseResource
    ( Resource (Buffer Index a) ( InputUse :: ResourceUsage ix ) (Specified PreInitialised) )
    ( Resource (Buffer Index a) ( InputUse :: ResourceUsage ix ) (Specified Initialised   ) )
  where
  initialiseResource physicalDevice device ( IndexBuffer bufferData ) = do
    ( buf, pokeFn, nbElems ) <- createIndexBuffer physicalDevice device bufferData
    pure (InputBuffer buf pokeFn nbElems)
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
    ( buf, pokeFn, nbElems ) <- createVertexBuffer physicalDevice device bufferData
    pure (InputBuffer buf pokeFn nbElems)

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
  = BindIndexBuffer  Vulkan.Buffer Word32 Vulkan.IndexType
  | BindVertexBuffer Vulkan.Buffer

-- Constraint used to turn resource into updating information:
--    * update the descriptor set with descriptor write
--    * keep track of how to bind resources.
class CanUpdateResource n a where
  descriptorWrites :: Vulkan.DescriptorSet -> Finite n -> a -> [ Word32 -> Vulkan.SomeStruct Vulkan.WriteDescriptorSet ]
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
  indexType :: Vulkan.IndexType
instance ValidIndexingType Word32 where
  indexType = Vulkan.INDEX_TYPE_UINT32
instance ValidIndexingType Word16 where
  indexType = Vulkan.INDEX_TYPE_UINT16
{-
instance ValidIndexingType Word8 where
  indexType = Vulkan.INDEX_TYPE_UINT8_EXT
-}

instance
  KnownResourceType (Buffer bufType a)
  => CanUpdateResource n
      ( Resource (Buffer bufType a) ( DescriptorUse Single :: ResourceUsage n ) (Specified Initialised) )
 where
  descriptorWrites descriptorSet _ ( BufferResource buffer _ ) = (:[]) \ bindingNumber ->
    mkDescriptorWrite descriptorSet ( descriptorType @(Buffer bufType a) ) bindingNumber [bufferDesc] Nothing
      where
        bufferDesc :: Vulkan.DescriptorBufferInfo
        bufferDesc =
          Vulkan.DescriptorBufferInfo
            { Vulkan.buffer = buffer
            , Vulkan.offset = 0
            , Vulkan.range  = Vulkan.WHOLE_SIZE
            }
  bindingInformation _ = Nothing


instance
  CanUpdateResource n
      ( Resource ( Image Store ) ( DescriptorUse Single :: ResourceUsage n ) (Specified Initialised) )
  where
  descriptorWrites descriptorSet _ ( StorageImage imgView ) = (:[]) $ \bindingNumber ->
    mkDescriptorWrite descriptorSet ( descriptorType @(Image Store) ) bindingNumber [] ( Just imageInfo )
      where
        imageInfo :: Vulkan.DescriptorImageInfo
        imageInfo =
          Vulkan.DescriptorImageInfo
            { Vulkan.sampler     = Vulkan.NULL_HANDLE
            , Vulkan.imageView   = imgView
            , Vulkan.imageLayout = Vulkan.IMAGE_LAYOUT_GENERAL
            }
  bindingInformation _ = Nothing

instance
  CanUpdateResource n
      ( Resource ( Image Sample ) ( DescriptorUse Single :: ResourceUsage n ) (Specified Initialised) )
  where
  descriptorWrites descriptorSet _ ( SampledImage sampler imgView ) = (:[]) $ \bindingNumber ->
    mkDescriptorWrite descriptorSet ( descriptorType @(Image Sample) ) bindingNumber [] ( Just imageInfo )
      where
        imageInfo :: Vulkan.DescriptorImageInfo
        imageInfo =
          Vulkan.DescriptorImageInfo
            { Vulkan.sampler     = sampler
            , Vulkan.imageView   = imgView
            , Vulkan.imageLayout = Vulkan.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            }
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
  :: Vulkan.DescriptorSet
  -> Vulkan.DescriptorType
  -> Word32
  -> [Vulkan.DescriptorBufferInfo]
  -> Maybe Vulkan.DescriptorImageInfo
  -> Vulkan.SomeStruct Vulkan.WriteDescriptorSet
mkDescriptorWrite descSet descType bindingNumber bufferInfos imageInfo =
  Vulkan.SomeStruct $
    Vulkan.WriteDescriptorSet
      { Vulkan.next             = ()
      , Vulkan.dstSet           = descSet
      , Vulkan.dstBinding       = bindingNumber
      , Vulkan.descriptorType   = descType
      , Vulkan.texelBufferView  = Boxed.Vector.empty
      , Vulkan.imageInfo        = Boxed.Vector.fromList ( maybeToList imageInfo )
      , Vulkan.bufferInfo       = Boxed.Vector.fromList bufferInfos
      , Vulkan.descriptorCount  = 1
      , Vulkan.dstArrayElement  = 0
      }
