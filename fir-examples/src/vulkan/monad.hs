{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vulkan.Monad where

-- base
import Control.Exception
  ( bracket )
import Control.Monad
  ( (>=>) )
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import qualified Foreign
import qualified Foreign.Marshal

-- lens
import Control.Lens
  ( Lens', lens )

-- managed
import Control.Monad.Managed
  ( MonadManaged )
import qualified Control.Monad.Managed

-- vulkan-api
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

-- fir
import Math.Linear
  ( V )

-- fir-example
import Vulkan.Observer
  ( Observer(..), initialObserver
  , Input(..), nullInput
  , Quit(..)
  )

-----------------------------------------------------------------------------------------------------

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . putStrLn

throwVkResult :: MonadIO m => Vulkan.VkResult -> m ()
throwVkResult Vulkan.VK_SUCCESS = pure ()
throwVkResult res = fail ( show res )

mainLoop :: Monad m => m Quit -> m ()
mainLoop mb
  = do
      b <- mb
      case b of
        Quit -> pure ()
        _    -> mainLoop mb

----------------------------------------------------------------------------
-- state

data RenderState
  = RenderState
    { observer   :: Observer
    , input      :: Input
    }

initialState :: RenderState
initialState
  = RenderState
      { observer   = initialObserver
      , input      = nullInput
      }

_observer :: Lens' RenderState Observer
_observer = lens observer ( \s v -> s { observer = v } )

_position :: Lens' Observer (V 3 Float)
_position = lens position ( \s v -> s { position = v } )

_angles :: Lens' Observer (V 2 Float)
_angles = lens angles ( \s v -> s { angles = v } )

_input :: Lens' RenderState Input
_input = lens input ( \s v -> s { input = v } )

----------------------------------------------------------------------------
-- resource management

managed :: MonadManaged m => (forall r. (a -> IO r) -> IO r) -> m a
managed f = Control.Monad.Managed.using ( Control.Monad.Managed.managed f )

manageBracket :: MonadManaged m => IO a -> (a -> IO b) -> m a
manageBracket create destroy = managed ( bracket create destroy )

managedVulkanResource
  :: ( MonadManaged m, Foreign.Storable x, Vulkan.VulkanPtr ptr )
  => ( ptr a -> Vulkan.Ptr x -> IO Vulkan.VkResult )
  -> ( x -> ptr a -> IO () )
  -> m x
managedVulkanResource create destroy =
  manageBracket
    ( allocaAndPeek ( create Vulkan.vkNullPtr >=> throwVkResult ) )
    ( `destroy` Vulkan.vkNullPtr )

allocaAndPeek
  :: ( Foreign.Storable a, MonadIO m )
  => ( Vulkan.Ptr a -> IO () )
  -> m a
allocaAndPeek f = liftIO $
  Foreign.Marshal.alloca
    ( \ptr -> f ptr *> Foreign.peek ptr )

allocaAndPeekArray
  :: Foreign.Storable a
  => Int
  -> ( Vulkan.Ptr a -> IO () )
  -> IO [ a ]
allocaAndPeekArray n f =
  Foreign.Marshal.allocaArray n
    ( \ptr -> f ptr *> Foreign.Marshal.peekArray n ptr )

fetchAll
  :: ( Foreign.Storable a, Foreign.Storable b, Integral b )
  => ( Vulkan.Ptr b -> Vulkan.Ptr a -> IO () )
  -> IO [a]
fetchAll f =
  Foreign.Marshal.alloca \nPtr -> do
    f nPtr Vulkan.vkNullPtr
    n <- fromIntegral <$> Foreign.peek nPtr
    allocaAndPeekArray n ( f nPtr )
