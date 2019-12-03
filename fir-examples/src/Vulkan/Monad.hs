{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Vulkan.Monad where

-- base
import Control.Monad
  ( (>=>) )
import qualified Foreign
import qualified Foreign.Marshal
import Foreign.Ptr
  ( Ptr )

-- logging-effect
import Control.Monad.Log
  ( MonadLog, LoggingT(..), runLoggingT
  , Severity(..), WithSeverity(..)
  )

-- mtl
import Control.Monad.State.Class
  ( MonadState )

-- resourcet
import Control.Monad.Trans.Resource
  ( MonadResource
  , ResourceT, runResourceT
  , ReleaseKey, allocate
  )

-- text
import Data.Text
  ( Text )
import qualified Data.Text.IO as Text
  ( putStrLn )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Control.Monad.Trans.State.Strict
  ( StateT(..), evalStateT )
import Control.Monad.Trans.Reader
  ( ReaderT(..) )

-- vulkan-api
import qualified Graphics.Vulkan          as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

----------------------------------------------------------------------------
-- Combination of effects needed.

type MonadVulkan m = ( MonadLog LogMessage m, MonadIO m, MonadResource m )

type LogMessage = WithSeverity Text
type Handler    = LogMessage -> ResourceT IO ()

newtype VulkanMonad s a =
  VulkanMonad
    { runVulkanMonad :: StateT s (LoggingT LogMessage (ResourceT IO)) a }

deriving newtype instance Functor             (VulkanMonad s)
deriving newtype instance Applicative         (VulkanMonad s)
deriving newtype instance Monad               (VulkanMonad s)
deriving newtype instance MonadFail           (VulkanMonad s)
deriving newtype instance MonadState s        (VulkanMonad s)
deriving newtype instance MonadLog LogMessage (VulkanMonad s)
deriving newtype instance MonadIO             (VulkanMonad s)
deriving via ( StateT s (ReaderT Handler (ResourceT IO) ) )
  instance MonadResource (VulkanMonad s)

runVulkan :: VulkanMonad s a -> s -> IO a
runVulkan m s
  = runResourceT
  . ( `runLoggingT` logHandler )
  . ( `evalStateT` s )
  . runVulkanMonad
  $ m

----------------------------------------------------------------------------
-- Logging.

logHandler :: MonadIO m => LogMessage -> m ()
logHandler ( WithSeverity sev mess )
  = liftIO $ Text.putStrLn ( showSeverity sev <> "  " <> mess )

showSeverity :: Severity -> Text
showSeverity Emergency     = "[EMERGENCY]"
showSeverity Alert         = "[ALERT]"
showSeverity Critical      = "[CRIT]"
showSeverity Error         = "[ERR]"
showSeverity Warning       = "[WARN]"
showSeverity Notice        = "(note)"
showSeverity Informational = "(info)"
showSeverity Debug         = "(debug)"

----------------------------------------------------------------------------
-- Resource management.

throwVkResult :: MonadFail m => Vulkan.VkResult -> m ()
throwVkResult Vulkan.VK_SUCCESS = pure ()
throwVkResult failure           = fail (show failure)

managedVulkanResource
  :: ( MonadVulkan m, Foreign.Storable x, Vulkan.VulkanPtr ptr )
  => ( ptr a -> Ptr x -> IO Vulkan.VkResult )
  -> ( x -> ptr a -> IO () )
  -> m x
managedVulkanResource create destroy = snd <$> allocateVulkanResource create destroy

allocateVulkanResource
  :: ( MonadVulkan m, Foreign.Storable x, Vulkan.VulkanPtr ptr )
  => ( ptr a -> Ptr x -> IO Vulkan.VkResult )
  -> ( x -> ptr a -> IO () )
  -> m ( ReleaseKey, x )
allocateVulkanResource create destroy =
  allocate
    ( allocaAndPeek ( create Vulkan.vkNullPtr >=> throwVkResult ) )
    ( `destroy` Vulkan.vkNullPtr )

allocaAndPeek
  :: ( Foreign.Storable a, MonadIO m )
  => ( Ptr a -> IO () )
  -> m a
allocaAndPeek f = liftIO $
  Foreign.Marshal.alloca
    ( \ptr -> f ptr *> Foreign.peek ptr )

allocaAndPeekArray
  :: Foreign.Storable a
  => Int
  -> ( Ptr a -> IO () )
  -> IO [ a ]
allocaAndPeekArray n f =
  Foreign.Marshal.allocaArray n
    ( \ptr -> f ptr *> Foreign.Marshal.peekArray n ptr )

fetchAll
  :: ( Foreign.Storable a, Foreign.Storable b, Integral b )
  => ( Ptr b -> Ptr a -> IO () )
  -> IO [a]
fetchAll f =
  Foreign.Marshal.alloca \nPtr -> do
    f nPtr Vulkan.vkNullPtr
    n <- fromIntegral <$> Foreign.peek nPtr
    allocaAndPeekArray n ( f nPtr )
