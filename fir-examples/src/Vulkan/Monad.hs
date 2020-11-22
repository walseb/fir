{-# OPTIONS_GHC -fno-warn-orphans #-}

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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Vulkan.Monad where

-- base
import Control.Category
  ( (>>>) )

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
  )

-- short-text
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Control.Monad.Trans.State.Strict
  ( StateT(..), evalStateT )
import Control.Monad.Trans.Reader
  ( ReaderT(..) )

----------------------------------------------------------------------------
-- Combination of effects needed.

type MonadVulkan m = ( MonadLog LogMessage m, MonadIO m, MonadResource m )

type LogMessage = WithSeverity ShortText
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

deriving via (ReaderT Handler (ResourceT IO) )
  instance MonadResource (LoggingT LogMessage (ResourceT IO))

runVulkan :: s -> VulkanMonad s a -> IO a
runVulkan s
  =    runVulkanMonad
  >>> ( `evalStateT` s )
  >>> ( `runLoggingT` logHandler )
  >>> runResourceT

statelessly :: LoggingT LogMessage (ResourceT IO) a -> VulkanMonad s a
statelessly ma = VulkanMonad ( StateT \s -> ( , s ) <$> ma )

----------------------------------------------------------------------------
-- Logging.

logHandler :: MonadIO m => LogMessage -> m ()
logHandler ( WithSeverity sev mess )
  = liftIO . putStrLn . ShortText.unpack $ showSeverity sev <> " " <> mess

showSeverity :: Severity -> ShortText
showSeverity Emergency     = "! PANIC !"
showSeverity Alert         = "! ALERT !"
showSeverity Critical      = "! CRIT !"
showSeverity Error         = "[ERR]  "
showSeverity Warning       = "[WARN] "
showSeverity Notice        = "(note) "
showSeverity Informational = "(info) "
showSeverity Debug         = "(debug)"
