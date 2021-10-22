{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module FIR.Examples.Reload
  ( shaderReloadWatcher
  , DynResources(..), readDynResources
  )
  where

-- base
import Control.Concurrent
  ( forkIO, killThread )
import Control.Monad
  ( when, void )
import Data.Foldable
  ( toList, for_, traverse_ )
import Data.Traversable
  ( for )

-- containers
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( toList, fromList, singleton, insert, member, null )

-- filepath
import System.FilePath
  ( takeFileName, equalFilePath )

-- fsnotify
import qualified System.FSNotify as FSNotify
  ( watchDir, withManager )
import System.FSNotify
  ( eventPath )

-- logging-effect
import Control.Monad.Log
  ( logDebug, logInfo )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, allocate, release )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( intercalate, pack )

-- stm
import Control.Concurrent.STM.TVar
  ( TVar, readTVar, writeTVar, newTVarIO )
import Control.Concurrent.STM.TMVar
  ( TMVar, takeTMVar, tryTakeTMVar, putTMVar
  , newEmptyTMVarIO
  )
import Control.Monad.STM
  ( atomically )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Control.Monad.Trans.Class
  ( lift )
import Control.Monad.Trans.State.Strict
  ( runStateT, modify )

-- unliftio-core
import Control.Monad.IO.Unlift
  ( MonadUnliftIO, askRunInIO )

-- vulkan
import qualified Vulkan

-- fir-examples
import FIR.Examples.Paths
  ( shaderDir )
import Vulkan.Monad
import Vulkan.Pipeline

----------------------------------------------------------------------------

-- | Reads a 'DynResources' 'TVar' and ensures the clean-up action is run exactly once.
--
-- The returned 'Bool' indicates whether the resources are new.
readDynResources :: MonadIO m => TVar (DynResources m r) -> m (r, Bool)
readDynResources tvar = do
  ( res, cleanup ) <- liftIO $ atomically do
    DynResources
      { currentResources = x
      , cleanupPrevious  = cleanup
      , resourcesAreNew  = new
      } <- readTVar tvar
    writeTVar tvar $
      DynResources
        { currentResources = x
        , cleanupPrevious  = pure ()
        , resourcesAreNew  = False
        }
    pure ( (x, new), cleanup )
  cleanup
  pure res

newDynResources
  :: ( MonadIO m, Applicative t )
  => r -> m ( TVar ( DynResources t r ) )
newDynResources r
  = liftIO $ newTVarIO dynRes
  where
    dynRes =
      DynResources
        { currentResources = r
        , cleanupPrevious  = pure ()
        , resourcesAreNew  = False
        }

data DynResources t r
  = DynResources
    { currentResources :: r
    , cleanupPrevious  :: t ()
    -- | A flag to indicate whether the resources are new,
    -- to be reset to 'False' once we start using them.
    , resourcesAreNew  :: Bool
    }

shaderReloadWatcher
  :: forall t l r
  . ( Traversable t, MonadVulkan l, MonadUnliftIO l )
  => Vulkan.Device
  -> t ( FilePath, (ReleaseKey, Vulkan.ShaderModule) )
  -> ( t Vulkan.ShaderModule -> l ( l (), r ) )
  -> l ( TVar (DynResources l ( l (), r )) )
shaderReloadWatcher device shaders createFromShaders = do
  logDebug "Starting shader reload watcher."
  originalResources  <- createFromShaders $ fmap ( snd . snd ) shaders
  resourcesTVar      <- newDynResources originalResources
  modifiedFilesTMVar <- liftIO $ newEmptyTMVarIO
  signalStop         <- liftIO $ newEmptyTMVarIO
  let
    reloader :: l ()
    reloader = resourceReloader device shaders createFromShaders modifiedFilesTMVar resourcesTVar
    shaderNames :: Set FilePath
    shaderNames = Set.fromList $ toList ( fmap ( takeFileName . fst ) shaders )
  liftIO $ startWatchOver signalStop shaderDir shaderNames modifiedFilesTMVar
  runInIO <- askRunInIO
  void $ allocate
    ( forkIO ( runInIO reloader ) )
    ( \ reloaderThreadId -> do
        -- Signal file watcher to stop.
        atomically $ putTMVar signalStop ()
        -- End resource reloader thread.
        killThread reloaderThreadId
        runInIO $ logDebug "Ended shader reload watcher."
    )
  pure resourcesTVar

resourceReloader
  :: forall t l r
  .  ( Traversable t, MonadVulkan l )
  => Vulkan.Device
  -> t ( FilePath, (ReleaseKey, Vulkan.ShaderModule) )
  -> ( t Vulkan.ShaderModule -> l ( l (), r ) )
  -> TMVar (Set FilePath)
  -> TVar (DynResources l ( l (), r ) )
  -> l ()
resourceReloader device shaders createFromShaders modifiedFilesTMVar resourcesTVar
  = outerLoop shaders
    where
      -- Outer loop: wait for a file to have been modified.
      outerLoop
        :: t ( FilePath, (ReleaseKey, Vulkan.ShaderModule) )
        -> l ()
      outerLoop oldShaders = do
        modifiedPaths <- liftIO . atomically $ takeTMVar modifiedFilesTMVar
        if Set.null modifiedPaths
        then outerLoop oldShaders
        else do
            let
              modifiedNames :: ShortText
              modifiedNames
                = ShortText.intercalate ", "
                . map ( ShortText.pack . takeFileName )
                . Set.toList
                $ modifiedPaths
              message :: ShortText
              message
                | length modifiedPaths == 1 = "Detected modified shader: "
                | otherwise                 = "Detected modified shaders: "
            logInfo ( message <> modifiedNames )
            innerLoop oldShaders modifiedPaths

      -- Inner loop: load new shaders and use them to create new resources.
      innerLoop
        :: t ( FilePath, (ReleaseKey, Vulkan.ShaderModule) )
        -> Set FilePath
        -> l ()
      innerLoop oldShaders modifiedPaths = do

        ( newShaders, oldShaderKeys )
          <- loadNewShaders device oldShaders modifiedPaths

        ( releaseNewResources, newResources )
          <- createFromShaders ( fmap ( snd . snd ) newShaders )

        newModifiedPaths <- liftIO . atomically $ tryTakeTMVar modifiedFilesTMVar
        case newModifiedPaths of

          Just moreModifiedPaths | not (Set.null moreModifiedPaths) -> do
            -- New files have been modified:
            --   * discard the resources we had created (out of date),
            --   * discard the shaders we had created that are now invalidated
            --     (keeping the still valid new ones),
            --   * go back and try again.
            releaseNewResources
            for_ newShaders \ ( path, ( key, _ ) ) ->
              when ( path `Set.member` modifiedPaths && path `Set.member` moreModifiedPaths )
                ( release key )
            logDebug ( "New resources already out of date, trying again." )
            innerLoop newShaders moreModifiedPaths

          _ -> do
            -- We are ready to use these new resources: all shaders up to date.
            -- As we don't own the resources currently in use (they are from the main thread),
            -- we return an action which performs cleanup of current resources before returning the new resources.
            liftIO $ atomically do
              DynResources
                { currentResources = (releaseCurrentResources, _)
                , cleanupPrevious  = releaseOldResources }
                <- readTVar resourcesTVar
              let
                releasePreviousResources :: l ()
                releasePreviousResources = do
                  releaseOldResources
                  releaseCurrentResources
                  traverse_ release oldShaderKeys
              writeTVar resourcesTVar $
                DynResources
                  { currentResources = (releaseNewResources, newResources)
                  , cleanupPrevious  = releasePreviousResources
                  , resourcesAreNew  = True
                  }
            logDebug ( "New resources ready to be used." )
            outerLoop newShaders

loadNewShaders
  :: ( MonadVulkan l, Traversable t )
  => Vulkan.Device
  -> t ( FilePath, (ReleaseKey, Vulkan.ShaderModule) )
  -> Set FilePath
  -> l ( t ( FilePath, (ReleaseKey, Vulkan.ShaderModule) ), [ ReleaseKey ] )
loadNewShaders device shaders modifiedPaths =
  ( `runStateT` [] ) $ for shaders \ oldShader@( path, ( oldKey, _ ) ) ->
    if any ( equalFilePath path ) modifiedPaths
    then do
      ( newKey, newModule ) <- lift $ loadShader device path
      modify ( oldKey : )
      pure ( path, ( newKey, newModule ) )
    else
      pure oldShader

startWatchOver :: TMVar () -> FilePath -> Set FilePath -> TMVar (Set FilePath) -> IO ()
startWatchOver signalStop dir watchNames modifiedPaths =
  void $ forkIO $
    FSNotify.withManager \ watchManager -> do
      stop <- FSNotify.watchDir watchManager dir
        ( ( `Set.member` watchNames ) . takeFileName . eventPath )
        ( \ ( eventPath -> path ) ->
          atomically do
            modified <- tryTakeTMVar modifiedPaths
            case modified of
              Nothing    -> putTMVar modifiedPaths ( Set.singleton path )
              Just paths -> putTMVar modifiedPaths ( Set.insert path paths )
        )
      atomically $ takeTMVar signalStop
      stop
