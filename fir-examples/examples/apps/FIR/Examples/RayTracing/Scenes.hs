{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module FIR.Examples.RayTracing.Scenes
  ( scenes, chooseScene ) where

-- base
import Data.Maybe
  ( fromMaybe, listToMaybe )
import System.Environment
  ( getArgs )

-- haskeline
import qualified System.Console.Haskeline as Haskeline

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( dropAround, isPrefixOf, pack, unpack )

-- unordered-containers
import Data.HashMap.Strict
  ( HashMap )
import qualified Data.HashMap.Strict as HashMap
  ( fromList, keys, lookup )

-- fir-examples
import FIR.Examples.RayTracing.Scene
  ( Scene )
import FIR.Examples.RayTracing.Scenes.CornellBox
  ( cornellBox )
import FIR.Examples.RayTracing.Scenes.CornellBox2
  ( cornellBox2 )
import FIR.Examples.RayTracing.Scenes.Furnace
  ( furnace )

--------------------------------------------------------------------------

scenes :: HashMap ShortText Scene
scenes = HashMap.fromList
  [ ( "cornellBox" , cornellBox  )
  , ( "cornellBox2", cornellBox2 )
  , ( "furnace"    , furnace     )
  ]

chooseScene :: IO Scene
chooseScene = do
  args <- getArgs
  Haskeline.runInputT haskelineSettings do
    inTerminal <- Haskeline.haveTerminalUI
    if inTerminal
    then do
      Haskeline.outputStrLn "Choose a scene."
      Haskeline.outputStrLn
        ( "Available scenes:\n" <> show ( HashMap.keys scenes ) )
      queryUserForScene
    else pure $
      fromMaybe cornellBox -- default scene
        ( ( `HashMap.lookup` scenes ) . ShortText.dropAround ( == ' ' ) . ShortText.pack =<< listToMaybe args )

  where
    queryUserForScene :: Haskeline.InputT IO Scene
    queryUserForScene = do
      mbLine <- Haskeline.getInputLine "> "
      case ( `HashMap.lookup` scenes ) . ShortText.dropAround ( == ' ' ) . ShortText.pack =<< mbLine of
        Nothing -> do
          Haskeline.outputStrLn "Could not parse scene name."
          Haskeline.outputStrLn ( "Available scenes are:\n" <> show ( HashMap.keys scenes ) )
          queryUserForScene
        Just scene -> pure scene

haskelineSettings :: Haskeline.Settings IO
haskelineSettings = Haskeline.Settings
  { Haskeline.complete       = Haskeline.completeWord Nothing " \t" ( pure . completeSceneName )
  , Haskeline.historyFile    = Nothing
  , Haskeline.autoAddHistory = False
  }

completeSceneName :: String -> [ Haskeline.Completion ]
completeSceneName str
  = map ( Haskeline.simpleCompletion . ShortText.unpack )
  . filter ( ShortText.pack str `ShortText.isPrefixOf` )
  $ HashMap.keys scenes
