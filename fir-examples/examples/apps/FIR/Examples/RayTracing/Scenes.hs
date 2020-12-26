{-# LANGUAGE OverloadedStrings #-}

module FIR.Examples.RayTracing.Scenes
  ( scenes, chooseScene ) where

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

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
import FIR.Examples.RayTracing.Scenes.Furnace
  ( furnace )

--------------------------------------------------------------------------

scenes :: HashMap ShortText Scene
scenes = HashMap.fromList
  [ ( "cornellBox", cornellBox )
  , ( "furnace"   , furnace    )
  ]

chooseScene :: IO Scene
chooseScene = do
  putStrLn "Choose a scene."
  putStrLn 
    ( "Available scenes:\n" <> show ( HashMap.keys scenes ) )
  go
  
    where
      go :: IO Scene
      go = do
        name <- getLine
        case HashMap.lookup ( ShortText.pack name ) scenes of
          Nothing -> do
            putStrLn ( "Could not find a scene named " <> name <> ".")
            putStrLn ( "Available scenes are:\n" <> show ( HashMap.keys scenes ) )
            go
          Just scene -> pure scene
