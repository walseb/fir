{-# LANGUAGE TemplateHaskell #-}

module FIR.Examples.Paths
  ( shaderDir, screenshotDir, assetDir )
  where

-- fir-examples
import FIR.Examples.Paths.CreateDirs
  ( createDataSubDir )

------------------------------------------------

shaderDir :: FilePath
shaderDir = $( createDataSubDir "shaders" )

screenshotDir :: FilePath
screenshotDir = $( createDataSubDir "screenshots" )

assetDir :: FilePath
assetDir = $( createDataSubDir "assets" )
