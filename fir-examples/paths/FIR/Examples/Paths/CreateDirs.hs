{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module FIR.Examples.Paths.CreateDirs where

-- base
import Language.Haskell.TH
  ( Exp(LitE), Lit(StringL), Q )
import Language.Haskell.TH.Syntax
  ( runIO )

-- directory
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing )

-- filepath
import System.FilePath
  ( (</>) )

-- fir-examples (auto-generated cabal paths module)
import Paths_fir_examples
  ( getDataDir )

------------------------------------------------

createSubDir :: IO FilePath -> FilePath -> Q Exp
createSubDir io_dir subdir = runIO do
  dir <- io_dir
  let fulldir = dir </> subdir
  createDirectoryIfMissing True fulldir
  pure ( LitE ( StringL fulldir ) )

createDataSubDir :: FilePath -> Q Exp
createDataSubDir = createSubDir ( canonicalizePath =<< getDataDir )
