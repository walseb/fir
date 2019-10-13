{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

module Folders where

-- base
import Control.Monad
  ( forM )

-- filepath
import System.FilePath
  ( (</>) )

-- fir-tests
import Test
  ( Test, TestOutput(..)
  , runTest
  )

data Folder a = Folder String [a]
  deriving stock (Functor, Foldable, Traversable)

------------------------------------------------------------------------------------------
-- running tests on folders

runTestsP :: (String -> String -> Test -> Bool)
          -> [ Folder (String, Test) ]
          -> IO [ Folder (String, Test, TestOutput) ]
runTestsP f
  = runTests
  . map ( \ (Folder g tests) -> Folder g (filter ( \(p,t) -> f g p t ) tests) )

runTests :: [ Folder (String, Test) ]
         -> IO [ Folder (String, Test, TestOutput) ]
runTests
  = traverse
     ( \ (Folder folderName folderTests) ->
          Folder folderName <$>
            forM folderTests \ (testName, testType) ->
              (testName, testType, )
                <$> runTest testType ( folderName </> testName )
     )

runTestsFromFolder
  :: String
  -> [ Folder (String, Test) ]
  -> IO [ Folder (String, Test, TestOutput) ]
runTestsFromFolder folderName = runTestsP ( \ x _ _ -> x == folderName )


------------------------------------------------------------------------------------------
-- displaying test result folders

deriving stock instance Show (Folder (String, Test))

instance Show (Folder (String, Test, TestOutput)) where
  show ( Folder folderName testResults )
    =  padRToLength 15 folderName
    <> "( " <> showPaddedL 2 passes <> " / " <> showPaddedL 2 total <> " )"
    <> foldMap showTestResult testResults
    <> "\n"
      where
        total, passes :: Int
        total  = length testResults
        passes = length . filter ( \(_,_,res) -> res == Success ) $ testResults
        showTestResult :: (String, Test, TestOutput) -> String
        showTestResult (testName, testType, testResult)
          =  "\n    " <> padRToLength 30 testName
          <> padRToLength 9 (show testType) <> ": " <> show testResult

instance {-# OVERLAPPING #-} Show [Folder (String, Test, TestOutput)] where
  show = unlines . map show


-- utility padding functions

padRToLength :: Int -> String  -> String
padRToLength n s
  | lg >= n   = s
  | otherwise = s <> replicate (n - lg) ' '
  where lg = length s

padLToLength :: Int -> String  -> String
padLToLength n s
  | lg >= n   = s
  | otherwise = replicate (n - lg) ' ' <> s
  where lg = length s

showPaddedR :: Show a => Int -> a  -> String
showPaddedR n = padRToLength n . show

showPaddedL :: Show a => Int -> a  -> String
showPaddedL n = padLToLength n . show
