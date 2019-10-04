module CabalTests ( tests ) where

-- cabal
import qualified Distribution.TestSuite as Cabal

-- filepath
import System.FilePath
  ( (</>) )

-- fir-tests
import Folders
  ( Folder(Folder) )
import Test
  ( Test, TestOutput(..), TestFailure(..)
  , runTest
  )
import Tests
  ( allTests )

--------------------------------------------------
-- package up tests for cabal

tests :: IO [ Cabal.Test ]
tests = pure ( map groupFolderTests allTests )

groupFolderTests :: Folder (String, Test) -> Cabal.Test
groupFolderTests ( Folder folderName folderTests )
  = Cabal.Group folderName True
  $ map ( \( testName, testType ) ->
          mkCabalTest folderName testName testType
        )
        folderTests

mkCabalTest :: String -> String -> Test -> Cabal.Test
mkCabalTest folderName testName testType
  = Cabal.Test
  $ Cabal.TestInstance
  { Cabal.run  = Cabal.Finished . cabalResult <$> runTest testType testPath
  , Cabal.name = testPath
  , Cabal.tags = [ folderName, show testType ]
  , Cabal.options   = []
  , Cabal.setOption = \ _ _ -> Left "Test does not have any options."
  }

    where
      testPath :: FilePath
      testPath = folderName </> testName

cabalResult :: TestOutput -> Cabal.Result
cabalResult Success = Cabal.Pass
cabalResult NewTest
  = Cabal.Error "New test: created 'golden' file from test run."
cabalResult (Failure CGOutputParseError)
  = Cabal.Error "Could not parse result of code-generation. Maybe something crashed?"
cabalResult (Failure OtherError)
  = Cabal.Error "Encountered a problem loading test in ghci. Maybe something crashed?"
cabalResult (Failure ModuleError)
  = Cabal.Error
      "Could not load 'fir' library.\n\
      \Possible cause: package environment file missing or of wrong version."
cabalResult (Failure failure)
  = Cabal.Fail (show failure)
