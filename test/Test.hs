{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Test where

-- base
import Control.Arrow
  ( second )
import Control.Monad
  ( when, replicateM )
import System.IO
  ( openBinaryTempFile, hClose )

-- bytestring
import Data.ByteString
  ( ByteString )
import qualified Data.ByteString as ByteString
  ( readFile, writeFile
  , null, take
  )
import qualified Data.ByteString.Char8 as ByteString
  ( lines, unlines )

-- directory
import System.Directory
  ( doesFileExist, renameFile, removeFile )

-- filepath
import System.FilePath
  ( (</>), (<.>), replaceExtension, splitFileName )

-- process
import System.Process
  ( proc, createProcess
  , CreateProcess(std_in, std_out, std_err)
  , StdStream(UseHandle, CreatePipe)
  , waitForProcess
  )

-- text
import Data.Text
  ( Text )
import qualified Data.Text    as Text
  ( pack, lines
  , take, drop, dropWhile
  , dropAround, breakOn
  )
import qualified Data.Text.IO as Text
  ( readFile, hPutStrLn )

-- fir
import FIR
  ( CompilerFlag(NoCode, Debug) )

--------------------------------------------------

tests :: [ (FilePath, Test) ]
tests = [ ( "Array"        </> "Applicative"   , Validate  )
        , ( "Bits"         </> "Bits"          , Validate  )
        , ( "Bits"         </> "Zipbits"       , Validate  )
        , ( "Control"      </> "Loop"          , Validate  )
        , ( "Geometry"     </> "Geometry"      , Validate  )
        , ( "Geometry"     </> "NotGeometry"   , TypeCheck )
        , ( "Images"       </> "Gather"        , Validate  )
        , ( "Images"       </> "Sample"        , Validate  )
        , ( "Matrix"       </> "Applicative"   , Validate  )
        , ( "Optics"       </> "ASTProducts"   , Validate  )
        , ( "Optics"       </> "MVP1"          , Validate  )
        , ( "Optics"       </> "MVP2"          , Validate  )
        , ( "Optics"       </> "NoMatrixIndex" , TypeCheck )
        , ( "Optics"       </> "NoStructField" , TypeCheck )
        , ( "Optics"       </> "NoStructIndex" , TypeCheck )
        , ( "Optics"       </> "NoVectorIndex" , TypeCheck )
        , ( "Optics"       </> "Overlapping"   , TypeCheck )
        , ( "Optics"       </> "ProductIndices", TypeCheck )
        , ( "Optics"       </> "PureProducts"  , TypeCheck )
        , ( "Optics"       </> "Various"       , Validate  )
        , ( "PrimOps"      </> "Rounding"      , Validate  )
        , ( "Tessellation" </> "Control"       , Validate  )
        , ( "Tessellation" </> "Evaluation"    , Validate  )
        , ( "Vector"       </> "Applicative"   , Validate  )
        , ( "Vector"       </> "Functor"       , Validate  )
        , ( "Vector"       </> "Swizzle"       , Validate  )
        , ( "Vector"       </> "MixedSwizzle"  , TypeCheck )
        ]

runTests :: IO [ (FilePath, Test, TestOutput) ]
runTests = traverse
              ( \(file, test) -> ( file, test, ) <$> runTest test file )
              tests

runTest :: Test -> FilePath -> IO TestOutput
runTest TypeCheck = typeCheck
runTest CodeGen   = codeGen
runTest Validate  = validate

--------------------------------------------------
-- truly disgusting testing setup

data Test
  = TypeCheck
  | CodeGen
  | Validate
  deriving ( Eq, Show )

data TestFailure
  = MissingSource
  | WrongTypeCheckOutput
  | ExpectedTypeCheck
  | UnexpectedTypeCheck
  | CodeGenFail  Text
  | ValidateFail Text
  | CGOutputParseError
  deriving ( Eq, Show )

data TestOutput
  = Failure TestFailure
  | Success
  | NewTest
  deriving ( Eq, Show )

ghc :: FilePath
ghc = "ghc"

ghci :: FilePath
ghci = "ghci"

validator :: FilePath
validator = "spirv-val"

codeGen, validate :: FilePath -> IO TestOutput
codeGen  testName = compileTest [Debug, Assert, NoCode] testName
validate testName = compileTest [Debug, Assert]         testName

compileTest :: [CompilerFlag] -> FilePath -> IO TestOutput
compileTest flags testName = do
  srcExists <- doesFileExist src
  case srcExists of
    False -> pure ( Failure MissingSource )
    True  ->
      do
        [   (out, outHandle)
          , (err, errHandle)
          ] <- replicateM 2
                ( ( uncurry openBinaryTempFile )
                      ( second
                          ( `replaceExtension` "tmp" )
                          ( splitFileName src )
                      )
                )
        let codeGenProcess
              = ( proc ghci [ src
                            , "-w"
                            , "-package", "fir"
                            ]
                ) { std_in  = CreatePipe
                  , std_out = UseHandle outHandle
                  , std_err = UseHandle errHandle
                  }
        (Just inputHandle, _, _, processHandle)
          <- createProcess codeGenProcess
        
        Text.hPutStrLn inputHandle compile
        Text.hPutStrLn inputHandle ":q"
        hClose inputHandle
        _ <- waitForProcess processHandle

        testContents <- Text.readFile out
        let res = parseCGOutput testContents
        case res of
          Failure ExpectedTypeCheck
            -> do renameFile err test
                  removeFile out
          Failure _
            -> do renameFile out test
                  removeFile err
          Success
            -> do removeFile out
                  removeFile err
                  failExists <- doesFileExist test
                  when ( NoCode `elem` flags && failExists ) ( removeFile test )
          _ -> do removeFile out
                  removeFile err
        if res /= Success || NoCode `elem` flags
        then pure res
        else
          do  (val, valHandle) <-
                  ( uncurry openBinaryTempFile )
                      ( second
                          ( `replaceExtension` "tmp" )
                          ( splitFileName src )
                      )
              let validateProcess
                    = ( proc validator [ spv ] )
                        { std_err = UseHandle valHandle }
              (_, _, _, valProcessHandle)
                <- createProcess validateProcess
              _ <- waitForProcess valProcessHandle

              valContents <- Text.readFile val
              case valContents of
                "" -> do removeFile val
                         removeFile spv
                         failExists <- doesFileExist test
                         when failExists ( removeFile test )
                         pure Success
                _  -> do renameFile val test
                         renameFile spv (spv <.> "fail")
                         pure ( Failure ( ValidateFail valContents ) )
                
  where dir  = "test" </> "Tests"
        src  = dir </> testName <.> "hs"        
        test = dir </> testName <.> "fail"
        spv  = dir </> testName <.> "spv"
        compile =  "compile "
                <> Text.pack (show spv)
                <> " "
                <> Text.pack (show flags)
                <> " program"

typeCheck :: FilePath -> IO TestOutput
typeCheck testName = do
  srcExists <- doesFileExist src
  case srcExists of
    False -> pure ( Failure MissingSource )
    True  -> 
      do 
        (temp, tempHandle) <- ( uncurry openBinaryTempFile )
                                ( second
                                    ( `replaceExtension` "tmp" )
                                    ( splitFileName src )
                                )
        let typeCheckProcess
              = ( proc ghc [ src
                           , "-w"
                           , "-fno-code"
                           , "-package", "fir"
                           ]
                ) { std_err = UseHandle tempHandle }
        (_, _, _, processHandle)
          <- createProcess typeCheckProcess
        _ <- waitForProcess processHandle
        hClose tempHandle
        testContents <- ByteString.readFile temp
        removeFile temp
        let testResult = parseTcOutput testContents

        goldenExists <- doesFileExist gold

        if goldenExists
        then do golden <- ByteString.readFile gold
                if golden == testResult
                then do pure Success                        
                else do ByteString.writeFile test testResult
                        if golden == cleanTc
                        then do pure ( Failure ExpectedTypeCheck)                            
                        else if testResult == cleanTc
                             then pure ( Failure UnexpectedTypeCheck )
                             else pure ( Failure WrongTypeCheckOutput )

        else do ByteString.writeFile gold testResult
                pure NewTest

    where dir  = "test" </> "Tests"
          src  = dir </> testName <.> "hs"
          gold = dir </> testName <.> "golden"
          test = dir </> testName <.> "fail"

cleanTc :: ByteString
cleanTc = "No type-checking errors."

parseTcOutput :: ByteString -> ByteString
parseTcOutput contents = go ( dropWhile ignore ( ByteString.lines contents ) )
  where
    go :: [ByteString] -> ByteString
    go [] = cleanTc
    go ls = ByteString.unlines ls

    ignore :: ByteString -> Bool
    ignore string
      |    ByteString.null string
        || ByteString.take 26 string == "Loaded package environment"
      = True
      | otherwise
      = False

parseCGOutput :: Text -> TestOutput
parseCGOutput contents
  = let ls = Text.lines contents
    in case ls of
          l1 : l2 : l3 : l4 : _
            |  Text.take 4 l1 == "GHCi"
            && Text.take 8 l2 == "[1 of 1]"
            -> case l3 of
                  "Ok, one module loaded."
                     -> let l4' = Text.drop 2
                                . Text.dropWhile ( /= '>' )
                                $ l4
                        in case Text.breakOn " " l4' of
                              ( "Left", rest )
                                -> Failure ( CodeGenFail
                                               ( Text.dropAround (== '\"' )
                                               . Text.drop 1
                                               $ rest
                                               )
                                           )
                              ( "Right", _)
                                -> Success
                              _ -> Failure CGOutputParseError
                  "Failed, no modules loaded."
                     -> Failure ExpectedTypeCheck
                  _  -> Failure CGOutputParseError
          _ -> Failure CGOutputParseError
