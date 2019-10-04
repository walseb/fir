{-# LANGUAGE OverloadedStrings #-}

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
  ( (</>), (<.>)
  , replaceExtension, splitFileName
  )

-- process
import System.Process
  ( proc, createProcess
  , CreateProcess
      ( std_in, std_out, std_err )
  , StdStream
      ( UseHandle, CreatePipe )
  , waitForProcess
  )

-- text
import Data.Text
  ( Text )
import qualified Data.Text    as Text
  ( pack, lines
  , take, drop, dropWhile
  , dropAround, breakOn
  , null
  )
import qualified Data.Text.IO as Text
  ( readFile, hPutStrLn )

-- fir
import FIR
  ( CompilerFlag(..) )

--------------------------------------------------

data Test
  = Typecheck
  | CodeGen
  | Validate
  deriving ( Eq, Show )

data TestFailure
  = MissingSource
  | WrongTypecheckOutput
  | ExpectedTypecheck
  | UnexpectedTypecheck
  | CodeGenFail  Text
  | ValidateFail Text
  | CGOutputParseError
  | ModuleError
  | OtherError
  deriving ( Eq, Show )

data TestOutput
  = Failure TestFailure
  | Success
  | NewTest
  deriving ( Eq, Show )

runTest :: Test -> FilePath -> IO TestOutput
runTest Typecheck = typeCheck
runTest CodeGen   = codeGen
runTest Validate  = validate

--------------------------------------------------
-- truly disgusting testing setup

ghc :: FilePath
ghc = "ghc"

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
                $ ( uncurry openBinaryTempFile )
                    ( second
                        ( `replaceExtension` "tmp" )
                        ( splitFileName src )
                    )
        let codeGenProcess
              = ( proc ghc [ "--interactive"
                           , src
                           , "-w"
                           , "-package", "fir"
                           , "-threaded"
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
        res' <- case res of
          Failure ExpectedTypecheck
            -> do renameFile err test
                  removeFile out
                  pure res
          Failure (CodeGenFail _)
            -> do renameFile out test
                  removeFile err
                  pure res
          Failure _
            -> do removeFile out
                  errLines <- dropWhile ignoreLineText . Text.lines <$> Text.readFile err
                  case errLines of
                    ( l1 : _ )
                      | Text.take 43 l1 == "<command line>: cannot satisfy -package fir"
                      -> do removeFile err
                            pure (Failure ModuleError)
                    _ -> do renameFile err test
                            pure res
          Success
            -> do removeFile out
                  removeFile err
                  failExists <- doesFileExist test
                  when ( NoCode `elem` flags && failExists ) ( removeFile test )
                  pure res
          NewTest
            -> do removeFile out
                  removeFile err
                  pure res
        if res' /= Success || NoCode `elem` flags
        then pure res'
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
                           , "-threaded"
                           ]
                ) { std_err = UseHandle tempHandle }
        (_, _, _, processHandle)
          <- createProcess typeCheckProcess
        _ <- waitForProcess processHandle
        hClose tempHandle
        testContents <- ByteString.readFile temp
        removeFile temp
        let mbTcOutput = parseTcOutput testContents

        case mbTcOutput of
          Nothing -> pure ( Failure ModuleError )
          Just tcOutput -> do

            goldenExists <- doesFileExist gold

            if goldenExists
            then do
              golden <- ByteString.readFile gold
              if golden == tcOutput
              then pure Success
              else do
                ByteString.writeFile test tcOutput
                if golden == cleanTc
                then pure ( Failure ExpectedTypecheck)
                else
                  if tcOutput == cleanTc
                  then pure ( Failure UnexpectedTypecheck )
                  else pure ( Failure WrongTypecheckOutput )

            else do ByteString.writeFile gold tcOutput
                    pure NewTest

    where dir  = "test" </> "Tests"
          src  = dir </> testName <.> "hs"
          gold = dir </> testName <.> "golden"
          test = dir </> testName <.> "fail"

cleanTc :: ByteString
cleanTc = "No type-checking errors."

ignoreLineBS :: ByteString -> Bool
ignoreLineBS string
  |    ByteString.null string
    || ByteString.take 4  string == "GHCi"
    || ByteString.take 26 string == "Loaded package environment"
  = True
  | otherwise
  = False

ignoreLineText :: Text -> Bool
ignoreLineText string
  |    Text.null string
    || Text.take 4  string == "GHCi"
    || Text.take 26 string == "Loaded package environment"
  = True
  | otherwise
  = False

parseTcOutput :: ByteString -> Maybe ByteString
parseTcOutput contents = go ( dropWhile ignoreLineBS ( ByteString.lines contents ) )
  where
    go :: [ByteString] -> Maybe ByteString
    go [] = Just cleanTc
    go ls@(l:_)
      | ByteString.take 43 l == "<command line>: cannot satisfy -package fir"
      = Nothing
      | otherwise
      = Just $ ByteString.unlines ls

parseCGOutput :: Text -> TestOutput
parseCGOutput contents
  = let ls = dropWhile ignoreLineText (Text.lines contents)
    in case ls of
          l1 : l2 : l3 : _
            | Text.take 8 l1 == "[1 of 1]"
            -> case l2 of
                  "Ok, one module loaded."
                     -> let l3' = Text.drop 2
                                . Text.dropWhile ( /= '>' )
                                $ l3
                        in case Text.breakOn " " l3' of
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
                     -> Failure ExpectedTypecheck
                  _  -> Failure OtherError
          _ -> Failure OtherError
