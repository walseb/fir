module Tests where

-- fir-tests
import Folders
  ( Folder(Folder)
  , runTests, runTestsP, runTestsFromFolder
  )
import Test
  ( Test(..), TestOutput )

--------------------------------------------------

runAllTestsP :: (String -> String -> Test -> Bool)
             -> IO [ Folder (String, Test, TestOutput) ]
runAllTestsP f = runTestsP f allTests

runAllTests :: IO [ Folder (String, Test, TestOutput) ]
runAllTests = runTests allTests

runAllTestsFromFolder :: String -> IO [ Folder (String, Test, TestOutput) ]
runAllTestsFromFolder = flip runTestsFromFolder allTests

--------------------------------------------------

allTests :: [ Folder (String, Test) ]
allTests
  = [ Folder "Array"
          [ ( "Applicative", Validate  )
          , ( "Runtime"    , Typecheck )
          ]
    , Folder "Bits"
          [ ( "Bits"   , Validate )
          , ( "Zipbits", Validate )
          ]
    , Folder "Components"
          [ ( "FloatInt"  , Typecheck )
          , ( "FloatWidth", Typecheck )
          , ( "IntWidth"  , Typecheck )
          , ( "Sign"      , Validate  )
          ]
    , Folder "Definitions"
          [ ( "Descriptors", Typecheck )
          ]
    , Folder "Control"
          [ ( "Def"      , Validate )
          , ( "Loop1"    , Validate )
          , ( "Loop2"    , Validate )
          , ( "Selection", Validate )
          , ( "Switch"   , Validate )
          ]
    , Folder "Debug"
          [ ( "Printf", Validate )
          ]
    , Folder "Geometry"
          [ ( "Geometry"   , Validate  )
          , ( "NotGeometry", Typecheck )
          ]
    , Folder "Groups"
          [ ( "GroupAdd", Validate )
          ]
    , Folder "Images"
          [ ( "Gather"   , Validate )
          , ( "ReadWrite", Validate )
          , ( "Sample"   , Validate )
          ]
    , Folder "Inference"
          [ ( "Ising1", Typecheck )
          , ( "Ising2", Typecheck )
          ]
    , Folder "Interface"
          [ ( "ComponentMismatch", Typecheck )
          , ( "ExtraOutput"      , Typecheck )
          , ( "InvalidComponent" , Typecheck )
          , ( "MissingInput"     , Typecheck )
          ]
    , Folder "Layout"
          [ ( "NestedUniform", Validate )
          ]
    , Folder "Matrix"
          [ ( "Applicative", Validate )
          ]
    , Folder "Module"
          [ ( "DiffStageDiffName", Validate  )
          , ( "DiffStageSameName", Validate  )
          , ( "SameStageDiffName", Validate  )
          , ( "SameStageSameName", Typecheck )
          ]
    , Folder "OpenCL"
          [ ( "Kernel", Validate )
          ]
    , Folder "Optics"
          [ ( "ASTIndices"    , Validate  )
          , ( "ASTProducts"   , Validate  )
          , ( "MVP1"          , Validate  )
          , ( "MVP2"          , Validate  )
          , ( "NoMatrixIndex" , Typecheck )
          , ( "NoStructField" , Typecheck )
          , ( "NoStructIndex" , Typecheck )
          , ( "NoVectorIndex" , Typecheck )
          , ( "Overlapping"   , Typecheck )
          , ( "ProductIndices", Typecheck )
          , ( "PureProducts"  , Typecheck )
          , ( "Various"       , Validate  )
          ]
    , Folder "PrimOps"
          [ ( "Rounding", Validate)
          ]
    , Folder "ProgramState"
          [ ( "Shadowing", Validate)
          ]
    , Folder "Small"
          [ ( "HalfArithmetic" , Validate )
          , ( "HalfInputOutput", Validate )
          , ( "IntArithmetic"  , Validate )
          , ( "IntInputOutput" , Validate )
          ]
    , Folder "Tessellation"
          [ ( "Control"   , Validate )
          , ( "Evaluation", Validate )
          ]
    , Folder "Unicode"
          [ ( "Syntax", Validate )
          ]
    , Folder "Vector"
          [ ( "Applicative" , Validate  )
          , ( "Functor"     , Validate  )
          , ( "Swizzle"     , Validate  )
          , ( "MixedSwizzle", Typecheck )
          ]
    , Folder "VertexInput"
          [ ( "CompatibleTypes"       , Typecheck )
          , ( "ComponentOutOfRange"   , Typecheck )
          , ( "DoubleComponents"      , Typecheck )
          , ( "EquivalentComponents"  , Typecheck )
          , ( "IncompatibleTypes"     , Typecheck )
          , ( "IncompatibleWidths"    , Typecheck )
          , ( "InvalidDoubleComponent", Typecheck )
          , ( "InvalidVectorComponent", Typecheck )
          , ( "MissingComponents"     , Typecheck )
          , ( "Overlap"               , Typecheck )
          , ( "V3Double_Double"       , Typecheck )
          ]
    ]
