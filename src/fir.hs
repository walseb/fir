{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR
Description: @---@ __Main module, re-exports what's needed to write programs and compile to SPIR-V.__

This module re-exports the main functionality needed to use the library.

For an overview of the library, refer to the project __readme__.

To illustrate the syntax of this library, the example of a trivial fragment shader follows.

@
\{\-\# LANGUAGE BlockArguments   \#\-\} -- cleaner syntax for 'do' blocks (optional)
\{\-\# LANGUAGE DataKinds        \#\-\} -- for datatype promotion and type-level literals
\{\-\# LANGUAGE RebindableSyntax \#\-\} -- 'do' notation to create ASTs with indexed monads
\{\-\# LANGUAGE TypeApplications \#\-\} -- to specify type-level arguments
\{\-\# LANGUAGE TypeOperators    \#\-\} -- for type operators such as ':->, which stands for key/value assignment

import FIR
import Math.Linear -- for vectors

-- Specify the input/output of the shader, with memory locations (and other interface parameters).
-- This consists of a type-level list of top-level definitions.
type FragmentDefs
  =  '[ "in_pos"  ':-> Input      '[ Location 0      ] (V 2 Float)    -- input  (varying) of type V 2 Float and memory location 0
      , "out_col" ':-> Output     '[ Location 0      ] (V 4 Float)    -- output (varying) of type V 4 Float and memory location 0
      , "image"   ':-> Texture2D  '[ Binding  0      ] (RGBA8 UNorm)  -- input sampled image
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment       -- fragment shader stage (using standard Cartesian coordinates)
      ]

fragment :: Program FragmentDefs ()
fragment =
  Program $ entryPoint \@"main" \@Fragment do
    pos <- get \@"in_pos"
    col <- use \@(ImageTexel "image") NoOperands pos
    put \@"out_col" col
@

Note the lens-like operations:

  - @'FIR.Instances.Codensity.get' \@"in_pos"@ – equivalent to
    @'FIR.Instances.Codensity.use' \@(Name "in_pos")@ – obtains the shader input varying "in_pos".
    This operation is similar to @get@ in a state monad, except that an additional binding name
    is provided via a type application.
  - @'FIR.Instances.Codensity.use' \@(ImageTexel "image") NoOperands pos@ samples the provided image
    at coordinates @pos@. Additional operands can be provided for this sampling operation,
    such as an explicit level of detail or whether to use projective coordinates.
  - @'FIR.Instances.Codensity.put' \@"out_col" col@ – equivalent to
    @'FIR.Instances.Codensity.assign' \@(Name "out_col") col@ –
    sets the output value of the shader.


This fragment shader can then be compiled using:

> compile filePath flags fragment

where @filePath@ is the desired output filepath, and @flags@ is a list of 'CompilerFlag's.


More meaningful examples can be found in the @fir-examples@ subdirectory of the project's repository.

-}

module FIR 
  ( compile, runCompilationsTH
  , DrawableProgram(draw)
  , CompilerFlag(Debug, NoCode)
  , module Control.Monad.Indexed
  , Control.Type.Optic.Optic
  , (Control.Type.Optic.:*:)
  , (Control.Type.Optic.:.:)
  , Control.Type.Optic.Joint
  , Control.Type.Optic.AnIndex
  , Control.Type.Optic.Index
  , Control.Type.Optic.Name
  , Control.Type.Optic.ReifiedGetter(view)
  , Control.Type.Optic.ReifiedSetter(set)
  , (Data.Type.Map.:->)((:->))
  , FIR.AST.AST((:$), Lit, Pair, Ops)
  , FIR.AST.fromAST, FIR.AST.toAST -- might be a bad idea
  , FIR.Binding.BindingsMap
  , FIR.Binding.Var, FIR.Binding.Fun
  , FIR.Binding.R
  , FIR.Binding.W
  , FIR.Binding.RW
  , FIR.Definition.Definition
      ( Function
      , EntryPoint
      )
  , FIR.ASTState.ASTState(ASTState)
  , FIR.ASTState.FunctionContext(TopLevel)
  , module FIR.Instances.AST
  , module FIR.Instances.Codensity
  , module FIR.Instances.Images
  , module FIR.Instances.Optics
  , FIR.Prim.Array.Array
  , FIR.Prim.Array.mkArray
  , FIR.Prim.Array.RuntimeArray(MkRuntimeArray)
  , FIR.Prim.Image.ImageProperties(..)
  , FIR.Prim.Image.Image
  , FIR.Prim.Image.GatherInfo(..)
  , FIR.Prim.Struct.Struct(End,(:&))
  , FIR.Program.Procedure
  , FIR.Program.Program(Program)
  , module FIR.Synonyms
  , FIR.Swizzle.Swizzle
  , module Math.Algebra.Class
  , module Math.Algebra.GradedSemigroup
  , module Math.Logic.Bits
  , module Math.Logic.Class
  , SPIRV.Decoration.Decoration(..)
  , SPIRV.ExecutionMode.ExecutionMode(..)
  , SPIRV.ExecutionMode.InputVertices -- synonym of 'OutputVertices' for tessellation evaluation shaders
  , SPIRV.FunctionControl.Inlineability(..)
  , SPIRV.FunctionControl.SideEffects(..)
  , SPIRV.FunctionControl.FunctionControl
  , SPIRV.Stage.Stage(..)
  -- image properties
  , SPIRV.Image.Dimensionality(..)
  , SPIRV.Image.HasDepth(..)
  , SPIRV.Image.Arrayness(..)
  , SPIRV.Image.MultiSampling(..)
  , SPIRV.Image.ImageUsage(..)
  , SNorm, UNorm, F, I, UI
  , RGBA32, RGBA16, RGBA8
  , RG32, RG16, RG8
  , R32, R16, R8
  , R11G11B10
  , RGB10A2

  -- known
  , Data.Type.Known.Known(..)
  , Data.Type.Known.knownValue

  -- re-exporting parts of Prelude
  , Prelude.Bool(..), Prelude.otherwise
  , Prelude.Maybe(..), Prelude.maybe
  , Prelude.Either(..), Prelude.either
  , Prelude.fst, Prelude.snd
  , Prelude.curry, Prelude.uncurry
  , Prelude.Float, Prelude.Double
  , Prelude.subtract
  , Prelude.Semigroup(..), Prelude.Monoid(..)
  , Prelude.id, Prelude.const, (Prelude..)
  , Prelude.flip, (Prelude.$)
  , Prelude.until, Prelude.asTypeOf
  , Prelude.error, Prelude.errorWithoutStackTrace
  , Prelude.undefined
  , Prelude.seq, (Prelude.$!)
  , Prelude.IO
  , Prelude.FilePath

  -- numeric datatypes
  , Data.Word.Word8
  , Data.Word.Word16
  , Data.Word.Word32
  , Data.Word.Word64
  , Data.Int.Int8
  , Data.Int.Int16
  , Data.Int.Int32
  , Data.Int.Int64
  , Numeric.Half.Half

  -- overloaded strings syntax
  , Data.String.IsString(..)

  ) where

-- base
import qualified Control.Monad as Monad
  ( unless )
import Data.String
  ( IsString(fromString) )
import Prelude
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import Data.Int
  ( Int8, Int16, Int32, Int64 )

-- bytestring
import qualified Data.ByteString.Lazy as ByteString

-- half
import Numeric.Half
  ( Half )

-- tree-view
import Data.Tree.View
  ( drawTree )

-- template-haskell
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

-- text-utf8
import "text-utf8" Data.Text
  ( Text )
import qualified "text-utf8" Data.Text as Text

-- fir
import CodeGen.CodeGen
import CodeGen.State
import Control.Arrow.Strength
import Control.Monad.Indexed
import Control.Type.Optic
import Data.Type.Known
import Data.Type.Map
import FIR.AST
import FIR.Binding
import FIR.Definition
import FIR.Instances.AST
import FIR.Instances.Codensity
import FIR.Instances.Images
import FIR.Instances.Optics
import FIR.ASTState
import FIR.Prim.Array
import FIR.Prim.Image
import FIR.Prim.Struct
import FIR.Program
import FIR.Synonyms
  hiding
    ( Row_, Col__, Ix_, Ix__
    , RowRes, ColRes, DiagRes, EntryRes
    ) -- internal helpers
import FIR.Swizzle
import Math.Algebra.Class
import Math.Algebra.GradedSemigroup
import Math.Logic.Bits
import Math.Logic.Class
import SPIRV.Decoration
import SPIRV.ExecutionMode
import SPIRV.FunctionControl
import SPIRV.Image
import SPIRV.Stage

------------------------------------------------

-- | Functionality for drawing a program AST as a tree.
--
-- Can be used on whole programs, or on snippets of code in the AST indexed monad.
class DrawableProgram prog where
  draw :: prog -> IO ()

instance DrawableProgram (AST a) where
  draw = drawTree . toTree
instance DrawableProgram (Codensity AST (AST a := j) i) where
  draw = drawTree . toTree . toAST
instance ( DrawableProgram
             ( CodensityProgram (StartState defs) (EndState defs) a )
         )
      => DrawableProgram (Program defs a)
      where
  draw (Program prog) = draw prog

-- | Compiler flags.
data CompilerFlag
  = NoCode -- ^ Don't emit any SPIR-V code.
  | Debug  -- ^ Include additional debug instructions, such as source-code line-number annotations.
  deriving ( Prelude.Eq, Show )

-- | Compiles a program, saving the SPIR-V assembly at the given filepath.
compile :: forall defs a. KnownDefinitions defs
        => FilePath
        -> [CompilerFlag]
        -> Program defs a 
        -> IO ( Either Text Text )
compile filePath flags (Program program) = case runCodeGen cgContext (toAST program) of
    Left  err -> Prelude.pure ( Left err )
    Right bin
      ->  do  Monad.unless ( NoCode `elem` flags )
                ( ByteString.writeFile filePath bin )
              Prelude.pure ( Right "OK" )
  where cgContext :: CGContext
        cgContext = (initialCGContext @defs) { debugMode = Debug `elem` flags }

instance TH.Lift Text where
  lift t = [| Text.pack $(TH.lift $ Text.unpack t) |]

-- | Utility function to run IO actions at compile time using Template Haskell.
-- Useful for compiling shaders at compile-time, before launching a graphics application.
--
-- __Usage__: in a module which imports this module and shaders @vertexShader@, @fragmentShader@
-- (bearing in mind the TH staging restriction), write:
--
-- > shaderCompilationResult =
-- >   $( runCompilationsTH
-- >        [ ("Vertex shader"  , compile vertPath [] vertexShader  )
-- >        , ("Fragment shader", compile fragPath [] fragmentShader)
-- >        ]
-- >   )
--
-- This will compile the vertexShader @vertexShader@ at filepath @vertPath@,
-- and similarly for the fragment shader.
-- These can then be loaded into Vulkan shader modules for use in rendering.
runCompilationsTH :: [ ( Text, IO (Either Text Text) ) ] -> TH.Q TH.Exp
runCompilationsTH namedCompilations
  = TH.lift Prelude.=<< TH.runIO (combineCompilations namedCompilations)
    where
      combineCompilations :: [ ( Text, IO (Either Text Text) ) ] -> IO (Either Text Text)
      combineCompilations
        = fmap ( foldl ( \ b (n,a) -> combineResult n b a ) (Right "OK") )
        . traverse ( uncurry rightStrength )

      combineResult :: Text -> Either Text Text -> Either Text Text -> Either Text Text
      combineResult _    a           (Right _ ) = a
      combineResult name (Right _)   (Left err) = Left (name <> ": " <> err)
      combineResult name (Left errs) (Left err)
        = Left (errs <> "\n" <> name <> ": " <> err)
