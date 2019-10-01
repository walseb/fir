{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
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

For an overview of the library, refer to the project [__readme__](https://gitlab.com/sheaf/fir/tree/master/readme.md).

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
  =  '[ "in_pos"  ':-> Input      '[ Location 0                 ] (V 2 Float)   -- input  (varying) of type V 2 Float and memory location 0
      , "out_col" ':-> Output     '[ Location 0                 ] (V 4 Float)   -- output (varying) of type V 4 Float and memory location 0
      , "image"   ':-> Texture2D  '[ DescriptorSet 0, Binding 0 ] (RGBA8 UNorm) -- input sampled image (provided as binding 0 of descriptor set 0)
      , "main"    ':-> EntryPoint '[ OriginLowerLeft            ] Fragment      -- fragment shader stage (using standard Cartesian coordinates)
      ]

fragment :: Program FragmentDefs ()
fragment =
  Program $ entryPoint \@"main" \@Fragment do
    pos <- get \@"in_pos"
    col <- use \@(ImageTexel "image") NilOps pos
    put \@"out_col" col
@

Note the lens-like operations:

  - @'FIR.Instances.Codensity.get' \@"in_pos"@ – equivalent to
    @'FIR.Instances.Codensity.use' \@(Name "in_pos")@ – obtains the shader input varying "in_pos".
    This operation is similar to @get@ in a state monad, except that an additional binding name
    is provided via a type application.
  - @'FIR.Instances.Codensity.use' \@(ImageTexel "image") NilOps pos@ samples the provided image
    at coordinates @pos@. Additional image operands can be provided for this sampling operation,
    such as an explicit level of detail or whether to use projective coordinates
    (refer to the SPIR-V specification for further information concerning image operands).
  - @'FIR.Instances.Codensity.put' \@"out_col" col@ – equivalent to
    @'FIR.Instances.Codensity.assign' \@(Name "out_col") col@ –
    sets the output value of the shader.


This fragment shader can then be compiled using:

> compile filePath flags fragment

where @filePath@ is the desired output filepath, and @flags@ is a list of 'CompilerFlag's.


More meaningful examples can be found in the (@fir-examples@ subdirectory)(https://gitlab.com/sheaf/fir/tree/master/fir-examples)
of the project's repository.

-}

module FIR 
  ( compile, runCompilationsTH
  , DrawableProgramAST(ast)
  , CompilerFlag(..)
  , module Control.Monad.Indexed
  , Control.Type.Optic.Optic
  , (Control.Type.Optic.:*:), Control.Type.Optic.Prod, Control.Type.Optic.EndProd
  , (Control.Type.Optic.:.:)
  , Control.Type.Optic.Joint
  , Control.Type.Optic.AnIndex
  , Control.Type.Optic.Index
  , Control.Type.Optic.Name
  , Control.Type.Optic.ReifiedGetter(view)
  , Control.Type.Optic.ReifiedSetter(set)
  , Control.Type.Optic.ReifiedLens(over)
  , module Data.Product
  , module Data.Type.List
  , (Data.Type.Map.:->)((:->))
  , FIR.AST.AST
    ( (:$), Lit
    -- image operands
    , NilOps, Proj, Dref, Bias
    , LOD, MinLOD, Grad
    , ConstOffsetBy, OffsetBy
    , Gather, SampleNo
    -- internal hlist
    , NilHList, ConsHList
    )
  , FIR.AST.fromAST, FIR.AST.toAST -- might be a bad idea
  , FIR.AST.HasUndefined(undefined)
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
  , FIR.Layout.Layout(..)
  , FIR.Layout.Poke(..)
  , FIR.Layout.pokeArray
  , FIR.Layout.roundUp
  , FIR.Layout.nextAligned
  , FIR.Pipeline.ShaderPipeline(..)
  , FIR.Pipeline.withVertexInput, FIR.Pipeline.withStructInput
  , FIR.Pipeline.ShaderPipelineWithInfo(..)
  , pipelineInfoShaders
  , FIR.Pipeline.ShaderStage(ShaderStage)
  , pipelineShaders
  , FIR.Pipeline.PrimitiveConnectedness(..)
  , FIR.Pipeline.PrimitiveTopology(..)
  , FIR.Pipeline.BindingStrides
  , FIR.Pipeline.VertexLocationDescriptions
  , module FIR.Prim.Array
  , FIR.Prim.Image.ImageProperties(..)
  , FIR.Prim.Image.Image
  , FIR.Prim.Image.GatherInfo(..)
  , module FIR.Prim.Struct
  , FIR.Program.Procedure
  , FIR.Program.Program(Program)
  , FIR.Swizzle.Swizzle
  , module FIR.Synonyms
  , module Math.Algebra.Class
  , module Math.Algebra.GradedSemigroup
  , module Math.Logic.Bits
  , module Math.Logic.Class
  , SPIRV.Control.Inlineability(..)
  , SPIRV.Control.SideEffects(..)
  , SPIRV.Control.FunctionControl
  , SPIRV.Control.SelectionControl(..)
  , SPIRV.Decoration.Decoration(..)
  , SPIRV.ExecutionMode.ExecutionMode(..)
  , SPIRV.ExecutionMode.InputVertices -- synonym of 'OutputVertices' for tessellation evaluation shaders
  , SPIRV.Stage.Vertex
  , SPIRV.Stage.TessellationControl
  , SPIRV.Stage.TessellationEvaluation
  , SPIRV.Stage.Geometry
  , SPIRV.Stage.Fragment
  , SPIRV.Stage.Compute
  , SPIRV.Stage.Shader(..)
  -- image properties
  , SPIRV.Image.Dimensionality(..)
  , SPIRV.Image.HasDepth(..)
  , SPIRV.Image.Arrayness(..)
  , SPIRV.Image.MultiSampling(..)
  , SPIRV.Image.ImageUsage(..)
  , SPIRV.Image.ImageFormat(ImageFormat)
  , SNorm, UNorm, F, I, UI
  , pattern SNorm, pattern UNorm
  , pattern F, pattern I, pattern UI
  , RGBA32, RGBA16, RGBA8
  , RG32, RG16, RG8
  , R32, R16, R8
  , R11G11B10
  , RGB10A2
  , pattern RGBA32, pattern RGBA16, pattern RGBA8
  , pattern RG32, pattern RG16, pattern RG8
  , pattern R32, pattern R16, pattern R8
  , pattern R11G11B10
  , pattern RGB10A2

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

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack, unpack )

-- fir
import CodeGen.CodeGen
import CodeGen.State
import Control.Arrow.Strength
import Control.Monad.Indexed
import Control.Type.Optic
import Data.Product
import Data.Type.Known
import Data.Type.List
import Data.Type.Map
import FIR.AST
import FIR.ASTState
import FIR.Binding
import FIR.Definition
import FIR.Instances.AST
  hiding
    ( WhichConversion(..) ) -- internal helper
import FIR.Instances.Codensity
import FIR.Instances.Images
import FIR.Instances.Optics
import FIR.Layout
import FIR.Pipeline
import FIR.Prim.Array
import FIR.Prim.Image
import FIR.Prim.Struct
import FIR.Program
import FIR.Swizzle
import FIR.Synonyms
  hiding
    ( Col_, Col__, Ix_, Ix__
    , RowRes, ColRes, DiagRes, EntryRes
    ) -- internal helpers
import Math.Algebra.Class
import Math.Algebra.GradedSemigroup
import Math.Logic.Bits
import Math.Logic.Class
import SPIRV.Control
import SPIRV.Decoration
import SPIRV.ExecutionMode
import SPIRV.Image
import SPIRV.Stage

------------------------------------------------

-- | Functionality for drawing a program AST as a tree.
--
-- Can be used on whole programs, or on snippets of code in the AST indexed monad.
class DrawableProgramAST prog where
  ast :: prog -> IO ()

instance DrawableProgramAST (AST a) where
  ast = drawTree . toTree
instance DrawableProgramAST (Codensity AST (AST a := j) i) where
  ast = drawTree . toTree . toAST
instance ( DrawableProgramAST
             ( CodensityProgram (StartState defs) endState a )
         )
      => DrawableProgramAST (Program defs a)
      where
  ast (Program prog) = ast prog
instance DrawableProgramAST (CodensityProgram (StartState defs) endState ())
      => DrawableProgramAST (FIR.Pipeline.ShaderStage name stage defs endState)
      where
  ast (FIR.Pipeline.ShaderStage prog) = ast prog

-- | Compiler flags.
data CompilerFlag
  = NoCode -- ^ Don't emit any SPIR-V code.
  | Debug  -- ^ Include additional debug instructions, such as source-code line-number annotations.
  | Assert -- ^ Include additional assertions.
  deriving ( Prelude.Eq, Show )

-- | Functionality for compiling a program, saving the SPIR-V assembly at the given filepath.
class CompilableProgram prog where
  compile :: FilePath -> [CompilerFlag] -> prog -> IO ( Either ShortText () )

instance KnownDefinitions defs => CompilableProgram (Program defs a) where
  compile filePath flags (Program program)
    = case runCodeGen cgContext (toAST program) of
        Left  err -> Prelude.pure ( Left err )
        Right bin
          ->  do  Monad.unless ( NoCode `elem` flags )
                    ( ByteString.writeFile filePath bin )
                  Prelude.pure (Right ())
      where cgContext :: CGContext
            cgContext = (initialCGContext @defs)
              { debugging = Debug  `elem` flags
              , asserting = Assert `elem` flags
              }

instance ( KnownDefinitions defs )
      => CompilableProgram (FIR.Pipeline.ShaderStage name stage defs endState)
      where
  compile filePath flags (FIR.Pipeline.ShaderStage prog)
    = compile filePath flags (Program @defs prog)

-- | Utility function to run IO actions at compile-time using Template Haskell.
-- Useful for compiling shaders at compile-time, before launching a graphics application.
--
-- __Usage example__: in a module which imports this module and shaders
-- @vertexShader@, @fragmentShader@ (bearing in mind the TH staging restriction), write:
--
-- > shaderCompilationResult =
-- >   $( runCompilationsTH
-- >        [ ("Vertex shader"  , compile vertPath [] vertexShader  )
-- >        , ("Fragment shader", compile fragPath [] fragmentShader)
-- >        ]
-- >   )
--
-- At compile-time, this will compile the vertexShader @vertexShader@ at filepath @vertPath@,
-- and similarly for the fragment shader.
-- These can then be loaded into Vulkan shader modules for use in rendering.
runCompilationsTH :: [ ( ShortText, IO (Either ShortText ()) ) ] -> TH.Q TH.Exp
runCompilationsTH namedCompilations
  = TH.lift Prelude.=<< TH.runIO (combineCompilations namedCompilations)
    where
      combineCompilations :: [ ( ShortText, IO (Either ShortText ()) ) ] -> IO (Either ShortText ())
      combineCompilations
        = fmap ( foldl ( \ b (n,a) -> combineResult n b a ) (Right ()) )
        . traverse ( uncurry rightStrength )

      combineResult :: ShortText -> Either ShortText () -> Either ShortText () -> Either ShortText ()
      combineResult _    a           (Right _ ) = a
      combineResult name (Right _)   (Left err) = Left (name <> ": " <> err)
      combineResult name (Left errs) (Left err)
        = Left (errs <> "\n" <> name <> ": " <> err)

-- Template Haskell 'Lift' instance for ShortText, needed for the above
instance TH.Lift ShortText where
  lift      t = [|  ShortText.pack  $(TH.lift      $ ShortText.unpack t)  |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped t = [|| ShortText.pack $$(TH.liftTyped $ ShortText.unpack t) ||]
#endif
