{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveLift           #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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

fragment :: Module FragmentDefs ()
fragment =
  Module $ entryPoint \@"main" \@Fragment do
    pos <- get \@"in_pos"
    col <- use \@(ImageTexel "image") NilOps pos
    put \@"out_col" col
@

Note the lens-like operations:

  - @'FIR.Syntax.Program.get' \@"in_pos"@ – equivalent to
    @'FIR.Syntax.Program.use' \@(Name "in_pos")@ – obtains the shader input varying "in_pos".
    This operation is similar to @get@ in a state monad, except that an additional binding name
    is provided via a type application.
  - @'FIR.Syntax.Program.use' \@(ImageTexel "image") NilOps pos@ samples the provided image
    at coordinates @pos@. Additional image operands can be provided for this sampling operation,
    such as an explicit level of detail or whether to use projective coordinates
    (refer to the SPIR-V specification for further information concerning image operands).
  - @'FIR.Syntax.Program.put' \@"out_col" col@ – equivalent to
    @'FIR.Syntax.Program.assign' \@(Name "out_col") col@ –
    sets the output value of the shader.


This fragment shader can then be compiled using:

> compileTo filePath flags fragment

where @filePath@ is the desired output filepath, and @flags@ is a list of 'CompilerFlag's.


More meaningful examples can be found in the (@fir-examples@ subdirectory)(https://gitlab.com/sheaf/fir/tree/master/fir-examples)
of the project's repository.

-}

module FIR
  ( DrawableProgramAST(showAST, drawAST)
  , CompilableProgram(compile, compileTo)
  , CompilerFlag(..)
  , ModuleBinary(..), ModuleRequirements(..)
  , runCompilationsTH

  -- re-exports
  , module Control.Monad.Indexed
  , Control.Type.Optic.Optic
  , Control.Type.Optic.ShowOptic
  , (Control.Type.Optic.:*:), Control.Type.Optic.Prod, Control.Type.Optic.EndProd
  , (Control.Type.Optic.:.:)
  , Control.Type.Optic.OfType
  , Control.Type.Optic.AnIndex
  , Control.Type.Optic.Index
  , Control.Type.Optic.Name
  , Control.Type.Optic.ReifiedGetter(view)
  , Control.Type.Optic.ReifiedSetter(set)
  , Control.Type.Optic.ReifiedLens(over)
  , module Data.Product
  , module Data.Type.List
  , (Data.Type.Map.:->)((:->))
  , module Data.Type.POrd
  , AST, FIR.AST.Code, EGADT
  , module FIR.AST.Type
  , pattern (:$), pattern Lit, pattern Struct, pattern Array
  -- image operands
  , pattern NilOps, pattern Proj, pattern Dref, pattern Bias
  , pattern LOD, pattern MinLOD, pattern Grad
  , pattern ConstOffsetBy, pattern OffsetBy
  , pattern Gather, pattern SampleNo
  -- internal hlist
  , pattern NilHList, pattern ConsHList
  , FIR.AST.Syntactic(..)
  , FIR.AST.SyntacticVal, FIR.AST.InternalType
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
  , FIR.Layout.Layout(..)
  , FIR.Layout.Poke(..)
  , FIR.Layout.pokeArray
  , FIR.Layout.pokeArrayOff
  , FIR.Layout.roundUp
  , FIR.Layout.nextAligned
  , module FIR.Module
  , module FIR.Pipeline
  , module FIR.Prim.Array
  , FIR.Prim.Image.ImageProperties(..)
  , FIR.Prim.Image.Image
  , FIR.Prim.Image.GatherInfo(..)
  , module FIR.Prim.RayTracing
  , module FIR.Prim.Struct
  , FIR.Prim.Types.PrimTy
  , FIR.Prim.Types.ScalarTy
  , FIR.Prim.Types.IntegralTy
  , FIR.Prim.Types.PrimTyMap
  , FIR.Prim.Types.HasOpaqueType
  , FIR.ProgramState.ProgramState(ProgramState)
  , FIR.ProgramState.FunctionContext(TopLevel)
  , module FIR.Syntax.AST
  , module FIR.Syntax.Complex
  , module FIR.Syntax.IfThenElse
  , module FIR.Syntax.Images
  , module FIR.Syntax.Optics
  , module FIR.Syntax.Option
  , module FIR.Syntax.Program
  , module FIR.Syntax.RayTracing
  , module FIR.Syntax.Swizzle
  , module FIR.Syntax.Synonyms
  , module FIR.Validation.Bindings
  , module FIR.Validation.Images
  , module FIR.Validation.RayTracing
  , module Math.Algebra.Class
  , module Math.Algebra.GradedSemigroup
  , module Math.Logic.Bits
  , module Math.Logic.Class
  , module SPIRV.Capability
  , SPIRV.Control.Inlineability(..)
  , SPIRV.Control.SideEffects(..)
  , SPIRV.Control.FunctionControl
  , SPIRV.Control.SelectionControl(..)
  , SPIRV.Decoration.Decoration(..)
  , SPIRV.ExecutionMode.ExecutionMode(..)
  , SPIRV.ExecutionMode.InputVertices -- synonym of 'OutputVertices' for tessellation evaluation shaders
  , module SPIRV.Extension
  , SPIRV.Groups.GroupOperation(..)
  , SPIRV.Stage.Vertex
  , SPIRV.Stage.TessellationControl
  , SPIRV.Stage.TessellationEvaluation
  , SPIRV.Stage.Geometry
  , SPIRV.Stage.Fragment
  , SPIRV.Stage.Compute
  , SPIRV.Stage.Stage(..)
  , SPIRV.Stage.Shader(..)
  , SPIRV.Stage.RayShader(..)
  , SPIRV.Stage.MeshShader(..)
  , SPIRV.Stage.ExecutionModel(..)
  , SPIRV.Stage.RayGeneration
  , SPIRV.Stage.Intersection
  , SPIRV.Stage.AnyHit
  , SPIRV.Stage.ClosestHit
  , SPIRV.Stage.Miss
  , SPIRV.Stage.Callable
  , SPIRV.Storage.DataOrigin(..)
  , module SPIRV.Synchronisation
  , SPIRV.Version.Version(..)
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
  ) where

-- base
import Data.Foldable
  ( foldl' )
import Data.Functor
  ( ($>) )
import GHC.Generics
  ( Generic )

-- template-haskell
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

-- atomic-file-ops
import System.IO.AtomicFileOps
  ( atomicReplaceFile )

-- bytestring
import qualified Data.ByteString.Lazy as ByteString
  ( writeFile )
import Data.ByteString.Lazy
  ( ByteString )

-- containers
import Data.Set
  ( Set )

-- directory
import System.Directory
  ( doesFileExist )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( EGADT )

-- generic-monoid
import Data.Monoid.Generic
  ( GenericSemigroup(..), GenericMonoid(..) )

-- tree-view
import Data.Tree.View
  ( showTree )

-- text-short
import Data.Text.Short
  ( ShortText )

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
import Data.Type.POrd
import FIR.AST
import FIR.AST.Type
import FIR.Binding
import FIR.Definition
import FIR.Layout
import FIR.Module
import FIR.Pipeline
import FIR.Prelude
import FIR.Prim.Array
import FIR.Prim.Image
import FIR.Prim.RayTracing
  hiding ( pattern RayQuery )
import FIR.Prim.Types
import FIR.Prim.Struct
import FIR.ProgramState
import FIR.Syntax.AST
import FIR.Syntax.Complex
import FIR.Syntax.IfThenElse
import FIR.Syntax.Images
import FIR.Syntax.Optics
import FIR.Syntax.Option
import FIR.Syntax.Program
import FIR.Syntax.RayTracing
import FIR.Syntax.Swizzle
import FIR.Syntax.Synonyms
import FIR.Validation.Bindings
  ( Has, CanGet, CanPut
  , ValidFunDef, ValidEntryPoint
  )
import FIR.Validation.Images
  ( ValidImageRead, ValidImageWrite )
import FIR.Validation.RayTracing
  ( CanTraceRay, CanExecuteCallable )
import Instances.TH.Lift
  ( ) -- Lift instances for ByteString, ShortText, Set
import Math.Algebra.Class
import Math.Algebra.GradedSemigroup
import Math.Logic.Bits
import Math.Logic.Class
import SPIRV.Capability
import qualified SPIRV.Capability as SPIRV
import SPIRV.Control
import SPIRV.Decoration
import SPIRV.ExecutionMode
import SPIRV.Extension
import qualified SPIRV.Extension  as SPIRV
import SPIRV.Image hiding ( LODOperand(..), Operand(..) )
import SPIRV.Groups
import SPIRV.Stage
import SPIRV.Storage
import SPIRV.Version
import qualified SPIRV.Version    as SPIRV
import SPIRV.Synchronisation

------------------------------------------------

-- | Functionality for drawing a program AST as a tree.
--
-- Can be used on whole programs, or on snippets of code in the AST indexed monad.
class DrawableProgramAST prog where
  showAST :: prog -> String
  drawAST :: prog -> IO ()
  drawAST = putStrLn . showAST

instance DrawableProgramAST (AST a) where
  showAST = showTree . toTree
instance SyntacticVal a => DrawableProgramAST (Program i j a) where
  showAST = showTree . toTree . toAST
instance DrawableProgramAST (Module def) where
  showAST (Module prog) = showAST prog
instance DrawableProgramAST (ShaderModule name stage defs endState) where
  showAST (ShaderModule prog) = showAST prog

-- | Compiler flags.
data CompilerFlag
  = NoCode -- ^ Don't emit any SPIR-V code.
  | Debug  -- ^ Include additional debug instructions, such as source-code line-number annotations and @debugPrintf@ output.
  | Assert -- ^ Include additional assertions.
  | SPIRV SPIRV.Version -- ^ SPIR-V version number.
  deriving stock ( Prelude.Eq, Show )

updateContextFromFlags :: CGContext -> [ CompilerFlag ] -> CGContext
updateContextFromFlags = foldl updateContext
  where
    updateContext :: CGContext -> CompilerFlag -> CGContext
    updateContext ctxt NoCode      = ctxt { emittingCode = False }
    updateContext ctxt Debug       = ctxt { debugging    = True  }
    updateContext ctxt Assert      = ctxt { asserting    = True  }
    updateContext ctxt (SPIRV ver) = ctxt { spirvVersion = ver   }

-- | Information about requirements for a given SPIR-V module,
-- such as which SPIR-V capabilities and extensions are needed.
--
-- This information can be useful to compute Vulkan requirements.
data ModuleRequirements
  = ModuleRequirements
  { requiredCapabilities :: Set SPIRV.Capability
  , requiredExtensions   :: Set SPIRV.Extension
  }
  deriving stock ( Prelude.Eq, Show, Generic, TH.Lift )
  deriving Semigroup via GenericSemigroup ModuleRequirements
  deriving Monoid    via GenericMonoid    ModuleRequirements

-- | Newtype wrapper for the binary source of a SPIR-V module.
--
-- This newtype emphasises the fact that module binaries are monolithic,
-- e.g. one can't combine binaries using '(<>)'.
newtype ModuleBinary = ModuleBinary { moduleBinary :: ByteString }

-- | Functionality for compiling a program into a SPIR-V module.
class CompilableProgram prog where
  compile :: [CompilerFlag] -> prog -> IO ( Either ShortText ( Maybe ModuleBinary, ModuleRequirements ) )
  compileTo :: FilePath -> [CompilerFlag] -> prog -> IO ( Either ShortText ModuleRequirements )
  compileTo filePath flags prog =
    compile flags prog Prelude.>>= \case
      Left err
        -> Prelude.pure (Left err)
      Right (mbBin, reqs)
        | Just (ModuleBinary bytes) <- mbBin
        -> writeBytesAtomically bytes $> Right reqs
        | otherwise
        -> Prelude.pure ( Right reqs )
    where
      writeBytesAtomically :: ByteString -> IO ()
      writeBytesAtomically bytes = 
       doesFileExist filePath Prelude.>>= \case
         False -> ByteString.writeFile      filePath bytes
         True  -> atomicReplaceFile Nothing filePath bytes

instance ( KnownDefinitions defs ) => CompilableProgram (Module defs) where
  compile flags (Module program)
    = case runCodeGen cgContext (toAST program) of
        Left  err -> Prelude.pure ( Left err )
        Right ( bytes, CGState { neededCapabilities, neededExtensions } )
          ->  let
                reqs :: ModuleRequirements
                reqs = ModuleRequirements
                  { requiredCapabilities = neededCapabilities
                  , requiredExtensions   = neededExtensions
                  }
                mbBin :: Maybe ModuleBinary
                mbBin
                  | emittingCode cgContext
                  = Just (ModuleBinary bytes)
                  | otherwise
                  = Nothing
              in Prelude.pure ( Right (mbBin, reqs) )
      where cgContext :: CGContext
            cgContext =
              updateContextFromFlags
                ( initialCGContext @defs )
                flags

instance ( KnownDefinitions defs )
      => CompilableProgram (ShaderModule name stage defs endState)
      where
  compile flags (ShaderModule prog)
    = compile flags (Module @defs prog)

-- | Utility function to run IO actions at compile-time using Template Haskell.
-- Useful for compiling shaders at compile-time, before launching a graphics application.
--
-- __Usage example__: in a module which imports this module and shaders
-- @vertexShader@, @fragmentShader@ (bearing in mind the TH staging restriction), write:
--
-- > shaderCompilationResult :: Either ShortText ModuleRequirements
-- > shaderCompilationResult =
-- >   $( runCompilationsTH
-- >        [ ("Vertex shader"  , compileTo vertPath [] vertexShader  )
-- >        , ("Fragment shader", compileTo fragPath [] fragmentShader)
-- >        ]
-- >   )
--
-- At compile-time, this will compile the vertexShader @vertexShader@ at filepath @vertPath@,
-- and similarly for the fragment shader.
-- These can then be loaded into Vulkan shader modules for use in rendering.
runCompilationsTH
  :: forall f. Traversable f
  => f ( ShortText, IO (Either ShortText ModuleRequirements) )
  -> TH.Q TH.Exp
runCompilationsTH namedCompilations
  = TH.lift Prelude.=<< TH.runIO (combineCompilations namedCompilations)
    where
      combineCompilations
        :: f ( ShortText, IO (Either ShortText ModuleRequirements) )
        -> IO (Either ShortText ModuleRequirements)
      combineCompilations
        = fmap ( foldl' ( \ b (n,a) -> combineResult n b a ) (Right mempty) )
        . traverse ( uncurry rightStrength )

      combineResult
        :: ShortText
        -> Either ShortText ModuleRequirements
        -> Either ShortText ModuleRequirements
        -> Either ShortText ModuleRequirements
      combineResult _    (Right rs  ) (Right ss) = Right (rs <> ss)
      combineResult _    (Left  errs) (Right _ ) = Left  errs
      combineResult name (Right _   ) (Left err) = Left  (name <> ": " <> err)
      combineResult name (Left errs ) (Left err) = Left  (errs <> "\n" <> name <> ": " <> err)
