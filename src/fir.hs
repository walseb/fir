{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module FIR 
  ( draw, compile
  , Arg(Debug, NoCode)
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
  , FIR.AST.AST((:$))
  , FIR.AST.fromAST, FIR.AST.toAST -- might be a bad idea
  , FIR.Binding.BindingsMap
  , FIR.Binding.Var, FIR.Binding.Fun
  , FIR.Binding.R
  , FIR.Binding.W
  , FIR.Binding.RW
  , FIR.Definition.Definition
      ( Global
      , Function
      , EntryPoint
      )
  , FIR.Definition.Global_
  , FIR.Definition.Function_
  , FIR.Definition.EntryPoint_
  , module FIR.Instances.AST
  , module FIR.Instances.Codensity
  , module FIR.Instances.Optics
  , FIR.Prim.Array.Array
  , FIR.Prim.Array.mkArray
  , FIR.Prim.Array.RuntimeArray(MkRuntimeArray)
  , FIR.Prim.Struct.Struct(End,(:&))
  , FIR.Program.Procedure
  , FIR.Program.Program(Program)
  , module Math.Algebra.Class
  , module Math.Algebra.GradedSemigroup
  , module Math.Logic.Class
  , SPIRV.Decoration.Decoration
    -- explicit export list to avoid conflicting exports of 'Uniform'
    -- (conflicts with SPIRV.Storage.StorageClass(Uniform))
      ( SpecId, RowMajor, ColMajor
      , ArrayStride, MatrixStride
      , Builtin
      , NoPerspective, Flat
      , Patch
      , Stream, Location, Component
      , Index, Binding, DescriptorSet
      , Offset, Alignment
      )
  , SPIRV.ExecutionMode.ExecutionMode(..)
  , SPIRV.FunctionControl.Inlineability(..)
  , SPIRV.FunctionControl.SideEffects(..)
  , SPIRV.FunctionControl.FunctionControl
  , SPIRV.Stage.Stage(..)
  , SPIRV.Storage.StorageClass(Input, Output, Uniform, UniformConstant, PushConstant)
  ) where

-- base
import qualified Control.Monad as Monad (unless)
import Prelude

-- bytestring
import qualified Data.ByteString.Lazy as ByteString

-- tree-view
import Data.Tree.View(drawTree)

-- text-utf8
import Data.Text(Text)

-- fir
import CodeGen.CodeGen
import CodeGen.State
import Control.Monad.Indexed
import Control.Type.Optic
import Data.Type.Map
import FIR.AST
import FIR.Binding
import FIR.Definition
import FIR.Instances.AST
import FIR.Instances.Codensity
import FIR.Instances.Optics
import FIR.Prim.Array
import FIR.Prim.Struct
import FIR.Program
import Math.Algebra.Class
import Math.Algebra.GradedSemigroup
import Math.Logic.Class
import SPIRV.Decoration
import SPIRV.ExecutionMode
import SPIRV.FunctionControl
import SPIRV.Stage
import SPIRV.Storage

------------------------------------------------

class DrawableProgram prog where
  draw :: prog -> IO ()

instance DrawableProgram (Codensity AST (AST a := j) i) where
  draw = drawTree . toTree . toAST
instance ( DrawableProgram
             ( UndecoratedProgram (InsertionSort defs) a )
         )
      => DrawableProgram (Program defs a)
      where
  draw (Program prog) = draw prog


data Arg
  = NoCode
  | Debug
  deriving ( Prelude.Eq, Show )

compile :: forall defs a. KnownDefinitions defs
        => FilePath
        -> [Arg]
        -> Program defs a
        -> IO ( Either Text Text )
compile filePath args (Program program) = case runCodeGen cgContext (toAST program) of
    Left  err -> Prelude.pure ( Left err )
    Right bin
      ->  do  Monad.unless ( NoCode `elem` args )
                ( ByteString.writeFile filePath bin )
              Prelude.pure ( Right "OK" )
  where cgContext :: CGContext
        cgContext = (context @defs) { debugMode = Debug `elem` args }