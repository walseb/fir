{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}

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
  , FIR.Binding.Binding(Variable,Function)
  , FIR.Binding.BindingsMap
  , FIR.Binding.Var, FIR.Binding.Fun
  , FIR.Binding.R
  , FIR.Binding.W
  , FIR.Binding.RW
  , FIR.Builtin.Stage(..)
  , module FIR.Instances.AST
  , module FIR.Instances.Codensity
  , module FIR.Instances.Optics
  , FIR.Prim.Array.Array
  , FIR.Prim.Array.mkArray
  , FIR.Prim.Array.RuntimeArray(MkRuntimeArray)
  , FIR.Prim.Struct.Struct(End,(:&))
  , FIR.Program.Procedure
  , FIR.Program.Program
  , module Math.Algebra.Class
  , module Math.Algebra.GradedSemigroup
  , module Math.Logic.Class
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
import CodeGen.CodeGen(runCodeGen)
import CodeGen.State(CGContext(..))
import Control.Monad.Indexed
import Control.Type.Optic
import Data.Type.Map
import FIR.AST
import FIR.Binding
import FIR.Builtin
import FIR.Instances.AST
import FIR.Instances.Codensity
import FIR.Instances.Optics
import FIR.Prim.Array
import FIR.Prim.Struct
import FIR.Prim.Singletons(KnownInterface)
import FIR.Program
import Math.Algebra.Class
import Math.Algebra.GradedSemigroup
import Math.Logic.Class

------------------------------------------------

draw :: Program i j a -> IO ()
draw = drawTree . toTree . toAST

data Arg
  = NoCode
  | Debug
  deriving ( Prelude.Eq, Show )

compile :: KnownInterface i
        => FilePath
        -> [Arg]
        -> CodensityProgram i j a
        -> IO ( Either Text Text )
compile filePath args program = case runCodeGen cgContext (toAST program) of
    Left  err -> Prelude.pure ( Left err )
    Right bin
      ->  do  Monad.unless ( NoCode `elem` args )
                ( ByteString.writeFile filePath bin )
              Prelude.pure ( Right "OK" )
  where cgContext :: CGContext
        cgContext
          = CGContext
              { userGlobals = programGlobals program
              , debugMode   = Debug `elem` args
              }