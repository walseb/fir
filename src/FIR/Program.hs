{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Program

Utility wrapper types for programs.

A program essentially consists of a value of type 'Codensity AST (AST a := j) i',
but with additional type-level information pertaining to the program inputs/outputs,
similar to the interface for a shader module in @OpenGL@ or @Vulkan.
-}

module FIR.Program
  ( Procedure
  , CodensityProgram
  , Program(Program)
  , programGlobals
  )
  where

-- base
import Control.Arrow
  ( second )
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map as Map

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import Control.Monad.Indexed
  ( (:=), Codensity )
import Data.Type.Map
  ( (:->) )
import FIR.AST
  ( AST )
import FIR.Definition
  ( Definition, KnownDefinitions
  , StartState
  )
import FIR.ASTState
  ( ASTState(ASTState) )
import FIR.Prim.Singletons
  ( KnownInterface(knownInterface) )
import qualified SPIRV.PrimTy as SPIRV

--------------------------------------------------------------------------
-- * Type (family) synonyms
-- 
-- $wrappers
-- These are wrappers around the main internal representation
-- @Codensity AST (AST a := j) i@

type Procedure (i :: ASTState) (j :: ASTState) (a :: Type)
  = Codensity AST (AST a := j) i

type family CodensityProgram
    ( i :: ASTState ) -- available data at the start (e.g. uniforms)
    ( j :: ASTState ) -- everything in 'i', plus top-level functions and entry-points of the program
    ( a :: Type     )
  = ( r :: Type     )
  | r -> i j a where
  CodensityProgram i j a = Codensity AST ( AST a := j ) i

--------------------------------------------------------------------------
-- * Program type

-- | A 'Program' wraps a value of type @Codensity AST (AST a := j) i@
-- with additional 'Definition's which specify the interface of the program,
-- such as memory layout of input and output data.
data Program
      ( defs :: [ Symbol :-> Definition ] )
      ( a    :: Type                      )
    :: Type where
  Program :: forall defs a endState
          .  ( KnownDefinitions defs )
          => CodensityProgram (StartState defs) endState a
          -> Program defs a

programGlobals :: forall bds j ctx funs eps a.
                  KnownInterface bds
               => CodensityProgram ('ASTState bds ctx funs eps) j a -> Map Text SPIRV.PrimTy
programGlobals _ = Map.fromList
                 . map ( second ( \ (ty, storage) -> SPIRV.Pointer storage ty ) )
                 $ knownInterface @bds
