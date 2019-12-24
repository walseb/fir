{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Module

Utility wrapper types for programs.

A program essentially consists of a value of type @Codensity AST (AST a := j) i@,
but with additional type-level information pertaining to the program inputs/outputs,
similar to the interface for a shader module in @OpenGL@ or @Vulkan@.

See "FIR.ProgramState" which defines the type-level data that programs keep track of.
-}

module FIR.Module
  ( Program
  , Module(Module)
  , ShaderModule(ShaderModule)
  )
  where

-- base
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol )

-- fir
import Control.Monad.Indexed
  ( (:=), Codensity )
import Data.Type.Map
  ( (:->) )
import FIR.AST
  ( AST, Code )
import FIR.Definition
  ( Definition, KnownDefinitions
  , StartState
  )
import FIR.ProgramState
  ( ProgramState )
import FIR.Validation.Definitions
  ( ValidDefinitions )
import qualified SPIRV.Stage  as SPIRV
  ( Shader )

--------------------------------------------------------------------------
-- * Program type synonym
--
-- $wrappers
-- Wrapper for the main internal representation
-- @Codensity AST (a := j) i@

type Program (i :: ProgramState) (j :: ProgramState) (a :: Type)
  = Codensity AST (a := j) i

--------------------------------------------------------------------------
-- * Module type

-- | A 'Module' wraps a value-level program with additional
-- 'Definition's which specify the interface of the module,
-- such as memory layout of input and output data.
--
-- Corresponds to the concept of a SPIR-V module.
data Module
      ( defs :: [ Symbol :-> Definition ] )
    :: Type where
  Module  :: forall defs endState
          .  ( KnownDefinitions defs, ValidDefinitions defs )
          => Program (StartState defs) endState (Code ())
          -> Module defs

--------------------------------------------------------------------------
-- * Shader stage type

-- | Value-level wrapper for a shader stage.
--
-- Takes a module (which may have multiple shaders defined),
-- and specifies (using name + stage) which shader
-- is being referred to (e.g. for use in a shader pipeline).
--
-- Keeps track of the final indexed monadic state of the shader,
-- which is used to perform type-level validation.
data ShaderModule
        ( name     :: Symbol                  )
        ( stage    :: SPIRV.Shader            )
        ( defs     :: [Symbol :-> Definition] )
        ( endState :: ProgramState            )
        where
  ShaderModule
    :: forall name stage defs endState
    .  ( KnownDefinitions defs, ValidDefinitions defs )
    => Program (StartState defs) endState (Code ())
    -> ShaderModule name stage defs endState
