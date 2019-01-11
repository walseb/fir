{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Program where

-- base
import Control.Arrow(second)
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import GHC.TypeLits(Symbol)

-- containers
import Data.Map(Map)
import qualified Data.Map as Map

-- text-utf8
import Data.Text(Text)

-- fir
import Control.Monad.Indexed((:=), Codensity)
import Data.Type.Map((:->), InsertionSort)
import FIR.AST(AST)
import FIR.Binding(BindingsMap)
import FIR.Definition(Definition, KnownDefinitions, StartBindings, EndBindings)
import FIR.Prim.Singletons(KnownInterface(knownInterface))
import qualified SPIRV.PrimTy as SPIRV

--------------------------------------------------------------------------
-- type (family) synonyms, all wrappers around main internal representation
-- Codensity AST (AST a := j) i

type Procedure (a :: Type) (i :: BindingsMap) (j :: BindingsMap)
  = Codensity AST (AST a := j) i

type family UndecoratedProgram
    ( defs :: [ Symbol :-> Definition ] )
    ( a    :: Type                      )
  = ( r    :: Type                      )
  where
  UndecoratedProgram defs a = CodensityProgram (StartBindings defs) (EndBindings defs) a

type family CodensityProgram
    (i :: BindingsMap) -- available data at the start (e.g. uniforms)
    (j :: BindingsMap) -- everything in 'i', plus top-level functions of the program
    (a :: Type)        --                      (currently this doesn't include entry points)
  = (r :: Type)
  | r -> i j a where
  CodensityProgram i j a = Codensity AST ( AST a := j ) i

programGlobals :: forall i j a. KnownInterface i
               => CodensityProgram i j a -> Map Text SPIRV.PrimTy
programGlobals _ = Map.fromList
                 . map ( second
                            ( \ (ty, storage) -> SPIRV.Pointer storage ty )
                       )
                 $ knownInterface (Proxy @i)

data Program
        ( defs :: [ Symbol :-> Definition ] )
        ( a    :: Type                      )
      :: Type where
  Program :: ( KnownDefinitions defs )
          => UndecoratedProgram (InsertionSort defs) a -> Program defs a
