{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Program
  ( Procedure, Program, CodensityProgram
  , programGlobals
  )
  where

-- base
import Control.Arrow(second)
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))

-- containers
import Data.Map(Map)
import qualified Data.Map as Map

-- text-utf8
import Data.Text(Text)

-- fir
import Control.Monad.Indexed((:=), Codensity)
import Data.Type.Map (Union, InsertionSort)
import FIR.AST(AST)
import FIR.Binding(BindingsMap)
import FIR.Prim.Singletons(KnownInterface(knownInterface))
import qualified SPIRV.PrimTy as SPIRV

--------------------------------------------------------------------------
-- type (family) synonyms, all wrappers around main internal representation
-- Codensity AST (AST a := j) i

type Procedure (a :: Type) (i :: BindingsMap) (j :: BindingsMap)
  = Codensity AST (AST a := j) i

type family Program 
    ( i :: BindingsMap ) -- available data at the start (e.g. uniforms)
    ( j :: BindingsMap ) -- top-level functions of the program (currently this doesn't include entry points)
    ( a :: Type )
  = ( r :: Type ) where
  Program i j a = Program' (InsertionSort i) (InsertionSort j) a

type family Program' 
    ( i :: BindingsMap )
    ( j :: BindingsMap )
    ( a :: Type )
  = ( r :: Type ) where
  Program' i j a = CodensityProgram i ( Union j i ) a

type family CodensityProgram
    (i :: BindingsMap)
    (j :: BindingsMap)
    (a :: Type)
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