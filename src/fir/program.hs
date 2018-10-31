{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Program
  ( Program
  , programGlobals
  )
  where

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
import Data.Type.Bindings( BindingsMap, Binding
                         , Assignment
                         , Union, FromList
                         )
import FIR.AST(AST)
import FIR.PrimTy(KnownVars(knownVars))
import qualified SPIRV.PrimTy as SPIRV

type family Program
              ( i :: [ Assignment Symbol Binding ] )
              ( j :: [ Assignment Symbol Binding ] )
              ( a :: Type )
              :: Type where
  Program i j a 
      = CodensityProgram
          ( FromList i ) 
          ( Union (FromList j) (FromList i) )
          a

type family CodensityProgram (i :: BindingsMap) (j :: BindingsMap) (a :: Type) = (r :: Type) | r -> i j a where
  CodensityProgram i j a = Codensity AST ( AST a := j ) i

programGlobals :: forall i j a. KnownVars i
               => CodensityProgram i j a -> Map Text SPIRV.PrimTy
programGlobals _ = Map.fromList . map (second fst) $ knownVars (Proxy @i)