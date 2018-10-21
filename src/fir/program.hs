{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module FIR.Program
  ( Program ) 
  where

-- base
import Data.Kind(Type)
import GHC.TypeLits(Symbol)

-- fir
import Control.Monad.Indexed((:=))
import Data.Type.Bindings( BindingsMap, Binding
                         , Assignment
                         , Union, FromList
                         )
import FIR.AST(AST, Codensity, S)

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

type family CodensityProgram (i :: BindingsMap) (j :: BindingsMap) (a :: Type) :: Type where
  CodensityProgram i j a = Codensity S ( AST a := j ) i
