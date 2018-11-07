{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Labels where

-- base
import qualified GHC.Stack
import GHC.TypeLits(Symbol, KnownSymbol)
import Prelude(Bool(True))

-- fir
import Control.Monad.Indexed(Codensity, (:=))
import Control.Type.Optic(Optic(Name))
import Data.Type.Map(Insert)
import FIR.AST(AST)
import FIR.Binding(BindingsMap, Var, R, RW)
import FIR.Instances.Bindings(ValidDef,Get, Put)
import FIR.Instances.Codensity(def, get, put)
import FIR.PrimTy(PrimTy)

-- short type synonym helpful for disambiguating
-- e.g. : [...] @(S _ _)
type S a i = Codensity AST (AST a := i) i

-- label 'k' for something of type 'a'
data Label k a = Label

data LabelAction
  = AssignLabel
  | GetLabel Symbol BindingsMap

class IsLabel k a (t :: LabelAction) v
    | v -> a, t v -> k, v k -> t
    where
  fromLabel :: GHC.Stack.HasCallStack => v

instance ( KnownSymbol k, PrimTy a
         , t ~ AssignLabel
         )
      => IsLabel k a t (Label k a) where
  fromLabel = Label

instance ( KnownSymbol k, PrimTy a
         , a ~ Get k i
         , r ~ (AST a := i)
         , t ~ 'GetLabel k i
         )
      => IsLabel k a t (Codensity AST r i) where
  fromLabel = get @(Name k)


infixr 4 #=
infixr 4 #=!
infixr 4 .=

(#=) :: forall a k i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , PrimTy a
        , ValidDef k i ~ 'True
        )
     => Label k a
     -> AST a
     -> Codensity AST (AST a := Insert k (Var RW a) i) i
_ #= a = def @k @RW a

(#=!) :: forall a k i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , PrimTy a
        , ValidDef k i ~ 'True
        )
     => Label k a
     -> AST a
     -> Codensity AST (AST a := Insert k (Var R a) i) i
_ #=! a = def @k @R a

(.=) :: forall a k i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , a ~ Put k i
        , PrimTy a
        )
     => Label k a
     -> AST a
     -> Codensity AST (AST () := i) i
_ .= a = put @(Name k) a
