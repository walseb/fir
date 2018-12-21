{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
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
import Data.Kind(Type)
import qualified GHC.Stack
import GHC.TypeLits(Symbol, KnownSymbol)
import Prelude(Bool(True))

-- fir
import Control.Monad.Indexed(Codensity, (:=))
import Control.Type.Optic(Optic, Name)
import Data.Type.Map(Insert)
import FIR.AST(AST)
import FIR.Binding(BindingsMap, Var, R, RW)
import FIR.Instances.Bindings(ValidDef,Get, Put)
import FIR.Instances.Codensity(def, use, assign, modifying)
import FIR.Prim.Singletons(PrimTy)

data Label (k :: Symbol) (a :: Type) = Label

data LabelUsage
  = Symbolic
  | Use Symbol BindingsMap

class IsLabel k a (usage :: LabelUsage) v
    | v -> a, usage v -> k, v k -> usage
    where
  fromLabel :: GHC.Stack.HasCallStack => v

instance ( KnownSymbol k
         , PrimTy a
         , usage ~ Symbolic
         )
       => IsLabel k a usage (Label k a) where
  fromLabel = Label @k @a

instance ( KnownSymbol k, PrimTy a
         , a ~ Get k i
         , r ~ (AST a := i)
         , usage ~ 'Use k i
         )
      => IsLabel k a usage (Codensity AST r i) where
  fromLabel = use @(Name k :: Optic '[] i a)


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
_ .= a = assign @(Name k :: Optic '[] i a) a

(%=) :: forall a k i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , a ~ Put k i
        , a ~ Get k i
        )
     => Label k a
     -> (AST a -> AST a)
     -> Codensity AST (AST () := i) i
_ %= f = modifying @(Name k :: Optic '[] i a) f
