{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module FIR.Binding where

-- base
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import GHC.TypeLits(Symbol)

-- fir
import Data.Type.Map((:->)((:->)), Map)

------------------------------------------------------------------------------------------------
-- bindings: variables, functions

data Permission = Read | Write
  deriving (Eq, Show)

type R  = '[ 'Read  ]
type W  = '[ 'Write ]
type RW = '[ 'Read, 'Write ]

data Binding where
  Variable :: [Permission] -> Type -> Binding
  Uniform  :: Type -> Binding
  Function :: [Symbol :-> Binding] -> Type -> Binding

type Var ps a    = 'Variable ps a
type Fun as b    = 'Function as b
type Unif a      = 'Uniform a
type BindingsMap = Map Symbol Binding

type family Variadic (as :: BindingsMap) (b :: Type) = (res :: Type) where
  Variadic '[]                b = b
  Variadic ((_ ':-> a) ': as) b = BindingType a -> Variadic as b

type family BindingType (bd :: Binding) :: Type where
  BindingType (Var  _ a) = a
  BindingType (Unif a  ) = a
  BindingType (Fun as b) = Variadic as b

------------------------------------------------------------------------------------------------
-- singletony stuff

class KnownPermission (p :: Permission) where
  permission :: Proxy p -> Permission

instance KnownPermission 'Read where
  permission _ = Read
instance KnownPermission 'Write where
  permission _ = Write

class KnownPermissions (ps :: [Permission]) where
  permissions :: Proxy ps -> [Permission]

instance KnownPermissions '[] where
  permissions _ = []
instance (KnownPermission p, KnownPermissions ps)
      => KnownPermissions ( p ': ps ) where
  permissions _ = permission (Proxy @p) : permissions (Proxy @ps)
