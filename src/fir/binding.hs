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
import qualified SPIRV.Storage as SPIRV

------------------------------------------------------------------------------------------------
-- bindings: variables, functions

data Permission = Read | Write
  deriving (Eq, Show)

type R  = '[ 'Read  ]
type W  = '[ 'Write ]
type RW = '[ 'Read, 'Write ]

data Binding where
  Variable :: [Permission] -> Type -> Binding
  Function :: [Symbol :-> Binding] -> Type -> Binding

type Var ps a    = 'Variable ps a
type Fun as b    = 'Function as b
type BindingsMap = Map Symbol Binding

type family Variadic (as :: BindingsMap) (b :: Type) = (res :: Type) where
  Variadic '[]                b = b
  Variadic ((_ ':-> a) ': as) b = BindingType a -> Variadic as b

type family BindingType (bd :: Binding) :: Type where
  BindingType (Var  _ a) = a
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

------------------------------------------------------------------------------------------------
-- relation to SPIRV storage classes

type family StoragePermissions (storage :: SPIRV.StorageClass) where
  StoragePermissions SPIRV.UniformConstant = R
  StoragePermissions SPIRV.Input           = R
  StoragePermissions SPIRV.Uniform         = R
  StoragePermissions SPIRV.Output          = W
  StoragePermissions SPIRV.Workgroup       = RW
  StoragePermissions SPIRV.CrossWorkgroup  = RW
  StoragePermissions SPIRV.Private         = RW
  StoragePermissions SPIRV.Function        = RW -- default value, specific functions can specialise this
  StoragePermissions SPIRV.Generic         = RW
  StoragePermissions SPIRV.PushConstant    = R
  StoragePermissions SPIRV.AtomicCounter   = RW
  StoragePermissions SPIRV.Image           = R
  StoragePermissions SPIRV.StorageBuffer   = RW