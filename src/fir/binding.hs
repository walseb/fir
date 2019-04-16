{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Binding
  ( Permission(Read,Write)
  , R, W, RW
  , Binding(Variable, Function)
  , Var, Fun
  , BindingsMap
  , FunctionType
  , StoragePermissions
  )
  where

-- base
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol )

-- fir
import Data.Type.Known
  ( Demotable(Demote), Known(known) )
import Data.Type.Map
  ( (:->)((:->)), Map )
import qualified SPIRV.Storage as SPIRV

------------------------------------------------------------------------------------------------
-- bindings: variables, functions, images

data Permission = Read | Write
  deriving (Eq, Show)

instance Demotable Permission where
  type Demote Permission = Permission
instance Known Permission 'Read where
  known = Read
instance Known Permission 'Write where
  known = Write

type R  = '[ 'Read  ]
type W  = '[ 'Write ]
type RW = '[ 'Read, 'Write ]

data Binding where
  Variable   :: [Permission] -> Type -> Binding
  Function   :: [Symbol :-> Binding] -> Type -> Binding

-- shorthands
type Var ps a = 'Variable ps a
type Fun as b = 'Function as b

type BindingsMap = Map Symbol Binding

type family FunctionType (as :: BindingsMap) (b :: Type) = (res :: Type) where
  FunctionType  '[]                b = b
  FunctionType  ((_ ':-> a) ': as) b = BindingType a -> FunctionType as b

-- auxiliary type family (non-exported),
-- used only in the above 'FunctionType' type family
type family BindingType (bd :: Binding) :: Type where
  BindingType (Var  _ a) = a
  BindingType (Fun as b) = FunctionType as b

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
  StoragePermissions SPIRV.Image           = RW -- default
  StoragePermissions SPIRV.StorageBuffer   = RW
