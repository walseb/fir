{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Binding

Keeping track of variable and function bindings at the type-level.
-}

module FIR.Binding
  ( Permission(Read,Write), Permissions
  , R, W, RW
  , Binding(Variable, Function)
  , Var, Fun
  , BindingsMap
  , FunctionType, FunctionAugType, FunArgTypes
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
import FIR.AST.Type
  ( AugType(Val,(:-->)), UnderlyingType )
import qualified SPIRV.Storage as SPIRV

------------------------------------------------------------------------------------------------
-- bindings: variables, functions, images

data Permission = Read | Write
  deriving stock (Eq, Show)

type Permissions = [Permission]

instance Demotable Permission where
  type Demote Permission = Permission
instance Known Permission 'Read where
  known = Read
instance Known Permission 'Write where
  known = Write

type R  = '[ 'Read  ]
type W  = '[ 'Write ]
type RW = '[ 'Read, 'Write ]

-- | A binding consists of either:
--   * a variable of given type and permissions (readable/writable), or
--   * a function, with (named) arguments of given types, and given return type.
data Binding where
  Variable   :: Permissions -> Type -> Binding
  Function   :: [Symbol :-> Binding] -> Type -> Binding

-- shorthands
type Var ps a = 'Variable ps a
type Fun as b = 'Function as b

type BindingsMap = Map Symbol Binding

type FunctionType as b = UnderlyingType (FunctionAugType as b)

type family FunArgTypes (as :: BindingsMap) :: [Type] where
  FunArgTypes '[] = '[]
  FunArgTypes ((_ ':-> a) ': as) = UnderlyingType (BindingAugType a) ': FunArgTypes as

type family FunctionAugType (as :: BindingsMap) (b :: Type) :: AugType where
  FunctionAugType '[]                b = Val b
  FunctionAugType ((_ ':-> a) ': as) b = BindingAugType a :--> FunctionAugType as b

-- auxiliary type family (non-exported),
-- used only in the above 'FunctionType' type family
type family BindingAugType (bd :: Binding) :: AugType where
  BindingAugType (Var  _ a) = Val a
  BindingAugType (Fun as b) = FunctionAugType as b

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
