{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Binding where

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
import FIR.Prim.Image
  ( ImageHandle, ImageProperties(Properties), ImageFetchType )
import Math.Linear
  ( V )
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
  Variable :: [Permission] -> Type -> Binding
  Function :: [Symbol :-> Binding] -> Type -> Binding
  Image    :: ImageProperties
           -> Binding

-- shorthands
type Var ps a = 'Variable ps a
type Fun as b = 'Function as b
type Img coord res dim depth arrayness ms usage fmt
  = 'Image ( 'Properties coord res dim depth arrayness ms usage fmt )

type BindingsMap = Map Symbol Binding

type family Variadic (as :: BindingsMap) (b :: Type) = (res :: Type) where
  Variadic '[]                b = b
  Variadic ((_ ':-> a) ': as) b = BindingType a -> Variadic as b

type family BindingType (bd :: Binding) :: Type where
  BindingType (Var  _ a) = a
  BindingType (Fun as b) = Variadic as b
  BindingType (Img coord res dim _ arr _ _ _)
    = ImageHandle (ImageFetchType coord dim arr) (V 4 res)

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
