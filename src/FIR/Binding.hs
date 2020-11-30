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
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Known
  ( Demotable(Demote), Known(known) )
import Data.Type.List
  ( Elem )
import Data.Type.Map
  ( (:->)((:->)), Map )
import FIR.AST.Type
  ( AugType(Val,(:-->)), UnderlyingType )
import qualified SPIRV.Decoration as SPIRV
  ( Decoration(NonReadable, NonWritable) )
import qualified SPIRV.Storage    as SPIRV

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

type family StoragePermissions (storage :: SPIRV.StorageClass) (decs :: [SPIRV.Decoration Nat]) :: Permissions where
  StoragePermissions SPIRV.UniformConstant  _    = R
  StoragePermissions SPIRV.Input            _    = R
  StoragePermissions SPIRV.Uniform          _    = R
  StoragePermissions SPIRV.Output           _    = W
  StoragePermissions SPIRV.Workgroup        decs = PermissionsFromDecorations decs
  StoragePermissions SPIRV.CrossWorkgroup   decs = PermissionsFromDecorations decs
  StoragePermissions SPIRV.Private          decs = PermissionsFromDecorations decs
  StoragePermissions SPIRV.Function         _    = RW -- default value, specific functions can specialise this
  StoragePermissions SPIRV.Generic          decs = PermissionsFromDecorations decs
  StoragePermissions SPIRV.PushConstant     _    = R
  StoragePermissions SPIRV.AtomicCounter    decs = PermissionsFromDecorations decs
  StoragePermissions SPIRV.Image            decs = PermissionsFromDecorations decs
  StoragePermissions SPIRV.StorageBuffer    decs = RW
  StoragePermissions ('SPIRV.RayStorage SPIRV.HitAttribute ) decs = RW
  StoragePermissions ('SPIRV.RayStorage SPIRV.ShaderRecordBuffer ) _ = R
  StoragePermissions ('SPIRV.RayStorage _ ) decs = PermissionsFromDecorations decs


{-
TODO:
Variables with storage class HitAttribute:

They can be written to only in Intersection execution model,
and read from only in AnyHit and ClosestHit execution models.
-}

type family PermissionsFromDecorations (decs :: [SPIRV.Decoration Nat]) :: Permissions where
  PermissionsFromDecorations decs =
    PermissionsFromDecorations'
      ( SPIRV.NonReadable `Elem` decs )
      ( SPIRV.NonWritable `Elem` decs )

type family PermissionsFromDecorations' (nonReadable :: Bool) (nonWritable :: Bool) :: Permissions where
  PermissionsFromDecorations' True  True  = '[]
  PermissionsFromDecorations' True  False = W
  PermissionsFromDecorations' False True  = R
  PermissionsFromDecorations' False False = RW
