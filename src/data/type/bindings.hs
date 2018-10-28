{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Bindings where

-- base 
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import Data.Type.Bool(If)
import Data.Type.Equality(type (==))
import GHC.TypeLits( Symbol, CmpSymbol
                   , TypeError, ErrorMessage(..)
                   )

------------------------------------------------
-- barebones type-level map functionality

data Assignment k v = Assignment k v
type BindingsMap = [Assignment Symbol Binding]

infixr 4 :->
type k :-> v = 'Assignment k v

type family Lookup (k :: Symbol) (i :: BindingsMap) :: Maybe Binding where
  Lookup _ '[]              = 'Nothing
  Lookup k ((k :-> a) ': b) = 'Just a
  Lookup k (_         ': b) = Lookup k b

-- insert a key/value pair in an already-sorted map
type family Insert (k :: Symbol) (v :: Binding) (i :: BindingsMap) :: BindingsMap where
  Insert k v '[]              = '[ k :-> v ]
  Insert k v ((k :-> a) ': b) = TypeError
      (     Text "Duplicate key: "
       :$$: ShowType '[ (k :-> v), (k :-> a) ]
      )
  Insert k v ((l :-> a) ': b) = If (CmpSymbol k l == 'LT)
                                   ((k :-> v) ': (l :-> a) ': b)
                                   ((l :-> a) ': Insert k v b)

type family Union (i :: BindingsMap) (j :: BindingsMap) :: BindingsMap where
  Union i '[]               = i
  Union i ( (k :-> a) ': b) = Union (Insert k a i) b

type family Delete (k :: Symbol) (i :: BindingsMap) :: BindingsMap where
  Delete k '[]               = '[]
  Delete k ( (k :-> _) ': i) = i -- assumes there are no duplicates
  Delete k ( _         ': i) = Delete k i

type family Remove (i :: BindingsMap) (j :: BindingsMap) :: BindingsMap where
  Remove '[]               j = j
  Remove ( (k :-> _) ': i) j = Remove i (Delete k j) 

type family FromList (i :: BindingsMap) :: BindingsMap where
  FromList '[]             = '[]
  FromList ((k :-> v) : l) = Insert k v (FromList l)



data Permission = Read | Write
  deriving (Eq, Show)

type R  = '[ 'Read  ]
type W  = '[ 'Write ]
type RW = '[ 'Read, 'Write ]

type family Elem x as where
  Elem x '[]       = 'False
  Elem x (x ': _ ) = 'True
  Elem x (_ ': as) = Elem x as

data Binding where
  Variable :: [Permission] -> Type -> Binding
  Function :: [Assignment Symbol Binding] -> Type -> Binding

type Var ps a = 'Variable ps a
type Fun as b = 'Function as b

type family Variadic (as :: BindingsMap) (b :: Type) = (res :: Type) where
  Variadic '[]               b = b
  Variadic ((_ :-> a) ': as) b = BindingType a -> Variadic as b

type family BindingType (bd :: Binding) :: Type where
  BindingType (Var  _ a) = a
  BindingType (Fun as b) = Variadic as b


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
instance (KnownPermission p, KnownPermissions ps) => KnownPermissions ( p ': ps ) where
  permissions _ = permission (Proxy @p) : permissions (Proxy @ps)