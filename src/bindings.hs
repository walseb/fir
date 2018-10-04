{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Bindings where

-- base 
import Data.Kind(Type)
import Data.Type.Bool(If, type (&&))
import Data.Type.Equality(type (==))
import GHC.TypeLits(Symbol, CmpSymbol, TypeError, ErrorMessage(Text, ShowType, type (:<>:), type (:$$:)))

------------------------------------------------
-- barebones type-level map functionality

type family (:++) as bs where
  (:++) '[]       bs = bs
  (:++) (a ': as) bs = a ': (as :++ bs)

infixr 4 :->
data Assignment k v = k :-> v
type BindingsMap = [Assignment Symbol Binding]

type family Lookup (k :: Symbol) (i :: BindingsMap) :: Maybe Binding where
  Lookup _ '[]               = 'Nothing
  Lookup k ((k ':-> a) ': b) = 'Just a
  Lookup k (_          ': b) = Lookup k b

-- insert a key/value pair in an already-sorted map
type family Insert (k :: Symbol) (v :: Binding) (i :: BindingsMap) :: BindingsMap where
  Insert k v '[]               = '[ k ':-> v ]
  Insert k v ((k ':-> a) ': b) = TypeError
      (     Text "Duplicate key " 
       :<>: ShowType k 
       :<>: Text " in list of bindings:"
       :$$: ShowType ((k ':-> v) ': (k ':-> a) ': b)
      )
  Insert k v ((l ':-> a) ': b) = If (CmpSymbol k l == 'LT)
                                    ((k ':-> v) ': (l ':-> a) ': b)
                                    ((l ':-> a) ': Insert k v b)

type family Union (i :: BindingsMap) (j :: BindingsMap) :: BindingsMap where
  Union i '[] = i
  Union i ( (k ':-> a) ': b) = Union (Insert k a i) b

type family Delete (k :: Symbol) (i :: BindingsMap) :: BindingsMap where
  Delete k '[] = '[]
  Delete k ( (k ':-> _) ': i) = i -- assumes there are no duplicates
  Delete k ( _          ': i) = Delete k i

type family Remove (i :: BindingsMap) (j :: BindingsMap) :: BindingsMap where
  Remove '[]                j = j
  Remove ( (k ':-> _) ': i) j = Remove i (Delete k j) 

type family FromList (i :: BindingsMap) :: BindingsMap where
  FromList '[] = '[]
  FromList ((k ':-> v) : l) = Insert k v (FromList l)



data Permission = Read | Write
  deriving (Eq, Show)

type family Elem x as where
  Elem x '[]       = 'False
  Elem x (x ': _ ) = 'True
  Elem x (_ ': as) = Elem x as

data Binding where
  Var  :: [Permission] -> Type -> Binding
  Fun  :: [Assignment Symbol Binding] -> Type -> Binding

type family Variadic (as :: BindingsMap) (b :: Type) = (res :: Type) where
  Variadic '[]                b = b
  Variadic ((_ ':-> a) ': as) b = BindingType a -> Variadic as b

type family BindingType (bd :: Binding) :: Type where
  BindingType ('Var  _ a) = a
  BindingType ('Fun as b) = Variadic as b

------------------------------------------------------------------------------------------------
-- constraints for 'get'

type family Get (k :: Symbol) (i :: BindingsMap) :: Type where
  Get k i = Get' k i (Lookup k i)

type family Get' (k :: Symbol) (i :: BindingsMap) (mbd :: Maybe Binding) :: Type where
  Get' k i 'Nothing = TypeError
    (      Text "'get': no binding named " :<>: ShowType k :<>: Text " is in scope."
      :$$: Text "In-scope bindings are:"
      :$$: ShowType i
    )
  Get' k _ ('Just ('Var p a)) = Get'' k ('Var p a) (Elem 'Read p)
  Get' _ _ ('Just bd) = BindingType bd -- functions

type family Get'' (k :: Symbol) (bd :: Binding) (readable :: Bool) :: Type where
  Get'' k _ 'False = TypeError
    ( Text "'get': variable named " :<>: ShowType k :<>: Text " is not readable." )
  Get'' _ bd 'True = BindingType bd

------------------------------------------------------------------------------------------------
-- constraints for 'put'

type family Put (k :: Symbol) (i :: BindingsMap) :: Type where 
  Put k i = PutAt k i (Lookup k i)

type family PutAt (k :: Symbol) (i :: BindingsMap) (lookup :: Maybe Binding) :: Type where
  PutAt k i 'Nothing = TypeError
    (      Text "'put': no binding named " :<>: ShowType k :<>: Text " is in scope."
      :$$: Text "To define a new binding, use 'def'."
      :$$: Text "In-scope bindings are:"
      :$$: ShowType i
    )
  PutAt k i ('Just ('Fun as b)) = TypeError
    (      Text "'put': function bound at name " :<>: ShowType k :<>: Text ": " :<>: ShowType ('Fun as b) :<>: Text "."
      :$$: Text "Use 'fundef' to define a function."
    )
  PutAt k i ('Just ('Var ps a)) = PutIfWritable k a (Elem 'Write ps)

type family PutIfWritable (k :: Symbol) (a :: Type) (writable :: Bool) :: Type where
  PutIfWritable _ a 'True  = a
  PutIfWritable k _ 'False = TypeError 
    ( Text "'put': variable " :<>: ShowType k :<>: Text " is not writable." )

------------------------------------------------------------------------------------------------
-- constraints for 'def'

type family CanDef (k :: Symbol) (i :: BindingsMap) :: Bool where
  CanDef k i = NotAlreadyDefined k i (Lookup k i)

type family NotAlreadyDefined (k :: Symbol) (i :: BindingsMap) (lookup :: Maybe Binding) :: Bool where
  NotAlreadyDefined _ _ 'Nothing  = 'True
  NotAlreadyDefined k i ('Just _) = TypeError
    (      Text "'def': a binding by the name " :<>: ShowType k :<>: Text " already exists."
      :$$: Text "In scope bindings are:"
      :$$: ShowType i
    )

------------------------------------------------------------------------------------------------
-- constraints for 'fundef'

type family CanFunDef 
            (k :: Symbol) (i :: BindingsMap) (j :: BindingsMap) (l :: BindingsMap) :: Bool where
  CanFunDef k i j l = CanFunDef' k i j (Lookup k i) (Lookup k j) (NotHigherOrder k j (Remove i (Remove j l)))

type family NotHigherOrder (k :: Symbol) (j :: BindingsMap) (l :: BindingsMap) :: Bool where
  NotHigherOrder k j l = NotHigherOrder' k j l j l

type family NotHigherOrder'
   (k :: Symbol) (j :: BindingsMap) (l :: BindingsMap) (j_rec :: BindingsMap) (l_rec :: BindingsMap) :: Bool where
  NotHigherOrder' _ _ _ '[]                          '[]   = 'True
  NotHigherOrder' k j l ((_ ':-> 'Var _ _) ': j_rec) l_rec = NotHigherOrder' k j l j_rec l_rec
  NotHigherOrder' k j _ ((v ':-> 'Fun _ _) ': _    ) _     = TypeError
    (     Text "'fundef': forbidden higher order argument " :<>: ShowType v :<>: Text ","
     :$$: Text "in function definition for " :<>: ShowType k :<>: Text ","
     :$$: Text "when trying to abstract over:"
     :$$: ShowType j
     :$$: Text "Function definitions can only abstract over variables, not over functions."
    )
  NotHigherOrder' k j l j_rec ((_ ':-> 'Var _ _) ': l_rec) = NotHigherOrder' k j l j_rec l_rec
  NotHigherOrder' k j l _     ((v ':-> 'Fun _ _) ': _    ) = TypeError
    (     Text "'fundef': unexpected nested function definition inside function " :<>: ShowType k :<>: Text ":"
     :$$: Text "local name " :<>: ShowType v :<>: Text "binds a function."
     :$$: Text "Local bindings for " :<>: ShowType k :<>: Text "are:"
     :$$: ShowType l
    )


type family CanFunDef' 
    (k :: Symbol)
    (i :: BindingsMap)
    (j :: BindingsMap)
    (mbd1 :: Maybe Binding)
    (mbd2 :: Maybe Binding)
    (nho :: Bool) 
    :: Bool where
  CanFunDef' k i j ('Just _) _ _ = TypeError
    (     Text "'fundef': cannot define a new function with name " :<>: ShowType k :<>: Text ";"
     :$$: Text "that name is already in scope. In scope bindings are:"
     :$$: ShowType i
     :$$: Text "Locally bound variables are:"
     :$$: ShowType j
    )
  CanFunDef' k i j _ ('Just _) _ = TypeError
    (     Text "'fundef': cannot define a new function with name " :<>: ShowType k :<>: Text ";"
     :$$: Text "that name is locally bound as an argument to the function."
     :$$: Text "In scope bindings are:"
     :$$: ShowType i
     :$$: Text "Locally bound variables are:"
     :$$: ShowType j
    )
  CanFunDef' _ _ _ 'Nothing 'Nothing 'True = 'True
