{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Type.Map where

-- base 
import Data.Type.Bool
  ( If )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

-- fir
import Data.Type.Ord
  ( POrd((:<)) )

------------------------------------------------
-- barebones type-level map functionality

infixr 4 :->
data (:->) k v = k :-> v
  deriving ( Eq, Show )

-- this synonym should only be used when we are working
-- with lists that are known to be ordered
-- (assumes keys are an instance of POrd)
type Map k v = [k :-> v]

type family Key (a :: (k :-> v)) :: k where
  Key (k ':-> _) = k

type family Value (a :: (k :-> v)) :: v where
  Value (_ ':-> v) = v

type family Keys (i :: [k :-> v]) :: [k] where
  Keys '[] = '[]
  Keys (kv ': kvs) = Key kv ': Keys kvs

type family Values (i :: [k :-> v]) :: [v] where
  Values '[] = '[]
  Values (kv ': kvs) = Value kv ': Values kvs

-- looks through the whole list always, in case the list is not sorted
type family Lookup (s :: k) (i :: [k :-> v]) :: Maybe v where
  Lookup _ '[]               = 'Nothing
  Lookup k ((k ':-> a) ': _) = 'Just a
  Lookup k (_          ': b) = Lookup k b

type family LookupKey (s :: k) (i :: [k :-> v]) :: Maybe k where
  LookupKey _ '[]               = 'Nothing
  LookupKey k ((k ':-> _) ': _) = 'Just k
  LookupKey k (_          ': b) = LookupKey k b

-- insert a key/value pair in an already-sorted map
type family Insert (s :: k) (t :: v) (i :: Map k v) :: Map k v where
  Insert k v '[]              = '[ k ':-> v ]
  Insert k v ((k ':-> a) ': _) =
    TypeError
      (     Text "Duplicate key: "
       :$$: ShowType '[ (k ':-> v), (k ':-> a) ]
      )
  Insert k v ((l ':-> a) ': b) =
    If ( k :< l )
      ( (k ':-> v) ': (l ':-> a) ': b )
      ( (l ':-> a) ': Insert k v b )

type family Union (i :: Map k v) (j :: Map k v) :: Map k v where
  Union i '[]                = i
  Union i ( (k ':-> a) ': b) = Union (Insert k a i) b

type family Delete (s :: k) (is :: Map k v) :: Map k v where
  Delete _ '[]                 = '[]
  Delete k ( (k ':-> _) ': is) = is -- assumes there are no duplicates
  Delete k ( i          ': is) = i ': Delete k is

type family Remove (i :: Map k v) (j :: Map k v) :: Map k v where
  Remove '[]                j = j
  Remove ( (k ':-> _) ': i) j = Remove i (Delete k j) 

type family InsertionSort (i :: [k :-> v]) :: Map k v where
  InsertionSort '[]              = '[]
  InsertionSort ((k ':-> v) : l) = Insert k v (InsertionSort l)

type family ZipValue (i :: [k]) (x :: v) :: Map k v where
  ZipValue '[]       _ = '[]
  ZipValue (a ': as) x = ( (a ':-> x) ': ZipValue as x )
