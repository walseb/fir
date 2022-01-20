{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: Data.Type.Map

Simple type-level maps (association lists).
-}

module Data.Type.Map where

-- base
import Data.Type.Bool
  ( If )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

-- fir
import Data.Type.POrd
  ( POrd(Compare, (:<)) )

------------------------------------------------
-- barebones type-level map functionality

infixr 4 :->
data (:->) k v = k :-> v
  deriving stock ( Eq, Show )

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

-- | Insert a key/value pair in an already-sorted map.
--
-- Throws an error if the key is already present.
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

-- | Insert a key/value pair in an already-sorted map.
--
-- If a value already exists with that key, it gets overwritten with the new value.
type family InsertOverwriting (s :: k) (t :: v) (i :: Map k v) :: Map k v where
  InsertOverwriting k v '[]               = '[ k ':-> v ]
  InsertOverwriting k v ((k ':-> a) ': b) = (k ':-> v) ': b
  InsertOverwriting k v ((l ':-> a) ': b) =
    If ( k :< l )
      ( (k ':-> v) ': (l ':-> a) ': b )
      ( (l ':-> a) ': InsertOverwriting k v b )

type family InsertWithAppend ( key :: k ) ( val :: v ) ( map :: Map k [v] ) :: Map k [v] where
  InsertWithAppend key val '[] = '[ key ':-> '[ val ] ]
  InsertWithAppend key val ( ( k ':-> vals ) ': kvs ) =
    InsertWithAppendWithComparison key val k vals kvs ( Compare key k )

type family InsertWithAppendWithComparison ( key :: k ) ( val :: v ) ( key' :: k ) ( vals :: [v] ) ( rest :: Map k [v] ) ( comp :: Ordering ) :: Map k [v] where
  InsertWithAppendWithComparison key val key' vals rest LT =
    ( ( key ':-> '[val] ) ': ( key' ':-> vals ) ': rest )
  InsertWithAppendWithComparison key val key' vals rest EQ =
    ( ( key ':-> ( val ': vals ) ) ': rest )
  InsertWithAppendWithComparison key val key' vals rest GT =
    ( ( key' ':-> vals ) ': InsertWithAppend key val rest )


type family Union (i :: Map k v) (j :: Map k v) :: Map k v where
  Union i '[]                = i
  Union i ( (k ':-> a) ': b) = Union (Insert k a i) b

type family UnionWithAppend (i :: Map k [v]) (j :: Map k [v]) :: Map k [v] where
  UnionWithAppend i '[] = i
  UnionWithAppend '[] i = i
  UnionWithAppend ((k1 ':-> '[]) ': kvs1) i = UnionWithAppend kvs1 i
  UnionWithAppend ((k1 ':-> val ': vals1) ': kvs1) ( ( k2 ':-> vals2 ) ': kvs2 ) =
    UnionWithAppend  ((k1 ':-> vals1) ': kvs1) (InsertWithAppendWithComparison k1 val k2 vals2 kvs2 (Compare k1 k2))

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

type family LookupAndLookup (key1 :: k1) (key2 :: k2) (map :: Map k1 (Map k2 v)) :: Maybe v where
  LookupAndLookup key1 key2 map = LookupAndLookupFrom key2 (Lookup key1 map)

type family LookupAndLookupFrom (key2 :: k2) (lk :: Maybe (Map k2 v)) :: Maybe b where
  LookupAndLookupFrom _    Nothing    = Nothing
  LookupAndLookupFrom key2 (Just map) = Lookup key2 map

type family InsertWithInsert (key1 :: k1) (key2 :: k2) (val :: v) (map :: Map k1 (Map k2 v)) :: Map k1 (Map k2 v) where
  InsertWithInsert key1 key2 val '[] = '[ key1 ':-> '[ key2 ':-> val ] ]
  InsertWithInsert key1 key2 val ((key1 ':-> map) ': rest) = ( key1 ':-> Insert key2 val map ) ': rest
  InsertWithInsert key1 key2 val ((key ':-> map) ': rest) =
    If (key1 :< key)
      ((key1 ':-> '[ key2 ':-> val ]) ': (key ':-> map) ': rest)
      ((key ':-> map) ': InsertWithInsert key1 key2 val rest)
