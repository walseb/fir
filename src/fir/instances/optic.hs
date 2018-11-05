{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Instances.Optic where

-- base
import Data.Kind(Type)
import GHC.TypeLits( Symbol, KnownSymbol
                   , TypeError
                   , ErrorMessage(Text, ShowType, (:<>:), (:$$:))
                   )
import GHC.TypeNats( Nat, KnownNat
                   , CmpNat
                   , type (-)
                   )

-- fir
import Control.Monad.Indexed((:=))
import Control.Type.Optic( Optic(..)
                         , Gettable(Get)
                         , Settable(Set)
                         , RequiredIndices
                         )
import Data.Type.Map ((:->)((:->)), Value
                     , Lookup, LookupKey
                     , type (:++:)
                     )
import FIR.Binding( BindingsMap
                  , Var, R, W, RW
                  )
import qualified FIR.Instances.Binding as Binding
import FIR.PrimTy(Array, RuntimeArray, Struct)
import Math.Linear(V,M)

----------------------------------------------------------------------
-- for testing

-- to force evaluation in GHCi
type family Id (a :: k) :: k
type instance Id (a :: k) = a

type I = '[ "x" ':-> Var W (M 3 4 Int)
          , "y" ':-> Var R (Array 4 Float)
          , "z" ':-> Var RW
                      ( Struct 
                         '[ "f1" ':-> Float
                          , "f2" ':-> Array 17 (M 3 4 Double)
                          , "f3" ':-> Struct '[ "x" ':-> RuntimeArray Word
                                              , "l" ':-> Bool
                                              ]
                          , "f4" ':-> Float
                          ]
                      )
          , "w" ':-> Var RW
                      ( RuntimeArray
                        ( RuntimeArray
                            ( Struct '[ "m" ':-> Int
                                      , "n" ':-> Array 9 Word
                                      ]
                            )
                        )
                      )
          ]

----------------------------------------------------------------------

type family VariadicList
              ( as :: [Type]      )
              ( b  :: Type        )
              ( i  :: BindingsMap )
            = ( r  :: Type        )
            | r -> as i b  where
  VariadicList '[]       b i = (b := i) i
  VariadicList (a ': as) b i = a -> VariadicList as b i

----------------------------------------------------------------------

type Getter g i = VariadicList (RequiredIndices g) (Get g i) i

-- bindings
instance KnownSymbol k
      => Gettable (Name k) (i :: BindingsMap) where
  type Get (Name k) i = Binding.Get k i

instance Gettable (Index n) (i :: [Symbol :-> v]) where
  type Get (Index n) i
    = TypeError (    Text "get: cannot get a binding using a numeric index."
                :$$: Text "Use the binding's name instead."
                )

-- arrays
instance (KnownNat n, KnownNat i)
      => Gettable (Index i) (Array n a) where
  type Get (Index i) (Array n a)
    = IfLT (CmpNat i n)
        a
        (     Text "get: array index "
         :<>: ShowType i
         :<>: Text " is out of bounds."
         :$$: Text "Array size is "
         :<>: ShowType n :<>: Text "."
         :$$: Text "Note: indexing starts from 0."
        )

instance KnownNat i
      => Gettable (Index i) (RuntimeArray a) where
  type Get (Index i) (RuntimeArray a) = a

instance Gettable AnIndex (Array l a) where
  type Get AnIndex (Array l a) = a

instance Gettable AnIndex (RuntimeArray a) where
  type Get AnIndex (RuntimeArray a) = a

instance Gettable (Name k) (Array l a) where
  type Get (Name k) (Array l a)
    = TypeError (    Text "get: attempt to access array element \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                )

instance Gettable (Name k) (RuntimeArray a) where
  type Get (Name k) (RuntimeArray a)
    = TypeError (    Text "get: attempt to access run-time array \
                          \element using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                )

-- structs
instance KnownSymbol k
       => Gettable (Name k) (Struct as) where
  type Get (Name k) (Struct as)
    = StructElemFromName (Text "get: ") k as (Lookup k as)

instance KnownNat n => Gettable (Index n) (Struct as) where
  type Get (Index n) (Struct as)
    = Value (StructElemFromIndex (Text "get: ") n as n as)

instance Gettable AnIndex (Struct as) where
  type Get AnIndex (Struct as)
    = TypeError (    Text "get: attempt to access struct element \
                          \using run-time index."
                :$$: Text "Structs can only be accessed using \
                          \compile-time indices or field names."
                )

-- vectors
instance KnownNat i
      => Gettable (Index i) (V n a) where
  type Get (Index i) (V n a)
    = IfLT (CmpNat i n)
        a
        (     Text "get: vector index "
         :<>: ShowType i
         :<>: Text " is out of bounds."
         :$$: Text "Vector dimension is "
         :<>: ShowType n :<>: Text "."
         :$$: Text "Note: indexing starts from 0."
        )

instance Gettable AnIndex (V n a) where
  type Get AnIndex (V n a) = a

instance Gettable (Name k) (V n a) where
  type Get (Name k) (V n a)
    = TypeError (    Text "get: attempt to access vector element \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )

-- matrices
instance KnownNat i
      => Gettable (Index i) (M m n a) where
    type Get (Index i) (M m n a)
      = IfLT (CmpNat i n)
          (V m a)
          (     Text "get: matrix column index "
           :<>: ShowType i
           :<>: Text " is out of bounds."
           :$$: Text "This matrix has "
           :<>: ShowType m :<>: Text " rows, "
           :<>: ShowType n :<>: Text " columns."
           :$$: Text "Note: indexing starts from 0."
          )

instance Gettable AnIndex (M m n a) where
  type Get AnIndex (M m n a)
    = V m a

instance Gettable (Name k) (M m n a) where
  type Get (Name k) (M m n a)
    = TypeError (    Text "get: attempt to access matrix column \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )

-- composition
instance ( Gettable g1 a
         , Gettable g2 (Get g1 a)
         ) => Gettable (g1 :.: g2) a where
  type Get (g1 :.: g2) a = Get g2 (Get g1 a)

-- products
instance (Gettable g1 (Struct as), Gettable g2 (Struct as))
      => Gettable (g1 :&: g2) (Struct as) where
  type Get (g1 :&: g2) (Struct as)
    = Struct (Fields (Text "get :") (g1 :&: g2) as 'True)
    -- this flag means we allow overlap for getters ^^^^

-- no equalisers for getters
instance Gettable (Diag g) a where
  type Get (Diag g) a
    = TypeError ( Text "get: cannot use diagonal optic as a getter." )

----------------------------------------------------------------------
-- setters

type Setter l i = VariadicList (RequiredIndices l :++: '[Set l i]) () i

-- bindings
instance KnownSymbol k
      => Settable (Name k) (i :: BindingsMap) where
  type Set (Name k) i = Binding.Put k i

instance Settable (Index n) (i :: [Symbol :-> v]) where
  type Set (Index n) i
    = TypeError (    Text "set: cannot set binding using a numeric index."
                :$$: Text "Use the binding's name instead."
                )

-- arrays
instance (KnownNat n, KnownNat i)
      => Settable (Index i) (Array n a) where
  type Set (Index i) (Array n a)
    = IfLT (CmpNat i n)
        a
        (     Text "set: array index "
         :<>: ShowType i
         :<>: Text " is out of bounds."
         :$$: Text "Array size is "
         :<>: ShowType n :<>: Text "."
         :$$: Text "Note: indexing starts from 0."
        )

instance KnownNat i
      => Settable (Index i) (RuntimeArray a) where
  type Set (Index i) (RuntimeArray a) = a

instance Settable AnIndex (Array l a) where
  type Set AnIndex (Array l a) = a

instance Settable AnIndex (RuntimeArray a) where
  type Set AnIndex (RuntimeArray a) = a
  
instance Settable (Name k) (Array l a) where
  type Set (Name k) (Array l a)
    = TypeError (    Text "set: attempt to update array element \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                )

instance Settable (Name k) (RuntimeArray a) where
  type Set (Name k) (RuntimeArray a)
    = TypeError (    Text "set: attempt to update run-time array \
                          \element using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                )

-- structs
instance KnownSymbol k
       => Settable (Name k) (Struct as) where
  type Set (Name k) (Struct as)
    = StructElemFromName (Text "put: ") k as (Lookup k as)

instance KnownNat n => Settable (Index n) (Struct as) where
  type Set (Index n) (Struct as)
    = Value (StructElemFromIndex (Text "put: ") n as n as)

instance Settable AnIndex (Struct as) where
  type Set AnIndex (Struct as)
    = TypeError (    Text "set: attempt to set struct element \
                          \using run-time index."
                :$$: Text "Structs can only be modified using \
                          \compile-time indices or field names."
                )

-- vectors
instance KnownNat i
      => Settable (Index i) (V n a) where
  type Set (Index i) (V n a)
    = IfLT (CmpNat i n)
        a
        (     Text "set: vector index "
         :<>: ShowType i
         :<>: Text " is out of bounds."
         :$$: Text "Vector dimension is "
         :<>: ShowType n :<>: Text "."
         :$$: Text "Note: indexing starts from 0."
        )

instance Settable AnIndex (V n a) where
  type Set AnIndex (V n a) = a

instance Settable (Name k) (V n a) where
  type Set (Name k) (V n a)
    = TypeError (    Text "set: attempt to update vector element \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )

-- matrices
instance KnownNat i
      => Settable (Index i) (M m n a) where
    type Set (Index i) (M m n a)
      = IfLT (CmpNat i n)
          (V m a)
          (     Text "set: matrix column index "
           :<>: ShowType i
           :<>: Text " is out of bounds."
           :$$: Text "This matrix has "
           :<>: ShowType m :<>: Text " rows, "
           :<>: ShowType n :<>: Text " columns."
           :$$: Text "Note: indexing starts from 0."
          )

instance Settable AnIndex (M m n a) where
  type Set AnIndex (M m n a)
    = V m a

instance Settable (Name k) (M m n a) where
  type Set (Name k) (M m n a)
    = TypeError (    Text "set: attempt to update matrix column \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )
                
-- composition
instance ( Settable s1 a
         , Settable s2 (Set s1 a)
         )
      => Settable (s1 :.: s2) a where
  type Set (s1 :.: s2) a = Set s2 (Set s1 a)

-- products
instance (Settable s1 (Struct as), Settable s2 (Struct as))
      => Settable (s1 :&: s2) (Struct as) where
  type Set (s1 :&: s2) (Struct as)
    = Struct (Fields (Text "set :") (s1 :&: s2) as 'False)
    --                 disallow overlap for setters ^^^^^

-- equalisers
instance Settable s (Struct as) => Settable (Diag s) (Struct as) where
  type Set (Diag s) (Struct as) = MonoType (Set s (Struct as))

type family MonoType (a :: polyKinded) :: Type where
  MonoType (Struct '[])                 = ()
  MonoType (Struct ( (_ ':-> v) ': as)) = IfAllEqual v as

type family IfAllEqual (a :: v) (as :: [k :-> v]) = (r :: v) where
  IfAllEqual v '[]                 = v
  IfAllEqual v ( (_ ':-> v) ': as) = IfAllEqual v as
  IfAllEqual v ( (_ ':-> w) ': _ )
    = TypeError (     Text "IfAllEqual: type-level list inhabitants "
                 :<>: ShowType v :<>: Text " and "
                 :<>: ShowType w :<>: Text "do not match."
                )

----------------------------------------------------------------------
-- helper type family for arrays, vectors, matrices

type family IfLT ( cmp  :: Ordering )                 
                 ( a    :: Type )
                 ( msg  :: ErrorMessage )
               = ( r    :: Type ) where
  IfLT LT a _   = a
  IfLT _  _ msg = TypeError msg

----------------------------------------------------------------------
-- helper type families for structs

type family StructElemFromName
    ( msg :: ErrorMessage   )
    ( k   :: Symbol         )
    ( as  :: [Symbol :-> v] )
    ( ma  :: Maybe a        )
  = ( r   :: v              ) where
  StructElemFromName _ _ _  (Just a) = a
  StructElemFromName msg k as Nothing
    = TypeError ( msg :<>: Text "struct has no field with name "
                 :<>: ShowType k :<>: Text "."
                 :$$: Text "This struct has the following fields:"
                 :$$: ShowType as
                )
              
type family StructElemFromIndex
    ( msg    :: ErrorMessage   )
    ( n      :: Nat            )
    ( as     :: [Symbol :-> v] )
    ( n_rec  :: Nat            )
    ( as_rec :: [Symbol :-> v] )
  = ( r      :: (Symbol :-> v) ) where
  StructElemFromIndex msg n as _ '[]
    = TypeError ( msg :<>: Text "index "
                 :<>: ShowType n
                 :<>: Text " out of bound when accessing struct with fields"
                 :$$: ShowType as
                )
  StructElemFromIndex _ _ _  0     (bd ': _) = bd
  StructElemFromIndex msg n as n_rec (_ ': as_rec)
    = StructElemFromIndex msg n as (n_rec - 1) as_rec

type family Fields
              ( msg :: ErrorMessage     )
              ( o   :: Optic            )
              ( as  :: [Symbol :-> v]   )
              ( canOverlap :: Bool      )
            = ( r   :: [Symbol :-> v]   ) where
  Fields msg (Name k) as _
     = '[ k ':-> 
          StructElemFromName
            msg
            k as (Lookup k as)
        ]
  Fields msg (Index i) as _
    = '[ StructElemFromIndex
            msg
            i as i as
       ]
  Fields msg (o1 :.: o2) as b     = Fields msg o1 as b
  Fields msg (o1 :&: o2) as 'True
    = Fields msg o1 as 'True :++: Fields msg o2 as 'True
  Fields msg (o1 :&: o2) as 'False
    = NoOverlap
        'Nothing
        (Fields msg o1 as 'False :++: Fields msg o2 as 'False)

type family NoOverlap
              ( overlap :: Maybe k )
              ( as :: [k :-> v]    )
            = ( r  :: [k :-> v]    ) where
  NoOverlap 'Nothing '[]                = '[]
  NoOverlap 'Nothing ((k ':-> v) ': as)
    = (k ':-> v) ': NoOverlap (LookupKey k as) as
  NoOverlap ('Just k) _
    = TypeError (     Text "set: cannot use this lens as a setter."
                 :$$: Text "The key " :<>: ShowType k
                 :<>: Text " appears more than once."
                )