{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Instances.Optics where

-- base
import Data.Kind(Type)
import Data.Type.Bool(If)
import Data.Type.Equality(type (==))
import GHC.TypeLits( Symbol, KnownSymbol
                   , TypeError
                   , ErrorMessage(Text, ShowType, (:<>:), (:$$:))
                   )
import GHC.TypeNats( Nat, KnownNat
                   , CmpNat, type (<=), type (<=?)
                   , type (+), type (-)
                   )

-- fir
import Control.Monad.Indexed((:=))
import Control.Type.Optic( Optic(..)
                         , Gettable(Get)
                         , Settable(Set)
                         , RequiredIndices
                         , Container(..)
                         , MonoContainer(..)
                         )
import Data.Type.Map ( (:->)((:->)), Key, Value
                     , Lookup
                     , type (:++:)
                     )
import FIR.Binding( BindingsMap
                  , Var, R, W, RW
                  )
import qualified FIR.Instances.Bindings as Binding
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
    = If
        ( CmpNat i n == LT )
        a
        ( TypeError
          (     Text "get: array index "
           :<>: ShowType i
           :<>: Text " is out of bounds."
           :$$: Text "Array size is "
           :<>: ShowType n :<>: Text "."
           :$$: Text "Note: indexing starts from 0."
          )
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
    = If
        (CmpNat i n == LT)
        a
        ( TypeError
          (     Text "get: vector index "
           :<>: ShowType i
           :<>: Text " is out of bounds."
           :$$: Text "Vector dimension is "
           :<>: ShowType n :<>: Text "."
           :$$: Text "Note: indexing starts from 0."
          )
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
      = If
          (CmpNat i n == LT)
          (V m a)
          ( TypeError
            (     Text "get: matrix column index "
             :<>: ShowType i
             :<>: Text " is out of bounds."
             :$$: Text "This matrix has "
             :<>: ShowType m :<>: Text " rows, "
             :<>: ShowType n :<>: Text " columns."
             :$$: Text "Note: indexing starts from 0."
            )
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
    = If
        (CmpNat i n == LT)
        a
        ( TypeError
          (     Text "set: array index "
           :<>: ShowType i
           :<>: Text " is out of bounds."
           :$$: Text "Array size is "
           :<>: ShowType n :<>: Text "."
           :$$: Text "Note: indexing starts from 0."
          )
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
    = If
        (CmpNat i n == LT)
        a
        ( TypeError
          (     Text "set: vector index "
           :<>: ShowType i
           :<>: Text " is out of bounds."
           :$$: Text "Vector dimension is "
           :<>: ShowType n :<>: Text "."
           :$$: Text "Note: indexing starts from 0."
          )
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
      = If
          (CmpNat i n == LT)
          (V m a)
          ( TypeError
            (     Text "set: matrix column index "
             :<>: ShowType i
             :<>: Text " is out of bounds."
             :$$: Text "This matrix has "
             :<>: ShowType m :<>: Text " rows, "
             :<>: ShowType n :<>: Text " columns."
             :$$: Text "Note: indexing starts from 0."
            )
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
                

----------------------------------------------------------------------
-- type class instances for products 

instance Container (V n a) where
  type Combine (V n a) (V i a) (V j a) = V (i+j) a
  type Singleton (V n a) _ a = V 1 a
  type Overlapping (V n a) k _
    = TypeError (    Text "optic: attempt to index a vector component with name " :<>: ShowType k
                :$$: Text "Maybe you intended to use a swizzle?"
                )

instance Container (M m n a) where
  type Combine (M m n a) (M m i a) (M m j a) = M m (i+j) a
  type Singleton (M m n a) _ a = M m 1 a
  type Overlapping (M m n a) k _
    = TypeError ( Text "optic: attempt to index a matrix component with name " :<>: ShowType k )

instance Container (Struct as) where
  type Combine (Struct as) (Struct xs) (Struct ys) = Struct (xs :++: ys)
  type Singleton (Struct as) (Name k ) v = Struct '[ k ':-> v ]
  type Singleton (Struct as) (Index i) v
    = Struct 
        '[ Key ( StructElemFromIndex 
                  (Text "key: ")
                  i as i as
               )
         ':-> v
         ]
  type Overlapping (Struct as) k i
    = k == Key (StructElemFromIndex (Text "key: ") i as i as)

instance Container (Array n a) where
  type Combine (Array n a) (Array i a) (Array j a) = Array (i+j) a
  type Singleton (Array n a) _ a = Array 1 a
  type Overlapping (Array n a) k _
        = TypeError ( Text "optic: attempt to index an array using name " :<>: ShowType k )

instance Container (RuntimeArray a) where
  type Combine (RuntimeArray a) (RuntimeArray a) (RuntimeArray a) = RuntimeArray a
  type Singleton (RuntimeArray a) _ a = (RuntimeArray a)
  type Overlapping (RuntimeArray a) k _
        = TypeError ( Text "optic: attempt to index an array using name " :<>: ShowType k )

----------------------------------------------------------------------
-- type class instances for equalisers

instance MonoContainer (Array n a) where
  type MonoType (Array n a) = a

instance MonoContainer (RuntimeArray a) where
  type MonoType (RuntimeArray a) = a

instance (KnownNat n, 1 <= n)
      => MonoContainer (V n a) where
  type MonoType (V n a) = a

instance (KnownNat m, 1 <= m)
      => MonoContainer (M m n a) where
  type MonoType (M m n a) = V m a

instance (AllValuesEqual v as ~ 'True)
      => MonoContainer (Struct ((k ':-> v) ': as)) where
  type MonoType (Struct ((k ':-> v) ': as)) = v

type family AllValuesEqual (a :: v) (as :: [k :-> v]) :: Bool where
  AllValuesEqual v '[]                 = True
  AllValuesEqual v ( (_ ':-> v) ': as) = AllValuesEqual v as
  AllValuesEqual v ( (_ ':-> w) ': _ ) = False

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

----------------------------------------------------------------------
-- synonyms for vector/matrix optics

type family MkRow (n :: Nat) (i :: Nat) (c :: Nat) :: Optic where
  MkRow n i c
    = MkRowHelper n i c (n <=? (c+1))

type family MkRowHelper n i c b :: Optic where
  MkRowHelper n i c False = (Index c :.: Index i) :&: MkRow n i (c+1)
  MkRowHelper n i c True  = Index c :.: Index i

type family MkDiag (n :: Nat) (i :: Nat) where
  MkDiag n i = MkDiagHelper n i (n <=? (i+1))

type family MkDiagHelper n i b where
  MkDiagHelper n i False = (Index i :.: Index i) :&: MkDiag n (i+1)
  MkDiagHelper n i True  = Index i :.: Index i

--type Center = forall n. Diag (MkDiag n 0)

--type Row i = forall n. MkRow n i 0