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

-- half
import Numeric.Half(Half)

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

type Getter i (g :: Optic i a) = VariadicList (RequiredIndices g) (Get g) i

-- bindings
instance (KnownSymbol k, r ~ Binding.Get k i)
      => Gettable (i :: BindingsMap) r (Name k) where

{-
instance Gettable (i :: [Symbol :-> v]) (Index n) where
  type Get i (Index n)
    = TypeError (    Text "get: cannot get a binding using a numeric index."
                :$$: Text "Use the binding's name instead."
                )

-- arrays
instance (KnownNat n, KnownNat i)
      => Gettable (Array n a) (Index i) where
  type Get (Array n a) (Index i)
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
      => Gettable (RuntimeArray a) (Index i) where
  type Get (RuntimeArray a) (Index i)  = a

instance Gettable (Array l a) AnIndex where
  type Get (Array l a) AnIndex = a

instance Gettable (RuntimeArray a) AnIndex  where
  type Get (RuntimeArray a) AnIndex = a

instance Gettable (Array l a) (Name k) where
  type Get (Array l a) (Name k)
    = TypeError (    Text "get: attempt to access array element \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                )

instance Gettable (RuntimeArray a) (Name k) where
  type Get (RuntimeArray a) (Name k) 
    = TypeError (    Text "get: attempt to access run-time array \
                          \element using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                )

-- structs
instance KnownSymbol k
       => Gettable (Struct as) (Name k) where
  type Get (Struct as) (Name k)
    = StructElemFromName (Text "get: ") k as (Lookup k as)

instance KnownNat n => Gettable (Struct as) (Index n) where
  type Get (Struct as) (Index n)
    = Value (StructElemFromIndex (Text "get: ") n as n as)

instance Gettable (Struct as) AnIndex where
  type Get (Struct as) AnIndex
    = TypeError (    Text "get: attempt to access struct element \
                          \using run-time index."
                :$$: Text "Structs can only be accessed using \
                          \compile-time indices or field names."
                )

-}
-- vectors
instance ( KnownNat i
         , r ~ If
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
          )
      => Gettable (V n a) r (Index i) where

instance Gettable (V n a) a AnIndex where

{-
instance Gettable (V n a) (Name k) where
  type Get (V n a) (Name k)
    = TypeError (    Text "get: attempt to access vector element \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )
-}

-- matrices
instance ( KnownNat i
         , r ~ If
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
         )

      => Gettable (M m n a) r (Index i) where

instance Gettable (M m n a) a AnIndex where

{-
instance Gettable (M m n a) (Name k) where
  type Get (M m n a) (Name k)
    = TypeError (    Text "get: attempt to access matrix column \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )
-}
----------------------------------------------------------------------
-- setters

type Setter i (o :: Optic i a) = VariadicList (RequiredIndices o :++: '[Set o]) () i
{-
-- bindings
instance KnownSymbol k
      => Settable (i :: BindingsMap) (Name k) where
  type Set i (Name k) = Binding.Put k i

instance Settable (i :: [Symbol :-> v]) (Index n) where
  type Set i (Index n)
    = TypeError (    Text "set: cannot set binding using a numeric index."
                :$$: Text "Use the binding's name instead."
                )

-- arrays
instance (KnownNat n, KnownNat i)
      => Settable (Array n a) (Index i) where
  type Set (Array n a) (Index i)
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
      => Settable (RuntimeArray a) (Index i) where
  type Set (RuntimeArray a) (Index i) = a

instance Settable (Array l a) AnIndex where
  type Set (Array l a) AnIndex = a

instance Settable (RuntimeArray a) AnIndex where
  type Set (RuntimeArray a) AnIndex = a
  
instance Settable (Array l a) (Name k) where
  type Set (Array l a) (Name k) 
    = TypeError (    Text "set: attempt to update array element \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                )

instance Settable (RuntimeArray a) (Name k) where
  type Set (RuntimeArray a) (Name k)
    = TypeError (    Text "set: attempt to update run-time array \
                          \element using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                )

-- structs
instance KnownSymbol k
       => Settable (Struct as) (Name k) where
  type Set (Struct as) (Name k)
    = StructElemFromName (Text "put: ") k as (Lookup k as)

instance KnownNat n => Settable (Struct as) (Index n) where
  type Set (Struct as) (Index n)
    = Value (StructElemFromIndex (Text "put: ") n as n as)

instance Settable (Struct as) AnIndex where
  type Set (Struct as) AnIndex
    = TypeError (    Text "set: attempt to set struct element \
                          \using run-time index."
                :$$: Text "Structs can only be modified using \
                          \compile-time indices or field names."
                )

-- vectors
instance KnownNat i
      => Settable (V n a) (Index i) where
  type Set (V n a) (Index i)
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

instance Settable (V n a) AnIndex where
  type Set (V n a) AnIndex = a

instance Settable (V n a) (Name k) where
  type Set (V n a) (Name k)
    = TypeError (    Text "set: attempt to update vector element \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )

-- matrices
instance KnownNat i
      => Settable (M m n a) (Index i) where
    type Set (M m n a) (Index i)
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

instance Settable (M m n a) AnIndex where
  type Set (M m n a) AnIndex
    = V m a

instance Settable (M m n a) (Name k) where
  type Set (M m n a) (Name k)
    = TypeError (    Text "set: attempt to update matrix column \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )
                
-}
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
  type Singleton (Struct as) (Name k) v = Struct '[ k ':-> v ]
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
{-
type family MkRow (n :: Nat) (i :: Nat) (c :: Nat) :: (Optic (M m n a)) where
  MkRow n i c
    = MkRowHelper n i c (n <=? (c+1))

type family MkRowHelper n i c b :: (Optic (M m n a)) where
  MkRowHelper n i c False = (Index c :.: Index i) :&: MkRow n i (c+1)
  MkRowHelper n i c True  = Index c :.: Index i

type family MkDiag (n :: Nat) (i :: Nat) where
  MkDiag n i = MkDiagHelper n i (n <=? (i+1))

type family MkDiagHelper n i b where
  MkDiagHelper n i False = (Index i :.: Index i) :&: MkDiag n (i+1)
  MkDiagHelper n i True  = Index i :.: Index i

--type Center = forall n. Diag (MkDiag n 0)

--type Row i = forall n. MkRow n i 0

type family Center :: Optic (M n n a) (V n a) where
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1))
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1) :&: (Index 2 :.: Index 2))
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1) :&: (Index 2 :.: Index 2) :&: (Index 3 :.: Index 3))
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1))
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1) :&: (Index 2 :.: Index 2))
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1) :&: (Index 2 :.: Index 2) :&: (Index 3 :.: Index 3))
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1))
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1) :&: (Index 2 :.: Index 2))
  Center = ((Index 0 :.: Index 0) :&: (Index 1 :.: Index 1) :&: (Index 2 :.: Index 2) :&: (Index 3 :.: Index 3))
-}