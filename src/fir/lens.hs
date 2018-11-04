{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module FIR.Lens where

-- base
import Data.Kind(Type)
import Data.Word(Word32)
import GHC.TypeLits( Symbol, KnownSymbol
                   , TypeError
                   , ErrorMessage(Text, ShowType, (:<>:), (:$$:))
                   )
import GHC.TypeNats( Nat, KnownNat
                   , CmpNat
                   , type (-)
                   )

-- fir
import qualified FIR.Binding as Binding
import FIR.PrimTy(IntegralTy, Array, RuntimeArray, Struct)
import Data.Type.Bindings( type (:->)
                         , SymbolMap, BindingsMap
                         , Lookup
                         , Var, R, W, RW
                         )
import Math.Linear(V,M)

----------------------------------------------------------------------
-- for testing

-- to force evaluation in GHCi
type family Id (a :: k) :: k
type instance Id (a :: k) = a

type I = [ "x" :-> Var W (M 3 4 Int)
         , "y" :-> Var R (Array 4 Float)
         , "z" :-> Var RW
                     ( Struct 
                        [ "f1" :-> Float
                        , "f2" :-> Array 17 (M 3 4 Double)
                        , "f3" :-> Struct [ "x" :-> RuntimeArray Word
                                          , "l" :-> Bool
                                          ]
                        , "f4" :-> Float
                        ]
                     )
         , "w" :-> Var RW
                    ( RuntimeArray
                      ( RuntimeArray
                          ( Struct [ "m" :-> Int
                                   , "n" :-> Array 9 Word
                                   ]
                          )
                      )
                    )
         ]

----------------------------------------------------------------------

data Lens where
  AnIndex :: Lens -- index not known at compile-time
  Index   :: Nat    -> Lens
  Name    :: Symbol -> Lens  
  (:.:)   :: Lens   -> Lens -> Lens

----------------------------------------------------------------------
-- snoc lists

data SnocList :: Type -> Type where
  NilS :: SnocList a
  Snoc :: SnocList a -> a -> SnocList a

type family (:+:) (l1 :: SnocList a) (l2 :: SnocList a) :: (SnocList a) where
  l1 :+: NilS        = l1
  l1 :+: (Snoc l2 a) = Snoc (l1 :+: l2) a

-- for indices provided at run-time
data Indices :: SnocList Type -> Type where
  NoIndex :: Indices NilS
  (:!&)   :: Indices is -> i -> Indices (is `Snoc` i)

data AnyIndex :: Type where
  AnyIndex :: IntegralTy ty => ty -> AnyIndex

{-
toList :: SnocList a -> [a]
toList as = reverse (go as)
  where go NilS       = []
        go (Snoc l a) = a : go l
-}

----------------------------------------------------------------------
-- getters

class Gettable (lens :: Lens) obj where
  type Get lens obj :: Type

-- bindings
instance KnownSymbol k
      => Gettable (Name k) (i :: BindingsMap) where
  type Get (Name k) i = Binding.Get k i -- throws a type error if key lookup fails

instance Gettable (Index n) (i :: SymbolMap v) where
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
  type Get AnIndex (Array l a) = Indices (NilS `Snoc` Word32) -> a

instance Gettable AnIndex (RuntimeArray a) where
  type Get AnIndex (RuntimeArray a) = Indices (NilS `Snoc` Word32) -> a

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
    = StructElemFromIndex (Text "get: ") n as n as

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
  type Get AnIndex (V n a) = Indices (NilS `Snoc` AnyIndex) -> a

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
    = Indices (NilS `Snoc` AnyIndex) -> V m a

instance Gettable (Name k) (M m n a) where
  type Get (Name k) (M m n a)
    = TypeError (    Text "get: attempt to access matrix column \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )

-- composition
instance ( Gettable l1 a
         , Gettable l2 (IgnoreGetIndices (Get l1 a))
         )
      => Gettable (l1 :.: l2) a where
  type Get (l1 :.: l2) a = GetWithIndices l2 (Get l1 a)

----------------------------------------------------------------------
-- setters

class Settable (lens :: Lens) obj where
  type Set lens obj :: Type

-- bindings
instance KnownSymbol k
      => Settable (Name k) (i :: BindingsMap) where
  type Set (Name k) i = Binding.Put k i

instance Settable (Index n) (i :: SymbolMap v) where
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
  type Set AnIndex (Array l a) = ( Indices (NilS `Snoc` Word32), a )

instance Settable AnIndex (RuntimeArray a) where
  type Set AnIndex (RuntimeArray a) = ( Indices (NilS `Snoc` Word32), a )
  
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
    = StructElemFromIndex (Text "put: ") n as n as

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
  type Set AnIndex (V n a) = ( Indices (NilS `Snoc` AnyIndex), a )

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
    = ( Indices (NilS `Snoc` AnyIndex), V m a )

instance Settable (Name k) (M m n a) where
  type Set (Name k) (M m n a)
    = TypeError (    Text "set: attempt to update matrix column \
                          \using symbolic identifier "
                :<>: ShowType k :<>: Text "."
                :$$: Text "Note: swizzling is not (yet?) supported."
                )
                
-- composition
instance ( Settable l1 a
         , Settable l2 (IgnoreSetIndices (Set l1 a))
         )
      => Settable (l1 :.: l2) a where
  type Set (l1 :.: l2) a = SetWithIndices l2 (Set l1 a)

----------------------------------------------------------------------
-- helper type family for arrays, vectors, matrices

type family IfLT ( cmp  :: Ordering )                 
                 ( a    :: Type )
                 ( msg  :: ErrorMessage )
               = ( r    :: Type ) where
  IfLT LT a _   = a
  IfLT _  _ msg = TypeError msg

----------------------------------------------------------------------
-- helper type families for lenses that require additional indices

------------
-- getters

type family IgnoreGetIndices (b :: Type) :: Type where
  IgnoreGetIndices (Indices is -> b) = b
  IgnoreGetIndices b                 = b

type family GetWithIndices
              (l :: Lens)
              (b :: Type)
            = (r :: Type) where
  GetWithIndices l (Indices is -> b) = PostpendGetIndices (Get l b) (Indices is)
  GetWithIndices l b                 = Get l b

type family PostpendGetIndices (c :: Type) (is :: Type) :: Type where
  PostpendGetIndices (Indices js -> c) (Indices is) = Indices (is :+: js) -> c
  PostpendGetIndices c                 (Indices is) = Indices is          -> c

------------
-- setters

type family IgnoreSetIndices (b :: Type) :: Type where
  IgnoreSetIndices (Indices is, b) = b
  IgnoreSetIndices b               = b

type family SetWithIndices
              (l :: Lens)
              (b :: Type)
            = (r :: Type) where
  SetWithIndices l (Indices is, b) = PostpendSetIndices (Set l b) (Indices is)
  SetWithIndices l b               = Set l b

type family PostpendSetIndices (c :: Type) (is :: Type) :: Type where
  PostpendSetIndices (Indices js, c) (Indices is) = (Indices (is :+: js), c)
  PostpendSetIndices c               (Indices is) = (Indices is         , c)

----------------------------------------------------------------------
-- helper type families for structs

type family StructElemFromName
    ( msg :: ErrorMessage )
    ( k   :: Symbol       )
    ( as  :: SymbolMap v  )
    ( ma  :: Maybe a      )
  = ( r   :: v            ) where
  StructElemFromName _ _ _  (Just a) = a
  StructElemFromName msg k as Nothing
    = TypeError ( msg :<>: Text "struct has no field with name "
                 :<>: ShowType k :<>: Text "."
                 :$$: Text "This struct has the following fields:"
                 :$$: ShowType as
                )

type family StructElemFromIndex
    ( msg    :: ErrorMessage )
    ( n      :: Nat          )
    ( as     :: SymbolMap v  )
    ( n_rec  :: Nat          )
    ( as_rec :: SymbolMap v  )
  = ( r      :: v            ) where
  StructElemFromIndex msg n as _ '[]
    = TypeError ( msg :<>: Text "index "
                 :<>: ShowType n
                 :<>: Text " out of bound when accessing struct with fields"
                 :$$: ShowType as
                )
  StructElemFromIndex _ _ _  0     ((_ :-> a) ': _) = a
  StructElemFromIndex msg n as n_rec (_ ': as_rec)
    = StructElemFromIndex msg n as (n_rec - 1) as_rec