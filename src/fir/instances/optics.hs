{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-} -- needed to help along GHC's SCC analysis
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
import Data.Proxy(Proxy(Proxy))
import GHC.TypeLits
  ( Symbol, KnownSymbol, symbolVal
  , TypeError
  , ErrorMessage(Text, ShowType, (:<>:), (:$$:))
  )
import GHC.TypeNats
  ( Nat, KnownNat, natVal
  , CmpNat, type (<=)
  , type (-)
  )

-- distributive
import Data.Distributive(Distributive(..))

-- vector
import qualified Data.Vector as Array

-- fir
import Control.Monad.Indexed((:=))
import Control.Type.Optic
  ( Optic(..)
  , Gettable, ReifiedGetter(view)
  , Settable, ReifiedSetter(set )
  , Contained(..)
  , ContainerKind, DegreeKind, LabelKind
  , MonoContained(..)
  , (:.:), (:*:), Index, All
  )
import Data.Function.Variadic(ListVariadic)
import Data.Type.Map
  ( (:->)((:->)), Key, Value
  , Lookup, Append, Length
  )
import FIR.Binding( BindingsMap )
import qualified FIR.Instances.Bindings as Binding
import FIR.Prim.Array(Array(MkArray), RuntimeArray(MkRuntimeArray))
import FIR.Prim.Singletons
  ( PrimTy, IntegralTy
  , ScalarTy(scalarTySing), SScalarTy
  , PrimTys(primTys)
  , SPrimTys
  )
import FIR.Prim.Struct(Struct((:&)))
import Math.Linear(V((:.)), M(M), (^!), at)

----------------------------------------------------------------------
-- singletons

infixr 9 :%.:
infixr 3 :%*:

data SOptic (optic :: Optic i s a) :: Type where
  -- split up run-time indexing: SPIR-V supports two different cases
  --  - vectors with VectorExtractDynamic / VectorInsertDynamic
  --  - arrays with OpAccessChain
  SAnIndexV   :: SScalarTy ix -> SOptic (AnIndex_ :: Optic '[ix] (V       n    a) a)
  SAnIndexRTA :: SScalarTy ix -> SOptic (AnIndex_ :: Optic '[ix] (RuntimeArray a) a)
  SAnIndexA   :: SScalarTy ix -> SOptic (AnIndex_ :: Optic '[ix] (Array   n    a) a)
  SIndex   :: KnownNat    n => Proxy n -> SOptic (Index_ n)
  SName    :: KnownSymbol k
           => Proxy k
           -> SPrimTys as
           -> SOptic (Name_  k :: Optic i (Struct as) b)
  SBinding :: KnownSymbol k
           => Proxy k
           -> SOptic (Name_ k :: Optic i (as :: BindingsMap) b)
  SAll     :: SOptic o  -> SOptic (All_ o)
  (:%.:)   :: SOptic o1 -> SOptic o2 -> SOptic (o1 `ComposeO` o2)
  (:%*:)   :: SOptic o1 -> SOptic o2 -> SOptic (o1 `ProductO` o2)


showSOptic :: SOptic (o :: Optic i s a) -> String
{-
showSOptic (SFromField a f)
  = case fieldVal f of
      SAnIndex -> "AnIndex"
      SIndex n -> "Index " ++ show (natVal    n)
      SName  k -> "Name "  ++ show (symbolVal k)
-}
showSOptic (SAnIndexV   _) = "AnIndexV"
showSOptic (SAnIndexRTA _) = "AnIndexRTA"
showSOptic (SAnIndexA   _) = "AnIndexA"
showSOptic (SIndex n  ) = "Index " ++ show (natVal    n)
showSOptic (SName  k _) = "Name "  ++ show (symbolVal k)
showSOptic (SBinding k) = "Name "  ++ show (symbolVal k)
showSOptic (SAll     l) = "All ( "   ++ showSOptic l ++ " )"
showSOptic (l1 :%.: l2) = showSOptic l1 ++ " :.: " ++ showSOptic l2
showSOptic (l1 :%*: l2) = showSOptic l1 ++ " :*: " ++ showSOptic l2


class KnownOptic optic where
  opticSing :: SOptic optic
{-
instance (KnownField f, HasField a f) => KnownOptic (FromField f :: Optic a) where
  opticSing = SFromField (Proxy @a) (Proxy @f)
-}
instance IntegralTy ix => KnownOptic (AnIndex_ :: Optic '[ix] (V n a) a) where
  opticSing = SAnIndexV (scalarTySing @ix)
instance IntegralTy ix => KnownOptic (AnIndex_ :: Optic '[ix] (RuntimeArray a) a) where
  opticSing = SAnIndexRTA (scalarTySing @ix)
instance IntegralTy ix => KnownOptic (AnIndex_ :: Optic '[ix] (Array n a) a) where
  opticSing = SAnIndexA (scalarTySing @ix)
instance KnownNat n => KnownOptic (Index_ n) where
  opticSing = SIndex (Proxy @n)
instance (KnownSymbol k, PrimTys as) => KnownOptic (Name_ k :: Optic i (Struct as) a) where
  opticSing = SName (Proxy @k) (primTys @as)
instance KnownSymbol k => KnownOptic (Name_ k :: Optic i (bds :: BindingsMap) a) where
  opticSing = SBinding (Proxy @k)
instance KnownOptic o => KnownOptic (All_ o) where
  opticSing = SAll (opticSing @o)
instance (KnownOptic o1, KnownOptic o2)
      => KnownOptic (o1 `ComposeO` o2) where
  opticSing = (opticSing @o1) :%.: (opticSing @o2)
instance (KnownOptic o1, KnownOptic o2)
      => KnownOptic (o1 `ProductO` o2) where
  opticSing = (opticSing @o1) :%*: (opticSing @o2)


----------------------------------------------------------------------

type family ListVariadicIx
              ( as :: [Type]      )
              ( b  :: Type        )
              ( i  :: BindingsMap )
            = ( r  :: Type        )
            | r -> as i b  where
  ListVariadicIx '[]       b i = (b := i) i
  ListVariadicIx (a ': as) b i = a -> ListVariadicIx as b i

----------------------------------------------------------------------
-- getters

type User (g :: Optic as i b) = ListVariadicIx as b i

-- bindings
instance ( KnownSymbol k
         , r ~ Binding.Get k i
         , empty ~ '[]
         )
      => Gettable (Name_ k :: Optic empty (i :: BindingsMap) r) where


instance
  TypeError (     Text "get: cannot get a binding using a numeric index."
             :$$: Text "Use the binding's name instead."
            )
  => Gettable (Index_ n :: Optic empty (i :: [Symbol :-> v]) r)
  where

-- arrays
instance ( KnownNat n, KnownNat i
         , r ~ If
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
         , empty ~ '[]
         )
      => Gettable (Index_ i :: Optic empty (Array n a) r) where
instance ( KnownNat i         
         , r ~ a
         , empty ~ '[]
         , PrimTy a
         , Gettable (Index_ i :: Optic empty (Array n a) r)
         )
       => ReifiedGetter (Index_ i :: Optic empty (Array n a) r) where
  view (MkArray arr) = arr Array.! (fromIntegral (natVal (Proxy @i)))

instance ( KnownNat i
         , empty ~ '[]
         , r ~ a
         )
      => Gettable (Index_ i :: Optic empty (RuntimeArray a) r) where
instance ( KnownNat i         
         , r ~ a
         , empty ~ '[]
         , PrimTy a
         , Gettable (Index_ i :: Optic empty (RuntimeArray a) r)
         )
       => ReifiedGetter (Index_ i :: Optic empty (RuntimeArray a) r) where
  view (MkRuntimeArray arr) = arr Array.! (fromIntegral (natVal (Proxy @i)))


instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Gettable (AnIndex_ :: Optic ix (Array n a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , Gettable (AnIndex_ :: Optic ix (Array n a) r)
         , PrimTy a
         )
      => ReifiedGetter (AnIndex_ :: Optic ix (Array n a) r) where
  view i (MkArray arr) = arr Array.! fromIntegral i

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Gettable (AnIndex_ :: Optic ix (RuntimeArray a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , Gettable (AnIndex_ :: Optic ix (RuntimeArray a) r)
         , PrimTy a
         )
      => ReifiedGetter (AnIndex_ :: Optic ix (RuntimeArray a) r) where
  view i (MkRuntimeArray arr) = arr Array.! fromIntegral i

instance
    TypeError (    Text "get: attempt to access array element \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Gettable (Name_ k :: Optic empty (Array n a) r) where


instance
    TypeError (    Text "get: attempt to access run-time array \
                        \element using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Gettable (Name_ k :: Optic empty (RuntimeArray a) r) where


-- structs
instance ( KnownSymbol k
         , r ~ StructElemFromName
                 (Text "get: ")
                 k
                 as
                 (Lookup k as)
         , empty ~ '[]
         )
       => Gettable (Name_ k :: Optic empty (Struct as) r) where
instance ( KnownSymbol k
         , r ~ StructElemFromName
                 (Text "get: ")
                 k
                 ((k ':-> a) ': as)
                 (Lookup k ((k ':-> a) ': as))
         , empty ~ '[]
         , a ~ ListVariadic '[] a
         , Gettable (Name_ k :: Optic empty (Struct ((k ':-> a) ': as)) r)
         )
      => ReifiedGetter
            (Name_ k :: Optic empty (Struct ((k ':-> a) ': as)) r)
      where
  view (a :& _) = a
instance {-# OVERLAPPABLE #-}
         ( KnownSymbol k
         , r ~ StructElemFromName
                  (Text "get: ")
                  k
                  (a ': as)
                  (Lookup k (a ': as))
         , empty ~ '[]
         , ReifiedGetter
             -- this forces evaluation of r before looking for the constraint
             -- this is needed to have the correct error messages
             (Name_ (If (r == r) k k) :: Optic '[] (Struct as) r)
         )
      => ReifiedGetter
            (Name_ k :: Optic empty (Struct (a ': as)) r)
      where
  view (_ :& as) = view @(Name_ (If (r == r) k k) :: Optic '[] (Struct as) r) as


instance ( KnownNat n
         , r ~ Value (StructElemFromIndex (Text "get: ") n as n as)
         , empty ~ '[]
         ) => Gettable (Index_ n :: Optic empty (Struct as) r) where
instance ( r ~ Value (StructElemFromIndex (Text "get: ") 0 (a ': as) 0 (a ': as))
         , empty ~ '[]
         , r ~ ListVariadic '[] r
         )
      => ReifiedGetter
           (Index_ 0 :: Optic empty (Struct (a ': as)) r)
       where
  view (a :& _) = a
instance {-# OVERLAPPABLE #-}
         ( KnownNat n, 1 <= n
         , r ~ Value (StructElemFromIndex (Text "get: ") n (a ': as) n (a ': as))
         , empty ~ '[]
         , ReifiedGetter
             -- as above, force evaluation of r for correct error message
             (Index_ (If (r == r) (n-1) (n-1)) :: Optic empty (Struct as) r)
         )
      => ReifiedGetter
          (Index_ n :: Optic empty (Struct (a ': as)) r)
      where
  view (_ :& as) = view @(Index_ (If (r == r) (n-1) (n-1)) :: Optic '[] (Struct as) r) as

instance
    TypeError (    Text "get: attempt to access struct element \
                        \using run-time index."
              :$$: Text "Structs can only be accessed using \
                        \compile-time indices, or field names."
              )
    => Gettable (AnIndex_ :: Optic ix (Struct as) r) where


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
          , empty ~ '[]
          )
      => Gettable (Index_ i :: Optic empty (V n a) r) where
instance ( KnownNat n
         , KnownNat i
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
         , empty ~ '[]
         , PrimTy a
         , CmpNat i n ~ 'LT
         )
      => ReifiedGetter
            (Index_ i :: Optic empty (V n a) r)
      where
  view v = at @i v

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
       => Gettable (AnIndex_ :: Optic ix (V n a) r) where
instance ( IntegralTy ty
         , r ~ a
         , ix ~ '[ty]
         , PrimTy a
         )
      => ReifiedGetter
            (AnIndex_ :: Optic ix (V n a) r)
      where
  view i v = v ^! fromIntegral i


instance 
    TypeError (    Text "get: attempt to access vector element \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              :$$: Text "For a vector swizzle, use 'Swizzle' (e.g. Swizzle '[\"x\",\"z\",\"y\"]),"
              :$$: Text "or a synonym such as XZY (limited to X/Y/Z/W)."
              )
    => Gettable (Name_ k :: Optic empty (V n a) r) where


-- matrices
instance ( KnownNat n
         , KnownNat i
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
         , empty ~ '[]
         )
      => Gettable (Index_ i :: Optic empty (M m n a) r) where
instance ( KnownNat m
         , KnownNat n
         , KnownNat i
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
         , ListVariadic '[] r ~ V m a
         , CmpNat i n ~ 'LT
         , empty ~ '[]
         )
      => ReifiedGetter
            (Index_ i :: Optic empty (M m n a) r)
      where
  view (M rows) = at @i (distribute rows)

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ V m a
         )
        => Gettable (AnIndex_ :: Optic ix (M m n a) r) where
instance ( KnownNat m
         , KnownNat n
         , IntegralTy ty
         , r ~ V m a
         , ix ~ '[ty]
         )
      => ReifiedGetter
            (AnIndex_ :: Optic ix (M m n a) r)
      where
  view i (M rows) = distribute rows ^! fromIntegral i


instance
    TypeError (    Text "get: attempt to access matrix column \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Gettable (Name_ k :: Optic empty (M m n a) r) where

----------------------------------------------------------------------
-- setters

type Assigner (g :: Optic as i b) = ListVariadicIx (Append as b) () i


-- bindings
instance ( KnownSymbol k
         , r ~ Binding.Put k i
         , empty ~ '[]
         )
      => Settable (Name_ k :: Optic empty (i :: BindingsMap) r ) where

  

instance
    TypeError (    Text "set: cannot set binding using a numeric index."
              :$$: Text "Use the binding's name instead."
              )
    => Settable (Index_ n :: Optic empty (i :: [Symbol :-> v]) r) where

-- arrays
instance ( KnownNat n, KnownNat i
         , r ~ If
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
         , empty ~ '[]
         )
      => Settable (Index_ i :: Optic empty (Array n a) r) where
instance ( KnownNat i         
         , r ~ a
         , empty ~ '[]
         , Settable (Index_ i :: Optic empty (Array n a) r)
         )
      => ReifiedSetter
           (Index_ i :: Optic empty (Array n a) r)
      where
  set a (MkArray arr) = MkArray ( arr Array.// [( fromIntegral (natVal (Proxy @i)), a)] )

instance ( KnownNat i
         , empty ~ '[]
         , r ~ a
         )
      => Settable (Index_ i :: Optic empty (RuntimeArray a) r) where
instance ( KnownNat i         
         , r ~ a
         , empty ~ '[]
         , Settable (Index_ i :: Optic empty (RuntimeArray a) r)
         )
      => ReifiedSetter
           (Index_ i :: Optic empty (RuntimeArray a) r)
      where
  set a (MkRuntimeArray arr)
    = MkRuntimeArray ( arr Array.// [( fromIntegral (natVal (Proxy @i)), a)] )

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Settable (AnIndex_ :: Optic ix (Array n a) r) where
instance ( IntegralTy ty
         , r ~ a
         , ix ~ '[ty]
         , Settable (AnIndex_ :: Optic ix (Array n a) r)
         )
      => ReifiedSetter
           (AnIndex_ :: Optic ix (Array n a) r)
      where
  set i a (MkArray arr) = MkArray ( arr Array.// [(fromIntegral i, a)] )

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Settable (AnIndex_ :: Optic ix (RuntimeArray a) r) where
instance ( IntegralTy ty
         , r ~ a
         , ix ~ '[ty]
         , Settable (AnIndex_ :: Optic ix (RuntimeArray a) r)
         )
      => ReifiedSetter
           (AnIndex_ :: Optic ix (RuntimeArray a) r)
      where
  set i a (MkRuntimeArray arr)
    = MkRuntimeArray ( arr Array.// [(fromIntegral i, a)] )
  
instance 
    TypeError (    Text "set: attempt to update array element \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Settable (Name_ k :: Optic empty (Array n a) r) where

instance
    TypeError (    Text "set: attempt to update run-time array \
                        \element using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Settable (Name_ k :: Optic empty (RuntimeArray a) r) where

-- structs
instance ( KnownSymbol k
         , r ~ StructElemFromName (Text "set: ") k as (Lookup k as)
         , empty ~ '[]
         )
  => Settable (Name_ k :: Optic empty (Struct as) r) where
instance ( KnownSymbol k
         , r ~ StructElemFromName
                  (Text "set: ")
                  k
                  ((k ':-> a) ': as)
                  (Lookup k ((k ':-> a) ': as))
         , empty ~ '[]
         , Settable (Name_ k :: Optic empty (Struct ((k ':-> a) ': as)) r)
         )
      => ReifiedSetter
            (Name_ k :: Optic empty (Struct ((k ':-> a) ': as)) r)
      where
  set b (_ :& as) = b :& as
instance {-# OVERLAPPABLE #-}
         ( KnownSymbol k
         , r ~ StructElemFromName
                  (Text "set: ")
                  k
                  (a ': as)
                  (Lookup k (a ': as))
         , empty ~ '[]
         , ReifiedSetter
             -- this forces evaluation of r before looking for the constraint
             -- this is needed to have the correct error messages
             (Name_ (If (r == r) k k) :: Optic '[] (Struct as) r)
         )
      => ReifiedSetter
            (Name_ k :: Optic empty (Struct (a ': as)) r)
      where
  set b (a :& as) = a :& set @(Name_ (If (r == r) k k) :: Optic '[] (Struct as) r) b as

instance ( KnownNat n
         , r ~ Value (StructElemFromIndex (Text "set: ") n as n as)
         , empty ~ '[]
         ) => Settable (Index_ n :: Optic empty (Struct as) r) where
instance ( r ~ Value (StructElemFromIndex (Text "set: ") 0 (a ': as) 0 (a ': as))
         , empty ~ '[]
         , r ~ ListVariadic '[] r
         )
      => ReifiedSetter
           (Index_ 0 :: Optic empty (Struct (a ': as)) r)
       where
  set b (_ :& as) = b :& as
instance {-# OVERLAPPABLE #-}
         ( KnownNat n, 1 <= n
         , r ~ Value (StructElemFromIndex (Text "set: ") n (a ': as) n (a ': as))
         , empty ~ '[]
         , ReifiedSetter
             -- as above, force evaluation of r for correct error message
             (Index_ (If (r == r) (n-1) (n-1)) :: Optic empty (Struct as) r)
         )
      => ReifiedSetter
          (Index_ n :: Optic empty (Struct (a ': as)) r)
      where
  set b (a :& as) = a :& set @(Index_ (If (r == r) (n-1) (n-1)) :: Optic '[] (Struct as) r) b as

instance
    TypeError (    Text "set: attempt to set struct element \
                        \using run-time index."
              :$$: Text "Structs can only be modified using \
                        \compile-time indices or field names."
              )
    => Settable (AnIndex_ :: Optic ix (Struct as) r) where

-- vectors
instance ( KnownNat i
         , r ~ If
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
         , empty ~ '[]
         )
      => Settable (Index_ i :: Optic empty (V n a) r) where

instance ( KnownNat n, 1 <= n
         , empty ~ '[]
         , r ~ a
         , r ~ ListVariadic '[] r
         , Settable (Index_ 0 :: Optic empty (V n a) r)
         )
      => ReifiedSetter
           (Index_ 0 :: Optic empty (V n a) r)
       where
  set b (_ :. as) = b :. as
instance {-# OVERLAPPABLE #-}
         ( KnownNat n, KnownNat i, 1 <= n
         , CmpNat i n ~ 'LT
         , r ~ a
         , empty ~ '[]
         , Settable (Index_ i :: Optic empty (V n a) r)
         , ReifiedSetter
             -- as above, force evaluation of r for correct error message
             (Index_ (If (r == r) (i-1) (i-1)) :: Optic empty (V (n-1) a) r)
         )
      => ReifiedSetter
          (Index_ i :: Optic empty (V n a) r)
      where
  set b (a :. as) = a :. set @(Index_ (If (r == r) (i-1) (i-1)) :: Optic '[] (V (n-1) a) r) b as

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , KnownNat n, 1 <= n
         )
    => Settable (AnIndex_ :: Optic ix (V n a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , KnownNat n, 1 <= n
         )
    => ReifiedSetter (AnIndex_ :: Optic ix (V n a) r) where
  set i b (a :. as)
    = if i == 0
      then b :. as
      else a :. set @(AnIndex_ :: Optic ix (V (n-1) a) r) (i-1) b as

instance
    TypeError (    Text "set: attempt to update vector element \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              :$$: Text "For a vector swizzle, use 'Swizzle' (e.g. Swizzle '[\"x\",\"z\",\"y\"]),"
              :$$: Text "or a synonym such as XZY (limited to X/Y/Z/W)."
              )
    => Settable (Name_ k :: Optic empty (V n a) r) where

-- matrices
instance ( KnownNat i
         , empty ~ '[]
         , r ~ If
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
         )
      => Settable (Index_ i :: Optic empty (M m n a) r) where
instance ( KnownNat m, KnownNat n, KnownNat i, 1 <= n
         , empty ~ '[]
         , CmpNat i n ~ 'LT
         , r ~ V m a
         , Settable (Index_ i :: Optic empty (M m n a) r)
         , ReifiedSetter ( Index_ i :: Optic '[] (V n (V m a)) (V m a) )
         )
      => ReifiedSetter
           (Index_ i :: Optic empty (M m n a) r)
       where
  set c (M m)
    = ( M
      . distribute
      . set @(Index_ i :: Optic '[] (V n (V m a)) (V m a)) c
      . distribute
      ) m

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ V m a
         )
       => Settable (AnIndex_ :: Optic ix (M m n a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ V m a
         , KnownNat m, KnownNat n, 1 <= n
         )
       => ReifiedSetter (AnIndex_ :: Optic ix (M m n a) r)
       where
  set i c (M m)
    = ( M
      . distribute
      . set @(AnIndex_ :: Optic ix (V n (V m a)) (V m a)) i c
      . distribute
      ) m

instance
    TypeError (    Text "set: attempt to update matrix column \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              :$$: Text "Note: swizzling is not (yet?) supported."
              )
    => Settable (Name_ k :: Optic empty (M m n a) r) where

----------------------------------------------------------------------
-- type class instances for equalisers

instance MonoContained (Array n a) where
  type MonoType (Array n a) = a

instance MonoContained (RuntimeArray a) where
  type MonoType (RuntimeArray a) = a

instance (KnownNat n, 1 <= n)
      => MonoContained (V n a) where
  type MonoType (V n a) = a

instance (KnownNat m, 1 <= m)
      => MonoContained (M m n a) where
  type MonoType (M m n a) = V m a

instance (AllValuesEqual v as ~ 'True)
      => MonoContained (Struct ((k ':-> v) ': as))
      where
  type MonoType (Struct ((k ':-> v) ': as)) = v

type family AllValuesEqual (a :: v) (as :: [k :-> v]) :: Bool where
  AllValuesEqual _ '[]                 = True
  AllValuesEqual v ( (_ ':-> v) ': as) = AllValuesEqual v as
  AllValuesEqual _ _                   = False

----------------------------------------------------------------------
-- helper type families for structs

type Field ( k :: Symbol )
  = ( Name_ k
        :: Optic '[]
            (Struct as)
            (StructElemFromName (Text "optic: ") k as (Lookup k as))
    )

type family StructElemFromName
    ( msg :: ErrorMessage   )
    ( k   :: Symbol         )
    ( as  :: [Symbol :-> v] )
    ( ma  :: Maybe a        )
  = ( r   :: v              ) where
  StructElemFromName _   _ _  (Just a) = a
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
                 :<>: Text " out of bounds when accessing struct with fields"
                 :$$: ShowType as
                 :$$: Text "Note: indexing starts at 0."
                )
  StructElemFromIndex _   _ _  0     (bd ': _)     = bd
  StructElemFromIndex msg n as n_rec (_ ': as_rec)
    = StructElemFromIndex msg n as (n_rec - 1) as_rec

----------------------------------------------------------------------
-- type class instances for products

type instance ContainerKind (V n a) = Type
type instance DegreeKind    (V n a) = Nat
type instance LabelKind     (V n a) = ()

type instance ContainerKind (M m n a) = Type
type instance DegreeKind    (M m n a) = Nat
type instance LabelKind     (M m n a) = ()

type instance ContainerKind (Struct as) = [Symbol :-> Type] -> Type
type instance DegreeKind    (Struct as) = [Symbol :-> Type]
type instance LabelKind     (Struct as) = (Symbol :-> Type)

type instance ContainerKind (Array n a) = Type
type instance DegreeKind    (Array n a) = Nat
type instance LabelKind     (Array n a) = ()

type instance ContainerKind (RuntimeArray a) = Type
type instance DegreeKind    (RuntimeArray a) = ()
type instance LabelKind     (RuntimeArray a) = ()

-- need to separate the above open type family instances before all their uses
-- https://ghc.haskell.org/trac/ghc/ticket/15987#comment:2
$(pure [])

instance Contained (V n a) where
  type Container (V n a)   = V 0 a
  type DegreeOf  (V n a)   = n
  type LabelOf   (V n a) _ = '()
  type Overlapping (V n a) k _
    = TypeError (    Text "optic: attempt to index a vector component with name " :<>: ShowType k
                :$$: Text "Maybe you intended to use a swizzle?"
                )

instance Contained (M m n a) where
  type Container (M m n a)   = M m 0 a
  type DegreeOf  (M m n a)   = n
  type LabelOf   (M m n a) _ = '()
  type Overlapping (M m n a) k _
    = TypeError ( Text "optic: attempt to index a matrix component with name " :<>: ShowType k )

instance Contained (Struct as) where
  type Container (Struct as) = Struct
  type DegreeOf  (Struct as) = as
  type LabelOf   (Struct as) (Name_  k :: Optic _ (Struct as) a)
    = k ':-> a
  type LabelOf   (Struct as) (Index_ i :: Optic _ (Struct as) a)
    = Key ( StructElemFromIndex
              (Text "key: ")
              i as i as
          )
      ':-> a
  type Overlapping (Struct as) k i
    = k == Key (StructElemFromIndex (Text "key: ") i as i as)

instance Contained (Array n a) where
  type Container (Array n a)   = Array 0 a
  type DegreeOf  (Array n a)   = n
  type LabelOf   (Array n a) _ = '()
  type Overlapping (Array n a) k _
        = TypeError ( Text "optic: attempt to index an array using name " :<>: ShowType k )

instance Contained (RuntimeArray a) where
  type Container (RuntimeArray a)   = RuntimeArray a
  type DegreeOf  (RuntimeArray a)   = '()
  type LabelOf   (RuntimeArray a) _ = '()
  type Overlapping (RuntimeArray a) k _
        = TypeError ( Text "optic: attempt to index an array using name " :<>: ShowType k )

----------------------------------------------------------------------
-- synonyms

type family Col i = (r :: Optic '[] (M m n a) (V m a)) | r -> i where
  Col i = Index i
type family Ix i = (r :: Optic '[] (V m a) a) | r -> i where
  Ix i = Index i

type family Row (i :: Nat) = (optic :: Optic '[] (M m n a) (V n a)) | optic -> i where
  Row i = ( (     (Col 0 :.: Ix i) 
              :*: (Col 1 :.: Ix i)
            ) :: Optic '[] (M m 2 a) (V 2 a)
          )
  Row i = ( ( ( (     ( Col 0 :.: Ix i ) 
                  :*: ( Col 1 :.: Ix i )
                ) :: Optic '[] (M m 3 a) (V 2 a)
              )
              :*: ( Col 2 :.: Ix i )
            ) :: Optic '[] (M m 3 a) (V 3 a)
          )
  Row i = ( ( ( ( ( (     ( Col 0 :.: Ix i )
                      :*: ( Col 1 :.: Ix i )
                    ) :: Optic '[] (M m 4 a) (V 2 a)
                  )
                  :*: ( Col 2 :.: Ix i )
                ) :: Optic '[] (M m 4 a) (V 3 a)
              )
              :*: ( Col 3 :.: Ix i )
            ) :: Optic '[] (M m 4 a) (V 4 a)
          )

type Entry (i :: Nat) (j :: Nat) = ( (Col i :.: Ix j) :: Optic '[] (M m n a) a )

type family Diag :: Optic '[] (M n n a) (V n a) where
  Diag = ( (     (Col 0 :.: Ix 0)
             :*: (Col 1 :.: Ix 1)
           ) :: Optic '[] (M 2 2 a) (V 2 a)
         )
  Diag = ( ( ( (     (Col 0 :.: Ix 0)
                 :*: (Col 1 :.: Ix 1)
               ) :: Optic '[] (M 3 3 a) (V 2 a)
             ) :*: (Col 2 :.: Ix 2)
           ) :: Optic '[] (M 3 3 a) (V 3 a)
         )
  Diag = ( ( ( ( ( (    (Col 0 :.: Ix 0)
                    :*: (Col 1 :.: Ix 1)
                   ) :: Optic '[] (M 4 4 a) (V 2 a)
                 ) :*: (Col 2 :.: Ix 2)
               ) :: Optic '[] (M 4 4 a) (V 3 a)
             ) :*: (Col 3 :.: Ix 3)
           ) :: Optic '[] (M 4 4 a) (V 4 a)
         )

type Center = All Diag

type family SwizzleReturn (a :: Type) (ks :: [Symbol]) :: Type where
  SwizzleReturn a '[_] = a
  SwizzleReturn a  ks  = V (Length ks) a

type family Swizzle (swizzle :: [Symbol]) :: Optic '[] (V n a) (SwizzleReturn a swizzle) where
  Swizzle '[ ]   = TypeError ( Text "empty swizzle" )
  Swizzle '["x"] = Ix 0
  Swizzle '["y"] = Ix 1
  Swizzle '["z"] = Ix 2
  Swizzle '["w"] = Ix 3
  Swizzle '["r"] = Ix 0
  Swizzle '["g"] = Ix 1
  Swizzle '["b"] = Ix 2
  Swizzle '["a"] = Ix 3
  Swizzle '["s"] = Ix 0
  Swizzle '["t"] = Ix 1
  Swizzle '["p"] = Ix 2
  Swizzle '["q"] = Ix 3
  Swizzle '[k]
    = TypeError 
        (     Text "Unsupported swizzle identifier: " :<>: ShowType k :<>: Text "."
         :$$: Text "Supported swizzle characters: \"xyzw\", \"rgba\", \"stpq\"."
         :$$: Text "Note: swizzle must be a type-level list of single character symbols."
        )
  Swizzle '[k1,k2]
    = ( ProductO
          ( Swizzle '[k1] )
          ( Swizzle '[k2] )
      )
  Swizzle (k1 ': k2 ': ks)
    = ( ProductO
          ( Swizzle '[k1]        )
          ( Swizzle ( k2 ': ks ) )
      )

{-
type X = Swizzle '["x"]
type Y = Swizzle '["y"]
type Z = Swizzle '["z"]
type W = Swizzle '["w"]

type XX = Swizzle '["x","x"]
type XY = Swizzle '["x","y"]
type XZ = Swizzle '["x","z"]
type XW = Swizzle '["x","w"]
type YX = Swizzle '["y","x"]
type YY = Swizzle '["y","y"]
type YZ = Swizzle '["y","z"]
type YW = Swizzle '["y","w"]
type ZX = Swizzle '["z","x"]
type ZY = Swizzle '["z","y"]
type ZZ = Swizzle '["z","z"]
type ZW = Swizzle '["z","w"]
type WX = Swizzle '["w","x"]
type WY = Swizzle '["w","y"]
type WZ = Swizzle '["w","z"]
type WW = Swizzle '["w","w"]

type XXX = Swizzle '["x","x","x"]
type XXY = Swizzle '["x","x","y"]
type XXZ = Swizzle '["x","x","z"]
type XXW = Swizzle '["x","x","w"]
type XYX = Swizzle '["x","y","x"]
type XYY = Swizzle '["x","y","y"]
type XYZ = Swizzle '["x","y","z"]
type XYW = Swizzle '["x","y","w"]
type XZX = Swizzle '["x","z","x"]
type XZY = Swizzle '["x","z","y"]
type XZZ = Swizzle '["x","z","z"]
type XZW = Swizzle '["x","z","w"]
type XWX = Swizzle '["x","w","x"]
type XWY = Swizzle '["x","w","y"]
type XWZ = Swizzle '["x","w","z"]
type XWW = Swizzle '["x","w","w"]
type YXX = Swizzle '["y","x","x"]
type YXY = Swizzle '["y","x","y"]
type YXZ = Swizzle '["y","x","z"]
type YXW = Swizzle '["y","x","w"]
type YYX = Swizzle '["y","y","x"]
type YYY = Swizzle '["y","y","y"]
type YYZ = Swizzle '["y","y","z"]
type YYW = Swizzle '["y","y","w"]
type YZX = Swizzle '["y","z","x"]
type YZY = Swizzle '["y","z","y"]
type YZZ = Swizzle '["y","z","z"]
type YZW = Swizzle '["y","z","w"]
type YWX = Swizzle '["y","w","x"]
type YWY = Swizzle '["y","w","y"]
type YWZ = Swizzle '["y","w","z"]
type YWW = Swizzle '["y","w","w"]
type ZXX = Swizzle '["z","x","x"]
type ZXY = Swizzle '["z","x","y"]
type ZXZ = Swizzle '["z","x","z"]
type ZXW = Swizzle '["z","x","w"]
type ZYX = Swizzle '["z","y","x"]
type ZYY = Swizzle '["z","y","y"]
type ZYZ = Swizzle '["z","y","z"]
type ZYW = Swizzle '["z","y","w"]
type ZZX = Swizzle '["z","z","x"]
type ZZY = Swizzle '["z","z","y"]
type ZZZ = Swizzle '["z","z","z"]
type ZZW = Swizzle '["z","z","w"]
type ZWX = Swizzle '["z","w","x"]
type ZWY = Swizzle '["z","w","y"]
type ZWZ = Swizzle '["z","w","z"]
type ZWW = Swizzle '["z","w","w"]
type WXX = Swizzle '["w","x","x"]
type WXY = Swizzle '["w","x","y"]
type WXZ = Swizzle '["w","x","z"]
type WXW = Swizzle '["w","x","w"]
type WYX = Swizzle '["w","y","x"]
type WYY = Swizzle '["w","y","y"]
type WYZ = Swizzle '["w","y","z"]
type WYW = Swizzle '["w","y","w"]
type WZX = Swizzle '["w","z","x"]
type WZY = Swizzle '["w","z","y"]
type WZZ = Swizzle '["w","z","z"]
type WZW = Swizzle '["w","z","w"]
type WWX = Swizzle '["w","w","x"]
type WWY = Swizzle '["w","w","y"]
type WWZ = Swizzle '["w","w","z"]
type WWW = Swizzle '["w","w","w"]

type XXXX = Swizzle '["x","x","x","x"]
type XXXY = Swizzle '["x","x","x","y"]
type XXXZ = Swizzle '["x","x","x","z"]
type XXXW = Swizzle '["x","x","x","w"]
type XXYX = Swizzle '["x","x","y","x"]
type XXYY = Swizzle '["x","x","y","y"]
type XXYZ = Swizzle '["x","x","y","z"]
type XXYW = Swizzle '["x","x","y","w"]
type XXZX = Swizzle '["x","x","z","x"]
type XXZY = Swizzle '["x","x","z","y"]
type XXZZ = Swizzle '["x","x","z","z"]
type XXZW = Swizzle '["x","x","z","w"]
type XXWX = Swizzle '["x","x","w","x"]
type XXWY = Swizzle '["x","x","w","y"]
type XXWZ = Swizzle '["x","x","w","z"]
type XXWW = Swizzle '["x","x","w","w"]
type XYXX = Swizzle '["x","y","x","x"]
type XYXY = Swizzle '["x","y","x","y"]
type XYXZ = Swizzle '["x","y","x","z"]
type XYXW = Swizzle '["x","y","x","w"]
type XYYX = Swizzle '["x","y","y","x"]
type XYYY = Swizzle '["x","y","y","y"]
type XYYZ = Swizzle '["x","y","y","z"]
type XYYW = Swizzle '["x","y","y","w"]
type XYZX = Swizzle '["x","y","z","x"]
type XYZY = Swizzle '["x","y","z","y"]
type XYZZ = Swizzle '["x","y","z","z"]
type XYZW = Swizzle '["x","y","z","w"]
type XYWX = Swizzle '["x","y","w","x"]
type XYWY = Swizzle '["x","y","w","y"]
type XYWZ = Swizzle '["x","y","w","z"]
type XYWW = Swizzle '["x","y","w","w"]
type XZXX = Swizzle '["x","z","x","x"]
type XZXY = Swizzle '["x","z","x","y"]
type XZXZ = Swizzle '["x","z","x","z"]
type XZXW = Swizzle '["x","z","x","w"]
type XZYX = Swizzle '["x","z","y","x"]
type XZYY = Swizzle '["x","z","y","y"]
type XZYZ = Swizzle '["x","z","y","z"]
type XZYW = Swizzle '["x","z","y","w"]
type XZZX = Swizzle '["x","z","z","x"]
type XZZY = Swizzle '["x","z","z","y"]
type XZZZ = Swizzle '["x","z","z","z"]
type XZZW = Swizzle '["x","z","z","w"]
type XZWX = Swizzle '["x","z","w","x"]
type XZWY = Swizzle '["x","z","w","y"]
type XZWZ = Swizzle '["x","z","w","z"]
type XZWW = Swizzle '["x","z","w","w"]
type XWXX = Swizzle '["x","w","x","x"]
type XWXY = Swizzle '["x","w","x","y"]
type XWXZ = Swizzle '["x","w","x","z"]
type XWXW = Swizzle '["x","w","x","w"]
type XWYX = Swizzle '["x","w","y","x"]
type XWYY = Swizzle '["x","w","y","y"]
type XWYZ = Swizzle '["x","w","y","z"]
type XWYW = Swizzle '["x","w","y","w"]
type XWZX = Swizzle '["x","w","z","x"]
type XWZY = Swizzle '["x","w","z","y"]
type XWZZ = Swizzle '["x","w","z","z"]
type XWZW = Swizzle '["x","w","z","w"]
type XWWX = Swizzle '["x","w","w","x"]
type XWWY = Swizzle '["x","w","w","y"]
type XWWZ = Swizzle '["x","w","w","z"]
type XWWW = Swizzle '["x","w","w","w"]
type YXXX = Swizzle '["y","x","x","x"]
type YXXY = Swizzle '["y","x","x","y"]
type YXXZ = Swizzle '["y","x","x","z"]
type YXXW = Swizzle '["y","x","x","w"]
type YXYX = Swizzle '["y","x","y","x"]
type YXYY = Swizzle '["y","x","y","y"]
type YXYZ = Swizzle '["y","x","y","z"]
type YXYW = Swizzle '["y","x","y","w"]
type YXZX = Swizzle '["y","x","z","x"]
type YXZY = Swizzle '["y","x","z","y"]
type YXZZ = Swizzle '["y","x","z","z"]
type YXZW = Swizzle '["y","x","z","w"]
type YXWX = Swizzle '["y","x","w","x"]
type YXWY = Swizzle '["y","x","w","y"]
type YXWZ = Swizzle '["y","x","w","z"]
type YXWW = Swizzle '["y","x","w","w"]
type YYXX = Swizzle '["y","y","x","x"]
type YYXY = Swizzle '["y","y","x","y"]
type YYXZ = Swizzle '["y","y","x","z"]
type YYXW = Swizzle '["y","y","x","w"]
type YYYX = Swizzle '["y","y","y","x"]
type YYYY = Swizzle '["y","y","y","y"]
type YYYZ = Swizzle '["y","y","y","z"]
type YYYW = Swizzle '["y","y","y","w"]
type YYZX = Swizzle '["y","y","z","x"]
type YYZY = Swizzle '["y","y","z","y"]
type YYZZ = Swizzle '["y","y","z","z"]
type YYZW = Swizzle '["y","y","z","w"]
type YYWX = Swizzle '["y","y","w","x"]
type YYWY = Swizzle '["y","y","w","y"]
type YYWZ = Swizzle '["y","y","w","z"]
type YYWW = Swizzle '["y","y","w","w"]
type YZXX = Swizzle '["y","z","x","x"]
type YZXY = Swizzle '["y","z","x","y"]
type YZXZ = Swizzle '["y","z","x","z"]
type YZXW = Swizzle '["y","z","x","w"]
type YZYX = Swizzle '["y","z","y","x"]
type YZYY = Swizzle '["y","z","y","y"]
type YZYZ = Swizzle '["y","z","y","z"]
type YZYW = Swizzle '["y","z","y","w"]
type YZZX = Swizzle '["y","z","z","x"]
type YZZY = Swizzle '["y","z","z","y"]
type YZZZ = Swizzle '["y","z","z","z"]
type YZZW = Swizzle '["y","z","z","w"]
type YZWX = Swizzle '["y","z","w","x"]
type YZWY = Swizzle '["y","z","w","y"]
type YZWZ = Swizzle '["y","z","w","z"]
type YZWW = Swizzle '["y","z","w","w"]
type YWXX = Swizzle '["y","w","x","x"]
type YWXY = Swizzle '["y","w","x","y"]
type YWXZ = Swizzle '["y","w","x","z"]
type YWXW = Swizzle '["y","w","x","w"]
type YWYX = Swizzle '["y","w","y","x"]
type YWYY = Swizzle '["y","w","y","y"]
type YWYZ = Swizzle '["y","w","y","z"]
type YWYW = Swizzle '["y","w","y","w"]
type YWZX = Swizzle '["y","w","z","x"]
type YWZY = Swizzle '["y","w","z","y"]
type YWZZ = Swizzle '["y","w","z","z"]
type YWZW = Swizzle '["y","w","z","w"]
type YWWX = Swizzle '["y","w","w","x"]
type YWWY = Swizzle '["y","w","w","y"]
type YWWZ = Swizzle '["y","w","w","z"]
type YWWW = Swizzle '["y","w","w","w"]
type ZXXX = Swizzle '["z","x","x","x"]
type ZXXY = Swizzle '["z","x","x","y"]
type ZXXZ = Swizzle '["z","x","x","z"]
type ZXXW = Swizzle '["z","x","x","w"]
type ZXYX = Swizzle '["z","x","y","x"]
type ZXYY = Swizzle '["z","x","y","y"]
type ZXYZ = Swizzle '["z","x","y","z"]
type ZXYW = Swizzle '["z","x","y","w"]
type ZXZX = Swizzle '["z","x","z","x"]
type ZXZY = Swizzle '["z","x","z","y"]
type ZXZZ = Swizzle '["z","x","z","z"]
type ZXZW = Swizzle '["z","x","z","w"]
type ZXWX = Swizzle '["z","x","w","x"]
type ZXWY = Swizzle '["z","x","w","y"]
type ZXWZ = Swizzle '["z","x","w","z"]
type ZXWW = Swizzle '["z","x","w","w"]
type ZYXX = Swizzle '["z","y","x","x"]
type ZYXY = Swizzle '["z","y","x","y"]
type ZYXZ = Swizzle '["z","y","x","z"]
type ZYXW = Swizzle '["z","y","x","w"]
type ZYYX = Swizzle '["z","y","y","x"]
type ZYYY = Swizzle '["z","y","y","y"]
type ZYYZ = Swizzle '["z","y","y","z"]
type ZYYW = Swizzle '["z","y","y","w"]
type ZYZX = Swizzle '["z","y","z","x"]
type ZYZY = Swizzle '["z","y","z","y"]
type ZYZZ = Swizzle '["z","y","z","z"]
type ZYZW = Swizzle '["z","y","z","w"]
type ZYWX = Swizzle '["z","y","w","x"]
type ZYWY = Swizzle '["z","y","w","y"]
type ZYWZ = Swizzle '["z","y","w","z"]
type ZYWW = Swizzle '["z","y","w","w"]
type ZZXX = Swizzle '["z","z","x","x"]
type ZZXY = Swizzle '["z","z","x","y"]
type ZZXZ = Swizzle '["z","z","x","z"]
type ZZXW = Swizzle '["z","z","x","w"]
type ZZYX = Swizzle '["z","z","y","x"]
type ZZYY = Swizzle '["z","z","y","y"]
type ZZYZ = Swizzle '["z","z","y","z"]
type ZZYW = Swizzle '["z","z","y","w"]
type ZZZX = Swizzle '["z","z","z","x"]
type ZZZY = Swizzle '["z","z","z","y"]
type ZZZZ = Swizzle '["z","z","z","z"]
type ZZZW = Swizzle '["z","z","z","w"]
type ZZWX = Swizzle '["z","z","w","x"]
type ZZWY = Swizzle '["z","z","w","y"]
type ZZWZ = Swizzle '["z","z","w","z"]
type ZZWW = Swizzle '["z","z","w","w"]
type ZWXX = Swizzle '["z","w","x","x"]
type ZWXY = Swizzle '["z","w","x","y"]
type ZWXZ = Swizzle '["z","w","x","z"]
type ZWXW = Swizzle '["z","w","x","w"]
type ZWYX = Swizzle '["z","w","y","x"]
type ZWYY = Swizzle '["z","w","y","y"]
type ZWYZ = Swizzle '["z","w","y","z"]
type ZWYW = Swizzle '["z","w","y","w"]
type ZWZX = Swizzle '["z","w","z","x"]
type ZWZY = Swizzle '["z","w","z","y"]
type ZWZZ = Swizzle '["z","w","z","z"]
type ZWZW = Swizzle '["z","w","z","w"]
type ZWWX = Swizzle '["z","w","w","x"]
type ZWWY = Swizzle '["z","w","w","y"]
type ZWWZ = Swizzle '["z","w","w","z"]
type ZWWW = Swizzle '["z","w","w","w"]
type WXXX = Swizzle '["w","x","x","x"]
type WXXY = Swizzle '["w","x","x","y"]
type WXXZ = Swizzle '["w","x","x","z"]
type WXXW = Swizzle '["w","x","x","w"]
type WXYX = Swizzle '["w","x","y","x"]
type WXYY = Swizzle '["w","x","y","y"]
type WXYZ = Swizzle '["w","x","y","z"]
type WXYW = Swizzle '["w","x","y","w"]
type WXZX = Swizzle '["w","x","z","x"]
type WXZY = Swizzle '["w","x","z","y"]
type WXZZ = Swizzle '["w","x","z","z"]
type WXZW = Swizzle '["w","x","z","w"]
type WXWX = Swizzle '["w","x","w","x"]
type WXWY = Swizzle '["w","x","w","y"]
type WXWZ = Swizzle '["w","x","w","z"]
type WXWW = Swizzle '["w","x","w","w"]
type WYXX = Swizzle '["w","y","x","x"]
type WYXY = Swizzle '["w","y","x","y"]
type WYXZ = Swizzle '["w","y","x","z"]
type WYXW = Swizzle '["w","y","x","w"]
type WYYX = Swizzle '["w","y","y","x"]
type WYYY = Swizzle '["w","y","y","y"]
type WYYZ = Swizzle '["w","y","y","z"]
type WYYW = Swizzle '["w","y","y","w"]
type WYZX = Swizzle '["w","y","z","x"]
type WYZY = Swizzle '["w","y","z","y"]
type WYZZ = Swizzle '["w","y","z","z"]
type WYZW = Swizzle '["w","y","z","w"]
type WYWX = Swizzle '["w","y","w","x"]
type WYWY = Swizzle '["w","y","w","y"]
type WYWZ = Swizzle '["w","y","w","z"]
type WYWW = Swizzle '["w","y","w","w"]
type WZXX = Swizzle '["w","z","x","x"]
type WZXY = Swizzle '["w","z","x","y"]
type WZXZ = Swizzle '["w","z","x","z"]
type WZXW = Swizzle '["w","z","x","w"]
type WZYX = Swizzle '["w","z","y","x"]
type WZYY = Swizzle '["w","z","y","y"]
type WZYZ = Swizzle '["w","z","y","z"]
type WZYW = Swizzle '["w","z","y","w"]
type WZZX = Swizzle '["w","z","z","x"]
type WZZY = Swizzle '["w","z","z","y"]
type WZZZ = Swizzle '["w","z","z","z"]
type WZZW = Swizzle '["w","z","z","w"]
type WZWX = Swizzle '["w","z","w","x"]
type WZWY = Swizzle '["w","z","w","y"]
type WZWZ = Swizzle '["w","z","w","z"]
type WZWW = Swizzle '["w","z","w","w"]
type WWXX = Swizzle '["w","w","x","x"]
type WWXY = Swizzle '["w","w","x","y"]
type WWXZ = Swizzle '["w","w","x","z"]
type WWXW = Swizzle '["w","w","x","w"]
type WWYX = Swizzle '["w","w","y","x"]
type WWYY = Swizzle '["w","w","y","y"]
type WWYZ = Swizzle '["w","w","y","z"]
type WWYW = Swizzle '["w","w","y","w"]
type WWZX = Swizzle '["w","w","z","x"]
type WWZY = Swizzle '["w","w","z","y"]
type WWZZ = Swizzle '["w","w","z","z"]
type WWZW = Swizzle '["w","w","z","w"]
type WWWX = Swizzle '["w","w","w","x"]
type WWWY = Swizzle '["w","w","w","y"]
type WWWZ = Swizzle '["w","w","w","z"]
type WWWW = Swizzle '["w","w","w","w"]
-}