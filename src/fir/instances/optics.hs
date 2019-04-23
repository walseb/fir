{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
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
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If )
import Data.Type.Equality
  ( type (==) )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word32 )
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
import Data.Distributive
  ( Distributive(..) )

-- vector
import qualified Data.Vector as Array

-- fir
import Control.Monad.Indexed
  ( (:=) )
import Control.Type.Optic
  ( Optic(..)
  , Indices
  , Gettable, ReifiedGetter(view)
  , Settable, ReifiedSetter(set )
  , Contained(..)
  , ContainerKind, DegreeKind, LabelKind
  , MonoContained(..)
  , (:.:), (:*:), Id, AnIndex, Name, Joint
  , Product
  )
import Data.Type.Known
  ( Known )
import Data.Function.Variadic
  ( ListVariadic )
import Data.Type.List
  ( type (:++:), Zip
  , SLength, KnownLength(sLength)
  , Postpend
  )
import Data.Type.Map
  ( (:->)((:->))
  , Key, Value
  , Lookup
  )
import qualified FIR.Instances.Bindings as Binding
import FIR.ASTState
  ( ASTState )
import FIR.Prim.Array
  ( Array(MkArray), RuntimeArray(MkRuntimeArray) )
import FIR.Prim.Image
  ( Image, ImageProperties, ImageData
  , ImageOperands, ImageCoordinates )
import FIR.Prim.Singletons
  ( PrimTy(primTySing), IntegralTy
  , ScalarTy(scalarTySing), SScalarTy
  , PrimTyMap
  , SPrimTy(SStruct)
  , HasField(fieldIndex)
  )
import FIR.Prim.Struct
  ( Struct((:&), End) )
import Math.Linear
  ( V((:.)), M(M), (^!), at )

----------------------------------------------------------------------
-- singletons

data SOptic (optic :: Optic i s a) :: Type where
  SId    :: SOptic Id
  SJoint :: SOptic Joint
  -- for indices, we additional specify the accessee, to distinguish
  -- the different methods SPIR-V supports for access:
  --  - vectors with VectorExtractDynamic / VectorInsertDynamic
  --  - arrays with OpAccessChain
  SAnIndex :: SPrimTy s -> SPrimTy a -> SScalarTy ix -> SOptic (AnIndex ix :: Optic '[ix] s a)
  SIndex   :: SPrimTy s -> SPrimTy a -> Word32 -> SOptic (o :: Optic '[] s a)
  -- we allow an overly-general return type for the above,
  -- as we convert 'Name' optics to 'Index' optics behind the scenes
  SBinding  :: KnownSymbol k
            => Proxy k
            -> SOptic (Name k :: Optic '[] (as :: ASTState) b)
  SImageTexel :: ( KnownSymbol k
                 , Known ImageProperties props
                 )
              => Proxy k
              -> Proxy props
              -> SOptic ( (  ( Name_  k :: Optic '[] i (Image props) )
                             `ComposeO`
                             ( RTOptic_ :: Optic
                                            '[ ImageOperands props ops, ImageCoordinates props ops ]
                                             (Image props)
                                             (ImageData props ops)
                             )
                          ) :: Optic
                                '[ ImageOperands props ops, ImageCoordinates props ops ]
                                i
                                (ImageData props ops)
                        )
  SComposeO :: forall is js s a b (o1 :: Optic is s a) (o2 :: Optic js a b).
               SLength is -> SOptic o1 -> SOptic o2 -> SOptic (o1 :.: o2)
     --        ^^^^^^^^^^
     -- we need to know the length of the first list to generate code for composite optics
     -- see the function 'opticalTree' in the code generator

     -- similar remark applies to products
  SProductO  :: forall is js s a b (o1 :: Optic is s a) (o2 :: Optic js s b).
                PrimTy (Product o1 o2) -- need to know result for SPIR-V code generation
             => SLength is -> SLength js -> SOptic o1 -> SOptic o2 -> SOptic (o1 :*: o2)

infixr 9 %:.:
infixr 3 %:*:

(%:.:) :: forall is js s a b (o1 :: Optic is s a) (o2 :: Optic js a b).
           KnownLength is
        => SOptic o1 -> SOptic o2 -> SOptic (o1 :.: o2)
o1 %:.: o2 = SComposeO (sLength @_ @is) o1 o2

(%:*:) :: forall is js s a b (o1 :: Optic is s a) (o2 :: Optic js s b).
          ( KnownLength is, KnownLength js, PrimTy (Product o1 o2) )
        => SOptic o1 -> SOptic o2 -> SOptic (o1 :*: o2)
o1 %:*: o2 = SProductO (sLength @_ @is) (sLength @_ @js) o1 o2

showSOptic :: SOptic (o :: Optic is s a) -> String
showSOptic SId    = "Id"
showSOptic SJoint = "Joint"
showSOptic (SAnIndex _ _ _) = "AnIndex"
showSOptic (SIndex   _ _ n) = "Index "   ++ show n
showSOptic (SBinding    k  ) = "Binding "    ++ show (symbolVal k)
showSOptic (SImageTexel k _) = "ImageTexel " ++ show (symbolVal k)
showSOptic (SComposeO _   o1 o2) = showSOptic o1 ++ " :.: " ++ showSOptic o2
showSOptic (SProductO _ _ o1 o2) = showSOptic o1 ++ " :*: " ++ showSOptic o2


class KnownLength (Indices optic) => KnownOptic optic where
  opticSing :: SOptic optic

instance ( empty ~ '[] ) => KnownOptic (Id_ :: Optic empty a a) where
  opticSing = SId
instance ( empty ~ '[]
         , MonoContained a
         , mono ~ MonoType a
         )
       => KnownOptic (Joint_ :: Optic empty a mono) where
  opticSing = SJoint
instance ( KnownNat n
         , empty ~ '[]
         , PrimTy s
         , PrimTy a
         )
       => KnownOptic (Index_ n :: Optic empty s a)
       where
  opticSing = SIndex (primTySing @s) (primTySing @a) (fromIntegral . natVal $ Proxy @n)
instance ( KnownSymbol k
         , PrimTyMap as
         , PrimTy a
         , HasField k as
         , empty ~ '[]
         ) => KnownOptic (Name_ k :: Optic empty (Struct as) a)
         where
  opticSing = SIndex (SStruct @as) (primTySing @a) (fieldIndex @k @as)
instance ( KnownSymbol k
         , empty ~ '[]
         , a ~ Binding.Has k bds
         )
      => KnownOptic (Name_ k :: Optic empty (bds :: ASTState) a)
      where
  opticSing = SBinding (Proxy @k)
instance forall is js ks (s :: Type) a b (o1 :: Optic is s a) (o2 :: Optic js a b).
         ( KnownOptic o1
         , KnownOptic o2
         , ks ~ (is :++: js)
         , KnownLength is
         , KnownLength js
         , KnownLength ks -- deduced from the above two in concrete situations
         )
       => KnownOptic ((o1 `ComposeO` o2) :: Optic ks s b) where
  opticSing = (opticSing @o1) %:.: (opticSing @o2)
instance forall k is js ks (s :: ASTState) x a b
                (o1 :: Optic is s x) (o2 :: Optic js x b) (o :: Optic ks a b)
                .
         ( KnownSymbol k
         , KnownOptic o
         , ( ( (o1 `ComposeO` o2) :: Optic ks s b )
             ~
             ( (Name_ k :: Optic '[] s a) `ComposeO` (o :: Optic ks a b) )
            )
         , KnownLength ks
         , ks ~ (is :++: js)
         , a ~ Binding.Has k s
         )
       => KnownOptic ( (o1 `ComposeO` o2) :: Optic ks s b ) where
  opticSing = SBinding (Proxy @k) %:.: (opticSing @o)
instance forall is js ks s a b c (o1 :: Optic is s a) (o2 :: Optic js s b).
         ( KnownOptic o1
         , KnownOptic o2
         , ks ~ Zip is js
         , c ~ Product o1 o2
         , KnownLength is
         , KnownLength js
         , KnownLength (Zip is js) -- deduced from the above two in concrete situations
         , PrimTy c
         )
      => KnownOptic ((o1 `ProductO` o2) :: Optic ks s c) where
  opticSing = (opticSing @o1) %:*: (opticSing @o2)

--------------------------------------------------------------
-- some trickery to account for peculiarities of image optics

-- normal run-time index optic, not allowed to be used for images on its own
instance ( ValidAnIndexOptic is s a, is ~ '[ix], IntegralTy ix, PrimTy s, PrimTy a )
  => KnownOptic (RTOptic_ :: Optic is s a)
  where
  opticSing = SAnIndex (primTySing @s) (primTySing @a) (scalarTySing @ix)


-- binding + image texel optic... the two parts must always occur together
instance {-# OVERLAPPING #-}
    ( KnownSymbol k, Binding.ProvidedSymbol k
    , Known ImageProperties props
    , empty ~ '[]
    , imgOps ~ ImageOperands props ops
    , imgCds ~ ImageCoordinates props ops
    , r ~ ImageData props ops
    )
  => KnownOptic ( ( ( Name_ k :: Optic empty i (Image props) )
                    `ComposeO`
                    ( RTOptic_ :: Optic '[imgOps, imgCds] (Image props) r)
                  ) :: Optic '[imgOps, imgCds] (i :: ASTState) r
                )
  where
  opticSing = SImageTexel (Proxy @k) (Proxy @props)

type family ValidAnIndexOptic (is :: [Type]) (s :: Type) (a :: Type) :: Constraint where
  ValidAnIndexOptic '[] _ _
    = TypeError ( Text "Run-time optic does not specify the type of its run-time indices." )
  ValidAnIndexOptic _ (Image _) _
    = TypeError (    Text "Forbidden standalone image texel optic."
                :$$: Text "Optics for image texels must be copresent with a binding optic."
                )
  ValidAnIndexOptic '[ix] _ _ = ()
  ValidAnIndexOptic is _ _
    = TypeError (    Text "Run-time optic specifies more than one type of index:"
                :$$: Text "    " :<>: ShowType is
                )

----------------------------------------------------------------------
-- Instances for compositions involving indexed monadic state.
-- These are provided for improved type inference,
-- by requiring that the part focused onto by the outer optic
-- is as dictated by the state.

instance forall (k :: Symbol) (i :: ASTState) empty js ks a b (o2 :: Optic js a b).
         ( KnownSymbol k
         , Gettable o2
         , empty ~ '[]
         , ks ~ js
         , a ~ Binding.Get k i -- this is the additional line that helps type inference
         ) => Gettable ( ((Name_ k :: Optic empty i a) `ComposeO` o2) :: Optic ks i b )
         where
instance forall (k :: Symbol) (i :: ASTState) empty js ks a b (o2 :: Optic js a b).
         ( KnownSymbol k
         , Settable o2
         , empty ~ '[]
         , ks ~ js
         , a ~ Binding.Put k i -- ditto
         ) => Settable ( ((Name_ k :: Optic empty i a) `ComposeO` o2) :: Optic ks i b )
         where

----------------------------------------------------------------------

type StatefulOptic (optic :: Optic is (s :: ASTState) a) = ( () :: Constraint )
-- to distinguish from (optic :: Optic is (s :: Type) a)

type family ListVariadicIx
              ( as :: [Type]   )
              ( i  :: ASTState )
              ( b  :: Type     )
            = ( r  :: Type     )
            | r -> as i b  where
  ListVariadicIx '[]       i b = (b := i) i
  ListVariadicIx (a ': as) i b = a -> ListVariadicIx as i b

----------------------------------------------------------------------
-- getters

type User   (g :: Optic as i b) = ListVariadicIx as i b
type Viewer (g :: Optic is s a) = ListVariadic (is `Postpend` s) a

-- bindings
instance ( KnownSymbol k
         , r ~ Binding.Get k i
         , empty ~ '[]
         )
      => Gettable (Name_ k :: Optic empty (i :: ASTState) r) where


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
      => Gettable (RTOptic_ :: Optic ix (Array n a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , Gettable (RTOptic_ :: Optic ix (Array n a) r)
         , PrimTy a
         )
      => ReifiedGetter (RTOptic_ :: Optic ix (Array n a) r) where
  view i (MkArray arr) = arr Array.! fromIntegral i

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Gettable (RTOptic_ :: Optic ix (RuntimeArray a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , Gettable (RTOptic_ :: Optic ix (RuntimeArray a) r)
         , PrimTy a
         )
      => ReifiedGetter (RTOptic_ :: Optic ix (RuntimeArray a) r) where
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
    => Gettable (RTOptic_ :: Optic ix (Struct as) r) where


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
       => Gettable (RTOptic_ :: Optic ix (V n a) r) where
instance ( IntegralTy ty
         , r ~ a
         , ix ~ '[ty]
         , PrimTy a
         )
      => ReifiedGetter
            (RTOptic_ :: Optic ix (V n a) r)
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
        => Gettable (RTOptic_ :: Optic ix (M m n a) r) where
instance ( KnownNat m
         , KnownNat n
         , IntegralTy ty
         , r ~ V m a
         , ix ~ '[ty]
         )
      => ReifiedGetter
            (RTOptic_ :: Optic ix (M m n a) r)
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

type Assigner (g :: Optic as i b) = ListVariadicIx (as `Postpend` b) i ()
type Setter   (g :: Optic is s a) = ListVariadic   (is `Postpend` a `Postpend` s) s


-- bindings
instance ( KnownSymbol k
         , r ~ Binding.Put k i
         , empty ~ '[]
         )
      => Settable (Name_ k :: Optic empty (i :: ASTState) r ) where

  

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
      => Settable (RTOptic_ :: Optic ix (Array n a) r) where
instance ( IntegralTy ty
         , r ~ a
         , ix ~ '[ty]
         , Settable (RTOptic_ :: Optic ix (Array n a) r)
         )
      => ReifiedSetter
           (RTOptic_ :: Optic ix (Array n a) r)
      where
  set i a (MkArray arr) = MkArray ( arr Array.// [(fromIntegral i, a)] )

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Settable (RTOptic_ :: Optic ix (RuntimeArray a) r) where
instance ( IntegralTy ty
         , r ~ a
         , ix ~ '[ty]
         , Settable (RTOptic_ :: Optic ix (RuntimeArray a) r)
         )
      => ReifiedSetter
           (RTOptic_ :: Optic ix (RuntimeArray a) r)
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
    => Settable (RTOptic_ :: Optic ix (Struct as) r) where

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
    => Settable (RTOptic_ :: Optic ix (V n a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , KnownNat n, 1 <= n
         )
    => ReifiedSetter (RTOptic_ :: Optic ix (V n a) r) where
  set i b (a :. as)
    = if i == 0
      then b :. as
      else a :. set @(RTOptic_ :: Optic ix (V (n-1) a) r) (i-1) b as

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
       => Settable (RTOptic_ :: Optic ix (M m n a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ V m a
         , KnownNat m, KnownNat n, 1 <= n
         )
       => ReifiedSetter (RTOptic_ :: Optic ix (M m n a) r)
       where
  set i c (M m)
    = ( M
      . distribute
      . set @(RTOptic_ :: Optic ix (V n (V m a)) (V m a)) i c
      . distribute
      ) m

instance
    TypeError (    Text "set: attempt to update matrix column \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Settable (Name_ k :: Optic empty (M m n a) r) where

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
-- needed because of [GHC trac #12088](https://gitlab.haskell.org/ghc/ghc/issues/12088)
-- see also [GHC trac #15987](https://gitlab.haskell.org/ghc/ghc/issues/15987#note_164461)
$(pure [])

instance Contained (V n a) where
  type Container (V n a)   = V 0 a
  type DegreeOf  (V n a)   = n
  type LabelOf   (V n a) _ = '()
  type Overlapping (V n a) k _
    = TypeError (    Text "optic: attempt to index a vector component with name " :<>: ShowType k
                :$$: Text "Maybe you intended to use a swizzle?"
                )

instance KnownNat n => Contained (M m n a) where
  type Container (M m n a)   = M 0 n a
  type DegreeOf  (M m n a)   = m
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
-- type class instances for equalisers

instance KnownNat n => MonoContained (Array n a) where
  type MonoType (Array n a) = a
  setAll a _ = MkArray @n $ Array.replicate (fromIntegral . natVal $ Proxy @n) a

instance MonoContained (RuntimeArray a) where
  type MonoType (RuntimeArray a) = a
  setAll a (MkRuntimeArray arr)
    = MkRuntimeArray $ Array.replicate n a
        where n = Array.length arr

instance (KnownNat n, 1 <= n)
      => MonoContained (V n a) where
  type MonoType (V n a) = a
  setAll = const . pure

instance (KnownNat m, KnownNat n, 1 <= m)
      => MonoContained (M m n a) where
  type MonoType (M m n a) = V m a
  setAll = const . M . distribute . pure

instance MonoContained (Struct ((k ':-> v) ': '[]))
      where
  type MonoType (Struct ((k ':-> v) ': '[])) = v
  setAll = const . (:& End)

instance {-# OVERLAPPABLE #-}
         ( AllValuesEqual k v as
         , MonoContained (Struct as)
         , MonoType (Struct as) ~ v
         )
       => MonoContained (Struct ((k ':-> v) ': as)) where
  type MonoType (Struct ((k ':-> v) ': as)) = v
  setAll a (_ :& as) = a :& setAll a as

type family AllValuesEqual (k0 :: Symbol) (a :: v) (as :: [k :-> v]) :: Constraint where
  AllValuesEqual _ _ '[]                 = ()
  AllValuesEqual k0 v ( (_ ':-> v) ': as) = AllValuesEqual k0 v as
  AllValuesEqual k0 v ( (k ':-> w) ': _ )
    = TypeError (      Text "Cannot create 'Joint' setter for struct type."
                  :$$: Text "Struct contains members of different types."
                  :$$: Text "Type at key " :<>: ShowType k0 :<>: Text " is:"
                  :$$: Text "    " :<>: ShowType v
                  :$$: Text "Type at key " :<>: ShowType k :<>: Text " is:"
                  :$$: Text "    " :<>: ShowType w
                )
