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
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Syntax.Optics

This module overloads 'Control.Type.Optic.view',
'Control.Type.Optic.set', and 'Control.Type.Optic.over'
to work with the AST,
in the way of getter/setter instances for types of the form @AST a@.

For instance:

> geometry = Module $ entrypoint @"main" @Geometry do
>   gl_in <- use @(Name "gl_in") -- geometry shader built-in input (structure array)
>   let pos0 = view @(Index 0 :.: Name "gl_Position") gl_in
>   ...

Singletons for optics are also provided, to assist in displaying
the names of optics, and internally for code generation.
-}

module FIR.Syntax.Optics where

-- base
import Data.Kind
  ( Type, Constraint )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol, KnownSymbol, symbolVal
  , TypeError
  , ErrorMessage(Text, ShowType, (:<>:), (:$$:))
  )
import GHC.TypeNats
  ( Nat, KnownNat, natVal
  , CmpNat, type (<=), type (-)
  )

-- typelits-witnesses
import GHC.TypeLits.Compare
  ( (:<=?)(LE), (%<=?) )

-- vector
import qualified Data.Vector as Unsized.Vector
  ( (!), (//) )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( index, (//) )

-- fir
import Control.Monad.Indexed
  ( (:=) )
import Control.Type.Optic
  ( Optic(..)
  , Indices
  , Gettable, ReifiedGetter(view)
  , Settable, ReifiedSetter(set )
  , Container(..)
  , (:.:), AnIndex, Name, OfType
  , ProductComponents(..)
  )
import Data.Constraint.All
  ( All )
import Data.Function.Variadic
  ( ListVariadic )
import Data.Product
  ( IsProduct, AreProducts
  , Distribute
  , MapHList
  )
import Data.Type.Known
  ( Known )
import Data.Type.LazyEquality
  ( LazyEq )
import Data.Type.List
  ( type (:++:), ZipCons
  , SLength, KnownLength(sLength)
  , SSameLength, SameLength(sSameLength)
  , Postpend
  )
import Data.Type.Map
  ( (:->)((:->))
  , Key
  )
import FIR.Prim.Array
  ( Array(..), RuntimeArray(..) )
import FIR.Prim.Image
  ( Image, ImageProperties
  , ImageOperands, OperandName
  )
import FIR.Prim.Struct
  ( Struct((:&))
  , HasStructField(getStructField, setStructField)
  )
import FIR.Prim.Types
  ( PrimTy(primTySing)
  , IntegralTy
  , ScalarTy(scalarTySing), SScalarTy
  , PrimTyMap
  , SPrimTy(SStruct)
  , HasOpaqueType
  )
import FIR.ProgramState
  ( ProgramState )
import qualified FIR.Validation.Bindings as Binding
  ( Has, CanGet, CanPut )
import FIR.Validation.Bounds
  ( VectorIndexInBounds
  , MatrixColumnIndexInBounds
  , ArrayIndexInBounds
  , StructFieldFromIndex, StructIndexFromName
  )
import FIR.Validation.Images
  ( LookupImageProperties, ImageTexelType )
import Math.Linear
  ( V((:.)), M(M)
  , (^!), at, replaceV
  )

----------------------------------------------------------------------
-- singletons

-- | Singletons associated to the type-level optics used by this library.
data SOptic (optic :: Optic i s a) :: Type where
  SOfType :: SPrimTy s -> SPrimTy a -> SOptic (OfType a :: Optic '[] s a)
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
            -> SOptic (Name k :: Optic '[] (as :: ProgramState) b)
  SImageTexel :: ( KnownSymbol k
                 , Known ImageProperties props
                 )
              => Proxy k
              -> Proxy props
              -> SPrimTy (ImageTexelType props ops)
              -> SOptic ( (  ( Field_ (k :: Symbol) :: Optic '[] i (Image props) )
                             `ComposeO`
                             ( RTOptic_ :: Optic
                                            '[ ImageOperands props ops, imgCds ]
                                             (Image props)
                                             (ImageTexelType props ops)
                             )
                          ) :: Optic
                                '[ ImageOperands props ops, imgCds ]
                                i
                                (ImageTexelType props ops)
                        )
  SComposeO :: forall is js s a b (o1 :: Optic is s a) (o2 :: Optic js a b)
            .  SLength is -> SOptic o1 -> SOptic o2 -> SOptic (o1 :.: o2)
     --        ^^^^^^^^^^
     -- we need to know the length of the first list to generate code for composite optics
     -- see the function 'opticalTree' in the code generator

     -- similar remark applies to products
  SProd :: forall
              ( iss :: [[Type]] )
              ( k   :: Type     )
              ( s   :: k        )
              ( as  :: [Type]   )
              ( os  :: ProductComponents iss s as)
              ( js  :: [Type]   )
              ( p   :: Type     )
        .  ( IsProduct p as
           , AreProducts js iss as
           , PrimTy p -- need to know result for SPIR-V code generation
           , All PrimTy as
           )
        => SSameLength (Distribute iss as) as
        -> SProductComponents os
        -> SOptic (Prod_ os :: Optic js s p)

data SProductComponents (os :: ProductComponents iss s as) :: Type where
  SEndProd  :: SProductComponents EndProd_
  SProductO :: forall
                ( k   :: Type     )
                ( is  :: [Type]   )
                ( s   :: k        )
                ( a   :: Type     )
                ( iss :: [[Type]] )
                ( as  :: [Type]   )
                ( o   :: Optic is s a )
                ( os  :: ProductComponents iss s as )
            .  SameLength is iss
            => SOptic o
            -> SProductComponents os
            -> SProductComponents (ProductO o os :: ProductComponents (ZipCons is iss) s (a ': as))

infixr 9 %:.:

(%:.:) :: forall is js s a b (o1 :: Optic is s a) (o2 :: Optic js a b).
           KnownLength is
        => SOptic o1 -> SOptic o2 -> SOptic (o1 :.: o2)
o1 %:.: o2 = SComposeO (sLength @_ @is) o1 o2

showSOptic :: SOptic (o :: Optic is s a) -> String
showSOptic (SOfType _ sTy ) = "OfType " ++ show sTy
showSOptic (SAnIndex _ _ _) = "AnIndex"
showSOptic (SIndex   _ _ n) = "Index "   ++ show n
showSOptic (SBinding    k  ) = "Binding "    ++ show (symbolVal k)
showSOptic (SImageTexel k _ _) = "ImageTexel " ++ show (symbolVal k)
showSOptic (SComposeO _   o1 o2) = showSOptic o1 ++ " :.: " ++ showSOptic o2
showSOptic (SProd _ comps) = "Prod ( " ++ showSProductComponents comps ++ " )"

showSProductComponents :: SProductComponents os -> String
showSProductComponents SEndProd = "EndProd"
showSProductComponents (o `SProductO` os)
  = showSOptic o ++ " :*: " ++ showSProductComponents os


class KnownLength (Indices optic) => KnownOptic optic where
  opticSing :: SOptic optic

instance ( empty ~ '[]
         , a ~ ty
         , Container s
         , PrimTy s
         , PrimTy a
         )
       => KnownOptic (OfType_ ty :: Optic empty s a) where
  opticSing = SOfType (primTySing @s) (primTySing @a)
instance ( KnownNat n
         , empty ~ '[]
         , PrimTy s
         , PrimTy a
         )
       => KnownOptic (Field_ (n :: Nat) :: Optic empty s a)
       where
  opticSing = SIndex (primTySing @s) (primTySing @a) (fromIntegral . natVal $ Proxy @n)
instance forall (as :: [Symbol :-> Type]) (k :: Symbol) (a :: Type)
                (empty :: [Type]) (i :: Nat)
         . ( KnownSymbol k
           , PrimTyMap as
           , PrimTy a
           , KnownNat i
           , (i ':-> a) ~ StructIndexFromName k as
           , empty ~ '[]
           )
        => KnownOptic (Field_ (k :: Symbol) :: Optic empty (Struct as) a)
        where
  opticSing = SIndex (SStruct @Symbol @as) (primTySing @a) (fromIntegral . natVal $ Proxy @i)
instance ( KnownSymbol k
         , empty ~ '[]
         , a ~ Binding.Has k bds
         )
      => KnownOptic (Field_ (k :: Symbol) :: Optic empty (bds :: ProgramState) a)
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
instance forall k is js ks (s :: ProgramState) a b
                (o1 :: Optic is s a) (o2 :: Optic js a b)
                .
         ( KnownSymbol k
         , o1 ~ ( Field_ k :: Optic is s a )
         , KnownOptic o2
         , KnownLength ks
         , is ~ '[]
         , ks ~ (is :++: js)
         , a ~ Binding.Has k s
         )
       => KnownOptic ( (o1 `ComposeO` o2) :: Optic ks s b ) where
  opticSing = SBinding (Proxy @k) %:.: (opticSing @o2)
instance forall iss s as js p (os :: ProductComponents iss s as)
       . ( KnownComponents os
         , SameLength (Distribute iss as) as
         , KnownLength js
         , IsProduct p as
         , AreProducts js iss as
         , js ~ MapHList iss
         , PrimTy p
         , All PrimTy as
         )
       => KnownOptic (Prod_ os :: Optic js s p) where
  opticSing = SProd sSameLength (componentsSing @os)

class KnownComponents os where
  componentsSing :: SProductComponents os
instance ( as ~ '[] ) => KnownComponents ( EndProd_ :: ProductComponents iss s as )  where
  componentsSing = SEndProd
instance  forall
            ( k   :: Type     )
            ( is  :: [Type]   )
            ( s   :: k        )
            ( a   :: Type     )
            ( iss :: [[Type]] )
            ( jss :: [[Type]] )
            ( as  :: [Type]   )
            ( o   :: Optic is s a )
            ( os  :: ProductComponents iss s as )
           . ( KnownOptic o
             , KnownComponents os
             , SameLength is iss
             , jss ~ ZipCons is iss
             )
           => KnownComponents (o `ProductO` os :: ProductComponents jss s (a ': as)) where
  componentsSing = opticSing @o `SProductO` componentsSing @os

--------------------------------------------------------------
-- some trickery to account for peculiarities of image optics

-- normal run-time index optic, not allowed to be used for images on its own
instance ( ValidAnIndexOptic is s a, is ~ '[ix], IntegralTy ix, PrimTy s, PrimTy a )
  => KnownOptic (RTOptic_ :: Optic is s a)
  where
  opticSing = SAnIndex (primTySing @s) (primTySing @a) (scalarTySing @ix)


-- binding + image texel optic... the two parts must always occur together
instance {-# OVERLAPPING #-}
         forall
           ( k        :: Symbol          )
           ( i        :: ProgramState    )
           ( props    :: ImageProperties )
           ( ops      :: [OperandName]   )
           ( empty    :: [Type]          )
           ( imgOps   :: Type            )
           ( imgCds   :: Type            )
           ( imgTexel :: Type            )
         .
         ( KnownSymbol k
         , PrimTy imgCds
         , PrimTy imgTexel
         , LookupImageProperties k i ~ props
         , Known ImageProperties props
         , imgOps ~ ImageOperands props ops
         , imgTexel ~ ImageTexelType props ops
         , empty ~ '[]
         )
      => KnownOptic
            ( ( ( Field_ (k :: Symbol) :: Optic empty i (Image props) )
                `ComposeO`
                ( RTOptic_ :: Optic '[imgOps, imgCds] (Image props) imgTexel)
              ) :: Optic '[imgOps, imgCds] i imgTexel
            )
  where
  opticSing = SImageTexel (Proxy @k) (Proxy @props) (primTySing @imgTexel)

type family ValidAnIndexOptic (is :: [Type]) (s :: Type) (a :: Type) :: Constraint where
  ValidAnIndexOptic '[] _ _
    = TypeError ( Text "Run-time optic does not specify the type of its run-time indices." )
  ValidAnIndexOptic _ (Image _) _
    = TypeError (    Text "Forbidden standalone image texel optic."
                :$$: Text "Use the 'ImageTexel` optic, or an `imageRead`/`imageWrite` operation."
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

instance forall (k :: Symbol) (i :: ProgramState) empty js ks a b (o2 :: Optic js a b).
         ( KnownSymbol k
         , Gettable o2
         , empty ~ '[]
         , ks ~ js
         , a ~ Binding.Has k i -- this is the additional line that helps type inference
         , Binding.CanGet k i
         , NoOpaqueTypes "'get'/'use'" k b ( HasOpaqueType b )
         ) => Gettable ( ((Field_ (k :: Symbol) :: Optic empty i a) `ComposeO` o2) :: Optic ks i b )
         where
instance forall (k :: Symbol) (i :: ProgramState) empty js ks a b (o2 :: Optic js a b).
         ( KnownSymbol k
         , Settable o2
         , empty ~ '[]
         , ks ~ js
         , a ~ Binding.Has k i -- ditto
         , Binding.CanPut k i
         , NoOpaqueTypes "'put'/'assign'" k b ( HasOpaqueType b )
         ) => Settable ( ((Field_ (k :: Symbol) :: Optic empty i a) `ComposeO` o2) :: Optic ks i b )
         where

type family NoOpaqueTypes ( msg :: Symbol ) ( k :: Symbol ) ( b :: Type ) ( hasOpaque :: Bool ) :: Constraint where
  NoOpaqueTypes _   _ _ False = ( () :: Constraint )
  NoOpaqueTypes msg k b True  = TypeError
    (    Text msg :<>: Text ": cannot focus on type "
    :$$: Text "  " :<>: ShowType b
    :$$: Text "within binding named " :<>: ShowType k :<>: Text ", as it is (or contains) an opaque type."
    )

----------------------------------------------------------------------

type StatefulOptic (optic :: Optic is (s :: ProgramState) a) = ( () :: Constraint )
-- to distinguish from (optic :: Optic is (s :: Type) a)

type family ListVariadicIx
              ( as :: [Type] )
              ( i  :: ProgramState )
              ( b  :: Type  )
            = ( r  :: Type  )
            | r -> as i b  where
  ListVariadicIx '[]       i b = (b := i) i
  ListVariadicIx (a ': as) i b = a -> ListVariadicIx as i b

----------------------------------------------------------------------
-- getters

type User   (g :: Optic as i b) = ListVariadicIx as i b
type Viewer (g :: Optic is s a) = ListVariadic (is `Postpend` s) a

-- bindings
instance ( KnownSymbol k
         , r ~ Binding.Has k i
         , Binding.CanGet k i
         , empty ~ '[]
         )
      => Gettable (Field_ (k :: Symbol) :: Optic empty (i :: ProgramState) r) where


instance
  TypeError (     Text "get: cannot get a binding using a numeric index."
             :$$: Text "Use the binding's name instead."
            )
  => Gettable (Field_ (n :: Nat) :: Optic empty (i :: [Symbol :-> v]) r)
  where

-- arrays
instance ( KnownNat n, KnownNat i
         , ArrayIndexInBounds n i
         , r ~ a
         , empty ~ '[]
         )
      => Gettable (Field_ (i :: Nat) :: Optic empty (Array n a) r) where
instance ( KnownNat n, KnownNat i
         , r ~ a
         , empty ~ '[]
         , PrimTy a
         , Gettable (Field_ (i :: Nat) :: Optic empty (Array n a) r)
         )
       => ReifiedGetter (Field_ (i :: Nat) :: Optic empty (Array n a) r) where
  view (MkArray arr) = arr `Vector.index` fromIntegral (natVal (Proxy @i))

instance ( KnownNat i
         , empty ~ '[]
         , r ~ a
         )
      => Gettable (Field_ (i :: Nat) :: Optic empty (RuntimeArray a) r) where
instance ( KnownNat n, KnownNat i
         , empty ~ '[]
         , r ~ a
         , PrimTy a
         )
       => ReifiedGetter (Field_ (i :: Nat) :: Optic empty (RuntimeArray a) r) where
  view (MkRuntimeArray arr) = arr Unsized.Vector.! fromIntegral (natVal (Proxy @i))


instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Gettable (RTOptic_ :: Optic ix (Array n a) r) where
instance ( KnownNat n
         , IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , Gettable (RTOptic_ :: Optic ix (Array n a) r)
         , PrimTy a
         )
      => ReifiedGetter (RTOptic_ :: Optic ix (Array n a) r) where
  view i (MkArray arr) = arr `Vector.index` fromIntegral i

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Gettable (RTOptic_ :: Optic ix (RuntimeArray a) r) where
instance ( IntegralTy ty, ix ~ '[ty], r ~ a, PrimTy a )
      => ReifiedGetter (RTOptic_ :: Optic ix (RuntimeArray a) r) where
  view i (MkRuntimeArray arr) = arr Unsized.Vector.! fromIntegral i

instance
    TypeError (    Text "get: attempt to access array element \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Gettable (Field_ (k :: Symbol) :: Optic empty (Array n a) r) where


instance
    TypeError (    Text "get: attempt to access run-time array \
                        \element using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Gettable (Field_ (k :: Symbol) :: Optic empty (RuntimeArray a) r) where


-- structs
instance ( KnownSymbol k
         , (n ':-> r) ~ StructIndexFromName k as
         , empty ~ '[]
         )
       => Gettable (Field_ (k :: Symbol) :: Optic empty (Struct as) r) where
instance ( KnownSymbol k
         , HasStructField k r as
         , (n ':-> r) ~ StructIndexFromName k as
         , empty ~ '[]
         , PrimTy r
         )
      => ReifiedGetter
            (Field_ (k :: Symbol) :: Optic empty (Struct as) r)
      where
  view = getStructField @k @r @as

instance ( KnownNat n
         , (k ':-> r) ~ StructFieldFromIndex n as
         , empty ~ '[]
         ) => Gettable (Field_ (n :: Nat) :: Optic empty (Struct as) r) where
instance ( KnownNat n
         , empty ~ '[]
         , PrimTy r
         , HasStructField n r as
         , (k ':-> r) ~ StructFieldFromIndex n as
         )
      => ReifiedGetter
           (Field_ (n :: Nat) :: Optic empty (Struct as) r)
       where
  view = getStructField @n @r @as

instance
    TypeError (    Text "get: attempt to access struct element \
                        \using run-time index."
              :$$: Text "Structs can only be accessed using \
                        \compile-time indices, or field names."
              )
    => Gettable (RTOptic_ :: Optic ix (Struct as) r) where


-- vectors
instance ( KnownNat i
         , VectorIndexInBounds n i
         , empty ~ '[]
         )
      => Gettable (Field_ (i :: Nat) :: Optic empty (V n a) r) where
instance ( KnownNat n
         , KnownNat i
         , VectorIndexInBounds n i
         , empty ~ '[]
         , PrimTy a
         , CmpNat i n ~ 'LT
         , r ~ a
         )
      => ReifiedGetter
            (Field_ (i :: Nat) :: Optic empty (V n a) r)
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
    => Gettable (Field_ (k :: Symbol) :: Optic empty (V n a) r) where


-- matrices
instance ( KnownNat n
         , KnownNat i
         , r ~ V m a
         , MatrixColumnIndexInBounds m n i
         , empty ~ '[]
         )
      => Gettable (Field_ (i :: Nat) :: Optic empty (M m n a) r) where
instance ( KnownNat m
         , KnownNat n
         , KnownNat i
         , MatrixColumnIndexInBounds m n i
         , r ~ V m a
         , CmpNat i n ~ 'LT
         , empty ~ '[]
         )
      => ReifiedGetter
            (Field_ (i :: Nat) :: Optic empty (M m n a) r)
      where
  view (M cols) = at @i cols

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
  view i (M cols) = cols ^! fromIntegral i


instance
    TypeError (    Text "get: attempt to access matrix column \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Gettable (Field_ (k :: Symbol) :: Optic empty (M m n a) r) where

----------------------------------------------------------------------
-- setters

type Assigner (g :: Optic as i b) = ListVariadicIx (as `Postpend` b) i ()
type Setter   (g :: Optic is s a) = ListVariadic   (is `Postpend` a `Postpend` s) s


-- bindings
instance ( KnownSymbol k
         , r ~ Binding.Has k i
         , Binding.CanPut k i
         , empty ~ '[]
         )
      => Settable (Field_ (k :: Symbol) :: Optic empty (i :: ProgramState) r ) where

  

instance
    TypeError (    Text "set: cannot set binding using a numeric index."
              :$$: Text "Use the binding's name instead."
              )
    => Settable (Field_ (n :: Nat) :: Optic empty (i :: [Symbol :-> v]) r) where

-- arrays
instance ( KnownNat n, KnownNat i
         , ArrayIndexInBounds n i
         , empty ~ '[]
         , r ~ a
         )
      => Settable (Field_ (i :: Nat) :: Optic empty (Array n a) r) where
instance ( KnownNat n, KnownNat i
         , r ~ a
         , empty ~ '[]
         , Settable (Field_ (i :: Nat) :: Optic empty (Array n a) r)
         )
      => ReifiedSetter
           (Field_ (i :: Nat) :: Optic empty (Array n a) r)
      where
  set a (MkArray arr) = MkArray ( arr Vector.// [( fromIntegral (natVal (Proxy @i)), a)] )

instance ( KnownNat i
         , empty ~ '[]
         , r ~ a
         )
      => Settable (Field_ (i :: Nat) :: Optic empty (RuntimeArray a) r) where
instance ( KnownNat i
         , empty ~ '[]
         , r ~ a
         , PrimTy a
         )
      => ReifiedSetter
           (Field_ (i :: Nat) :: Optic empty (RuntimeArray a) r)
      where
  set a (MkRuntimeArray arr)
    = MkRuntimeArray ( arr Unsized.Vector.// [( fromIntegral (natVal (Proxy @i)), a)] )

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Settable (RTOptic_ :: Optic ix (Array n a) r) where
instance ( KnownNat n
         , IntegralTy ty
         , r ~ a
         , ix ~ '[ty]
         , Settable (RTOptic_ :: Optic ix (Array n a) r)
         )
      => ReifiedSetter
           (RTOptic_ :: Optic ix (Array n a) r)
      where
  set i a (MkArray arr) = MkArray ( arr Vector.// [(fromIntegral i, a)] )

instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         )
      => Settable (RTOptic_ :: Optic ix (RuntimeArray a) r) where
instance ( IntegralTy ty
         , ix ~ '[ty]
         , r ~ a
         , PrimTy a
         )
      => ReifiedSetter
           (RTOptic_ :: Optic ix (RuntimeArray a) r)
      where
  set i a (MkRuntimeArray arr)
    = MkRuntimeArray ( arr Unsized.Vector.// [(fromIntegral i, a)] )
  
instance 
    TypeError (    Text "set: attempt to update array element \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Settable (Field_ (k :: Symbol) :: Optic empty (Array n a) r) where

instance
    TypeError (    Text "set: attempt to update run-time array \
                        \element using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Settable (Field_ (k :: Symbol) :: Optic empty (RuntimeArray a) r) where

-- structs
instance ( KnownSymbol k
         , (n ':-> r) ~ StructIndexFromName k as
         , empty ~ '[]
         )
  => Settable (Field_ (k :: Symbol) :: Optic empty (Struct as) r) where
instance ( KnownSymbol k
         , HasStructField k r as
         , (n ':-> r) ~ StructIndexFromName k as
         , empty ~ '[]
         )
      => ReifiedSetter
            (Field_ (k :: Symbol) :: Optic empty (Struct as) r)
      where
  set = setStructField @k @r @as

instance ( KnownNat n
         , (k ':-> r) ~ StructFieldFromIndex n as
         , empty ~ '[]
         ) => Settable (Field_ (n :: Nat) :: Optic empty (Struct as) r) where
instance ( KnownNat n
         , HasStructField n r as
         , (k ':-> r) ~ StructFieldFromIndex n as
         , empty ~ '[]
         )
      => ReifiedSetter
          (Field_ (n :: Nat) :: Optic empty (Struct as) r)
      where
  set = setStructField @n @r @as

instance
    TypeError (    Text "set: attempt to set struct element \
                        \using run-time index."
              :$$: Text "Structs can only be modified using \
                        \compile-time indices or field names."
              )
    => Settable (RTOptic_ :: Optic ix (Struct as) r) where

-- vectors
instance ( KnownNat i
         , VectorIndexInBounds n i
         , empty ~ '[]
         )
      => Settable (Field_ (i :: Nat) :: Optic empty (V n a) r) where
instance ( KnownNat n, KnownNat i, 1 <= n
         , CmpNat i n ~ 'LT
         , r ~ a
         , empty ~ '[]
         , VectorIndexInBounds n i
         )
      => ReifiedSetter
          (Field_ (i :: Nat) :: Optic empty (V n a) r)
      where
  set = replaceV ( fromIntegral ( natVal ( Proxy @i ) ) )

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
  set 0 b (_ :. as) = b :. as
  set i b (a :. as) = case Proxy @1 %<=? Proxy @(n-1) of
    LE Refl -> a :. set @(RTOptic_ :: Optic ix (V (n-1) a) r) (i-1) b as
    _       -> a :. as

instance
    TypeError (    Text "set: attempt to update vector element \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              :$$: Text "For a vector swizzle, use 'Swizzle' (e.g. Swizzle '[\"x\",\"z\",\"y\"]),"
              :$$: Text "or a synonym such as XZY (limited to X/Y/Z/W)."
              )
    => Settable (Field_ (k :: Symbol) :: Optic empty (V n a) r) where

-- matrices
instance ( KnownNat i
         , empty ~ '[]
         , MatrixColumnIndexInBounds m n i
         )
      => Settable (Field_ (i :: Nat) :: Optic empty (M m n a) r) where
instance ( KnownNat m, KnownNat n, KnownNat i, 1 <= n
         , empty ~ '[]
         , r ~ V m a
         , MatrixColumnIndexInBounds m n i
         )
      => ReifiedSetter
           (Field_ (i :: Nat) :: Optic empty (M m n a) r)
       where
  set c (M m) = M $ replaceV ( fromIntegral ( natVal ( Proxy @i ) ) ) c m

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
      . set @(RTOptic_ :: Optic ix (V n (V m a)) (V m a)) i c
      ) m

instance
    TypeError (    Text "set: attempt to update matrix column \
                        \using symbolic identifier "
              :<>: ShowType k :<>: Text "."
              )
    => Settable (Field_ (k :: Symbol) :: Optic empty (M m n a) r) where


----------------------------------------------------------------------
-- type class instances for containers

instance Container (Array n a) where
instance Container (RuntimeArray a) where
instance KnownNat n => Container (M m n a) where
instance Container (V n a) where
  type FieldIndexFromName (V n a) k
    = TypeError (    Text "optic: attempt to index a vector component with name " :<>: ShowType k
                :$$: Text "Maybe you intended to use a swizzle?"
                )
instance Container (Struct (as :: [Symbol :-> Type])) where
  type FieldIndexFromName (Struct (as :: [Symbol :-> Type])) k
    = Key (StructIndexFromName k as)

----------------------------------------------------------------------
-- type class instances for "OfType" optic

-- vectors
class SetVector n a b (a_eq_b :: Bool) where
  setVector :: b -> V n a -> V n a
instance (a ~ b, KnownNat n) => SetVector n a b 'True where
  setVector a _ = pure a
instance SetVector n a b 'False where
  setVector _ = id

instance ( empty ~ '[]
         , KnownNat n
         , SetVector n a b (a `LazyEq` b)
         )
      => Settable (OfType_ ty :: Optic empty (V n a) b)
      where
instance ( empty ~ '[]
         , b ~ a
         , KnownNat n
         , SetVector n a b (a `LazyEq` b)
         )
      => ReifiedSetter (OfType_ ty :: Optic empty (V n a) b)
      where
  set = setVector @n @a @b @(a `LazyEq` b)

-- matrices
data MatrixComponent
  = Column
  | Entry

type family WhichMatrixComponent
              (m :: Nat) (a :: Type) (b :: Type)
            :: Maybe MatrixComponent
            where
  WhichMatrixComponent _ a a       = Just Entry
  WhichMatrixComponent m a (V m a) = Just Column
  WhichMatrixComponent _ _ _       = Nothing

class SetMatrix m n a b (w :: Maybe MatrixComponent) where
  setMatrix :: b -> M m n a -> M m n a
instance ( b ~ a
         , KnownNat m
         , KnownNat n
         )
      => SetMatrix m n a b (Just Entry) where
  setMatrix a _ = pure a
instance ( b ~ V m a
         , KnownNat m
         , KnownNat n
         )
      => SetMatrix m n a b (Just Column) where
  setMatrix col _ = M ( pure col :: V n (V m a) )
instance SetMatrix m n a b Nothing where
  setMatrix _ = id

instance ( empty ~ '[]
         , KnownNat m
         , KnownNat n
         , b ~ ty
         , SetMatrix m n a b (WhichMatrixComponent m a b)
         )
      => Settable (OfType_ ty :: Optic empty (M m n a) b)
      where
instance ( empty ~ '[]
         , KnownNat m
         , KnownNat n
         , b ~ ty
         , SetMatrix m n a b (WhichMatrixComponent m a b)
         )
      => ReifiedSetter (OfType_ ty :: Optic empty (M m n a) b)
      where
  set = setMatrix @m @n @a @b @(WhichMatrixComponent m a b)

-- arrays
instance ( empty ~ '[]
         , KnownNat n
         , b ~ ty
         , Settable (OfType ty :: Optic '[] a ty)
         )
      => Settable (OfType_ ty :: Optic empty (Array n a) b)
      where
instance ( empty ~ '[]
         , KnownNat n
         , b ~ ty
         , ReifiedSetter (OfType ty :: Optic '[] a ty)
         , ListVariadic '[] a ~ a
         )
      => ReifiedSetter (OfType_ ty :: Optic empty (Array n a) b)
      where
  set a = fmap ( set @(OfType ty :: Optic '[] a ty) a )

instance ( TypeError ( Text "Cannot use 'OfType' optic on a runtime array." ) )
      => Settable (OfType_ ty :: Optic empty (RuntimeArray a) r)
      where
instance ( TypeError ( Text "Cannot use 'OfType' optic on a runtime array." ) )
      => ReifiedSetter (OfType_ ty :: Optic empty (RuntimeArray a) r)
      where
  set = error "unreachable"

-- structs
instance ( empty ~ '[] ) => Settable      (OfType_ ty :: Optic empty (Struct '[]) b) where
instance ( empty ~ '[] ) => ReifiedSetter (OfType_ ty :: Optic empty (Struct '[]) b) where
  set _ = id
instance ( empty ~ '[]
         , Settable (OfType ty :: Optic '[] a           ty)
         , Settable (OfType ty :: Optic '[] (Struct as) ty)
         , b ~ ty
         )
      => Settable ( OfType_ ty :: Optic empty (Struct ( (fld ':-> a) ': as ) ) b ) where
instance ( empty ~ '[]
         , ReifiedSetter (OfType ty :: Optic '[] a           ty)
         , ReifiedSetter (OfType ty :: Optic '[] (Struct as) ty)
         , b ~ ty
         , ListVariadic '[] a ~ a
         )
      => ReifiedSetter ( OfType_ ty :: Optic empty (Struct ( (fld ':-> a) ': as ) ) b ) where
  set x ( a :& as )
    =  set @(OfType ty :: Optic '[] a           b) x a
    :& set @(OfType ty :: Optic '[] (Struct as) b) x as
