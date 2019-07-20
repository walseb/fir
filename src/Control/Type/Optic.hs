{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: Control.Type.Optic

This module provides __type-level optics__, in the form of /getters/ and /setters/.

@Optic is s a@ describes the kind of an optic:

  * @is@ is a list of the types of the indices which have to be provided at run-time.
  When all accessor information is known at compile-time, this is the empty list @'[]@.
  * @s@ is the type of the container,
  * @a@ is the type of the component which is being focused on inside the container.

For instance, an optic which focuses on a single component of a vector of size 4 with a compile-time index
has kind @Optic '[] (V 4 a) a@.


The basic optics provided, to focus into an object of type @s@ onto a subobject of type @a@, are:

  * @AnIndex ix :: Optic '[ix] s a@: focus via a run-time index of type @ix@,
  * @Index   i  :: Optic '[]   s a@: focus via the compile-time index @i :: Nat@,
  * @Name    k  :: Optic '[]   s a@: focus via the compile-time literal @k :: Symbol@.

The compile-time nature of 'Index' and 'Name' means that we can type-check their usage
to prevent focusing on a non-existent field (such as an out-of-bounds index).

These optics can be combined in the following ways:

  * @(:.:) :: Optic is s a -> Optic js a b -> Optic (is :++: js) s b@ composes two optics,
  allowing for focusing into nested structures.
  \[ s \to a \to b \]
  * @(:*:) :: (o1 :: Optic is s a) -> (o2 :: Optic js s b) -> Optic (Zip is js) s (Product o1 o2)@
  takes the product of two optics, to focus onto multiple components simultaneously.
  \[ \begin{array}{ccc}
  s & \to & a \\
  \downarrow & & \\
  b & &
  \end{array} \]
  * @Joint :: Optic '[] a (MonoType a)@ is an equaliser optic,
  which allows setting to a given value multiple components of the same type
  by post-composing with 'Joint' (see example below).
  \[ s \to a \rightrightarrows \textrm{MonoType}(a) \]

Again, these are type-checked for validity. For instance, one cannot create a product setter
unless the two argument setters are disjoint.
(Note that this disallows product setters involving run-time indices,
as the required disjointness property can't be checked at compile-time.)


Getters\/setters are optics which support accessing\/setting components.
Type-level optics which can be reified to provide value-level getters and setters are defined
through the 'ReifiedGetter' and 'ReifiedSetter' type classes, instances of which are provided
for types used in this library in the "FIR.Instances.Optics" module, or in this module
as far as combinators are concerned (e.g. the getter instance for the composite of two getters).

The usage of these optics mimics the [Lens](http://hackage.haskell.org/package/lens/docs/Control-Lens.html)
library, but type-level arguments are provided with type applications. For instance:

@
> view @(Index 2) (V4 00 11 22 33)
22
@

Note that numeric indexing starts at 0.

@
> view @AnIndex 2 (V4 00 11 22 33)
22
@

Here the index is provided at the value-level instead of at the type-level.
Multiple indices can be provided in this manner:

  * as separate arguments for a composition,
  * as pairs for a product optic.

@
> view @(Index 1 :.: Index 0)
    $ M22
        0 1
        2 3
1
@

Here we first access the outer layer (the column with index 1, i.e. second of two columns),
then access the first component of that column. We always access the column first then the row,
following the column-major convention.
The type synonym 'Entry' can also be used: @Entry i j@ stands for @Index i :.: Index j@,
restricted to work specifically on matrices.

Note that, in a composition, the outermost optic is on the left, and the nesting increases
as one reads from left to right.

@
> struct :: Struct '[ "a" ':-> V 4 Float, "b" ':-> V 2 Float ]
> struct = V4 0 1 2 3 :& V2 4 5 :& End

> set @( (Name "a" :.: Index 2) :*: (Name "b" :.: Index 0) ) (V2 6 7) struct
{ "a" ':-> V4 0 1 6 3, "b" ':-> V2 7 5 }
@

Setting multiple values at once: focusing on the component at index 2 of the field @"a"@,
and the component at index 0 of the field @"b"@.

Because the last type accessed by each optic in the product is a vector type,
the type-system combines these two setters using vectors.
This explains why the value-level argument to @set@ is a 2-vector.

@
> mat :: M 3 3 Double
> mat = M33
          0 1 2
          3 4 5
          6 7 8

> set @( ( Entry 0 0 :*: Entry 0 2 :*: Entry 2 0 :*: Entry 2 2 ) :.: Joint ) 9 mat
M33
  9 1 9
  3 4 5
  9 7 9
@

'Joint' allows us to simultaneously set several entries (of the same type) to the same value.
__Warning:__ 'Joint' is only a /setter/, not a /getter/.

Note that the 'FIR.Instances.Optics.Diag' synonym exists for focusing on the diagonal of a matrix:

@
> view @Diag mat
V3 0 4 8
@

There is also the 'FIR.Instances.Optics.Center' synonym for the center of a matrix,
which allows setting all diagonal entries of a square matrix to a single value:

@
> set @Center 9 mat
M33
  9 1 2
  3 9 5
  6 7 9
@

In fact 'FIR.Instances.Optics.Center' can be simply defined as @Diag :.: Joint@.
-}

module Control.Type.Optic
  ( -- * Type-level optics
    Optic(..)
    -- $kind_coercion
  , AnIndex, Index, Name

    -- ** Getters and setters
  , Gettable, Getter, ReifiedGetter(view)
  , Settable, Setter, ReifiedSetter(set)
  , ReifiedLens(over)
    -- $kind_synonyms
  , Whole, Part, Indices

    -- ** Containers
    -- $containers
  , ContainerKind, DegreeKind, LabelKind
  , Contained(..), MonoContained(..)

    -- * Getter & setter instances
    -- $instances

    -- ** Identity
  , Id
    -- $identity_instances

    -- ** Equaliser optics
  , Joint
    -- $equaliser_instances

    -- ** Composition of optics
  , (:.:)
    -- $composition_instances

    -- ** Product of optics
  , (:*:)
  , Product, ProductIfDisjoint
    -- $product_instances

  ) where


-- base
import Data.Kind
  ( Type )
import Data.Type.Bool
  ( If, type (&&), Not )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.List
  ( type (:++:), Zip, Postpend )
import Data.Function.Variadic
  ( ListVariadic )
import FIR.Prim.Image
  ( Image )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GeneratedGradedSemigroup(..)
  , FreeGradedSemigroup(..)
  , GenDegAt
  )

----------------------------------------------------------------------

infixr 9 :.:
infixr 9 `ComposeO`
infixr 3 :*:
infixr 3 `ProductO`

-- | Optic data (kind).
data Optic (is :: [Type]) (s :: k) (a :: Type) where
  -- | Identity.
  Id_      :: Optic is a a
  -- | Equaliser optic.
  Joint_   :: Optic is s a
  -- | Optic with indexing information provided at run-time.
  RTOptic_ :: Optic is s a
  -- | Compile-time field (e.g. numeric index or symbolic field name).
  Field_   :: fld -> Optic is s a
  -- | Composition of optics.
  ComposeO :: Optic is s a -> Optic js a b -> Optic ks s b
  -- | Product of optics.
  ProductO :: Optic is s a -> Optic ix s b -> Optic js s c 

-- $kind_coercion
--
-- /__Warning__/: the data constructors of the 'Optic' data type are not type-correct.
-- This is to bypass difficulties with kind coercions:
-- at the time of writing, GHC does not full support kind coercions, in that
-- given the context @a ~ b@, GHC is unable to unify a type
-- of kind @a@ with a type of kind @b@.
-- (See [GHC trac #15710](https://gitlab.haskell.org/ghc/ghc/issues/15710).)
--
-- As a result, the constructors for the 'Optic' data type have overly general types.
-- Kind-correct type-level smart constructors are instead provided (and their use recommended):
--
--   * 'AnIndex', 'Index', 'Name', 'Id' and 'Joint' create specific optics,
--   * ':.:' composes two optics (left-most argument = outer-most optic),
--   * ':*:' takes the product of two optics.
--
-- See also "FIR.Instances.Images" for an overview of how to use optics with images.


-- | Run-time index (kind-correct).
type AnIndex (ix :: Type  ) = (RTOptic_ :: Optic '[ix] s a)
-- | Compile-time index (kind-correct).
type Index   (i  :: Nat   ) = (Field_ i :: Optic '[]   s a)
-- | Compile-time field name (kind-correct).
type Name    (k  :: Symbol) = (Field_ k :: Optic '[]   s a)

-- | Identity (kind-correct).
type Id = (Id_ :: Optic '[] a a)
-- | Equaliser optic (kind-correct).
type Joint = (Joint_ :: Optic '[] a (MonoType a))
-- | Composition of optics (kind-correct).
type (:.:) (o1 :: Optic is s a) (o2 :: Optic js a b)
  = ( (o1 `ComposeO` o2) :: Optic (is :++: js) s b )
-- | Product of optics (kind-correct).
type (:*:) (o1 :: Optic is s a) (o2 :: Optic js s b)
  = ( (o1 `ProductO` o2) :: Optic ( Zip is js ) s (Product o1 o2) )

type family ShowOptic (o :: Optic is s a) :: ErrorMessage where
  ShowOptic Id_ = Text "Id"
  ShowOptic Joint_ = Text "Joint"
  -- special case for image optics
  ShowOptic
    ( ( Field_ (k :: Symbol) :: Optic '[] i (Image props) )
    `ComposeO`
      ( RTOptic_ :: Optic '[ _, _ ] (Image props) _ )
    )
      = Text "ImageTexel " :<>: ShowType k
  ShowOptic (RTOptic_ :: Optic '[ix] s a)
    = Text "AnIndex (" :<>: ShowType ix :<>: Text ")"
  ShowOptic (RTOptic_ :: Optic is s a)
    = Text "SomeIndices (" :<>: ShowType is :<>: Text ")"
  ShowOptic (Field_ (i :: Nat))
    = Text "Index " :<>: ShowType i
  ShowOptic (Field_ (k :: Symbol) :: Optic is (s :: Type) a)
    = Text "Name " :<>: ShowType k
  ShowOptic (Field_ (k :: Symbol) :: Optic is (s :: _) a)
    = Text "Binding " :<>: ShowType k
  ShowOptic (o1 `ComposeO` o2)
    = Text "( " :<>: ShowOptic o1 :<>: Text " :.: " :<>: ShowOptic o2 :<>: Text " )"
  ShowOptic (o1 `ProductO` o2)
    = Text "( " :<>: ShowOptic o1 :<>: Text " :*: " :<>: ShowOptic o2 :<>: Text " )"

----------------------------------------------------------------------
-- Type classes and synonyms.

-- | Type-level getter.
class Gettable (optic :: Optic is (s :: k) a) | optic -> is k s a where

type  Getter (optic :: Optic is (s :: Type) a) = ListVariadic (is `Postpend` s) a

-- | Type-level getter which can be turned into a value-level getter.
class Gettable optic => ReifiedGetter optic where
  view :: Getter optic

-- | Type-level setter.
class Settable (optic :: Optic is (s :: k) a) | optic -> is k s a where

type  Setter (optic :: Optic is (s :: Type) a) = ListVariadic (is `Postpend` a `Postpend` s) s

-- | Type-level setter which can be turned into a value-level setter.
class Settable optic => ReifiedSetter optic where
  set :: Setter optic

type Over (optic :: Optic is (s :: Type) a) = ListVariadic (is `Postpend` (a -> a) `Postpend` s) s

-- | Type-level lens: can be used to modify the part under its focus.
class ( ReifiedSetter optic, ReifiedGetter optic ) => ReifiedLens optic where
  over :: Over optic

-- $kind_synonyms
--
-- Synonyms that allow kind variables to remain invisible.
--
-- See [explicit specificity (GHC proposal #26)](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0026-explicit-specificity.rst).

type Whole   (optic :: Optic is s a) = s
type Part    (optic :: Optic is s a) = a
type Indices (optic :: Optic is s a) = is

-------------------------------
-- $containers
--
-- Auxiliary internal type class describing types that provide the functionality necessary
-- to be able to create product optics.
--
-- In particular, given a particular \"contained\" type such as @Vec a n@,
-- this type class returns the overall container type (in this case @Vec a :: Nat -> Type@)
-- which is supposed to be an instance of the 'Math.Algebra.GradedSemigroup.GradedSemigroup' type class,
-- meaning that some form of concatenation/product is possible,
-- compatibly with type-level indexing information (if any; in this case @Nat@).
--
-- Intuitively, it is best to think of this type class as dispatching on a type,
-- returning a corresponding graded semigroup.
--
-- However, certain additional capabilities are also required,
-- such as (for structs) the ability to check that a numeric index
-- and a symbolic field name do not refer to the same component,
-- as needed for overlap checking.
--
-- A minor technicality: the following three open type families,
-- 'ContainerKind', 'DegreeKind' and 'LabelKind',
-- should ideally be associated to the 'Contained' type class,
-- but GHC doesn't currently allow this,
-- complaining about type constructors being \"defined and used in the same recursive group\".
-- See [GHC Trac #11962](https://ghc.haskell.org/trac/ghc/ticket/11962).

type family ContainerKind (s :: Type) :: Type
type family DegreeKind    (s :: Type) :: Type
type family LabelKind     (s :: Type) :: Type

-- | Recovers the types necessary for a 'Math.Algebra.GradedSemigroup.GradedSemigroup'.
--
-- For instance, we usually expect the following instances:
--
--  * @GradedSemigroup (Container s) (DegreeKind s)@,
--  * @GeneratedGradedSemigroup (Container s) (DegreeKind s) (LabelKind s)@,
--  * @FreeGradedSemigroup (Container s) (DegreeKind s) (LabelKind s)@.
--
-- For additional flexibility, these are not currently enforced as superclass constraints.
-- For instance, with run-time arrays we do not have a
-- 'Math.Algebra.GradedSemigroup.FreeGradedSemigroup' structure,
-- as the lack of compile-time indexing information in this situation
-- prevents us from being able to unambiguously recover the factors of a product.
class Contained (s :: Type) where
  type Container  s :: ContainerKind s
  type DegreeOf   s :: DegreeKind s
  type LabelOf    s (o :: Optic i s a) :: LabelKind s
  -- | Additional utility type family, chiefly needed for overlap checking for 'FIR.Prim.Struct.Struct's.
  -- This associated type family has a trivial definition in cases where it is not possible
  -- to access components using /both/ type-level literals and type-level natural numbers.
  type Overlapping s (k :: Symbol) (n :: Nat) :: Bool

class Contained s => MonoContained s where
  type MonoType s
  setAll :: MonoType s -> s -> s

----------------------------------------------------------------------
-- $instances
--
-- This module defines getter and setter instances that are applicable in general situations:
--
--   * identity optic,
--   * equaliser optic,
--   * composition of optics,
--   * product of optics,
--
-- In other words, this module provides the general framework for /combining/ optics.
-- To manipulate specific types (e.g. the ability to access a component of a vector by its index),
-- corresponding specific instances are required.
--
-- The specific instances, as they pertain to datatypes used by this library,
-- are found in the "FIR.Instances.Optics" module.
-- This includes instances for vectors, matrices and structs.
-- See also "FIR.Instances.Images" for the peculiar 'FIR.Instances.Images.ImageTexel' lens.

--------------------------
-- $identity_instances
--
-- The identity optic is a lens.

instance (empty ~ '[])
       => Gettable (Id_ :: Optic empty a a) where
instance (empty ~ '[])
       => Settable (Id_ :: Optic empty a a) where

instance (empty ~ '[], a ~ ListVariadic '[] a)
       => ReifiedGetter (Id_ :: Optic empty a a) where
  view = id
instance (empty ~ '[], a ~ ListVariadic '[] a)
       => ReifiedSetter (Id_ :: Optic empty a a) where
  set = const

--------------------------
-- $lens_instances
--
-- Combine a getter and a setter into a lens.

class GetterAndSetter is s a where
  getAndSet
    :: ListVariadic (is `Postpend` s) a
    -> ListVariadic (is `Postpend` a `Postpend` s) s
    -> ListVariadic (is `Postpend` (a -> a) `Postpend` s) s

instance ( s ~ ListVariadic '[] s, a ~ ListVariadic '[] a ) => GetterAndSetter '[] s a where
 getAndSet view1 set1 f s = set1 ( f (view1 s) ) s

instance GetterAndSetter is s a => GetterAndSetter (i ': is) s a where
  getAndSet view1 set1 i
    = getAndSet @is @s @a (view1 i) (set1 i)

instance forall is s a (optic :: Optic is s a).
                ( ReifiedGetter optic, ReifiedSetter optic
                , GetterAndSetter is s a
                )
            => ReifiedLens optic where
  over = getAndSet @is @s @a (view @optic) (set @optic)

--------------------------
-- $equaliser_instances
--
-- The equaliser optic is a setter.
--
-- The instances for equalisers depend on instances for 'MonoContained',
-- which are provided separately for individual types
-- (see "FIR.Instances.Optics").

instance
  ( TypeError ( Text "get: cannot use equaliser as a getter." ) )
  => Gettable (Joint_ :: Optic i s a) where
instance
  ( TypeError ( Text "get: cannot use equaliser as a getter." ) )
  => ReifiedGetter (Joint_ :: Optic i s a) where
  view = error "unreachable"

instance ( empty ~ '[]
         , MonoContained a
         , mono ~ MonoType a
         )
      => Settable (Joint_ :: Optic empty a mono) where
instance ( empty ~ '[]
         , MonoContained a
         , mono ~ MonoType a
         , a ~ ListVariadic '[] a
         )
      => ReifiedSetter (Joint_ :: Optic empty a mono) where
  set = setAll

--------------------------
-- $composition_instances
--
-- Included are the following instances for composed optics:
--
--   * the composite of two getters is a getter,
--   * the composite of a lens (outside) with a setter (inside) is a setter.
--
-- Note that compositions where the first optic accesses from a monadic state
-- is not included here.
-- This is to provide improved type inference, see "FIR.Instances.Optics".

instance forall (s :: Type) is js ks a b (o1 :: Optic is s a) (o2 :: Optic js a b).
         ( Gettable o1
         , Gettable o2
         , ks ~ (is :++: js)
         ) => Gettable ((o1 `ComposeO` o2) :: Optic ks s b) where
instance forall (s :: Type) is js ks a b (o1 :: Optic is s a) (o2 :: Optic js a b).
         ( Settable o1
         , Settable o2
         , ks ~ (is :++: js)
         ) => Settable ((o1 `ComposeO` o2) :: Optic ks s b) where

instance forall (s :: Type) is js ks a b (o1 :: Optic is s a) (o2 :: Optic js a b).
         ( ReifiedGetter o1
         , ReifiedGetter o2
         , ComposeGetters is js s a b
         , ks ~ (is :++: js)
         )
      => ReifiedGetter ((o1 `ComposeO` o2) :: Optic ks s b) where
  view = composeGetters @is @js @s @a @b (view @o1) (view @o2)

class ComposeGetters is js s a b where
  composeGetters :: ListVariadic (is `Postpend` s) a
                 -> ListVariadic (js `Postpend` a) b
                 -> ListVariadic ((is :++: js) `Postpend` s) b
instance (a ~ ListVariadic '[] a) => ComposeGetters '[] '[] s a b where
  composeGetters view1 view2 = view2 . view1
instance ComposeGetters is js s a b => ComposeGetters (i ': is) js s a b where
  composeGetters view1 view2 i
    = composeGetters @is @js @s @a @b (view1 i) view2
instance ComposeGetters '[] js s a b => ComposeGetters '[] (j ': js) s a b where
  composeGetters view1 view2 j
    = composeGetters @'[] @js @s @a @b view1 (view2 j)

instance forall (s :: Type) is js ks a b (o1 :: Optic is s a) (o2 :: Optic js a b).
         ( ReifiedSetter o1
         , ReifiedSetter o2
         , ReifiedGetter o1
         , ComposeSetters is js s a b
         , ks ~ (is :++: js)
         )
      => ReifiedSetter ((o1 `ComposeO` o2) :: Optic ks s b) where
  set = composeSetters @is @js @s @a @b (view @o1) (set @o1) (set @o2)

class ComposeSetters is js s a b where
  composeSetters :: ListVariadic (is `Postpend` s) a
                 -> ListVariadic (is `Postpend` a `Postpend` s) s
                 -> ListVariadic (js `Postpend` b `Postpend` a) a
                 -> ListVariadic ((is :++: js) `Postpend` b `Postpend` s) s
instance (a ~ ListVariadic '[] a) => ComposeSetters '[] '[] s a b where
  composeSetters view1 set1 set2 b s
    = set1 (set2 b (view1 s)) s
instance ComposeSetters is js s a b => ComposeSetters (i ': is) js s a b where
  composeSetters view1 set1 set2 i
    = composeSetters @is @js @s @a @b (view1 i) (set1 i) set2
instance ComposeSetters '[] js s a b => ComposeSetters '[] (j ': js) s a b where
  composeSetters view1 set1 set2 j
    = composeSetters @'[] @js @s @a @b view1 set1 (set2 j)

--------------------------
-- $product_instances
--
-- Included are the following instances for product optics:
--
--   * the product of two setters is a setter,
--   * the product of two getters is a getter.

-- getter products
instance forall is js ks s a b c (o1 :: Optic is s a) (o2 :: Optic js s b).
         ( Gettable o1
         , Gettable o2
         , ks ~ Zip is js
         , c ~ Product o1 o2
         ) => Gettable ((o1 `ProductO` o2) :: Optic ks s c) where

-- setter products
instance forall is js ks s a b c (o1 :: Optic is s a) (o2 :: Optic js s b) .
         ( Settable o1
         , Settable o2
         , ks ~ Zip is js
         , c ~ ProductIfDisjoint o1 o2
         ) => Settable ((o1 `ProductO` o2) :: Optic ks s c) where

type family IsProduct (o :: Optic is s a) :: Bool where
  IsProduct (_ `ProductO` _) = True
  IsProduct (_ `ComposeO` o) = IsProduct o
  IsProduct o                = False

type family WithKind (a :: l) k :: k where
  WithKind (a :: k) k = a
  WithKind (a :: l) k
    = TypeError (     Text "Kind coercion: non-matching kinds."
                 :$$: Text "Expected kind: " :<>: ShowType k
                 :$$: Text "Actual kind: " :<>: ShowType l
                )

-- | Return type of a product optic.
type family Product
              ( o1 :: Optic is s a )
              ( o2 :: Optic js t b )
            = ( r  :: Type         )
              where
  Product (o1 `ComposeO` o3) o2 = Product o3 o2
  Product o1 (o2 `ComposeO` o4) = Product o1 o4
  Product (o1 :: Optic is s a) (o2 :: Optic js t b)
    = Combine o1 o2
        ( IsProduct o1 ) ( IsProduct o2 )

type family ProductIfDisjoint
              ( o1 :: Optic is s a )
              ( o2 :: Optic js s b )
            = ( r  :: Type         )
              where
  ProductIfDisjoint o1 o2
    = If
        ( Disjoint o1 o2 )
        ( Product  o1 o2 )
        ( TypeError 
           ( Text "set: cannot create product setter."
            :$$: Text "Setters "
            :$$: Text "  " :<>: ShowOptic o1
            :$$: Text "and "
            :$$: Text "  " :<>: ShowOptic o2
            :$$: Text "are not disjoint."
           )
        )

type family LastAccessee ( o :: Optic is (s :: Type) a ) :: Type where
  LastAccessee (o1 `ComposeO` o2 ) = LastAccessee o2
  LastAccessee (o :: Optic is s a) = s

type family LastIndices ( o :: Optic is s a ) :: [Type] where
  LastIndices (o1 `ComposeO` o2 ) = LastIndices o2
  LastIndices (o :: Optic is s a) = s

type family LastOptic ( o :: Optic is s a) :: Optic (LastIndices o) (LastAccessee o) a where
  LastOptic (o1 `ComposeO` o2) = LastOptic o2
  LastOptic Joint_             = Joint_
  LastOptic Id_                = Id_
  LastOptic RTOptic_           = RTOptic_
  LastOptic (Field_ f        ) = Field_ f
  LastOptic (o1 `ProductO` o2) = o1 `ProductO` o2
  --LastOptic (o :: Optic is s a) = o `WithKind` ( Optic (LastIndices o) (LastAccessee o) a )

type family WhichKind
              ( b1 :: Bool ) ( b2 :: Bool )
              k1 k2 k3 k4
              where
  WhichKind True  True  _ k _ k = k
  WhichKind True  False _ k k _ = k
  WhichKind False True  k _ _ k = k
  WhichKind False False k _ k _ = k

type family WhichContainer
                ( b1 :: Bool ) ( b2 :: Bool )
                ck1 ck2 ck3 ck4
                ( c1 :: ck1 ) ( c2 :: ck2 ) ( c3 :: ck3 ) ( c4 :: ck4 )
              = ( r :: WhichKind b1 b2 ck1 ck2 ck3 ck4 )
                where
  WhichContainer True  True  _  ck _  ck _ c _ c = c
  WhichContainer True  False _  ck ck _  _ c c _ = c
  WhichContainer False True  ck _  _  ck c _ _ c = c
  WhichContainer False False ck _  ck _  c _ c _ = c
  WhichContainer _ _ _ _ _ _ c1 c2 c3 c4
    = TypeError (     Text "Cannot create product optic: incompatible containers."
                 :$$: Text "LHS containers: " :<>: ShowType c1 :<>: Text " and " :<>: ShowType c2
                 :$$: Text "RHS containers: " :<>: ShowType c3 :<>: Text " and " :<>: ShowType c4
                )

type family WhichDegree
              ( b1 :: Bool ) ( b2 :: Bool )
              kha kva khb kvb
              ( ha :: kha ) ( va :: kva ) ( hb :: khb ) ( vb :: kvb )
            = ( r :: WhichKind b1 b2 kha kva khb kvb )
              where
  WhichDegree True  True  _ k _ k _  va _  vb = va :<!>: vb
  WhichDegree True  False _ k k _ _  va hb _  = va :<!>: hb
  WhichDegree False True  k _ _ k ha _  _  vb = ha :<!>: vb
  WhichDegree False False k _ k _ ha _  hb _  = ha :<!>: hb
  WhichDegree _ _ _ _ _ _ ha va hb vb
    = TypeError (     Text "Cannot create product optic: incompatible gradings."
                 :$$: Text "LHS gradings: " :<>: ShowType ha :<>: Text " and " :<>: ShowType va
                 :$$: Text "RHS gradings: " :<>: ShowType hb :<>: Text " and " :<>: ShowType vb
                )

type family ProductContainer
                ( o1 :: Optic is s a ) ( o2 :: Optic js t b )
                ( b1 :: Bool ) ( b2 :: Bool )
              = ( r :: WhichKind b1 b2
                          ( ContainerKind s )
                          ( ContainerKind a )
                          ( ContainerKind t )
                          ( ContainerKind b )
                )
                where
  ProductContainer (o1 :: Optic is s a) (o2 :: Optic js t b) b1 b2
    = WhichContainer b1 b2
        (ContainerKind s) (ContainerKind a) (ContainerKind t) (ContainerKind b)
        (Container s) (Container a) (Container t) (Container b )

type family ProductDegree
                ( o1 :: Optic is s a ) ( o2 :: Optic js t b )
                ( b1 :: Bool ) ( b2 :: Bool )
              = ( r :: WhichKind b1 b2
                          ( DegreeKind s )
                          ( DegreeKind a )
                          ( DegreeKind t )
                          ( DegreeKind b )
                )
                where
  ProductDegree (o1 :: Optic is s a) (o2 :: Optic js t b) b1 b2
    = WhichDegree b1 b2
        (DegreeKind s) (DegreeKind a) (DegreeKind t) (DegreeKind b)
        ( GenDegAt (DegreeKind s) (Container s) (LabelOf s o1) )
        ( DegreeOf a )
        ( GenDegAt (DegreeKind t) (Container t) (LabelOf t o2) )
        ( DegreeOf b )

type family Combine
                ( o1 :: Optic is s a ) ( o2 :: Optic js t b )
                ( b1 :: Bool ) ( b2 :: Bool )
              = ( r :: Type )
                where
  Combine (o1 :: Optic is s a) (o2 :: Optic js t b) b1 b2
    = Grade
        ( WhichKind b1 b2 (DegreeKind s) (DegreeKind a) (DegreeKind t) (DegreeKind b) )
        ( ProductContainer o1 o2 b1 b2 )
        ( ProductDegree o1 o2 b1 b2 )

type family Disjoint
              ( o1 :: Optic is s a )
              ( o2 :: Optic js s b )
            = ( r  :: Bool         )
              where
  Disjoint (Field_ f) (Field_ f) = False
  Disjoint RTOptic_   _       
    = TypeError (    Text "set: cannot create a product setter involving run-time indexing."
                :$$: Text "Impossible to verify the required disjointness property."
                )
  Disjoint o RTOptic_
    = Disjoint RTOptic_ o
  Disjoint ((Field_ (k :: Symbol)) :: Optic is s a) ((Field_ (n :: Nat)) :: Optic js s b)
    = Not (Overlapping s k n)
  Disjoint ((Field_ (n :: Nat)) :: Optic is s a) ((Field_ (k :: Symbol)) :: Optic js s b)
    = Not (Overlapping s k n)
  Disjoint (Field_ (k :: Symbol)) (Field_ (l :: Symbol))
    = 'True
  Disjoint (Field_ (i :: Nat)) (Field_ (j :: Nat))
    = 'True
  Disjoint (Field_ (f1 :: fld1)) (Field_ (f2 :: fld2))
    = TypeError
      (    Text "Disjointness check: unsupported optics field kinds "
      :<>: ShowType fld1 :<>: Text " and " :<>: ShowType fld2 :<>: Text "."
      )
  Disjoint (o1 `ProductO` o3) (o2 `ProductO` o4)
    =  Disjoint o1 o2
    && Disjoint o1 o4
    && Disjoint o3 o2
    && Disjoint o3 o4
  Disjoint (o1 `ProductO` o3) o2
    =  Disjoint o1 o2
    && Disjoint o3 o2
  Disjoint o1 (o2 `ProductO` o4)
    =  Disjoint o1 o2
    && Disjoint o1 o4
  Disjoint (o1 `ComposeO` o2)
           (o3 `ComposeO` o4)
    = If 
        ( Disjoint o1 o3 )
        'True
        ( Disjoint o2 o4 )
  Disjoint _ _ = 'True

class MultiplyGetters is js s a b c lka lkb (mla :: Maybe lka) (mlb :: Maybe lkb) | c -> lka lkb where
  multiplyGetters :: ListVariadic (is `Postpend` s) a
                  -> ListVariadic (js `Postpend` s) b
                  -> ListVariadic (Zip is js `Postpend` s) c

instance ( Contained c
         , GradedSemigroup (Container c) (DegreeKind c)
         , a ~ ListVariadic '[] a
         , a ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf a `WithKind` DegreeKind c)
         , b ~ ListVariadic '[] b
         , b ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf b `WithKind` DegreeKind c)
         , c ~ ListVariadic '[] c
         , c ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (       (DegreeOf a `WithKind` DegreeKind c)
                    :<!>: (DegreeOf b `WithKind` DegreeKind c)
                  )
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         , ValidDegree (Container c) (DegreeOf a `WithKind` DegreeKind c)
         , ValidDegree (Container c) (DegreeOf b `WithKind` DegreeKind c)
         )
      => MultiplyGetters '[] '[] s a b c lka lkb 'Nothing 'Nothing where
  multiplyGetters view1 view2 s
    = (<!>) @(Container c) @_ @(DegreeOf a `WithKind` DegreeKind c) @(DegreeOf b `WithKind` DegreeKind c)
        (view1 s)
        (view2 s)
instance ( Contained c
         , GradedSemigroup (Container c) (DegreeKind c)
         , GeneratedGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ ListVariadic '[] a
         , a ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf a `WithKind` DegreeKind c)
         , b ~ ListVariadic '[] b
         , b ~ GenType (Container c) (LabelKind c) (lb `WithKind` LabelKind c)
         , hdb ~ GenDeg
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (lb `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Grade
                  (DegreeKind c)
                  (Container c)
                  ((DegreeOf a `WithKind` DegreeKind c) :<!>: hdb)
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         , ValidDegree (Container c) (DegreeOf a `WithKind` DegreeKind c)
         , ValidDegree (Container c) hdb
         )
      => MultiplyGetters '[] '[] s a b c lka lkb 'Nothing ('Just lb) where
  multiplyGetters view1 view2 s
    = (<!>) @(Container c) @_ @(DegreeOf a `WithKind` DegreeKind c) @hdb
        ( view1 s )
        ( generator
            @(Container c)
            @(DegreeKind c)
            @(LabelKind c)
            @(lb `WithKind` LabelKind c)
            ( view2 s )
        )
instance ( Contained c
         , GradedSemigroup (Container c) (DegreeKind c)
         , GeneratedGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ ListVariadic '[] a
         , a ~ GenType (Container c) (LabelKind c) (la `WithKind` LabelKind c)
         , b ~ ListVariadic '[] b
         , b ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf b `WithKind` DegreeKind c)
         , hda ~ GenDeg
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (la `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (hda :<!>: (DegreeOf b `WithKind` DegreeKind c))
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         , ValidDegree (Container c) hda
         , ValidDegree (Container c) (DegreeOf b `WithKind` DegreeKind c)
         )
      => MultiplyGetters '[] '[] s a b c lka lkb ('Just la) 'Nothing where
  multiplyGetters view1 view2 s
    = (<!>) @(Container c) @_ @hda @(DegreeOf b `WithKind` DegreeKind c)
        ( generator
            @(Container c)
            @(DegreeKind c)
            @(LabelKind c)
            @(la `WithKind` LabelKind c)
            ( view1 s )
        )
        ( view2 s )
instance ( Contained c
         , GradedSemigroup (Container c) (DegreeKind c)
         , GeneratedGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ ListVariadic '[] a
         , a ~ GenType (Container c) (LabelKind c) (la `WithKind` LabelKind c)
         , b ~ ListVariadic '[] b
         , b ~ GenType (Container c) (LabelKind c) (lb `WithKind` LabelKind c)
         , hda ~ GenDeg
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (la `WithKind` LabelKind c)
         , hdb ~ GenDeg
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (lb `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (hda :<!>: hdb)
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         , ValidDegree (Container c) hda
         , ValidDegree (Container c) hdb
         )
      => MultiplyGetters '[] '[] s a b c lka lkb ('Just la) ('Just lb) where
  multiplyGetters view1 view2 s
    = (<!>) @(Container c) @_ @hda @hdb
        ( generator
            @(Container c)
            @(DegreeKind c)
            @(LabelKind c)
            @(la `WithKind` LabelKind c)
            ( view1 s )
        )
        ( generator
            @(Container c)
            @(DegreeKind c)
            @(LabelKind c)
            @(lb `WithKind` LabelKind c)
            ( view2 s )
        )

instance MultiplyGetters is        js        s a b c lka lkb mla mlb
      => MultiplyGetters (i ': is) (j ': js) s a b c lka lkb mla mlb where
  multiplyGetters view1 view2 (i,j)
    = multiplyGetters @is @js @s @a @b @c @lka @lkb @mla @mlb (view1 i) (view2 j)
      :: ListVariadic (Zip is js `Postpend` s) c
instance MultiplyGetters '[] js        s a b c lka lkb mla mlb
      => MultiplyGetters '[] (j ': js) s a b c lka lkb mla mlb where
  multiplyGetters view1 view2 j
    = multiplyGetters @'[] @js @s @a @b @c @lka @lkb @mla @mlb view1 (view2 j)
      :: ListVariadic (Zip '[] js `Postpend` s) c
instance MultiplyGetters is        '[] s a b c lka lkb mla mlb
      => MultiplyGetters (i ': is) '[] s a b c lka lkb mla mlb where
  multiplyGetters view1 view2 i
    = multiplyGetters @is @'[] @s @a @b @c @lka @lkb @mla @mlb (view1 i) view2
      :: ListVariadic (Zip is '[] `Postpend` s) c

instance forall is js ks (s :: Type) a b c
               (o1 :: Optic is s a) (o2 :: Optic js s b)
               (lka :: Type) (lkb :: Type)
               (mla :: Maybe lka)
               (mlb :: Maybe lkb)
               .
         ( ReifiedGetter o1
         , ReifiedGetter o2
         , ks ~ Zip is js
         , c ~ Product o1 o2
         , lka ~ LabelKind (LastAccessee o1)
         , lkb ~ LabelKind (LastAccessee o2)
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         , mla ~ ( If (IsProduct o1)
                    'Nothing
                    ( 'Just
                        ( LabelOf (LastAccessee o1) (LastOptic o1) `WithKind` lka )
                    )
                  )
         , mlb ~ ( If (IsProduct o2)
                    'Nothing
                    ( 'Just
                        ( LabelOf (LastAccessee o2) (LastOptic o2) `WithKind` lkb )
                    )
                  )
         , MultiplyGetters
            is js s a b c lka lkb mla mlb
         )
      => ReifiedGetter ((o1 `ProductO` o2) :: Optic ks s c) where
  view = multiplyGetters @is @js @s @a @b @c @lka @lkb @mla @mlb (view @o1) (view @o2)


class MultiplySetters is js s a b c lka lkb (mla :: Maybe lka) (mlb :: Maybe lkb) | c -> lka lkb where
  multiplySetters :: ListVariadic (is `Postpend` a `Postpend` s) s
                  -> ListVariadic (js `Postpend` b `Postpend` s) s
                  -> ListVariadic (Zip is js `Postpend` c `Postpend` s) s

instance ( Contained c
         , GradedSemigroup (Container c) (DegreeKind c)
         , FreeGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf a `WithKind` DegreeKind c)
         , b ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf b `WithKind` DegreeKind c)
         , c ~ ListVariadic '[] c
         , c ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (      (DegreeOf a `WithKind` DegreeKind c)
                   :<!>: (DegreeOf b `WithKind` DegreeKind c)
                  )
         , s ~ ListVariadic '[] s
         , ValidDegree (Container c) (DegreeOf a `WithKind` DegreeKind c)
         , ValidDegree (Container c) (DegreeOf b `WithKind` DegreeKind c)
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         )
       => MultiplySetters '[] '[] s a b c lka lkb 'Nothing 'Nothing where
  multiplySetters set1 set2 c
    = let (a,b) = (>!<) c
      in  set2 b . set1 a
instance ( Contained c
         , GradedSemigroup (Container c) (DegreeKind c)
         , GeneratedGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , FreeGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf a `WithKind` DegreeKind c)
         , b ~ GenType (Container c) (LabelKind c) (lb `WithKind` LabelKind c)
         , hdb ~ GenDeg
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (lb `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Grade
                  (DegreeKind c)
                  (Container c)
                  ((DegreeOf a `WithKind` DegreeKind c) :<!>: hdb)
         , s ~ ListVariadic '[] s
         , ValidDegree (Container c) (DegreeOf a `WithKind` DegreeKind c)
         , ValidDegree (Container c) hdb
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         )
      => MultiplySetters '[] '[] s a b c lka lkb 'Nothing ('Just lb) where
  multiplySetters set1 set2 c
    = let (a,hb) = (>!<) @(Container c) @_ @_ @(DegreeOf a `WithKind` DegreeKind c) @hdb c
          b = generated @(Container c) @_ @_ @(lb `WithKind` LabelKind c) hb
      in set2 b . set1 a
instance ( Contained c
         , GradedSemigroup (Container c) (DegreeKind c)
         , GeneratedGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , FreeGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ GenType (Container c) (LabelKind c) (la `WithKind` LabelKind c)
         , hda ~ GenDeg
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (la `WithKind` LabelKind c)
         , b ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf b `WithKind` DegreeKind c)
         , c ~ ListVariadic '[] c
         , c ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (hda :<!>: (DegreeOf b `WithKind` DegreeKind c))
         , s ~ ListVariadic '[] s
         , ValidDegree (Container c) hda
         , ValidDegree (Container c) (DegreeOf b `WithKind` DegreeKind c)
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         )
       => MultiplySetters '[] '[] s a b c lka lkb ('Just la) 'Nothing where
  multiplySetters set1 set2 c
    = let (ha,b) = (>!<) @(Container c) @_ @_ @hda @(DegreeOf b `WithKind` DegreeKind c) c
          a = generated @(Container c) @_ @_ @(la `WithKind` LabelKind c) ha
      in set2 b . set1 a
instance ( Contained c
         , GradedSemigroup (Container c) (DegreeKind c)
         , GeneratedGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , FreeGradedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ GenType (Container c) (LabelKind c) (la `WithKind` LabelKind c)
         , hda ~ GenDeg
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (la `WithKind` LabelKind c)
         , b ~ GenType (Container c) (LabelKind c) (lb `WithKind` LabelKind c)
         , hdb ~ GenDeg
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (lb `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Grade
                  (DegreeKind c)
                  (Container c)
                  (hda :<!>: hdb)
         , s ~ ListVariadic '[] s
         , ValidDegree (Container c) hda
         , ValidDegree (Container c) hdb
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         )
      => MultiplySetters '[] '[] s a b c lka lkb ('Just la) ('Just lb) where
  multiplySetters set1 set2 c
    = let (ha,hb) = (>!<) @(Container c) @_ @_ @hda @hdb c
          a = generated @(Container c) @_ @_ @(la `WithKind` LabelKind c) ha
          b = generated @(Container c) @_ @_ @(lb `WithKind` LabelKind c) hb
      in set2 b . set1 a

instance MultiplySetters is        js        s a b c lka lkb mla mlb
      => MultiplySetters (i ': is) (j ': js) s a b c lka lkb mla mlb where
  multiplySetters set1 set2 (i,j)
    = multiplySetters @is @js @s @a @b @c @lka @lkb @mla @mlb (set1 i) (set2 j)
      :: ListVariadic (Zip is js `Postpend` c `Postpend` s) s
instance MultiplySetters '[] js        s a b c lka lkb mla mlb
      => MultiplySetters '[] (j ': js) s a b c lka lkb mla mlb where
  multiplySetters set1 set2 j
    = multiplySetters @'[] @js @s @a @b @c @lka @lkb @mla @mlb set1 (set2 j)
      :: ListVariadic (Zip '[] js `Postpend` c `Postpend` s) s
instance MultiplySetters is        '[] s a b c lka lkb mla mlb
      => MultiplySetters (i ': is) '[] s a b c lka lkb mla mlb where
  multiplySetters set1 set2 i
    = multiplySetters @is @'[] @s @a @b @c @lka @lkb @mla @mlb (set1 i) set2
      :: ListVariadic (Zip is '[] `Postpend` c `Postpend` s) s

instance forall is js ks (s :: Type) a b c
               (o1 :: Optic is s a) (o2 :: Optic js s b)
               (lka :: Type) (lkb :: Type)
               (mla :: Maybe lka)
               (mlb :: Maybe lkb)
       . ( ReifiedSetter o1
         , ReifiedSetter o2
         , ks ~ Zip is js
         , c ~ ProductIfDisjoint o1 o2
         , lka ~ LabelKind (LastAccessee o1)
         , lkb ~ LabelKind (LastAccessee o2)
         , lka ~ LabelKind c
         , lkb ~ LabelKind c
         , mla ~ ( If (IsProduct o1)
                    'Nothing
                    ( 'Just
                        ( LabelOf (LastAccessee o1) (LastOptic o1) `WithKind` lka )
                    )
                  )
         , mlb ~ ( If (IsProduct o2)
                    'Nothing
                    ( 'Just
                        ( LabelOf (LastAccessee o2) (LastOptic o2) `WithKind` lkb )
                    )
                  )
         , MultiplySetters
            is js s a b c lka lkb mla mlb
         )
      => ReifiedSetter ((o1 `ProductO` o2) :: Optic ks s c) where
  set = multiplySetters @is @js @s @a @b @c @lka @lkb @mla @mlb (set @o1) (set @o2)
