{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
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
  * @OfType ty :: Optic '[] a ty@ focuses on all components of 'a' of type 'ty'.
  This allows setting multiple components of the same type to a given value.

Again, these are type-checked for validity. For instance, one cannot create a product setter
unless the two argument setters are disjoint.
(Note that this disallows product setters involving run-time indices,
as the required disjointness property can't be checked at compile-time.)


Getters\/setters are optics which support accessing\/setting components.
Type-level optics which can be reified to provide value-level getters and setters are defined
through the 'ReifiedGetter' and 'ReifiedSetter' type classes, instances of which are provided
for types used in this library in the "FIR.Syntax.Optics" module, or in this module
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

> set @( ( Entry 0 0 :*: Entry 0 2 :*: Entry 2 0 :*: Entry 2 2 ) :.: OfType Double ) 9 mat
M33
  9 1 9
  3 4 5
  9 7 9
@

'OfType' allows us to simultaneously set several entries (of the same type) to the same value.
__Warning:__ 'OfType' only creates a /setter/, not a /getter/.

Note that the 'FIR.Syntax.Optics.Diag' synonym exists for focusing on the diagonal of a matrix:

@
> view @Diag mat
V3 0 4 8
@

There is also the 'FIR.Syntax.Optics.Center' synonym for the center of a matrix,
which allows setting all diagonal entries of a square matrix to a single value:

@
> set @Center 9 mat
M33
  9 1 2
  3 9 5
  6 7 9
@

In fact 'FIR.Syntax.Optics.Center' can be simply defined as @Diag :.: OfType eltTy@.
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
    -- ** Indexing information (used for overlap checking)
  , IndexInfo(..), IndexChain

    -- ** Containers
    -- $containers
  , Container(..)

    -- * Getter & setter instances
    -- $instances

    -- ** OfType optic
  , OfType
    -- $oftype_instances

    -- ** Composition of optics
  , (:.:)
    -- $composition_instances

    -- ** Product of optics
  , (:*:), Prod, EndProd
  , ProductComponents(..)
  , ComponentsGettable, ComponentsSettable
  , ArePairwiseDisjoint
    -- $product_instances

  ) where


-- base
import Data.Kind
  ( Type, Constraint )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import GHC.TypeLits
  ( Symbol, AppendSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Data.Product
  ( HList(HNil, (:>))
  , IsProduct(fromHList, toHList)
  , AreProducts
  , MapHList
  )
import Data.Type.Error
  ( IsRight, And )
import Data.Type.List
  ( type (:++:), Postpend
  , Tail, MapTail
  , Replicate, Length
  , MapSingleton
  , ZipCons
  , SameLength(sSameLength)
  , SSameLength(SSameSucc, SSameZero)
  )
import Data.Type.Maybe
  ( IfNothingThen )
import Data.Type.String
  ( ShowNat )
import Data.Function.Variadic
  ( ListVariadic )
import FIR.Prim.Image
  ( Image )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( PrimTy(FieldsOfType) )

----------------------------------------------------------------------

infixr 9 :.:
infixr 9 `ComposeO`
infixr 3 `ProductO`
infixr 3 :*:

-- | Optic data (kind).
data Optic (is :: [Type]) (s :: k) (a :: Type) where
  -- | Focus onto all the sub-parts which have a certain type.
  OfType_  :: Type -> Optic is s a
  -- | Optic with indexing information provided at run-time.
  RTOptic_ :: Optic is s a
  -- | Compile-time field (e.g. numeric index or symbolic field name).
  Field_   :: fld -> Optic is s a
  -- | Composition of optics.
  ComposeO :: Optic is s a -> Optic js a b -> Optic ks s b
  -- | Unbiased product of optics.
  Prod_ :: ProductComponents iss s as -> Optic js s p

-- | Wrapper to keep track of components of a product optic.
--
-- The first argument keeps track of runtime indices.
-- Its length keeps track of how many additional arguments need to be provided when using the optic.
-- For instance, @ iss ~ [ [X1,X2], [Y1,Y2], [Z1,Z2] ]@ means that we expect 3 additional indexing arguments,
-- with first argument consisting of indices of types @X1@ and X2@,
-- second argument consisting of indices of types @Y1@ and @Y2@,
-- and third argument consisting of indices of types @Z1@ and @Z2@.
--
-- @ view \@( optic :: Optic [ [x1,x2], [y1,y2], [z1,z2] ] S A) (x1,x2) (y1,y2) (z1,z2) s :: A @
data ProductComponents (iss :: [[Type]]) (s :: k) (as :: [Type]) where
  EndProd_ :: ProductComponents iss s as
  ProductO :: Optic is s a -> ProductComponents iss s as -> ProductComponents jss s (a ': as)

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
--   * 'AnIndex', 'Index', 'Name', 'OfType' create specific optics,
--   * ':.:' composes two optics (left-most argument = outer-most optic),
--   * 'Prod', ':*:' and 'EndProd' allow the formation of (unbiased) product optics.
--
-- See also "FIR.Syntax.Images" for an overview of how to use optics with images.


-- | Run-time index (kind-correct).
type AnIndex (ix :: Type  ) = (RTOptic_ :: Optic '[ix] s a)
-- | Compile-time index (kind-correct).
type Index   (i  :: Nat   ) = (Field_ i :: Optic '[]   s a)
-- | Compile-time field name (kind-correct).
type Name    (k  :: Symbol) = (Field_ k :: Optic '[]   s a)

-- | Optic for components of a particular type (kind-correct).
type OfType (ty :: Type) = (OfType_ ty :: Optic '[] s ty)
-- | Composition of optics (kind-correct).
type (:.:) (o1 :: Optic is s a) (o2 :: Optic js a b)
  = ( (o1 `ComposeO` o2) :: Optic (is :++: js) s b )
-- | Gather optics together in order to take an unbiased product over them (kind-correct).
type family (:*:) (o :: Optic is s a) (os :: ProductComponents iss s as) :: ProductComponents (ProductIndices is os) s (a ': as) where
  ( o :: Optic is s a ) :*: EndProd_
    = o `ProductO` ( EndProd_ :: ProductComponents (Replicate (Length is) '[]) s '[] )
  ( o :: Optic is s a ) :*: ( os :: ProductComponents iss s as )
    = ( o `ProductO` os :: ProductComponents (ProductIndices is os) s (a ': as) )
-- | Empty set of optic components.
type family EndProd :: ProductComponents '[] s '[] where
  EndProd = EndProd_
-- | Kind of indices resulting from a product.
type family ProductIndices (is :: [Type]) (os :: ProductComponents iss s as) :: [[Type]] where
  ProductIndices is EndProd_ = MapSingleton is
  ProductIndices is ( os :: ProductComponents iss s as ) = ZipCons is iss
-- | Unbiased product of a set of component optics (kind-correct).
type Prod (os :: ProductComponents iss s as)
  = ( Prod_ os :: Optic (MapHList iss) s p )
-- TODO: it would be nice to have "p" as a first argument for visible kind application
-- (this might require standalone kind signatures?)

type family ShowOptic (o :: Optic is s a) :: ErrorMessage where
  ShowOptic (OfType_ ty) = Text "OfType " :<>: ShowType ty
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
  ShowOptic (Prod_ comps)
    = Text "Prod ( " :<>: ShowComponents comps :<>: Text " )"

type family ShowComponents (comps :: ProductComponents iss s as) :: ErrorMessage where
  ShowComponents EndProd_ = Text "EndProd"
  ShowComponents (o `ProductO` comps)
    = ShowOptic o :<>: Text " :*: " :<>: ShowComponents comps

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
-- This type class contains type-level methods for checking disjointness of optics,
-- in particular the ability to check that a numeric index
-- and a symbolic field name do not refer to the same component,
-- when applicable.

class Container (s :: Type) where

  -- | Compute a numeric indexing from a symbolic index.
  -- Can return a type-error if symbolic indexing is not supported.
  --
  -- Chiefly needed for overlap checking for 'FIR.Prim.Struct.Struct's.
  type FieldIndexFromName s (k :: Symbol) :: Nat

----------------------------------------------------------------------
-- $instances
--
-- This module defines getter and setter instances that are applicable in general situations:
--
--   * composition of optics,
--   * product of optics,
--
-- In other words, this module provides the general framework for /combining/ optics.
-- To manipulate specific types (e.g. the ability to access a component of a vector by its index),
-- corresponding specific instances are required.
--
-- The specific instances, as they pertain to datatypes used by this library,
-- are found in the "FIR.Syntax.Optics" module.
-- This includes instances for vectors, matrices and structs.
-- See also "FIR.Syntax.Images" for the peculiar 'FIR.Syntax.Images.ImageTexel' lens.

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
-- $oftype_instances
--
-- The 'OfType' optic constructor creates setters, not getters.
-- So we prevent setter instances.
--
-- Getter instances are defined individually for specific types,
-- see "FIR.Syntax.Optics".

instance
  ( TypeError ( Text "get: cannot use 'OfType' optic as a getter." ) )
  => Gettable (OfType_ ty :: Optic i s a) where
instance
  ( TypeError ( Text "get: cannot use 'OfType' optic as a getter." ) )
  => ReifiedGetter (OfType_ ty :: Optic i s a) where
  view = error "unreachable"

instance {-# OVERLAPPING #-}
         ( r ~ a, empty ~ '[] )
      => Settable      (OfType_ a :: Optic empty a r) where
instance {-# OVERLAPPING #-}
         ( r ~ a, empty ~ '[], ListVariadic '[] a ~ a )
      => ReifiedSetter (OfType_ a :: Optic empty a r) where
  set = const

-- Default instance to use: "a" does not contain components of type "ty".
-- Should be overridden by specific instances that
-- provide evidence that "a" does in fact contain components of type "ty".
instance {-# OVERLAPPABLE #-}
         ( r ~ ty, empty ~ '[] )
      => Settable      (OfType_ ty :: Optic empty a r) where
instance {-# OVERLAPPABLE #-}
         ( r ~ ty, empty ~ '[], ListVariadic '[] a ~ a )
      => ReifiedSetter (OfType_ ty :: Optic empty a r) where
  set _ = id

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
-- This is to provide improved type inference, see "FIR.Syntax.Optics".

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
--   * the product of disjoint setters is a setter,
--   * the product of getters is a getter.

-------------
-- disjointness checking

data IndexInfo
  = ThisIndex Nat
  | AnyIndex -- any index (e.g. runtime index, or a situation such as "TypeOf a :: Optic '[] (Array n a) a")

type IndexChain = [IndexInfo]

data IndexChoice
  = OneIndex IndexInfo
  | Choice   [IndexChoices]

type IndexChoices = [IndexChoice]

type family Disjoint ( o1 :: Optic is s a ) ( o2 :: Optic js s b ) :: Either ErrorMessage () where
  Disjoint o1 o2
    = ErrorIfOverlap o1 o2
        ( FirstOverlap (IndexChoicesOf o1) (IndexChoicesOf o2) )

type ArePairwiseDisjoint (os :: ProductComponents iss s as)
  = ( IsRight (PairwiseDisjoint os) :: Constraint )

type family PairwiseDisjoint (os :: ProductComponents iss s as) :: Either ErrorMessage () where
  PairwiseDisjoint EndProd_          = Right '()
  PairwiseDisjoint (o `ProductO` os) =
    ( o `DisjointFrom` os ) `And` PairwiseDisjoint os

type family DisjointFrom (o :: Optic is s a) (os :: ProductComponents iss s as) :: Either ErrorMessage () where
  DisjointFrom o EndProd_           = Right '()
  DisjointFrom o (o' `ProductO` os) =
    Disjoint o o' `And` (o `DisjointFrom` os)

type family ErrorIfOverlap
              ( o1      :: Optic is s a )
              ( o2      :: Optic js s b )
              ( overlap :: Maybe [Nat]  )
            :: Either ErrorMessage ()
            where
  ErrorIfOverlap _  _ 'Nothing = Right '()
  ErrorIfOverlap (o1 :: Optic is s a) o2 ('Just nfo)
    = TypeError
        (    Text "Cannot create product setter at type:"
        :$$: Text "  " :<>: ShowType s
        :$$: Text "Overlap between optics"
        :$$: Text "  - " :<>: ShowOptic o1 :<>: Text ","
        :$$: Text "  - " :<>: ShowOptic o2
        :<>: IndexChainErrorMessage nfo
        )

type family IndexChainErrorMessage (chain :: [Nat]) :: ErrorMessage where
  IndexChainErrorMessage '[]
    = Text "."
  IndexChainErrorMessage '[i]
    = Text "," :$$: Text "at index " :<>: Text (ShowNat i) :<>: Text "."
  IndexChainErrorMessage is
    = Text "," :$$: Text "at index chain ( " :<>: Text (ShowIndexChain is) :<>: Text " )."

type family ShowIndexChain (chain :: [Nat]) :: Symbol where
  ShowIndexChain '[ ] = ""
  ShowIndexChain '[i] = ShowNat i
  ShowIndexChain (i ': j ': is)
    = ShowNat i `AppendSymbol` " -- " `AppendSymbol` ( ShowIndexChain (j ': is) )

type family FirstOverlap
              ( nfo1 :: IndexChoices )
              ( nfo2 :: IndexChoices )
            :: Maybe [Nat]
            where
  FirstOverlap '[] _  = Just '[]
  FirstOverlap _  '[] = Just '[]
  FirstOverlap ( OneIndex  i1 ': nfo1 ) ( OneIndex  i2 ': nfo2 )
    = FirstOverlapOneIndex i1 nfo1 i2 nfo2
  FirstOverlap ( Choice nfos1 ': nfo1 ) ( Choice nfos2 ': nfo2 )
    = FirstCrosswiseOverlap  nfos1 (Just nfo1)  nfos2 (Just nfo2)
  FirstOverlap ( Choice nfos1 ': nfo1 ) nfo2
    = FirstCrosswiseOverlap  nfos1 (Just nfo1) '[nfo2] Nothing
  FirstOverlap nfo1                     ( Choice nfos2 ': nfo2 )
    = FirstCrosswiseOverlap '[nfo1] Nothing     nfos2 (Just nfo2)

type family FirstOverlapOneIndex
              ( i1   :: IndexInfo    )
              ( nfo1 :: IndexChoices )
              ( i2   :: IndexInfo    )
              ( nfo2 :: IndexChoices )
            :: Maybe [Nat] where
  FirstOverlapOneIndex (ThisIndex i ) nfo1 (ThisIndex i ) nfo2
    = FmapConsMaybe i ( FirstOverlap nfo1 nfo2 )
  FirstOverlapOneIndex (ThisIndex i1) _    (ThisIndex i2) _
    = Nothing
  FirstOverlapOneIndex (ThisIndex i1) nfo1 AnyIndex       nfo2
    = FmapConsMaybe i1 ( FirstOverlap nfo1 nfo2 )
  FirstOverlapOneIndex  AnyIndex      nfo1 (ThisIndex i2) nfo2
    = FmapConsMaybe i2 ( FirstOverlap nfo1 nfo2 )
  FirstOverlapOneIndex  AnyIndex      nfo1 AnyIndex       nfo2
    = FmapConsMaybe 0  ( FirstOverlap nfo1 nfo2 )

type family FirstCrosswiseOverlap
              ( nfos1 :: [IndexChoices]     )
              ( nxt1  :: Maybe IndexChoices )
              ( nfos2 :: [IndexChoices]     )
              ( nxt2  :: Maybe IndexChoices )
           :: Maybe [Nat]
           where
  FirstCrosswiseOverlap '[]               _      _     _
    = Nothing
  FirstCrosswiseOverlap ( nfo1 ': nfos1 ) mbNxt1 nfos2 mbNxt2
    = ( FirstOverlapIn nfo1 mbNxt1 nfos2 mbNxt2 )
      `IfNothingThen`
      ( FirstCrosswiseOverlap nfos1 mbNxt1 nfos2 mbNxt2 )

type family FirstOverlapIn
              ( nfo1  :: IndexChoices       )
              ( nxt1  :: Maybe IndexChoices )
              ( nfos2 :: [IndexChoices]     )
              ( nxt2  :: Maybe IndexChoices )
           :: Maybe [Nat]
           where
  FirstOverlapIn _    _      '[]               _
    = Nothing
  FirstOverlapIn nfo1 mbNxt1 ( nfo2 ': nfos2 ) mbNxt2
    = ( FirstOverlap ( nfo1 `MaybeAppend` mbNxt1 ) ( nfo2 `MaybeAppend` mbNxt2 ) )
      `IfNothingThen`
      ( FirstOverlapIn nfo1 mbNxt1 nfos2 mbNxt2 )

type family FmapConsMaybe ( i :: k ) ( js :: Maybe [k] ) :: Maybe [k] where
  FmapConsMaybe _ Nothing   = Nothing
  FmapConsMaybe i (Just js) = Just (i ': js)

type family MaybeAppend ( xs :: [k] ) ( mb_ys :: Maybe [k] ) :: [k] where
  MaybeAppend xs Nothing   = xs
  MaybeAppend xs (Just ys) = xs :++: ys

type family IndexChoicesOf ( o :: Optic is s a ) :: IndexChoices where
  IndexChoicesOf ( Field_ ( i :: Nat    ) ) = '[ OneIndex (ThisIndex i) ]
  IndexChoicesOf ( Field_ ( k :: Symbol ) :: Optic is s a )
    = '[ OneIndex (ThisIndex ( FieldIndexFromName s k )) ]
  IndexChoicesOf ( Field_ ( i :: fld    ) )
    = TypeError
        (    Text "Cannot create product setter."
        :$$: Text "Index " :<>: ShowType i
        :<>: Text " has unsupported kind " :<>: ShowType fld :<>: Text "."
        )
  IndexChoicesOf RTOptic_ = '[ OneIndex AnyIndex ]
  IndexChoicesOf ( o1 `ComposeO` o2 )
    = IndexChoicesOf o1 :++: IndexChoicesOf o2
  IndexChoicesOf ( Prod_ comps )
    = '[ Choice ( ComponentIndexChoices comps ) ]
  IndexChoicesOf ( OfType_ ty :: Optic is s a )
    = '[ Choice ( MapMapOneIndex (FieldsOfType s ty) ) ]


type family ComponentIndexChoices ( comps :: ProductComponents iss s as ) :: [IndexChoices] where
  ComponentIndexChoices EndProd_ = '[]
  ComponentIndexChoices (o `ProductO` os)
    = IndexChoicesOf o ': ComponentIndexChoices os

type family MapMapOneIndex ( flds :: [IndexChain] ) :: [IndexChoices] where
  MapMapOneIndex '[] = '[]
  MapMapOneIndex ( nfo ': nfos ) = MapOneIndex nfo ': MapMapOneIndex nfos

type family MapOneIndex ( flds :: IndexChain ) :: IndexChoices where
  MapOneIndex '[] = '[]
  MapOneIndex ( nfo ': nfos ) = OneIndex nfo ': MapOneIndex nfos

-------------
-- instances

class ComponentsGettable (os :: ProductComponents iss s as) where
instance ComponentsGettable EndProd_ where
instance ( ComponentsGettable os, Gettable o ) => ComponentsGettable (o `ProductO` os)
class ComponentsSettable (os :: ProductComponents iss s as) where
instance ComponentsSettable EndProd_ where
instance ( ComponentsSettable os, Settable o ) => ComponentsSettable (o `ProductO` os)

instance forall
            ( iss :: [[Type]] )
            ( js  :: [Type]   )
            ( k   :: Type     )
            ( s   :: k        )
            ( as  :: [Type]   )
            ( p   :: Type     )
            ( os  :: ProductComponents iss s as )
          .
          ( ComponentsGettable os
          , IsProduct p as
          , AreProducts js iss as
          )
       => Gettable (Prod_ os :: Optic js s p)
       where

instance forall
            ( iss :: [[Type]] )
            ( js  :: [Type]   )
            ( k   :: Type     )
            ( s   :: k        )
            ( as  :: [Type]   )
            ( p   :: Type     )
            ( os  :: ProductComponents iss s as )
          .
          ( ComponentsSettable os
          , IsProduct p as
          , AreProducts js iss as
          , ArePairwiseDisjoint os
          )
       => Settable (Prod_ os :: Optic js s p)
       where

instance forall
            ( iss :: [[Type]] )
            ( js  :: [Type]   )
            ( s   :: Type     )
            ( as  :: [Type]   )
            ( p   :: Type     )
            ( os  :: ProductComponents iss s as )
          .
          ( ComponentsGettable os
          , IsProduct p as
          , AreProducts js iss as
          , MultiplyGetters iss js s as p
          , GetViewers os
          )
       => ReifiedGetter (Prod_ os :: Optic js s p)
       where
  view = multiplyGetters @iss @js @s @as @p
           ( viewers @iss @s @as @os )

instance forall
            ( iss :: [[Type]] )
            ( js  :: [Type]   )
            ( s   :: Type     )
            ( as  :: [Type]   )
            ( p   :: Type     )
            ( os  :: ProductComponents iss s as )
          .
          ( ComponentsSettable os
          , IsProduct p as
          , AreProducts js iss as
          , ArePairwiseDisjoint os
          , MultiplySetters iss js s as p
          , GetSetters os
          )
       => ReifiedSetter (Prod_ os :: Optic js s p)
       where
  set = multiplySetters @iss @js @s @as @p
           ( setters @iss @s @as @os )

class GetViewers (os :: ProductComponents iss (s :: Type) as) where
  viewers :: Viewers iss s as

instance ( as ~ '[] ) => GetViewers ( EndProd_ :: ProductComponents iss s as ) where
  viewers = NilViewer
instance forall
          ( iss :: [[Type]]    )
          ( kss :: [[Type]]    )
          ( is :: [Type]       )
          ( s  :: Type         )
          ( as :: [Type]       )
          ( a  :: Type         )
          ( o  :: Optic is s a )
          ( os :: ProductComponents iss s as )
         .
         ( GetViewers os
         , ReifiedGetter o
         , SameLength is kss
         , kss ~ ZipCons is iss
         )
       => GetViewers (o `ProductO` os :: ProductComponents kss s (a ': as)) where
  viewers = ConsViewer @is @s @a (sSameLength @_ @_ @is @kss) Proxy Proxy Proxy
              (view @o) (viewers @iss @s @as @os)

class GetSetters (os :: ProductComponents iss (s :: Type) as) where
  setters :: Setters iss s as

instance ( as ~ '[] ) => GetSetters ( EndProd_ :: ProductComponents iss s as ) where
  setters = NilSetter
instance forall
          ( iss :: [[Type]]    )
          ( kss :: [[Type]]    )
          ( is :: [Type]       )
          ( s  :: Type         )
          ( as :: [Type]       )
          ( a  :: Type         )
          ( o  :: Optic is s a )
          ( os :: ProductComponents iss s as )
         .
         ( GetSetters os
         , ReifiedSetter o
         , SameLength is kss
         , kss ~ ZipCons is iss
         )
       => GetSetters (o `ProductO` os :: ProductComponents kss s (a ': as)) where
  setters = ConsSetter @is @s @a (sSameLength @_ @_ @is @kss) Proxy Proxy Proxy
              (set @o) (setters @iss @s @as @os)

class MultiplyGetters (iss :: [[Type]]) (js :: [Type]) (s :: Type) (as :: [Type]) (p :: Type) where
  multiplyGetters :: Viewers iss s as -> ListVariadic (js `Postpend` s) p

class MultiplySetters (iss :: [[Type]]) (js :: [Type]) (s :: Type) (as :: [Type]) (p :: Type) where
  multiplySetters :: Setters iss s as -> ListVariadic (js `Postpend` p `Postpend` s) s

instance ( IsProduct p as, p ~ ListVariadic '[] p )
       => MultiplyGetters iss '[] s as p where
  multiplyGetters :: Viewers iss s as -> s -> p
  multiplyGetters views s = fromHList @p @as ( applyGetters views s )

instance ( IsProduct j is, SameLength is as, MultiplyGetters iss js s as p )
      => MultiplyGetters (is ': iss) (j ': js) s as p where
  multiplyGetters :: Viewers (is ': iss) s as -> ( j -> ListVariadic (js `Postpend` s) p )
  multiplyGetters views j =
    multiplyGetters @iss @js @s @as @p
      ( passGetterIndex (sSameLength @_ @_ @is @as) ( toHList j ) views )

instance ( IsProduct p as, s ~ ListVariadic '[] s )
      => MultiplySetters iss '[] s as p where
  multiplySetters :: Setters iss s as -> p -> s -> s
  multiplySetters sets p s = applySetters sets ( toHList @p @as p ) s

instance ( IsProduct j is, SameLength is as, MultiplySetters iss js s as p )
      => MultiplySetters (is ': iss) (j ': js) s as p where
  multiplySetters :: Setters (is ': iss) s as -> ( j -> ListVariadic (js `Postpend` p `Postpend` s) s )
  multiplySetters sets j
    = multiplySetters @iss @js @s @as @p
        ( passSetterIndex (sSameLength @_ @_ @is @as) ( toHList j ) sets )


applyGetters :: Viewers iss s bs -> s -> HList bs
applyGetters   NilViewer                                        _ = HNil
applyGetters ( ConsViewer _ _ _ (_ :: Proxy b) getter getters ) s =
  case getters of
    ( _ :: Viewers jss s cs )
      -> ( unsafeCoerce getter :: s -> b ) s
         :> applyGetters ( unsafeCoerce getters :: Viewers '[] s cs ) s

applySetters :: Setters iss s bs -> HList bs -> s -> s
applySetters   NilSetter                                     _         s  = s
applySetters ( ConsSetter _ _ _ (_ :: Proxy b) setter sets ) (b :> bs) s  =
  case sets of
    ( _ :: Setters jss s cs )
      -> applySetters ( unsafeCoerce sets :: Setters '[] s cs ) bs
       $ ( unsafeCoerce setter :: b -> s -> s ) b s

passGetterIndex
  :: forall (js :: [Type]) (jss :: [[Type]]) (s :: Type) (as :: [Type])
  . SSameLength js as -> HList js -> Viewers (js ': jss) s as -> Viewers jss s as
passGetterIndex SSameZero                                        _  _    = unsafeCoerce NilViewer
passGetterIndex (SSameSucc ( same1 :: SSameLength t_js t_as ) ) js views =
  case ( js, views ) of
    ( (k :: j) :> (ks :: HList t_js), ConsViewer same2 (_ :: Proxy is) (_ :: Proxy iss) _ getter getters ) ->
      case same2 of
        ( sameSucc@(SSameSucc (same3 :: SSameLength t_is jss) ) ) ->
          case sameSucc of
            ( _ :: SSameLength (i ': t_is) (js ': jss) ) ->
              case ( unsafeCoerce Refl :: ZipCons t_is (Tail iss) :~: jss
                   , unsafeCoerce Refl :: ( t_js ': MapTail jss ) :~: iss
                   , unsafeCoerce Refl :: i :~: j
                   ) of
                ( Refl, Refl, Refl ) ->
                    ConsViewer same3 (Proxy @t_is) (Proxy @(Tail iss)) Proxy
                      ( getter k )
                      ( passGetterIndex @t_js @(MapTail jss) @s @t_as same1 ks getters )

passSetterIndex
  :: forall (js :: [Type]) (jss :: [[Type]]) (s :: Type) (as :: [Type])
  . SSameLength js as -> HList js -> Setters (js ': jss) s as -> Setters jss s as
passSetterIndex SSameZero                                        _  _   = unsafeCoerce NilViewer
passSetterIndex (SSameSucc ( same1 :: SSameLength t_js t_as ) ) js sets =
  case ( js, sets ) of
    ( (k :: j) :> (ks :: HList t_js), ConsSetter same2 (_ :: Proxy is) (_ :: Proxy iss) _ setter setts ) ->
      case same2 of
        ( sameSucc@(SSameSucc (same3 :: SSameLength t_is jss) ) ) ->
          case sameSucc of
            ( _ :: SSameLength (i ': t_is) (js ': jss) ) ->
              case ( unsafeCoerce Refl :: ZipCons t_is (Tail iss) :~: jss
                   , unsafeCoerce Refl :: ( t_js ': MapTail jss ) :~: iss
                   , unsafeCoerce Refl :: i :~: j
                   ) of
                ( Refl, Refl, Refl ) ->
                    ConsSetter same3 (Proxy @t_is) (Proxy @(Tail iss)) Proxy
                      ( setter k )
                      ( passSetterIndex @t_js @(MapTail jss) @s @t_as same1 ks setts )

data Viewers (iss :: [[Type]]) (s :: Type) (as :: [Type]) where
  NilViewer  :: Viewers iss s '[] -- iss should always be a list of empty lists
  ConsViewer :: forall (is :: [Type]) (s :: Type) (a :: Type) (iss :: [[Type]]) (as :: [Type])
             .  SSameLength is (ZipCons is iss)
             -> Proxy is
             -> Proxy iss
             -> Proxy a
             -> ListVariadic (is `Postpend` s) a
             -> Viewers iss s as
             -> Viewers (ZipCons is iss) s (a ': as)

data Setters (iss :: [[Type]]) (s :: Type) (as :: [Type]) where
  NilSetter  :: Setters iss s '[]  -- iss should always be a list of empty lists
  ConsSetter :: forall (is :: [Type]) (s :: Type) (a :: Type) (iss :: [[Type]]) (as :: [Type])
             .  SSameLength is (ZipCons is iss)
             -> Proxy is
             -> Proxy iss
             -> Proxy a
             -> ListVariadic (is `Postpend` a `Postpend` s) s
             -> Setters iss s as
             -> Setters (ZipCons is iss) s (a ': as)
