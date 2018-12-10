{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Type.Optic where

-- base
import Data.Kind(Type)
import Data.Type.Bool(If, type (&&), Not)
import GHC.TypeLits( Symbol
                   , TypeError, ErrorMessage(..)
                   )
import GHC.TypeNats(Nat)

-- lens
import qualified Control.Lens as Lens(view)
import qualified Control.Lens.Iso as Lens(from)

-- fir
import Data.Type.Map(type (:++:))
import Data.Function.Variadic(ListVariadic)
import Math.Algebra.GradedSemigroup ( GradedSemigroup(..)
                                    , GradedPresentedSemigroup(..)
                                    , GradedFreeSemigroup(..)
                                    , DegreeAt
                                    )

----------------------------------------------------------------------

infixr 9 :.:
infixr 3 :*:

-- optic data (kind)
data Optic (is :: [Type]) (s :: k) (a :: Type) where
  -- built-in lenses (unsafe)
  AnIndex_ :: Optic is s a
  Index_   :: Nat    -> Optic is s a
  Name_    :: Symbol -> Optic is s a
  -- optic combinators (unsafe)
  All_     :: Optic is s a -> Optic is s b
  ComposeO :: Optic is s a -> Optic js a b -> Optic ks s b
  ProductO :: Optic is s a -> Optic ix s b -> Optic js s c 

-- safe synonyms (with correct kinds)
type Name (k :: Symbol) = (Name_ k :: Optic '[] s a)
type Index (i :: Nat) = (Index_ i :: Optic '[] s a)
type AnIndex (ix :: Type) = (AnIndex_ :: Optic '[ix] s a)

type family ProductIndices (is :: [Type]) (js :: [Type]) :: [Type] where
  ProductIndices '[] js = js
  ProductIndices is '[] = is
  ProductIndices (i ': is) (j ': js) = (i,j) ': ProductIndices is js

type (:*:) (o1 :: Optic is s a) (o2 :: Optic js s b)
  = ( (o1 `ProductO` o2)
        :: Optic
              ( ProductIndices is js )
              s
              ( Product o1 o2 )
    )
type (:.:) (o1 :: Optic is s a) (o2 :: Optic js a b)
  = ((o1 `ComposeO` o2) :: Optic (is :++: js) s b)
type All (o :: Optic is s a) = (All_ o :: Optic is s (MonoType a))

----------------------------------------------------------------------
-- type classes and synonyms

-- synonyms that allow kind variables to remain invisible
-- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0026-explicit-specificity.rst
type Whole   (optic :: Optic is s a) = s
type Part    (optic :: Optic is s a) = a
type Indices (optic :: Optic is s a) = is

-- type level getter
class Gettable (optic :: Optic is (s :: k) a) | optic -> is s a where
type  Getter (optic :: Optic is (s :: Type) a) = ListVariadic (is :++: '[s]) a

-- type level getter which can be turned into a value-level getter
class Gettable optic => ReifiedGetter optic where
  view :: Getter optic

-- type level setter
class Settable (optic :: Optic is (s :: k) a) | optic -> is s a where
type  Setter (optic :: Optic is (s :: Type) a) = ListVariadic (is :++: '[a,s]) s

-- type level setter which can be turned into a value-level setter
class Settable optic => ReifiedSetter optic where
  set :: Setter optic

-------------------------------

type family ContainerKind (s :: Type) :: k
type family DegreeKind    (s :: Type) :: d
type family LabelKind     (s :: Type) :: l

class Contained (s :: Type) where
  type Container  s :: ContainerKind s
  type DegreeOf   s :: DegreeKind s
  type LabelOf    s (o :: Optic i s a) :: LabelKind s
  type Overlapping s (k :: Symbol) (n :: Nat) :: Bool

class Contained s => MonoContained s where
  type MonoType s

----------------------------------------------------------------------
-- composition

instance forall k (s :: k) is js ks a b (o1 :: Optic is s a) (o2 :: Optic js a b).
         ( Gettable o1
         , Gettable o2
         , ks ~ (is :++: js)
         ) => Gettable ((o1 `ComposeO` o2) :: Optic ks s b) where
instance forall k (s :: k) is js ks a b (o1 :: Optic is s a) (o2 :: Optic js a b).
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
  composeGetters :: ListVariadic (is :++: '[s]) a
                 -> ListVariadic (js :++: '[a]) b
                 -> ListVariadic ((is :++: js) :++: '[s]) b
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
  composeSetters :: ListVariadic (is :++: '[s]) a
                 -> ListVariadic (is :++: '[a,s]) s
                 -> ListVariadic (js :++: '[b,a]) a
                 -> ListVariadic ((is :++: js) :++: '[b,s]) s
instance (a ~ ListVariadic '[] a) => ComposeSetters '[] '[] s a b where
  composeSetters view1 set1 set2 b s
    = set1 (set2 b (view1 s)) s
instance ComposeSetters is js s a b => ComposeSetters (i ': is) js s a b where
  composeSetters view1 set1 set2 i
    = composeSetters @is @js @s @a @b (view1 i) (set1 i) set2
instance ComposeSetters '[] js s a b => ComposeSetters '[] (j ': js) s a b where
  composeSetters view1 set1 set2 j
    = composeSetters @'[] @js @s @a @b view1 set1 (set2 j)

----------------------------------------------------------------------
-- products

-- getter products
instance forall is js ks s a b c (o1 :: Optic is s a) (o2 :: Optic js s b).
         ( Gettable o1
         , Gettable o2
         , ks ~ ProductIndices is js
         , c ~ Product o1 o2
         ) => Gettable ((o1 `ProductO` o2) :: Optic ks s c) where

-- setter products
instance forall is js ks s a b c (o1 :: Optic is s a) (o2 :: Optic js s b) .
         ( Settable o1
         , Settable o2
         , ks ~ ProductIndices is js
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
            :$$: Text "  " :<>: ShowType o1
            :$$: Text "and "
            :$$: Text "  " :<>: ShowType o2
            :$$: Text "are not disjoint."
           )
        )

type family LastAccessee ( o :: Optic is (s :: Type) a ) :: Type where
  LastAccessee (o1 `ComposeO` o2) = LastAccessee o2
  LastAccessee (o :: Optic is s a) = s

type family LastIndices ( o :: Optic is s a ) :: [Type] where
  LastIndices (o1 `ComposeO` o2) = LastIndices o2
  LastIndices (o :: Optic is s a) = s

type family LastOptic ( o :: Optic is s a) :: Optic (LastIndices o) (LastAccessee o) a where
  LastOptic (o1 `ComposeO` o2) = LastOptic o2
  LastOptic (o :: Optic is s a) = o `WithKind` ( Optic (LastIndices o) (LastAccessee o) a )


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
        ( DegreeAt (DegreeKind s) (Container s) (LabelOf s o1) )
        ( DegreeOf a )
        ( DegreeAt (DegreeKind t) (Container t) (LabelOf t o2) )
        ( DegreeOf b )

type family Combine
                ( o1 :: Optic is s a ) ( o2 :: Optic js t b )
                ( b1 :: Bool ) ( b2 :: Bool )
              = ( r :: Type )
                where
  Combine (o1 :: Optic is s a) (o2 :: Optic js t b) b1 b2
    = Apply
        ( WhichKind b1 b2 (DegreeKind s) (DegreeKind a) (DegreeKind t) (DegreeKind b) )
        ( ProductContainer o1 o2 b1 b2 )
        ( ProductDegree o1 o2 b1 b2 )

type family Disjoint
              ( o1 :: Optic is s a )
              ( o2 :: Optic js t b )
            = ( r  :: Bool         )
              where
  Disjoint (Name_  k) (Name_  k) = False
  Disjoint (Index_ n) (Index_ n) = False
  Disjoint AnIndex_   _       
    = TypeError (    Text "set: cannot create a product setter involving run-time indices."
                :$$: Text "Impossible to verify the required disjointness property."
                )
  Disjoint o AnIndex_
    = Disjoint AnIndex_ o
  Disjoint ((Name_ k) :: Optic is s a) ((Index_ n) :: Optic js s b)
    = Not (Overlapping s k n)
  Disjoint ((Index_ n) :: Optic is s a) ((Name_ k) :: Optic js s b)
    = Not (Overlapping s k n)
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

class MultiplyGetters is js s a b c (mla :: Maybe lka) (mlb :: Maybe lkb) where
  multiplyGetters :: ListVariadic (is :++: '[s]) a
                  -> ListVariadic (js :++: '[s]) b
                  -> ListVariadic (ProductIndices is js :++: '[s]) c

instance ( GradedSemigroup (Container c) (DegreeKind c)
         , a ~ ListVariadic '[] a
         , a ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf a `WithKind` DegreeKind c)
         , b ~ ListVariadic '[] b
         , b ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf b `WithKind` DegreeKind c)
         , c ~ ListVariadic '[] c
         , c ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (       (DegreeOf a `WithKind` DegreeKind c)
                    :<!>: (DegreeOf b `WithKind` DegreeKind c)
                  )
         )
      => MultiplyGetters '[] '[] s a b c 'Nothing 'Nothing where
  multiplyGetters view1 view2 s
    = (<!>) @(Container c) @_ @(DegreeOf a `WithKind` DegreeKind c) @(DegreeOf b `WithKind` DegreeKind c)
        (view1 s)
        (view2 s)
instance ( GradedSemigroup (Container c) (DegreeKind c)
         , GradedPresentedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ ListVariadic '[] a
         , a ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf a `WithKind` DegreeKind c)
         , b ~ ListVariadic '[] b
         , b ~ Element (Container c) (LabelKind c) (lb `WithKind` LabelKind c)
         , hdb ~ Degree
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (lb `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Apply
                  (DegreeKind c)
                  (Container c)
                  ((DegreeOf a `WithKind` DegreeKind c) :<!>: hdb)
         )
      => MultiplyGetters '[] '[] s a b c 'Nothing ('Just lb) where
  multiplyGetters view1 view2 s
    = (<!>) @(Container c) @_ @(DegreeOf a `WithKind` DegreeKind c) @hdb
        ( view1 s )
        ( (Lens.view . Lens.from)
            ( homogeneous
                @(Container c)
                @(DegreeKind c)
                @(LabelKind c)
                @(lb `WithKind` LabelKind c)
            )
          ( view2 s )
        )
instance ( GradedSemigroup (Container c) (DegreeKind c)
         , GradedPresentedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ ListVariadic '[] a
         , a ~ Element (Container c) (LabelKind c) (la `WithKind` LabelKind c)
         , b ~ ListVariadic '[] b
         , b ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf b `WithKind` DegreeKind c)
         , hda ~ Degree
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (la `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (hda :<!>: (DegreeOf b `WithKind` DegreeKind c))
         )
      => MultiplyGetters '[] '[] s a b c ('Just la) 'Nothing where
  multiplyGetters view1 view2 s
    = (<!>) @(Container c) @_ @hda @(DegreeOf b `WithKind` DegreeKind c)
        ( (Lens.view . Lens.from)
            ( homogeneous
                @(Container c)
                @(DegreeKind c)
                @(LabelKind c)
                @(la `WithKind` LabelKind c)
            )
          ( view1 s )
        )
        ( view2 s )
instance ( GradedSemigroup (Container c) (DegreeKind c)
         , GradedPresentedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ ListVariadic '[] a
         , a ~ Element (Container c) (LabelKind c) (la `WithKind` LabelKind c)
         , b ~ ListVariadic '[] b
         , b ~ Element (Container c) (LabelKind c) (lb `WithKind` LabelKind c)
         , hda ~ Degree
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (la `WithKind` LabelKind c)
         , hdb ~ Degree
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (lb `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (hda :<!>: hdb)
         )
      => MultiplyGetters '[] '[] s a b c ('Just la) ('Just lb) where
  multiplyGetters view1 view2 s
    = (<!>) @(Container c) @_ @hda @hdb
        ( (Lens.view . Lens.from)
            ( homogeneous
                @(Container c)
                @(DegreeKind c)
                @(LabelKind c)
                @(la `WithKind` LabelKind c)
            )
          ( view1 s )
        )
        ( (Lens.view . Lens.from)
            ( homogeneous
                @(Container c)
                @(DegreeKind c)
                @(LabelKind c)
                @(lb `WithKind` LabelKind c)
            )
          ( view2 s )
        )


instance MultiplyGetters is        js        s a b c mla mlb
      => MultiplyGetters (i ': is) (j ': js) s a b c mla mlb where
  multiplyGetters view1 view2 (i,j)
    = multiplyGetters @is @js @s @a @b @c @mla @mlb (view1 i) (view2 j)
      :: ListVariadic (ProductIndices is js :++: '[s]) c
instance MultiplyGetters '[] js        s a b c mla mlb
      => MultiplyGetters '[] (j ': js) s a b c mla mlb where
  multiplyGetters view1 view2 j
    = multiplyGetters @'[] @js @s @a @b @c @mla @mlb view1 (view2 j)
      :: ListVariadic (ProductIndices '[] js :++: '[s]) c
instance MultiplyGetters is        '[] s a b c mla mlb
      => MultiplyGetters (i ': is) '[] s a b c mla mlb where
  multiplyGetters view1 view2 i
    = multiplyGetters @is @'[] @s @a @b @c @mla @mlb (view1 i) view2
      :: ListVariadic (ProductIndices is '[] :++: '[s]) c

instance forall is js ks (s :: Type) a b c
               (o1 :: Optic is s a) (o2 :: Optic js s b)
               (mla :: Maybe (LabelKind (LastAccessee o1)))
               (mlb :: Maybe (LabelKind (LastAccessee o2)))
       . ( ReifiedGetter o1
         , ReifiedGetter o2
         , ks ~ ProductIndices is js
         , c ~ Product o1 o2
         , mla ~ ( If (IsProduct o1) 'Nothing ('Just (LabelOf (LastAccessee o1) (LastOptic o1))) )
         , mlb ~ ( If (IsProduct o2) 'Nothing ('Just (LabelOf (LastAccessee o2) (LastOptic o2))) )
         , MultiplyGetters
            is js s a b c mla mlb
         )
      => ReifiedGetter ((o1 `ProductO` o2) :: Optic ks s c) where
  view = multiplyGetters @is @js @s @a @b @c @mla @mlb (view @o1) (view @o2)


class MultiplySetters is js s a b c (mla :: Maybe lka) (mlb :: Maybe lkb) where
  multiplySetters :: ListVariadic (is :++: '[a,s]) s
                  -> ListVariadic (js :++: '[b,s]) s
                  -> ListVariadic (ProductIndices is js :++: '[c,s]) s

instance ( GradedSemigroup (Container c) (DegreeKind c)
         , GradedFreeSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf a `WithKind` DegreeKind c)
         , b ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf b `WithKind` DegreeKind c)
         , c ~ ListVariadic '[] c
         , c ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (      (DegreeOf a `WithKind` DegreeKind c)
                   :<!>: (DegreeOf b `WithKind` DegreeKind c)
                  )
         , s ~ ListVariadic '[] s
         , ValidDegree (Container c) (DegreeOf a `WithKind` DegreeKind c)
         , ValidDegree (Container c) (DegreeOf b `WithKind` DegreeKind c)
         )
       => MultiplySetters '[] '[] s a b c 'Nothing 'Nothing where
  multiplySetters set1 set2 c
    = let (a,b) = (>!<) c
      in  set2 b . set1 a
instance ( GradedSemigroup (Container c) (DegreeKind c)
         , GradedPresentedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , GradedFreeSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf a `WithKind` DegreeKind c)
         , b ~ Element (Container c) (LabelKind c) (lb `WithKind` LabelKind c)
         , hdb ~ Degree
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (lb `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Apply
                  (DegreeKind c)
                  (Container c)
                  ((DegreeOf a `WithKind` DegreeKind c) :<!>: hdb)
         , s ~ ListVariadic '[] s
         , ValidDegree (Container c) (DegreeOf a `WithKind` DegreeKind c)
         , ValidDegree (Container c) hdb
         )
      => MultiplySetters '[] '[] s a b c 'Nothing ('Just lb) where
  multiplySetters set1 set2 c
    = let (a,hb) = (>!<) @(Container c) @_ @_ @(DegreeOf a `WithKind` DegreeKind c) @hdb c
          b = Lens.view
                ( homogeneous @(Container c) @_ @_ @(lb `WithKind` LabelKind c) )
                hb
      in set2 b . set1 a
instance ( GradedSemigroup (Container c) (DegreeKind c)
         , GradedPresentedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , GradedFreeSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ Element (Container c) (LabelKind c) (la `WithKind` LabelKind c)
         , hda ~ Degree
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (la `WithKind` LabelKind c)
         , b ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (DegreeOf b `WithKind` DegreeKind c)
         , c ~ ListVariadic '[] c
         , c ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (hda :<!>: (DegreeOf b `WithKind` DegreeKind c))
         , s ~ ListVariadic '[] s
         , ValidDegree (Container c) hda
         , ValidDegree (Container c) (DegreeOf b `WithKind` DegreeKind c)
         )
       => MultiplySetters '[] '[] s a b c ('Just la) 'Nothing where
  multiplySetters set1 set2 c
    = let (ha,b) = (>!<) @(Container c) @_ @_ @hda @(DegreeOf b `WithKind` DegreeKind c) c
          a = Lens.view
                ( homogeneous @(Container c) @_ @_ @(la `WithKind` LabelKind c) )
                ha
      in set2 b . set1 a
instance ( GradedSemigroup (Container c) (DegreeKind c)
         , GradedPresentedSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , GradedFreeSemigroup (Container c) (DegreeKind c) (LabelKind c)
         , a ~ Element (Container c) (LabelKind c) (la `WithKind` LabelKind c)
         , hda ~ Degree
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (la `WithKind` LabelKind c)
         , b ~ Element (Container c) (LabelKind c) (lb `WithKind` LabelKind c)
         , hdb ~ Degree
                    (DegreeKind c)
                    (Container c)
                    (LabelKind c)
                    (lb `WithKind` LabelKind c)
         , c ~ ListVariadic '[] c
         , c ~ Apply
                  (DegreeKind c)
                  (Container c)
                  (hda :<!>: hdb)
         , s ~ ListVariadic '[] s
         , ValidDegree (Container c) hda
         , ValidDegree (Container c) hdb
         )
      => MultiplySetters '[] '[] s a b c ('Just la) ('Just lb) where
  multiplySetters set1 set2 c
    = let (ha,hb) = (>!<) @(Container c) @_ @_ @hda @hdb c
          a = Lens.view
                ( homogeneous @(Container c) @_ @_ @(la `WithKind` LabelKind c) )
                ha
          b = Lens.view
                ( homogeneous @(Container c) @_ @_ @(lb `WithKind` LabelKind c) )
                hb
      in set2 b . set1 a

instance MultiplySetters is        js        s a b c mla mlb
      => MultiplySetters (i ': is) (j ': js) s a b c mla mlb where
  multiplySetters set1 set2 (i,j)
    = multiplySetters @is @js @s @a @b @c @mla @mlb (set1 i) (set2 j)
      :: ListVariadic (ProductIndices is js :++: '[c,s]) s
instance MultiplySetters '[] js        s a b c mla mlb
      => MultiplySetters '[] (j ': js) s a b c mla mlb where
  multiplySetters set1 set2 j
    = multiplySetters @'[] @js @s @a @b @c @mla @mlb set1 (set2 j)
      :: ListVariadic (ProductIndices '[] js :++: '[c,s]) s
instance MultiplySetters is        '[] s a b c mla mlb
      => MultiplySetters (i ': is) '[] s a b c mla mlb where
  multiplySetters set1 set2 i
    = multiplySetters @is @'[] @s @a @b @c @mla @mlb (set1 i) set2
      :: ListVariadic (ProductIndices is '[] :++: '[c,s]) s

instance forall is js ks (s :: Type) a b c
               (o1 :: Optic is s a) (o2 :: Optic js s b)
               (mla :: Maybe (LabelKind (LastAccessee o1)))
               (mlb :: Maybe (LabelKind (LastAccessee o2)))
       . ( ReifiedSetter o1
         , ReifiedSetter o2
         , ks ~ ProductIndices is js
         , c ~ ProductIfDisjoint o1 o2
         , mla ~ ( If (IsProduct o1) 'Nothing ('Just (LabelOf (LastAccessee o1) (LastOptic o1))) )
         , mlb ~ ( If (IsProduct o2) 'Nothing ('Just (LabelOf (LastAccessee o2) (LastOptic o2))) )
         , MultiplySetters
            is js s a b c mla mlb
         )
      => ReifiedSetter ((o1 `ProductO` o2) :: Optic ks s c) where
  set = multiplySetters @is @js @s @a @b @c @mla @mlb (set @o1) (set @o2)

----------------------------------------------------------------------
-- equalisers

instance
  ( TypeError ( Text "get: cannot use equaliser as a getter." ) )
  => Gettable (All_ o :: Optic i s r) where


instance forall i s a r (o :: Optic i s a).
         ( Settable o
         , MonoContained a
         , r ~ MonoType a
         ) => Settable (All_ o :: Optic i s r)
        where