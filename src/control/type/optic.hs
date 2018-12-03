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

-- fir
import Data.Type.Map(Zip, type (:++:))
import Data.Function.Variadic(ListVariadic)
import Math.Algebra.GradedSemigroup ( GradedSemigroup(..)
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
type (:*:) (o1 :: Optic is s a) (o2 :: Optic js s b)
  = ( (o1 `ProductO` o2)
        :: Optic
              ( Zip
                  (      Text "Cannot create product optic: \
                              \different amounts of runtime indices."
                    :$$: Text "First factor indices: "  :<>: ShowType is
                    :$$: Text "Second factor indices: " :<>: ShowType js
                  )
                  is
                  js
              )
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

instance forall s is js ks a b (o1 :: Optic is s a) (o2 :: Optic js a b).
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

instance forall s is js ks a b (o1 :: Optic is s a) (o2 :: Optic js a b).
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
instance forall empty i s a b r (o1 :: Optic i s a) (o2 :: Optic i s b).
         ( Gettable o1
         , Gettable o2
         , r ~ Product o1 o2
         , empty ~ '[]
         ) => Gettable ((o1 `ProductO` o2) :: Optic empty s r) where

-- setter products
instance forall empty i s a b r (o1 :: Optic i s a) (o2 :: Optic i s b) .
         ( Settable o1
         , Settable o2
         , r ~ ProductIfDisjoint o1 o2
         , empty ~ '[]
         ) => Settable ((o1 `ProductO` o2) :: Optic empty s r) where

type family IsProduct (o :: Optic is s a) :: Bool where
  IsProduct (_ `ProductO` _) = True
  IsProduct (_ `ComposeO` o) = IsProduct o
  IsProduct o                = False

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
        ( ContainerKind s ) ( ContainerKind a ) ( ContainerKind t ) ( ContainerKind b )
        ( Container s ) ( Container a ) ( Container t ) ( Container b )
        ( DegreeKind s ) ( DegreeKind a ) ( DegreeKind t ) ( DegreeKind b )
        ( DegreeAt (DegreeKind s) (Container s) (LabelOf s o1) )
        ( DegreeOf a )
        ( DegreeAt (DegreeKind t) (Container t) (LabelOf t o2) )
        ( DegreeOf b )

-- sorry about this
type family Combine
              ( o1 :: Optic is s a) ( o2 :: Optic js t b )
              ( b1 :: Bool ) ( b2 :: Bool )
              ck1 ck2 ck3 ck4
              ( c1 :: ck1 ) ( c2 :: ck2 ) ( c3 :: ck3 ) ( c4 :: ck4 )
              kha kva khb kvb
              ( ha :: kha ) ( va :: kva ) ( hb :: khb ) ( vb :: kvb )
            = ( r :: Type )
              where
  Combine o1 o2 True True _ ck _ ck _ c _ c _ k _ k _ (va :: k) _ (vb :: k)
    = Apply k c ( va :<!>: vb )
  Combine o1 o2 True False _ ck ck _ _ c c _ _ k k _ _ (va :: k) (hb :: k) _
    = Apply k c ( va :<!>: hb )
  Combine o1 o2 False True ck _ _ ck c _ _ c k _ _ k (ha :: k) _ _ (vb :: k)
    = Apply k c ( ha :<!>: vb )
  Combine o1 o2 False False ck _ ck _ c _ c _ k _ k _ (ha :: k) _ (hb :: k) _
    = Apply k c ( ha :<!>: hb )
  Combine o1 o2 _ _ _ _ _ _ c1 c2 c3 c4 _ _ _ _ _ _ _ _
    = TypeError (     Text "Cannot create product optic: incompatible containers."
                 :$$: Text "LHS containers: " :<>: ShowType c1 :<>: Text " and " :<>: ShowType c2
                 :$$: Text "RHS containers: " :<>: ShowType c3 :<>: Text " and " :<>: ShowType c4
                 :$$: Text "When creating product of LHS optic"
                 :$$: ShowType o1
                 :$$: Text "and RHS optic"
                 :$$: ShowType o2
                )
  Combine o1 o2 _ _ _ _ _ _ _ _ _ _ k1 k2 k3 k4 _ _ _ _
    = TypeError (     Text "Cannot create product optic: incompatible gradings."
                 :$$: Text "LHS gradings: " :<>: ShowType k1 :<>: Text " and " :<>: ShowType k2
                 :$$: Text "RHS gradings: " :<>: ShowType k3 :<>: Text " and " :<>: ShowType k4
                 :$$: Text "When creating product of LHS optic"
                 :$$: ShowType o1
                 :$$: Text "and RHS optic"
                 :$$: ShowType o2
                )

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
            :$$: Text "Setters " :$$: Text "  " :<>: ShowType o1
            :$$: Text "and "     :$$: Text "  " :<>: ShowType o2
            :$$: Text "are not disjoint."
           )
        )

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