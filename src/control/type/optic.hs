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
import Data.Type.Map(type (:++:))
import Data.Function.Variadic(ListVariadic)

----------------------------------------------------------------------

infixr 9 :.:
infixr 3 :&:

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
type Name k = (Name_ k :: Optic '[] s a)
type Index i = (Index_ i :: Optic '[] s a)
type AnIndex ix = (AnIndex_ :: Optic '[ix] s a)
type (:&:) (o1 :: Optic '[] s a) (o2 :: Optic '[] s b)
  = ( (o1 `ProductO` o2)
        :: Optic '[] s ( Product o1 o2 )
    )
type (:.:) (o1 :: Optic is s a) (o2 :: Optic js a b)
  = ((o1 `ComposeO` o2) :: Optic (is :++: js) s b)
type All (o :: Optic is s a) = (All_ o :: Optic is s (MonoType a))

----------------------------------------------------------------------
-- type classes and synonyms

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

class Container c where
  type Combine c (x :: Type) (y :: Type) :: Type
  type Singleton c (o :: Optic is s a) (x :: Type) = (r :: Type) | r -> x
  type Overlapping c (k :: Symbol) (n :: Nat) :: Bool

class Container a => MonoContainer a where
  type MonoType a

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


type family Product
              ( o1 :: Optic i s a )
              ( o2 :: Optic i s b )
            = ( r  :: Type        )
              where
  Product (o1 :: Optic i s a) (o2 :: Optic i s b)
    = Combine (LastAccessee o2) -- hackish
        ( If (IsProduct o1) a (Singleton (LastAccessee o1) o1 a) )
        ( If (IsProduct o2) b (Singleton (LastAccessee o2) o2 b) )

type family IsProduct (o :: Optic i s a) :: Bool where
  IsProduct (_ `ProductO` _) = True
  IsProduct (_ `ComposeO` o) = IsProduct o
  IsProduct o                = False

type family LastAccessee (o :: Optic i (s :: Type) a) :: Type where
  LastAccessee (_ `ComposeO` o) = LastAccessee o
  LastAccessee (_ `ProductO` o) = LastAccessee o -- hack
  LastAccessee (o :: Optic i s a) = s



type family ProductIfDisjoint
              ( o1 :: Optic i s a )
              ( o2 :: Optic i s b )
            = ( r  :: Type        )
              where
  ProductIfDisjoint o1 o2
    = If
        ( Disjoint o1 o2 )
        ( Product  o1 o2 )
        ( TypeError 
           ( Text "set: cannot create product setter."
            :$$: Text "Setters " :<>: ShowType o1
            :$$: Text "and "     :<>: ShowType o2
            :$$: Text "are not disjoint."
           )
        )

type family Disjoint
              ( o1 :: Optic i s a )
              ( o2 :: Optic j t b )
            = ( r  :: Bool        )
              where
  Disjoint (Name_  k) (Name_  k) = False
  Disjoint (Index_ n) (Index_ n) = False
  Disjoint AnIndex_   _       
    = TypeError (    Text "set: cannot create a product setter involving run-time indices."
                :$$: Text "Impossible to verify the required disjointness property."
                )
  Disjoint o AnIndex_
    = Disjoint AnIndex_ o
  Disjoint ((Name_ k) :: Optic i s a) ((Index_ n) :: Optic j s b)
    = Not (Overlapping s k n)
  Disjoint ((Index_ n) :: Optic i s a) ((Name_ k) :: Optic j s b)
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
  Disjoint ((o1 :: Optic i s a) `ComposeO` o2)
           ((o3 :: Optic i t b) `ComposeO` o4)
    = If
        ( Disjoint o1 o3 )
        ( Disjoint o2 o4 )
        'True
  Disjoint _ _ = 'True

----------------------------------------------------------------------
-- equalisers


instance
  ( TypeError ( Text "get: cannot use equaliser as a getter." ) )
  => Gettable (All_ o :: Optic i s r) where


instance forall i s a r (o :: Optic i s a).
         ( Settable o
         , MonoContainer a
         , r ~ MonoType a
         ) => Settable (All_ o :: Optic i s r)
        where