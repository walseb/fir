{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
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
import Data.Proxy(Proxy(Proxy))
import Data.Type.Bool(If, type (&&), Not)
import Data.Word(Word32)
import GHC.TypeLits( Symbol, KnownSymbol, symbolVal
                   , TypeError, ErrorMessage(..)
                   )
import GHC.TypeNats(Nat, KnownNat, natVal)

-- fir
import Data.Type.Map(Zip, type (:++:))

----------------------------------------------------------------------

infixr 9 :.:
infixr 9 :%.:
infixr 3 :&:
infixr 3 :%&:

-- optic data (kind)
data Optic where
  -- built-in lenses
  AnIndex :: Optic -- index not known at compile-time
  Index   :: Nat    -> Optic
  Name    :: Symbol -> Optic
  -- optic combinators
  Diag    :: Optic  -> Optic -- equaliser
  (:.:)   :: Optic  -> Optic -> Optic -- composition
  (:&:)   :: Optic  -> Optic -> Optic -- product


-- singletons for optics
data SOptic :: Optic -> Type where
  SAnIndex :: SOptic AnIndex
  SIndex   :: KnownNat n    => Proxy n -> SOptic (Index   n)
  SName    :: KnownSymbol k => Proxy k -> SOptic (Name    k)
  SDiag    :: SOptic l  -> SOptic (Diag l)
  (:%.:)   :: SOptic l1 -> SOptic l2 -> SOptic (l1 :.: l2)
  (:%&:)   :: SOptic l1 -> SOptic l2 -> SOptic (l1 :&: l2)
  

showSOptic :: SOptic optic -> String
showSOptic SAnIndex = "AnIndex"
showSOptic (SIndex n) = "Index " ++ show (natVal    n)
showSOptic (SName  k) = "Name "  ++ show (symbolVal k)
showSOptic (SDiag l) = "Diag " ++ showSOptic l
showSOptic (l1 :%.: l2) = showSOptic l1 ++ " :.: " ++ showSOptic l2
showSOptic (l1 :%&: l2) = showSOptic l1 ++ " :&: " ++ showSOptic l2


class KnownOptic optic where
  opticSing :: SOptic optic
instance KnownOptic AnIndex where
  opticSing = SAnIndex
instance KnownNat n => KnownOptic (Index n) where
  opticSing = SIndex (Proxy @n)
instance KnownSymbol k => KnownOptic (Name k) where
  opticSing = SName (Proxy @k)
instance KnownOptic opt => KnownOptic (Diag opt) where
  opticSing = SDiag (opticSing @opt)  
instance (KnownOptic opt1, KnownOptic opt2)
      => KnownOptic (opt1 :.: opt2) where
  opticSing = (opticSing @opt1) :%.: (opticSing @opt2)
instance (KnownOptic opt1, KnownOptic opt2)
      => KnownOptic (opt1 :&: opt2) where
  opticSing = (opticSing @opt1) :%&: (opticSing @opt2)


----------------------------------------------------------------------
-- dealing with additional information that needs to be passed at runtime
-- this is mostly for passing an array index at runtime,
-- but can be used to pass vector/matrix indices at runtime too

type family RequiredIndices (optic :: Optic) = (r :: [Type]) where
  RequiredIndices (Name  _)   = '[]
  RequiredIndices (Index _)   = '[]
  RequiredIndices AnIndex     = '[Word32]
  RequiredIndices (Diag l)    = RequiredIndices l
  RequiredIndices (l1 :.: l2) = RequiredIndices l1 :++: RequiredIndices l2
  RequiredIndices (l1 :&: l2)
  -- using (possibly nested) pairs... not a great solution
    = Zip (     Text "Cannot combine optics "
           :<>: ShowType l1 :<>: Text " and " :<>: ShowType l2 :<>: Text "."
           :$$: Text "These optics use different amounts of run-time indices."
          )
        (RequiredIndices l1)
        (RequiredIndices l2) 

----------------------------------------------------------------------
-- type classes

class Gettable (optic :: Optic) obj where
  type Get optic obj :: Type

class Settable (optic :: Optic) obj where
  type Set optic obj :: Type

class Container (c :: Type) where
  type Combine c (x :: Type) (y :: Type) :: Type
  type Singleton c (o :: Optic) (a :: Type) = (r :: Type) | r -> a
  type Overlapping c (k :: Symbol) (n :: Nat) :: Bool
  -- only useful for containers which can be accessed both ways

class Container c => MonoContainer c where
  type MonoType c :: Type

----------------------------------------------------------------------
-- composition

instance ( Gettable g1 a
         , Gettable g2 (Get g1 a)
         ) => Gettable (g1 :.: g2) a where
  type Get (g1 :.: g2) a = Get g2 (Get g1 a)

instance ( Settable s1 a
         , Settable s2 (Set s1 a)
         )
      => Settable (s1 :.: s2) a where
  type Set (s1 :.: s2) a = Set s2 (Set s1 a)

----------------------------------------------------------------------
-- products

type family Product 
              ( a  :: Type  )
              ( g1 :: Optic )
              ( g2 :: Optic )
              ( x  :: Type  )
              ( y  :: Type  )
            = ( r  :: Type  )
              where
  Product a (_ :&: _) (_ :&: _) x y
    = Combine a
        x
        y
  Product a g1        (_ :&: _) x y
    = Combine a
        (Singleton a g1 x)
        y 
  Product a (_ :&: _) g2        x y
    = Combine a
        x
        (Singleton a g2 y)
  Product a g1        g2        x y
    = Combine a
        (Singleton a g1 x)
        (Singleton a g2 y)

type family LastOptic (o :: Optic) :: Optic where
  LastOptic (o1 :.: o2) = LastOptic o2
  LastOptic o           = o

-- getter products
instance (Gettable g1 a, Gettable g2 a)
      => Gettable (g1 :&: g2) a where
  type Get (g1 :&: g2) a
    = Product
        (LastGettee g1 a) -- find out which product structure to use
        (LastOptic g1)    --    e.g. a product of two vector component getters 
        (LastOptic g2)    --    should return a vector
        (Get g1 a)
        (Get g2 a)

type family LastGettee (o :: Optic) (a :: k) :: l where
  LastGettee (Name  k)   a = a
  LastGettee (Index n)   a = a
  LastGettee AnIndex     a = a
  LastGettee (o  :&: _ ) a = LastGettee o a -- could pick either factor
  LastGettee (o1 :.: o2) a = LastGettee o2 (Get o1 a)
  LastGettee o           a = a

-- setter products
instance (Settable s1 a, Settable s2 a)
      => Settable (s1 :&: s2) a where
  type Set (s1 :&: s2) a
    = ProductIfDisjoint
        (LastSettee s1 a)
        (LastOptic s1)
        (LastOptic s2)
        (Set s1 a)
        (Set s2 a)

type family LastSettee (o :: Optic) (a :: k) :: l where
  LastSettee (Name  k)   a = a
  LastSettee (Index n)   a = a
  LastSettee AnIndex     a = a
  LastSettee (o  :&: _ ) a = LastSettee o a -- could pick either factor
  LastSettee (o1 :.: o2) a = LastSettee o2 (Set o1 a)
  LastSettee o           a = a

type family ProductIfDisjoint
              ( a  :: Type  )
              ( s1 :: Optic )
              ( s2 :: Optic )
              ( x  :: Type  )
              ( y  :: Type  )
            = ( r  :: Type  )
              where
  ProductIfDisjoint a s1 s2 x y
    = If
        ( Disjoint s1 s2 a )
        ( Product a s1 s2 x y)
        ( TypeError 
           ( Text "set: cannot create product setter."
            :$$: Text "Setters " :<>: ShowType s1
            :$$: Text "and "     :<>: ShowType s2
            :$$: Text "are not disjoint."
           )
        )

type family Disjoint
              ( o1 :: Optic )
              ( o2 :: Optic )
              ( a  :: l     ) -- accessee
            = ( r  :: Bool  )
              where
  Disjoint (Name  k) (Name  k) _ = False
  Disjoint (Index n) (Index n) _ = False
  Disjoint AnIndex    _        _
    = TypeError (    Text "set: cannot create a product setter involving run-time indices."
                :$$: Text "Impossible to verify the required disjointness property."
                )
  Disjoint o         AnIndex   a
    = Disjoint AnIndex o a
  Disjoint (Name  k) (Index n) a
    = Not (Overlapping a k n)
  Disjoint (Index n) (Name  k) a
    = Disjoint (Name k) (Index n) a
  Disjoint (o1 :&: o3) (o2 :&: o4) a
    =  Disjoint o1 o2 a
    && Disjoint o1 o4 a
    && Disjoint o3 o2 a
    && Disjoint o3 o4 a
  Disjoint (o1 :&: o3) o2 a
    =  Disjoint o1 o2 a
    && Disjoint o3 o2 a
  Disjoint o1 (o2 :&: o4) a
    =  Disjoint o1 o2 a
    && Disjoint o1 o4 a
  Disjoint (o1 :.: o2) (o3 :.: o4) a
    = If
        ( Disjoint o1 o3 a )
        ( Disjoint o2 o4 (Set o1 a) )
        'True
  Disjoint _ _ a = 'True

----------------------------------------------------------------------
-- equalisers

instance Gettable (Diag g) a where
  type Get (Diag g) a
    = TypeError ( Text "get: cannot use diagonal optic as a getter." )

instance (Settable s a, MonoContainer (Set s a))
        => Settable (Diag s) a
        where
  type Set (Diag s) a = MonoType (Set s a)