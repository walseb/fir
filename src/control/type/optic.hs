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
import Data.Proxy(Proxy(Proxy))
import Data.Type.Bool(If, type (&&), Not)
import Data.Word(Word32)
import GHC.TypeLits( Symbol, KnownSymbol, symbolVal
                   , TypeError, ErrorMessage(..)
                   )
import GHC.TypeNats(Nat, KnownNat, natVal)

-- fir
import Data.Type.Map(Elem, Zip, type (:++:))

----------------------------------------------------------------------

infixr 9 :.:
infixr 9 :%.:
infixr 3 :&:
infixr 3 :%&:

-- optic data (kind)
data Optic (s :: k) (a :: Type) where
  -- built-in lenses
  AnIndex :: Optic s a
  Index   :: Nat    -> Optic s a
  Name    :: Symbol -> Optic s a
  -- optic combinators
  All     :: Optic s a -> Optic s b              -- equaliser
  (:.:)   :: Optic s a -> Optic t b -> Optic q c -- composition
  (:&:)   :: Optic s a -> Optic s b -> Optic s c -- products


-- singletons for optics
{-
data SField :: Field -> Type where
  SAnIndex :: SField AnIndexField
  SIndex :: KnownNat n => Proxy n -> SField (IndexField n)
  SName :: KnownSymbol k => Proxy k -> SField (NameField k)

class KnownField f where
  fieldSing :: SField f

fieldVal :: KnownField f => Proxy f -> SField f
fieldVal _ = fieldSing

instance KnownField AnIndexField where
  fieldSing = SAnIndex
instance KnownNat n => KnownField (IndexField n) where
  fieldSing = SIndex (Proxy @n)
instance KnownSymbol k => KnownField (NameField k) where
  fieldSing = SName (Proxy @k)
-}
data SOptic :: Optic (s :: k) (a :: Type) -> Type where
  --SFromField :: (KnownField f, HasField a f)
  --           => Proxy a -> Proxy f -> SOptic (FromField f)
  SAnIndex :: SOptic AnIndex
  SIndex :: KnownNat    n => Proxy n -> SOptic (Index n)
  SName  :: KnownSymbol k => Proxy k -> SOptic (Name  k)
  SAll   :: SOptic o -> SOptic (All o)
  (:%.:) :: SOptic o1 -> SOptic o2 -> SOptic (o1 :.: o2)
  (:%&:) :: SOptic o1 -> SOptic o2 -> SOptic (o1 :&: o2)
  

showSOptic :: SOptic optic -> String
{-
showSOptic (SFromField a f)
  = case fieldVal f of
      SAnIndex -> "AnIndex"
      SIndex n -> "Index " ++ show (natVal    n)
      SName  k -> "Name "  ++ show (symbolVal k)
-}
showSOptic SAnIndex   = "AnIndex"
showSOptic (SIndex n) = "Index " ++ show (natVal    n)
showSOptic (SName  k) = "Name "  ++ show (symbolVal k)
showSOptic (SAll l) = "All " ++ showSOptic l
showSOptic (l1 :%.: l2) = showSOptic l1 ++ " :.: " ++ showSOptic l2
showSOptic (l1 :%&: l2) = showSOptic l1 ++ " :&: " ++ showSOptic l2


class KnownOptic optic where
  opticSing :: SOptic optic
{-
instance (KnownField f, HasField a f) => KnownOptic (FromField f :: Optic a) where
  opticSing = SFromField (Proxy @a) (Proxy @f)
-}
instance KnownOptic AnIndex where
  opticSing = SAnIndex
instance KnownNat n => KnownOptic (Index n) where
  opticSing = SIndex (Proxy @n)
instance KnownSymbol k => KnownOptic (Name k) where
  opticSing = SName (Proxy @k)
instance KnownOptic opt => KnownOptic (All opt) where
  opticSing = SAll (opticSing @opt)
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

type family RequiredIndices (optic :: Optic s a) = (r :: [Type]) where
  RequiredIndices (Name  _)   = '[]
  RequiredIndices (Index _)   = '[]
  RequiredIndices AnIndex     = '[Word32]
  RequiredIndices (All l)     = RequiredIndices l
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

class Gettable s (a :: Type) (optic :: Optic s a) | optic -> s a where
  type Get (optic :: Optic s a) :: Type
  type Get (optic :: Optic s a) = a

class Settable s (a :: Type) (optic :: Optic s a) | optic -> s a where
  type Set (optic :: Optic s a) :: Type
  type Set (optic :: Optic s a) = a

class Container c where
  type Combine c (x :: Type) (y :: Type) :: Type
  type Singleton c (o :: Optic s a) (x :: Type) = (r :: Type) | r -> x
  type Overlapping c (k :: Symbol) (n :: Nat) :: Bool

class Container a => MonoContainer a where
  type MonoType a

----------------------------------------------------------------------
-- composition

instance forall a b s (o1 :: Optic s a) (o2 :: Optic a b).
         ( Gettable s a o1
         , Gettable a b o2
         ) => Gettable s b ((o1 :: Optic s a) :.: (o2 :: Optic a b)) where


instance forall a b s (o1 :: Optic s a) (o2 :: Optic a b).
         ( Settable s a o1
         , Settable a b o2
         )
      => Settable s b (o1 :.: o2) where

----------------------------------------------------------------------
-- products

type family Product
              ( s  :: k         )
              ( g1 :: Optic s a )
              ( g2 :: Optic s b )
              ( x  :: Type      )
              ( y  :: Type      )
            = ( r  :: Type      )
              where
  Product s (_ :&: _) (_ :&: _) x y
    = Combine s
        x
        y
  Product s o1 (_ :&: _) x y
    = Combine s
        (Singleton s o1 x)
        y 
  Product s (_ :&: _) o2 x y
    = Combine s
        x
        (Singleton s o2 y)
  Product s o1 o2 x y
    = Combine s
        (Singleton s o1 x)
        (Singleton s o2 y)

type family LastOptic (o :: Optic s a) :: Optic t a where
  LastOptic (o1 :.: o2) = LastOptic o2
  LastOptic (All o)     = All (LastOptic o) -- TODO: this case is not being dealt with properly
  LastOptic o           = o

-- getter products
instance ( Gettable s a o1
         , Gettable s b o2
         , r ~ Product
                (LastGettee s o1) -- find out which product structure to use
                (LastOptic o1)        --    e.g. a product of two vector component getters 
                (LastOptic o2)        --    should return a vector
                (Get o1)
                (Get o2)
         ) => Gettable s r (o1 :&: o2) where

type family LastGettee (s :: k) (o :: Optic s a) :: t where
  LastGettee s (Name  k)   = s
  LastGettee s (Index n)   = s
  LastGettee s AnIndex     = s
  LastGettee s (o  :&: _ ) = LastGettee s o -- could pick either factor
  LastGettee s ((o1 :: Optic s a) :.: o2) = LastGettee a o2
  LastGettee s _           = s

-- setter products
instance ( Settable s a o1
         , Settable s b o2
         , r ~ ProductIfDisjoint
                (LastSettee s o1)
                (LastOptic o1)
                (LastOptic o2)
                (Set o1)
                (Set o2)
         ) => Settable s r (o1 :&: o2) where

type family LastSettee (s :: k) (o :: Optic s a) :: t where
  LastSettee s (Name  k)   = s
  LastSettee s (Index n)   = s
  LastSettee s AnIndex     = s
  LastSettee s (o  :&: _ ) = LastSettee s o -- could pick either factor
  LastSettee s ((o1 :: Optic s a) :.: o2) = LastSettee a o2
  LastSettee s o           = s

type family ProductIfDisjoint
              ( s  :: k         )
              ( o1 :: Optic s a )
              ( o2 :: Optic s b )
              ( x  :: Type      )
              ( y  :: Type      )
            = ( r  :: Type      )
              where
  ProductIfDisjoint s o1 o2 x y
    = If
        ( Disjoint s o1 o2 )
        ( Product s o1 o2 x y)
        ( TypeError 
           ( Text "set: cannot create product setter."
            :$$: Text "Setters " :<>: ShowType o1
            :$$: Text "and "     :<>: ShowType o2
            :$$: Text "are not disjoint."
           )
        )

type family Disjoint
              ( s  :: k         ) -- accessee
              ( o1 :: Optic s a )
              ( o2 :: Optic s b )
            = ( r  :: Bool      )
              where
  Disjoint _ (Name  k) (Name  k) = False
  Disjoint _ (Index n) (Index n) = False
  Disjoint _ AnIndex   _       
    = TypeError (    Text "set: cannot create a product setter involving run-time indices."
                :$$: Text "Impossible to verify the required disjointness property."
                )
  Disjoint s o AnIndex
    = Disjoint s AnIndex o
  Disjoint s (Name k) (Index n)
    = Not (Overlapping s k n)
  Disjoint s (Index n) (Name k)
    = Disjoint s (Name  k) (Index n)
  Disjoint s (o1 :&: o3) (o2 :&: o4)
    =  Disjoint s o1 o2
    && Disjoint s o1 o4
    && Disjoint s o3 o2
    && Disjoint s o3 o4
  Disjoint s (o1 :&: o3) o2
    =  Disjoint s o1 o2
    && Disjoint s o3 o2
  Disjoint s o1 (o2 :&: o4)
    =  Disjoint s o1 o2
    && Disjoint s o1 o4
  Disjoint s ((o1 :: Optic s a) :.: o2) (o3 :.: o4)
    = If
        ( Disjoint s o1 o3 )
        ( Disjoint a o2 o4 )
        'True
  Disjoint _ _ _ = 'True

----------------------------------------------------------------------
-- equalisers

instance
  ( TypeError ( Text "get: cannot use equaliser as a getter." ) )
  => Gettable s a (All o) where
instance 
  ( Settable s a (o :: Optic s a)
  , MonoContainer s
  , r ~ MonoType s
  ) => Settable s r (All o)
        where