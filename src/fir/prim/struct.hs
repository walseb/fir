{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module FIR.Prim.Struct where

-- base
import Control.Applicative(liftA2)
import Control.Arrow(first)
import Data.Kind(Type)
import GHC.TypeLits(Symbol)
import Unsafe.Coerce(unsafeCoerce)

-- fir
import Data.ProxyProxy(asProxyProxyTypeOf)
import Data.Type.Map((:->)((:->)), type (:++:), Key, Value)
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GradedPresentedSemigroup(..)
  , GradedFreeSemigroup(..)
  )
import {-# SOURCE #-} FIR.Prim.Singletons(PrimTy, SPrimTys(SNil, SCons), PrimTys(primTysSing))

------------------------------------------------------------
-- structs

infixr 4 :&
-- order *matters* for structs (memory layout!)
data Struct :: [Symbol :-> Type] -> Type where
  End  :: Struct '[]
  (:&) :: forall k a as. a -> Struct as -> Struct ((k ':-> a) ': as)


instance PrimTys as => Eq (Struct as) where
  s1 == s2 = case primTysSing @as of
    SNil
      -> True
    SCons {}
      -> case (s1, s2) of
            (a :& as, b :& bs)
              -> a == b && as == bs

instance PrimTys as => Ord (Struct as) where
  compare s1 s2 = case primTysSing @as of
    SNil
      -> EQ
    SCons {}
      -> case (s1, s2) of
            (a :& as, b :& bs)
              -> case compare a b of
                    EQ -> compare as bs
                    un -> un

class Display as where
  display :: as -> String
instance PrimTys as => Display (Struct as) where
  display s = case primTysSing @as of
    SNil
      -> ""
    SCons {}
      -> case s of
            (a :& as)
              -> case display as of
                    "" -> show a
                    d  -> show a ++ ", " ++ d

instance PrimTys as => Show (Struct as) where
  show s = "{ " ++ display s ++ " }" 

instance GradedSemigroup Struct [Symbol :-> Type] where
  type Apply [Symbol :-> Type] Struct as = Struct as
  type as :<!>: bs = as :++: bs
  (<!>) :: Struct as -> Struct bs -> Struct (as :++: bs)
  End <!> t = t
  (a :& s) <!> t = a :& ( s <!> t )

instance GradedPresentedSemigroup
            Struct
            [Symbol :-> Type]
            (Symbol :-> Type)
            where
  type Element                  Struct (Symbol :-> Type) kv = Value kv
  type Degree [Symbol :-> Type] Struct (Symbol :-> Type) kv = '[ kv ]
  homogeneous :: forall (kv :: Symbol :-> Type). Value kv -> Struct '[ kv ]
  homogeneous a = unsafeCoerce ( (a :& End) :: Struct '[ Key kv ':-> Value kv])

instance GradedFreeSemigroup
            Struct
            [Symbol :-> Type]
            (Symbol :-> Type)
            where
  type ValidDegree Struct as = PrimTys as
  (>!<) :: forall as bs. (PrimTys as, PrimTys bs)
        => Struct (as :++: bs) -> ( Struct as, Struct bs )
  (>!<) End = unsafeCoerce ( End, End ) -- GHC cannot deduce (as ~ '[], bs ~ '[]) from (as :++: bs) ~ '[]
  (>!<) (s :& ss)
    = case primTysSing @as of
        SNil
          -> ( End, s :& ss )
        SCons _ _ nxt
          ->  let
                r :: Struct bs
                -- workaround to specify the type of l
                (l,r) = first (`asProxyProxyTypeOf` nxt)
                          ( (>!<) @_ @_ @_ @_ @bs ss)
              in ( s :& l, r )
  generator :: Struct '[ kv ] -> Value kv
  generator (a :& End) = a


class FoldrStruct x where
  foldrStruct
    :: forall b.
       (forall a. PrimTy a => a -> b -> b)
    -> b -> x -> b

instance PrimTys as => FoldrStruct (Struct as) where
  foldrStruct f b s = case primTysSing @as of
    SNil
      -> b
    SCons {}
      -> case s of
          (a :& as)
            -> f a (foldrStruct f b as)

traverseStruct :: (Applicative f, FoldrStruct (Struct as)) =>
  ( forall a. PrimTy a => a -> f b )
  -> Struct as
  -> f [b]
traverseStruct f
   = foldrStruct ( \a bs -> liftA2 (:) (f a) bs )
       ( pure [] )