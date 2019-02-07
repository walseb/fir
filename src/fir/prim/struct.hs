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
import Control.Applicative
  ( liftA2 )
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Data.Type.List
  ( type (:++:) )
import Data.Type.Map
  ( (:->)((:->)), Key, Value )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GeneratedGradedSemigroup(..)
  , FreeGradedSemigroup(..)
  )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( PrimTy
  , PrimTyMap(primTyMapSing)
  , SPrimTyMap(SNil, SCons)
  )

------------------------------------------------------------
-- structs

infixr 4 :&
-- order *matters* for structs (memory layout!)
data Struct :: [Symbol :-> Type] -> Type where
  End  :: Struct '[]
  (:&) :: forall k a as. a -> Struct as -> Struct ((k ':-> a) ': as)


instance PrimTyMap as => Eq (Struct as) where
  s1 == s2 = case primTyMapSing @as of
    SNil
      -> True
    SCons {}
      -> case (s1, s2) of
            (a :& as, b :& bs)
              -> a == b && as == bs

instance PrimTyMap as => Ord (Struct as) where
  compare s1 s2 = case primTyMapSing @as of
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
instance PrimTyMap as => Display (Struct as) where
  display s = case primTyMapSing @as of
    SNil
      -> ""
    SCons {}
      -> case s of
            (a :& as)
              -> case display as of
                    "" -> show a
                    d  -> show a ++ ", " ++ d

instance PrimTyMap as => Show (Struct as) where
  show s = "{ " ++ display s ++ " }" 

instance GradedSemigroup Struct [Symbol :-> Type] where
  type Apply [Symbol :-> Type] Struct as = Struct as
  type as :<!>: bs = as :++: bs
  (<!>) :: Struct as -> Struct bs -> Struct (as :++: bs)
  End <!> t = t
  (a :& s) <!> t = a :& ( s <!> t )

instance GeneratedGradedSemigroup
            Struct
            [Symbol :-> Type]
            (Symbol :-> Type)
            where
  type GenType                  Struct (Symbol :-> Type) kv = Value kv
  type GenDeg [Symbol :-> Type] Struct (Symbol :-> Type) kv = '[ kv ]
  generator :: forall (kv :: Symbol :-> Type). Value kv -> Struct '[ kv ]
  generator a = unsafeCoerce ( (a :& End) :: Struct '[ Key kv ':-> Value kv] )
                --   ^^^^   GHC cannot deduce that kv ~ Key kv ':-> Value kv

instance FreeGradedSemigroup
            Struct
            [Symbol :-> Type]
            (Symbol :-> Type)
            where
  type ValidDegree Struct as = PrimTyMap as
  (>!<) :: forall as bs. (PrimTyMap as, PrimTyMap bs)
        => Struct (as :++: bs) -> ( Struct as, Struct bs )
  (>!<) End = unsafeCoerce ( End, End )
          --    ^^^^^^   GHC cannot deduce (as ~ '[], bs ~ '[]) from (as :++: bs) ~ '[]
  (>!<) (s :& ss)
    = case primTyMapSing @as of
        SNil
          -> ( End, s :& ss )
        sCons@SCons
          -> case sCons of
                ( _ :: SPrimTyMap ((k ':-> a) ': as'))
                  -> let
                        l :: Struct as'
                        r :: Struct bs
                        (l,r) = ( (>!<) @_ @_ @_ @_ @bs ss)
                      in ( s :& l, r )
  generated :: Struct '[ kv ] -> Value kv
  generated (a :& End) = a


class FoldrStruct x where
  foldrStruct
    :: forall b.
       (forall a. PrimTy a => a -> b -> b)
    -> b -> x -> b

instance PrimTyMap as => FoldrStruct (Struct as) where
  foldrStruct f b s = case primTyMapSing @as of
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
