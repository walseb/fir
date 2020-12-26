{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE TypeOperators          #-}

module Control.Type.Optic
  ( Optic
  , Whole, Part, Indices
  , IndexInfo(..), IndexChain
  , Gettable, ReifiedGetter
  , Settable, ReifiedSetter
  ) where

-- base
import Data.Kind
  ( Type )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Function.Variadic
  ( ListVariadic )
import Data.Type.List
  ( Postpend )

------------------------------------------------------------

data Optic (is :: [Type]) (s :: k) (a :: Type)
type role Optic phantom phantom phantom

type Whole   (optic :: Optic is s a) = s
type Part    (optic :: Optic is s a) = a
type Indices (optic :: Optic is s a) = is

data IndexInfo
  = ThisIndex Nat
  | AnyIndex

type IndexChain = [IndexInfo]

class Gettable (optic :: Optic (is :: [Type]) (s :: k) a) | optic -> is k s a
type  Getter (optic :: Optic is (s :: Type) a) = ListVariadic (is `Postpend` s) a
class Gettable optic => ReifiedGetter optic where
  view :: Getter optic

class Settable (optic :: Optic is (s :: k) a) | optic -> is k s a
type  Setter (optic :: Optic is (s :: Type) a) = ListVariadic (is `Postpend` a `Postpend` s) s
class Settable optic => ReifiedSetter optic where
  set :: Setter optic
