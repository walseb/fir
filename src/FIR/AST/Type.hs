{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.AST.Type

Augmented annotation type for the AST GADT.

Distinguishes between a partially applied constructor of the AST
and a fully applied constructor of the AST that returns a function.

Enforces a distinction between pure and stateful operations,
as necessary to be able to write an evaluator.
-}

module FIR.AST.Type
  ( AugType(..), Eff, UnderlyingType
  , FunArgs, FunRes, FunTys, MkFunType, ApplyFun
  , Nullary, MapVal
  , ApplyFAug, MapApplyFAug
  )
  where

-- base
import Data.Kind
  ( Type )

-- fir
import Control.Monad.Indexed
  ( (:=) )
import {-# SOURCE #-} FIR.ProgramState
  ( ProgramState )

------------------------------------------------------------

infixr 0 :-->
-- | Types of values the AST can hold: concrete values (pure or effectful), and functions.
data AugType
  = Val Type
  | MkEff Type ProgramState ProgramState -- last parameter must be starting indexed monadic state (McBride formalism)
  | AugType :--> AugType

type Eff i j a = MkEff a j i

-- | Return the underlying type of an augmented type.
type family UnderlyingType ( t :: AugType ) :: Type where
  UnderlyingType (Val ty) = ty
  UnderlyingType (Eff i j a) = (a := j) i
  UnderlyingType (s :--> t) = UnderlyingType s -> UnderlyingType t

class    ( as ~ FunArgs t, b ~ FunRes t, t ~ MkFunType as b, Nullary b )
      => FunTys (t :: AugType) (as :: [AugType]) (b :: AugType) | t -> as b, as b -> t
instance ( as ~ FunArgs t, b ~ FunRes t, t ~ MkFunType as b, Nullary b )
      => FunTys t as b

-- | Returns list of arguments of an augmented type.
type family FunArgs (t :: AugType) :: [AugType] where
  FunArgs (Val _) = '[]
  FunArgs (Eff _ _ _) = '[]
  FunArgs (a :--> b) = a ': FunArgs b

type family FunRes (t :: AugType) :: AugType where
  FunRes (a :--> b) = FunRes b
  FunRes b          = b
type family MkFunType (as :: [AugType]) (b :: AugType) :: AugType where
  MkFunType '[]       b = b
  MkFunType (a ': as) b = a :--> MkFunType as b

class    ( FunArgs a ~ '[], FunRes a ~ a ) => Nullary a
instance ( FunArgs a ~ '[], FunRes a ~ a ) => Nullary a

type family ApplyFun (a :: AugType) (as :: [AugType]) :: AugType where
  ApplyFun a '[] = a
  ApplyFun (a :--> b) (a ': as) = ApplyFun b as

-- | Augment a list of types of values.
type family MapVal (ts :: [Type]) = (as :: [AugType]) | as -> ts where
  MapVal '[]       = '[]
  MapVal (t ': ts) = Val t ': MapVal ts

------------------------------------------------
-- Dealing with applying/unapplying functors.

-- | Apply a functor to an augmented type.
type family ApplyFAug (f :: Type -> Type) (t :: AugType) = ( r :: AugType ) | r -> f t where
  ApplyFAug f (Val a    ) = Val (f a)
  ApplyFAug f (a :--> b ) = ApplyFAug f a :--> ApplyFAug f b
  ApplyFAug f (Eff i j a) = Eff i j (f a)

type family MapApplyFAug (f :: Type -> Type) (fas :: [AugType]) :: [AugType] where
  MapApplyFAug _ '[] = '[]
  MapApplyFAug f ( Val a ': as ) = Val (f a) ': MapApplyFAug f as
