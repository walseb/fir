{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Ambiguity

Contains some type errors that catch ambiguous type variables.

This module is currently unused, as these ambiguity checks
seem to hinder more than they help, by preventing polymorphic
metaprogramming (unless the user passes additional constraints around).

-}

module FIR.Validation.Ambiguity where

-- base
import Prelude
  hiding ( Integral, Floating )
import Data.Kind
  ( Constraint )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )

-- fir
import Control.Type.Optic
  ( Optic )
import Data.Type.Map
  ( (:->)((:->)) )
import FIR.Binding
  ( BindingsMap, Var )
import qualified SPIRV.Stage    as SPIRV
  ( ExecutionModel )

-------------------------------------------------
-- * Checking for ambiguous type variables.

data family Dummy :: k

type family Assert ( err :: Constraint ) (ambig :: k) (x :: l) :: l where
  Assert _ Dummy _ = Dummy
  Assert _  _    l = l

-- | Check that monadic state is not ambiguous.
type DefiniteState i = AssertDefiniteState i ( () :: Constraint )

type family AssertDefiniteState ( i :: BindingsMap ) (x :: l) :: l where
  AssertDefiniteState i x =
    AssertProvidedBindings
      ( TypeError
          (    Text "Ambiguous state in indexed monad."
          :$$: Text "Potential causes:"
          :$$: Text "  - lack of a type signature indicating the starting monadic state,"
          :$$: Text "  - an earlier overly general value (such as 'undefined') resulting in an ambiguous state."
          :$$: Text " "
          :$$: Text "If you are not expecting a specific state"
          :$$: Text "    ( for instance if you are writing a function which is polymorphic over monadic state )"
          :$$: Text "add the constraint 'DefiniteState " :<>: ShowType i :<>: Text "' "
          :<>: Text "to defer this error message to the use-site."
         )
      )
      i
      x

type family AssertProvidedBindings ( err :: Constraint ) ( i :: BindingsMap ) (x :: l) :: l where
  AssertProvidedBindings _   ( ("dummy" ':-> Var '[] Dummy) ': nxt) x = Dummy
  AssertProvidedBindings err ( _                            ': nxt) x = AssertProvidedBindings err nxt x
  AssertProvidedBindings _ _ x = x

-- | Check that a type-level symbol is not ambiguous.
type ProvidedSymbol k = AssertProvidedSymbol k ( () :: Constraint )
type family AssertProvidedSymbol ( k :: Symbol ) (x :: l) :: l where
  AssertProvidedSymbol k x =
    Assert
      ( TypeError
          (      Text "Ambiguous type-level symbol."
            :$$: Text "Suggestion: provide a specific name using a type application."
            :$$: Text " "
            :$$: Text "If you intend for " :<>: ShowType k :<>: Text " to remain un-instantiated,"
            :$$: Text "add the constraint 'ProvidedSymbol " :<>: ShowType k :<>: Text "' "
            :<>: Text "to defer this error message to the use-site."
          )
       )
      k
      x

-- | Check that a shader stage is not ambiguous.
type family ProvidedStage ( s :: SPIRV.ExecutionModel ) :: Constraint where
  ProvidedStage s =
    Assert
      ( TypeError
          (      Text "Ambiguous entry point stage."
            :$$: Text "Suggestion: specify the stage using a type application."
            :$$: Text " "
            :$$: Text "If you intend for " :<>: ShowType s :<>: Text " to remain un-instantiated,"
            :$$: Text "add the constraint 'ProvidedStage " :<>: ShowType s :<>: Text "' "
            :<>: Text "to defer this error message to the use-site."
          )
       )
      s
      ( () :: Constraint )

-- | Check that an optic is not ambiguous.
type family ProvidedOptic ( optic :: Optic is s a ) :: Constraint where
  ProvidedOptic (optic :: Optic is s a) =
    Assert
      ( TypeError
          (     Text "Ambiguous type-level optic."
           :$$: Text "Suggestion: provide a specific optic using a type application."
           :$$: Text "Expecting an optic:"
           :$$: Text "  - with run-time indices of types: " :<>: ShowType is
           :$$: Text "  - focusing within an object of type " :<>: ShowType s
           :$$: Text "  - onto an object of type " :<>: ShowType a
           :$$: Text " "
           :$$: Text "If " :<>: ShowType optic :<>: Text " is not intended to be a specific optic,"
           :$$: Text "for instance if you are writing an optic-polymorphic function,"
           :$$: Text "add the constraint 'ProvidedOptic " :<>: ShowType optic :<>: Text "' "
           :<>: Text "to defer this error message to the use-site."
          )
       )
      optic
      ( () :: Constraint )
