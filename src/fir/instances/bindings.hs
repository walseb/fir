{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Instances.Bindings

Auxiliary helper module providing checks for stateful operations,
(such as 'FIR.Instances.Codensity.get', 'FIR.Instances.Codensity.put'),
using type families with custom type errors.

These are used, for instance, to prevent any name from being bound twice,
or accessing a binding that does not exist.

-}

module FIR.Instances.Bindings
  ( Get
  , Put
  , ValidDef
  , ValidFunDef
  , ValidEntryPoint
  )
  where

-- base
import Data.Kind(Type)
import Data.Type.Bool(If)
import GHC.TypeLits
  ( Symbol
  , TypeError
  , ErrorMessage(..)
  )

-- fir
import Data.Type.List(Elem)
import Data.Type.Map
  ( (:->)((:->))
  , Lookup, Remove
  )
import FIR.Binding
  ( Binding, BindingsMap, BindingType
  , Permission(Read,Write)
  , Var, Fun
  )
import FIR.Builtin(StageBuiltins)
import SPIRV.Stage(Stage)

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.get'

-- | Compute the type of a binding with a given name.
--
-- Returns a type error if no binding by that name exists,
-- or if the binding is not readable.
type family Get (k :: Symbol) (i :: BindingsMap) :: Type where
  Get k i = GetBinding k i (Lookup k i)

type family GetBinding (k :: Symbol) (i :: BindingsMap) (mbd :: Maybe Binding) :: Type where
  GetBinding  k i 'Nothing
   = TypeError
      (      Text "'get': no binding named " :<>: ShowType k :<>: Text " is in scope."
        :$$: Text "In-scope bindings are:"
        :$$: ShowType i
      )
  GetBinding  k _ ('Just (Var p a))
    = If 
        ( Elem 'Read p )
        a
        ( TypeError
          ( Text "'get': variable named " :<>: ShowType k :<>: Text " is not readable." )
        )
  GetBinding  _ _ ('Just bd) = BindingType bd -- functions

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.put'

-- | Compute the type of a binding with a given name.
--
-- Returns a type error if no binding by that name exists,
-- or if the binding is not readable.
type family Put (k :: Symbol) (i :: BindingsMap) :: Type where
  Put k i = PutBinding k i (Lookup k i)

type family PutBinding (k :: Symbol) (i :: BindingsMap) (lookup :: Maybe Binding) :: Type where
  PutBinding k i 'Nothing = TypeError
    (      Text "'put': no binding named " :<>: ShowType k :<>: Text " is in scope."
      :$$: Text "To bind a new variable, use 'def'."
      :$$: Text "In-scope bindings are:"
      :$$: ShowType i
    )
  PutBinding k _ ('Just (Fun as b)) = TypeError
    (      Text "'put': function bound at name "
      :<>: ShowType k :<>: Text ": "
      :<>: ShowType (Fun as b) :<>: Text "."
      :$$: Text "Use 'fundef' to define a function."
    )
  PutBinding k _ ('Just (Var ps a))
    = If
        (Elem 'Write ps)
        a
        ( TypeError 
          ( Text "'put': variable " :<>: ShowType k :<>: Text " is not writable." )
        )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.def'

-- | Check that it is valid to define a new binding with given name.
--
-- Returns a type error if a binding by this name already exists.
type family ValidDef (k :: Symbol) (i :: BindingsMap) :: Bool where
  ValidDef k i = NotAlreadyDefined k i (Lookup k i)

type family NotAlreadyDefined (k :: Symbol) (i :: BindingsMap) (lookup :: Maybe Binding) :: Bool where
  NotAlreadyDefined _ _ 'Nothing  = 'True
  NotAlreadyDefined k i ('Just _) = TypeError
    (      Text "'def': a binding by the name " :<>: ShowType k :<>: Text " already exists."
      :$$: Text "In scope bindings are:"
      :$$: ShowType i
    )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.fundef'

-- | Check that a function definition is valid.
--
-- Returns a type error if:
-- 
--   * function name is already in use,
--   * one of the function's argument is a function (higher order functions are not allowed natively)
--   * another function is defined inside the function (nested functions are not allowed natively)
type family ValidFunDef 
      ( k  :: Symbol      )  -- name of function to be defined
      ( as :: BindingsMap )  -- function arguments
      ( i  :: BindingsMap )  -- variables in scope
      ( l  :: BindingsMap )  -- @l@ contains the above three sets, together with the function's local variables
      :: Bool where          -- ( it is the total state at the end of the function definition )  
  ValidFunDef k as i l
      = NoFunctionNameConflict k as i
          ( Lookup k i  )    -- check that function name is not already in use
          ( NotHigherOrder k as (Remove i (Remove as l)) )
  --           ╱               └━━━━━━┬━━━━━━┘
  --         ╱                             │
  --        │     local variables ├━━━━━┘
  --        │
  --        └━━┤ check that none of the function arguments or local variables are themselves functions


type family NotHigherOrder (k :: Symbol) (as :: BindingsMap) (l :: BindingsMap) :: Bool where
  NotHigherOrder k as l = NotHigherOrder' k as l as l

type family NotHigherOrder'
   (k :: Symbol) (as :: BindingsMap) (l :: BindingsMap) (as_rec :: BindingsMap) (l_rec :: BindingsMap) :: Bool where
  NotHigherOrder' _ _ _ '[]                           '[]   = 'True
  NotHigherOrder' k as l ((_ ':-> Var _ _) ': as_rec) l_rec = NotHigherOrder' k as l as_rec l_rec
  NotHigherOrder' k as _ ((v ':-> Fun _ _) ': _     ) _     = TypeError
    (     Text "'fundef': forbidden higher order argument " :<>: ShowType v :<>: Text ","
     :$$: Text "in function definition for " :<>: ShowType k :<>: Text ","
     :$$: Text "when trying to abstract over:"
     :$$: ShowType as
     :$$: Text "Function definitions can only abstract over variables, not over functions."
    )
  NotHigherOrder' k as l as_rec ((_ ':-> Var _ _) ': l_rec) = NotHigherOrder' k as l as_rec l_rec
  NotHigherOrder' k _  l _      ((v ':-> Fun _ _) ': _    ) = TypeError
    (     Text "'fundef': unexpected nested function definition inside function " :<>: ShowType k :<>: Text ":"
     :$$: Text "local name " :<>: ShowType v :<>: Text " binds a function."
     :$$: Text "Local bindings for " :<>: ShowType k :<>: Text " are:"
     :$$: ShowType l
    )

type family NoFunctionNameConflict
      ( k     :: Symbol        )
      ( as    :: BindingsMap   )
      ( i     :: BindingsMap   )
      ( mb_bd :: Maybe Binding ) -- conflict with in-scope variables?
      ( notHO :: Bool          )
      :: Bool where
  NoFunctionNameConflict k as i ('Just _) _ = TypeError
    (     Text "'fundef': cannot define a new function with name " :<>: ShowType k :<>: Text ";"
     :$$: Text "that name is already in scope. In scope bindings are:"
     :$$: ShowType i
     :$$: Text "Locally bound variables are:"
     :$$: ShowType as
    )
  NoFunctionNameConflict _ _ _ 'Nothing 'True = 'True

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.entryPoint'

-- | Check that an entry-point definition is valid.
--
-- Returns a type error if:
--
--   * a function is defined within the entry-point
--   * the name of a builtin for this entry-point is already in use
type family ValidEntryPoint
              ( s :: Stage       )
              ( i :: BindingsMap )
              ( l :: BindingsMap ) 
              :: Bool where
  ValidEntryPoint s i l
    = If
        ( NoNestedFunDefs s (Remove i l) )
        ( BuiltinsDoNotAppearBefore s ( StageBuiltins s ) i )
        False

type family NoNestedFunDefs (s :: Stage) ( l :: BindingsMap) :: Bool where
  NoNestedFunDefs s l = NoNestedFunDefs' s l l

type family NoNestedFunDefs' (s :: stage) ( l :: BindingsMap ) ( l_rec :: BindingsMap ) where
  NoNestedFunDefs' _ _ '[]                         = 'True
  NoNestedFunDefs' s l ((_ ':-> Var _ _) ': l_rec) = NoNestedFunDefs' s l l_rec
  NoNestedFunDefs' s l ((v ':-> Fun _ _) ': _    ) = TypeError
    (     Text "'entryPoint': unexpected nested function definition inside " :<>: ShowType s :<>: Text " entry point:"
     :$$: Text "local name " :<>: ShowType v :<>: Text "binds a function."
     :$$: Text "Local bindings for " :<>: ShowType s :<>: Text " entry point are:"
     :$$: ShowType l
    )

type family BuiltinsDoNotAppearBefore 
              (s        :: Stage )
              (builtins :: BindingsMap )
              (i        :: BindingsMap )
              :: Bool where
  BuiltinsDoNotAppearBefore _ '[]                _  = 'True
  BuiltinsDoNotAppearBefore s ((b ':-> _) ': bs) i 
    = BuiltinDoesNotAppearBefore s b bs i (Lookup b i)

type family BuiltinDoesNotAppearBefore 
              ( s      :: Stage  )
              ( b      :: Symbol )
              ( bs     :: BindingsMap  )
              ( i      :: BindingsMap  )
              ( lookup :: Maybe Binding )
              :: Bool where
  BuiltinDoesNotAppearBefore s _ bs i 'Nothing  = BuiltinsDoNotAppearBefore s bs i
  BuiltinDoesNotAppearBefore s b _  i ('Just _)
    = TypeError (     Text "'entryPoint': conflict with built-in variable for " :<>: ShowType s :<>: Text " stage."
                 :$$: Text "Variable with name " :<>: ShowType b :<>: Text " is a built-in, yet is already bound."
                 :$$: Text "Variables in scope at site of entry point definition are:"
                 :$$: ShowType i
                )