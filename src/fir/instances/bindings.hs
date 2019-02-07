{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE ConstraintKinds      #-}
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
  , LookupImageProperties
  , ValidImageSample
  , ValidImageRead
  , ValidImageWrite
  )
  where

-- base
import Prelude
  hiding ( Integral, Floating )
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If )
import GHC.TypeLits
  ( Symbol
  , TypeError
  , ErrorMessage(..)
  )

-- fir
import Data.Type.List
  ( Elem )
import Data.Type.Map
  ( (:->)((:->))
  , Lookup, Remove
  )
import Math.Algebra.Class
  ( Integral, Floating )
import FIR.Binding
  ( Binding(EntryPoint), BindingsMap, FunctionType
  , Permission(Read,Write)
  , Var, Fun
  )
import FIR.Prim.Image
  ( ImageProperties(Properties), Image )
import FIR.Builtin
  ( StageBuiltins )
import SPIRV.Image
  ( ImageUsage(Sampled, Storage), SamplingMethod )
import SPIRV.Stage
  ( Stage )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.get'

-- | Compute the type of a binding with a given name.
--
-- Returns a type error if no binding by that name exists,
-- or if the binding is not readable.
type Get (k :: Symbol) (i :: BindingsMap) = ( GetBinding k i (Lookup k i) :: Type )

type family GetBinding (k :: Symbol) (i :: BindingsMap) (mbd :: Maybe Binding) :: Type where
  GetBinding k i 'Nothing
   = TypeError
      (      Text "'get': no binding named " :<>: ShowType k :<>: Text " is in scope."
        :$$: Text "In-scope bindings are:"
        :$$: ShowType i
      )
  PutBinding k _ ('Just (Var _ (Image _)))
    = TypeError 
          (     Text "'get': variable named " :<>: ShowType k :<>: Text " refers to an image."
           :$$: Text "To access image data, use 'sample' or 'imageRead'."
          )
  GetBinding k _ ('Just (Var perms a))
    = If 
        ( Elem 'Read perms )
        a
        ( TypeError
          ( Text "'get': variable named " :<>: ShowType k :<>: Text " is not readable." )
        )
  GetBinding _ _ ('Just (Fun as b)) = FunctionType as b
  GetBinding k _ ('Just ('EntryPoint _))
    = TypeError (     Text "'get': Entry point bound by name " :<>: ShowType k :<>: Text "."
                 :$$: Text "Entry points cannot be called, only defined."
                )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.put'

-- | Compute the type of a binding with a given name.
--
-- Returns a type error if no binding by that name exists,
-- or if the binding is not readable.
type Put (k :: Symbol) (i :: BindingsMap) = ( PutBinding k i (Lookup k i) :: Type )

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
  PutBinding k _ ('Just (Var _ (Image _)))
    = TypeError 
          (     Text "'put': image bound by name " :<>: ShowType k :<>: Text "."
           :$$: Text "To write to a storage image, use 'imageWrite'."
          )
  PutBinding k _ ('Just (Var perms a))
    = If
        (Elem 'Write perms)
        a
        ( TypeError 
          ( Text "'put': variable " :<>: ShowType k :<>: Text " is not writable." )
        )
  PutBinding k _ ('Just ('EntryPoint _)) = TypeError
    (      Text "'put': entry point bound at name "
      :<>: ShowType k :<>: Text ". "
      :$$: Text "Use 'entryPoint' to define a new entry point."
    )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.def'

-- | Check that it is valid to define a new binding with given name.
--
-- Returns a type error if a binding by this name already exists.
type ValidDef (k :: Symbol) (i :: BindingsMap)
  = ( NotAlreadyDefined k i (Lookup k i) :: Constraint )

type family NotAlreadyDefined (k :: Symbol) (i :: BindingsMap) (lookup :: Maybe Binding) :: Constraint where
  NotAlreadyDefined _ _ 'Nothing  = ()
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
type ValidFunDef 
      ( k  :: Symbol      )  -- name of function to be defined
      ( as :: BindingsMap )  -- function arguments
      ( i  :: BindingsMap )  -- variables in scope
      ( l  :: BindingsMap )  -- @l@ contains the above three sets, together with the function's local variables
                             -- ( it is the total state at the end of the function definition )  
 = ( ( NoFunctionNameConflict k as i ( Lookup k i ) -- check that function name is not already in use
     ,            ValidArguments k as (Remove i (Remove as l)) 
     ) :: Constraint ) -- ╱           └━━━━━━┬━━━━━┘
               --       ╱                         │
               --       │                          │
               --       │     local variables ├━━┘
               --       │
               --       └━━┤ check that none of the function arguments or local variables are themselves functions or images


type ValidArguments (k :: Symbol) (as :: BindingsMap) (l :: BindingsMap)
  = ( ValidArguments' k as l as l :: Constraint )

type family ValidArguments'
              ( k      :: Symbol      )
              ( as     :: BindingsMap )
              ( l      :: BindingsMap )
              ( as_rec :: BindingsMap )
              ( l_rec  :: BindingsMap )
              :: Constraint where
  ValidArguments' _ _ _ '[]                           '[] = ()
  ValidArguments' k as _ ((v ':-> Fun _ _) ': _     ) _   = TypeError
    (     Text "'fundef': forbidden higher order argument " :<>: ShowType v :<>: Text ","
     :$$: Text "in function definition for " :<>: ShowType k :<>: Text ","
     :$$: Text "when trying to abstract over:"
     :$$: ShowType as
     :$$: Text "Function definitions can only abstract over variables, not over functions."
    )
  ValidArguments' k as _ ((v ':-> EntryPoint _) ': _     ) _   = TypeError
    (     Text "'fundef': function argument " :<>: ShowType v
     :<>: Text "denotes an entry point; expecting a variable."
     :$$: Text "In function definition for " :<>: ShowType k :<>: Text ","
     :$$: Text "when trying to abstract over:"
     :$$: ShowType as
    )
  ValidArguments' k as l (_ ': as_rec) l_rec = ValidArguments' k as l as_rec l_rec
  ValidArguments' k _  l _ ((v ':-> Fun _ _) ': _    ) = TypeError
    (     Text "'fundef': unexpected nested function definition inside function " :<>: ShowType k :<>: Text ":"
     :$$: Text "local name " :<>: ShowType v :<>: Text " binds a function."
     :$$: Text "Local bindings for " :<>: ShowType k :<>: Text " are:"
     :$$: ShowType l
    )
  ValidArguments' k _  l _ ((v ':-> EntryPoint _) ': _    ) = TypeError
    (     Text "'fundef': unexpected entry point defined within function " :<>: ShowType k :<>: Text ":"
     :$$: Text "local name " :<>: ShowType v :<>: Text " binds an entry point."
     :$$: Text "Local bindings for " :<>: ShowType k :<>: Text " are:"
     :$$: ShowType l
    )
  ValidArguments' k as l as_rec (_ ': l_rec) = ValidArguments' k as l as_rec l_rec

type family NoFunctionNameConflict
      ( k     :: Symbol        )
      ( as    :: BindingsMap   )
      ( i     :: BindingsMap   )
      ( mb_bd :: Maybe Binding ) -- conflict with in-scope variables?
      :: Constraint where
  NoFunctionNameConflict k as i ('Just _) = TypeError
    (     Text "'fundef': cannot define a new function with name " :<>: ShowType k :<>: Text ";"
     :$$: Text "that name is already in scope. In scope bindings are:"
     :$$: ShowType i
     :$$: Text "Locally bound variables are:"
     :$$: ShowType as
    )
  NoFunctionNameConflict _ _ _ 'Nothing = ()

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.entryPoint'

-- | Check that an entry point definition is valid.
--
-- Returns a type error if:
--
--   * a function is defined within the entry point
--   * the name of a builtin for this entry point is already in use
type ValidEntryPoint
              ( s :: Stage       )
              ( i :: BindingsMap )
              ( l :: BindingsMap ) 
  = ( ( ValidLocalBehaviour s (Remove i l)
      , BuiltinsDoNotAppearBefore s ( StageBuiltins s ) i
      ) :: Constraint
    )

type ValidLocalBehaviour (s :: Stage) ( l :: BindingsMap)
  = ( ValidLocalBehaviour' s l l :: Constraint )

type family ValidLocalBehaviour'
              ( s     :: stage       )
              ( l     :: BindingsMap )
              ( l_rec :: BindingsMap )
              :: Constraint where
  ValidLocalBehaviour' _ _ '[]                         = ()
  ValidLocalBehaviour' s l ((_ ':-> Var _ _) ': l_rec) = ValidLocalBehaviour' s l l_rec
  ValidLocalBehaviour' s l ((v ':-> Fun _ _) ': _    ) = TypeError
    (     Text "'entryPoint': unexpected nested function definition inside " :<>: ShowType s :<>: Text " entry point:"
     :$$: Text "local name " :<>: ShowType v :<>: Text "binds a function."
     :$$: Text "Local bindings for " :<>: ShowType s :<>: Text " entry point are:"
     :$$: ShowType l
    )
  ValidLocalBehaviour' s l ((v ':-> EntryPoint _) ': _    ) = TypeError
    (     Text "'entryPoint': unexpected entry point definition inside " :<>: ShowType s :<>: Text " entry point:"
     :$$: Text "local name " :<>: ShowType v :<>: Text "binds an entry point."
     :$$: Text "Local bindings for " :<>: ShowType s :<>: Text " entry point are:"
     :$$: ShowType l
    )

type family BuiltinsDoNotAppearBefore
              (s        :: Stage       )
              (builtins :: BindingsMap )
              (i        :: BindingsMap )
              :: Constraint where
  BuiltinsDoNotAppearBefore _ '[]                _  = ()
  BuiltinsDoNotAppearBefore s ((b ':-> _) ': bs) i 
    = BuiltinDoesNotAppearBefore s b bs i (Lookup b i)

type family BuiltinDoesNotAppearBefore
              ( s      :: Stage         )
              ( b      :: Symbol        )
              ( bs     :: BindingsMap   )
              ( i      :: BindingsMap   )
              ( lookup :: Maybe Binding )
              :: Constraint where
  BuiltinDoesNotAppearBefore s _ bs i 'Nothing  = BuiltinsDoNotAppearBefore s bs i
  BuiltinDoesNotAppearBefore s b _  i ('Just _)
    = TypeError (     Text "'entryPoint': conflict with built-in variable for " :<>: ShowType s :<>: Text " stage."
                 :$$: Text "Variable with name " :<>: ShowType b :<>: Text " is a built-in, yet is already bound."
                 :$$: Text "Variables in scope at site of entry point definition are:"
                 :$$: ShowType i
                )

-------------------------------------------------
-- * Constraints for images.

-- | Retrieve the properties of an image.
type LookupImageProperties k i
  = ( ImagePropertiesFromLookup k i (Lookup k i) :: ImageProperties )

type family ImagePropertiesFromLookup
              ( k      :: Symbol        )
              ( i      :: BindingsMap   )
              ( lookup :: Maybe Binding )
            :: ImageProperties
              where
  ImagePropertiesFromLookup _ _ (Just (Var _ (Image props))) = props
  ImagePropertiesFromLookup k i  Nothing
    = TypeError (     Text "Expected an image\
                           \but nothing is bound by name " :<>: ShowType k
                )
  ImagePropertiesFromLookup k i (Just nonImage)
    = TypeError (     Text "Unexpected type " :<>: ShowType nonImage
                 :<>: Text " bound by name " :<>: ShowType k
                 :$$: Text "Expected an image."
                 )

-- | Check that we can call 'FIR.Instances.Codensity.sample'.
type family ValidImageSample
              ( meth  :: Maybe SamplingMethod )
              ( props :: ImageProperties      )
            :: Constraint where
  ValidImageSample _ ( Properties _ _ _ _ _ _ Storage _ )
    = TypeError (      Text "'sample': cannot sample a storage image."
                  :$$: Text "Image data must be accessed directly."
                 )
  ValidImageSample (Just _) ( Properties a _ _ _ _ _ _ _ )
    = Floating a -- accessing with a sampler: must use floating-point coordinates
  ValidImageSample Nothing ( Properties a _ _ _ _ _ _ _ )
    = Integral a -- accessing without a sampler: must use integral coordinates

-- | Check that we can call 'FIR.Instances.Codensity.imageRead'.
type family ValidImageRead (props :: ImageProperties) :: Constraint where
  ValidImageRead ( Properties _ _ _ _ _ _ Sampled _ )
    = TypeError (      Text "'imageRead': cannot directly access sampling image."
                  :$$: Text "Image data must be accessed using a sampler."
                 )
  ValidImageRead ( Properties a _ _ _ _ _ _ _ ) = Integral a

-- | Check that we can call 'FIR.Instances.Codensity.imageWrite'.
type family ValidImageWrite (props :: ImageProperties) :: Constraint where
  ValidImageWrite ( Properties _ _ _ _ _ _ Sampled _ )
    = TypeError ( Text "'imageWrite': Cannot write to a sampling image.")
  ValidImageWrite ( Properties a _ _ _ _ _ _ _ ) = Integral a


type family ValidAccessCoordinates
              ( meth  :: Maybe SamplingMethod )
              ( props :: ImageProperties      )
            :: Constraint where
  ValidAccessCoordinates
    Nothing
    ( Properties a _ _ _ _ _ _ _ )
      = Integral a -- accessing without a sampler: must use integral coordinates
  ValidAccessCoordinates
    (Just _)
    ( Properties a _ _ _ _ _ _ _ )
      = Floating a 
