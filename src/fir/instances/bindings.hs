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
  ( Get, Put
  , ValidDef
  , ValidFunDef
  , ValidEntryPoint
  , LookupImageProperties
  , ValidImageRead
  , ValidImageWrite
  , DefiniteState
  , ProvidedSymbol, ProvidedStage, ProvidedOptic
  )
  where

-- base
import Prelude
  hiding ( Integral, Floating )
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( Symbol
  , TypeError
  , ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Control.Type.Optic
  ( Optic )
import Data.Type.List
  ( Elem )
import Data.Type.Map
  ( (:->)((:->))
  , Lookup, Remove
  )
import FIR.Binding
  ( Binding(EntryPoint), BindingsMap, FunctionType
  , Permission(Read,Write)
  , Var, Fun
  )
import FIR.Builtin
  ( StageBuiltins )
import FIR.Prim.Image
  ( ImageProperties(Properties), Image
  , OperandName(DepthComparison, BaseOperand)
  )
import FIR.Prim.Singletons
  ( Integrality )
import SPIRV.Image
  ( ImageUsage(Sampled, Storage)
  , ImageFormat(ImageFormat), RequiredFormatUsage
  , Normalisation(..)
  , HasDepth(..)
  , MultiSampling(..)
  , Operand(LODOperand)
  )
import qualified SPIRV.Image    as Image
  ( Component(Integer, Floating)
  , Operand(..)
  )
import qualified SPIRV.ScalarTy as SPIRV
  ( ScalarTy(Integer, Floating) )
import SPIRV.Stage
  ( Stage )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.get' and 'FIR.Instances.Codensity.use'

-- | Compute the type of a variable bound by a given name.
--
-- Throws a type error if no variable by that name exists,
-- or if the variable is not readable.
type family Get (k :: Symbol) (i :: BindingsMap) :: Type where
  Get k i = ( AssertProvidedSymbol k (GetBinding k (Lookup k i) ) )

type family GetBinding (k :: Symbol) (mbd :: Maybe Binding) :: Type where
  GetBinding k 'Nothing
   = TypeError
      (  Text "'get'/'use': no binding named " :<>: ShowType k :<>: Text " is in scope." )
  PutBinding k ('Just (Var _ (Image _)))
    = TypeError 
          (     Text "'get'/'use': variable named " :<>: ShowType k :<>: Text " refers to an image."
           :$$: Text "To access image data, use the 'ImageTexel' optic or the 'imageRead' function."
          )
  GetBinding k ('Just (Var perms a))
    = If 
        ( Elem 'Read perms )
        a
        ( TypeError
          ( Text "'get'/'use': variable named " :<>: ShowType k :<>: Text " is not readable." )
        )
  GetBinding _ ('Just (Fun as b)) = FunctionType as b
  GetBinding k ('Just ('EntryPoint _))
    = TypeError (     Text "'get'/'use': an entry point is bound by name " :<>: ShowType k :<>: Text "."
                 :$$: Text "Entry points cannot be called, only defined."
                )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.put' and 'FIR.Instances.Codensity.assign'

-- | Compute the type of a variable bound by a given name.
--
-- Throws a type error if no variable is bound by that name,
-- or if the variable is not writable.
type family Put (k :: Symbol) (i :: BindingsMap) :: Type where
  Put k i = ( AssertProvidedSymbol k (PutBinding k (Lookup k i) ) )

type family PutBinding (k :: Symbol) (lookup :: Maybe Binding) :: Type where
  PutBinding k 'Nothing = TypeError
    (      Text "'put'/'assign': no binding named " :<>: ShowType k :<>: Text " is in scope."
      :$$: Text "To bind a new variable, use 'def'."
    )
  PutBinding k ('Just (Fun as b)) = TypeError
    (      Text "'put'/'assign': function bound at name "
      :<>: ShowType k :<>: Text ": "
      :<>: ShowType (Fun as b) :<>: Text "."
      :$$: Text "Use 'fundef' to define a function."
    )
  PutBinding k ('Just (Var _ (Image _)))
    = TypeError 
          (     Text "'put'/'assign': image bound by name " :<>: ShowType k :<>: Text "."
           :$$: Text "To write to a storage image, assign with the 'ImageTexel' optic or use 'imageWrite'."
          )
  PutBinding k ('Just (Var perms a))
    = If
        (Elem 'Write perms)
        a
        ( TypeError 
          ( Text "'put'/'assign': variable " :<>: ShowType k :<>: Text " is not writable." )
        )
  PutBinding k ('Just ('EntryPoint _)) = TypeError
    (      Text "'put'/'assign': entry point bound at name "
      :<>: ShowType k :<>: Text ". "
      :$$: Text "Use 'entryPoint' to define a new entry point."
    )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.def'

-- | Check that it is valid to define a new variable with given name.
--
-- Throws a type error if a binding by this name already exists.
type family ValidDef (k :: Symbol) (i :: BindingsMap) :: Constraint where
  ValidDef k i
    = ( ProvidedSymbol k
      , NotAlreadyDefined k (Lookup k i)
      )

type family NotAlreadyDefined (k :: Symbol) (lookup :: Maybe Binding) :: Constraint where
  NotAlreadyDefined _ 'Nothing  = ()
  NotAlreadyDefined k ('Just _) = TypeError
    ( Text "'def': a binding by the name " :<>: ShowType k :<>: Text " already exists." )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.fundef'

-- | Check that a function definition is valid.
--
-- Throws a type error if:
-- 
--   * function name is already in use,
--   * one of the function's arguments is itself a function,
--   * another function is defined inside the function.
type family ValidFunDef 
      ( k  :: Symbol      )  -- name of function to be defined
      ( as :: BindingsMap )  -- function arguments
      ( i  :: BindingsMap )  -- variables in scope
      ( l  :: BindingsMap )  -- @l@ contains the above three sets, together with the function's local variables
    :: Constraint where      -- ( it is the total state at the end of the function definition )  
  ValidFunDef k as i l
    = ( ProvidedSymbol k
      , NoFunctionNameConflict k ( Lookup k i ) -- check that function name is not already in use
      , ValidArguments k as (Remove i (Remove as l))
      ) --     │             └━━━━━━┬━━━━━┘
        --     │                         │
        --     │                         │
        --     │    local variables ├━━┘
        --     │
        --     └━━┤ check that none of the function arguments or local variables are themselves functions or images


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
      ( mb_bd :: Maybe Binding ) -- conflict with in-scope variables?
      :: Constraint where
  NoFunctionNameConflict k ('Just _) = TypeError
    (     Text "'fundef': cannot define a new function with name " :<>: ShowType k :<>: Text "."
     :$$: Text "that name is already in scope."
    )
  NoFunctionNameConflict _ 'Nothing = ()

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.entryPoint'

-- | Check that an entry point definition is valid.
--
-- Throws a type error if:
--
--   * a function is defined within the entry point,
--   * the name of a builtin for this entry point is already in use.
type family ValidEntryPoint
              ( k :: Symbol      )
              ( s :: Stage       )
              ( i :: BindingsMap )
              ( l :: BindingsMap ) 
            :: Constraint where
  ValidEntryPoint k s i l
    = ( ProvidedSymbol k
      , NoEntryPointNameConflict k (Lookup k i)
      , ProvidedStage s
      , ValidLocalBehaviour s (Remove i l)
      , BuiltinsDoNotAppearBefore s ( StageBuiltins s ) i
      )

type family NoEntryPointNameConflict
               ( k     :: Symbol        )
               ( mb_bd :: Maybe Binding )
               :: Constraint where
  NoEntryPointNameConflict k ('Just _) = TypeError
    (     Text "'entryPoint': cannot define a new entry point with name " :<>: ShowType k :<>: Text "."
     :$$: Text "That name is already in scope."
    )
  NoEntryPointNameConflict _ 'Nothing = ()

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
--
-- Throws a type error if there is no image with given name.
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
    = TypeError (     Text "Expected an image, \
                           \but nothing is bound by name " :<>: ShowType k
                )
  ImagePropertiesFromLookup k i (Just nonImage)
    = TypeError (     Text "Unexpected type " :<>: ShowType nonImage
                 :<>: Text " bound by name " :<>: ShowType k
                 :$$: Text "Expected an image."
                 )

-- | Check that we can read from an image.
--
-- Depending on the coordinate type, this is either a sampling operation
-- or a direct image read operation.
--
-- Throws a type error if the operation is incompatible with the given
-- image properties. For instance, if the image is a depth image,
-- a depth-comparison reference value must be provided.
--
-- Refer to the @SPIR-V@ specification for what operations are allowed.
type family ValidImageRead
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
            :: Constraint where
  ValidImageRead
    ( Properties coords res _ depth _ ms usage fmt )
    ops
      = ( AllowedIndexing (Integrality coords) ms usage
        , CheckDepthTest (DepthComparison `Elem` ops) (Integrality coords) depth
        , CheckLODOperands (Integrality coords) ops
        , CompatibleFormat (Integrality res) usage fmt
        )

-- | Check that we can write to an image.
--
-- Throws a type error if:
--
--   * the image is not a storage image,
--   * the image is a depth image,
--   * the coordinate type is not an integral type,
--   * an operand is incompatible with the given image properties,
--   * an operand doesn't make sense for write operations.
--
-- Refer to the @SPIR-V@ specification for what operations are allowed.
type family ValidImageWrite
              ( props :: ImageProperties )
              ( ops   :: [OperandName]   )
           :: Constraint where
  ValidImageWrite
    ( Properties _ _ _ _ _ _ Sampled _ )
    ops
      = TypeError ( Text "Cannot write to a sampled image; must be a storage image." )
  ValidImageWrite
    ( Properties _ _ _ (Just DepthImage) _ _ _ _ )
    ops
      = TypeError ( Text "Cannot write to a depth image." )

  ValidImageWrite
    ( Properties coords res _ _ _ _ usage fmt )
    ops
      = ( IntegralIndexing (Integrality coords)
        , CompatibleFormat (Integrality res) usage fmt
        , AllowedWriteOps ops
        )

type family IntegralIndexing (inty :: SPIRV.ScalarTy) :: Constraint where
  IntegralIndexing (SPIRV.Floating _)
    = TypeError ( Text "Cannot write to an image using floating-point coordinates." )
  IntegralIndexing _ = ()

-- Check whether floating-point coordinates are allowed.
type family AllowedIndexing
              ( inty  :: SPIRV.ScalarTy )
              ( ms    :: MultiSampling  )
              ( usage :: ImageUsage     )
            :: Constraint where
  AllowedIndexing (SPIRV.Floating _) _ Storage
    = TypeError
        ( Text "Cannot use floating-point coordinates with a storage image." )
  AllowedIndexing (SPIRV.Floating _) MultiSampled _
    = TypeError
        ( Text "Cannot use floating-point coordinates with multi-sampling." )
  AllowedIndexing _ _ _ = ()

-- Check that depth-testing is appropriately performed.
type family CheckDepthTest
              ( depthTesting :: Bool   )
              ( inty :: SPIRV.ScalarTy )
              ( depth :: Maybe HasDepth )
            :: Constraint where
  CheckDepthTest False _ (Just DepthImage)
    = TypeError
        ( Text "Must use a depth comparison with this depth image." )
  CheckDepthTest True _ (Just NotDepthImage)
    = TypeError
        ( Text "Cannot perform depth comparison: not a depth image." )
  CheckDepthTest True (SPIRV.Integer _ _ ) _
    = TypeError
        ( Text "Cannot perform depth comparison using integral coordinates." )
  CheckDepthTest _ _ _ = ()

-- If using integral coordinates, LOD instructions cannot be provided.
type family CheckLODOperands
                ( inty :: SPIRV.ScalarTy )
                ( ops  :: [OperandName]  )
              :: Constraint where
  CheckLODOperands (SPIRV.Floating _) _ = ()
  CheckLODOperands _ ( BaseOperand ('LODOperand lod) ': _ )
    = TypeError 
        ( ShowType lod :<>: Text " operand not allowed: using integral coordinates." )
  CheckLODOperands inty ( op ': ops ) = CheckLODOperands inty ops

type family CompatibleFormat
                ( inty  :: SPIRV.ScalarTy          )
                ( usage :: ImageUsage              )
                ( fmt   :: Maybe (ImageFormat Nat) )
              :: Constraint
              where
  CompatibleFormat (SPIRV.Integer _ _) _ (Just ('ImageFormat (Image.Integer Normalised _) _))
    = TypeError
       (    Text "Expected a floating-point type, but provided an integer type."
       :$$: Text "Image uses a normalised integer format, resulting in floating-point texel data."
       )
  CompatibleFormat (SPIRV.Integer _ _) _ (Just ('ImageFormat Image.Floating _))
    = TypeError
       (    Text "Expected a floating-point type, but provided an integer type."
       :$$: Text "Image uses a floating-point format."
       )
  CompatibleFormat (SPIRV.Floating _) _ (Just ('ImageFormat (Image.Integer Unnormalised _) _))
    = TypeError
       (    Text "Expected an integral type, but provided a floating-point type."
       :$$: Text "Image uses unnormalised integers, resulting in integral texel data."
       )
  CompatibleFormat _ Storage _
    = ()
  CompatibleFormat _ Sampled (Just fmt)
    = If
        ( RequiredFormatUsage fmt == Just Storage )
        ( TypeError
           (     Text "Image format " :<>: ShowType fmt
            :<>: Text " can only be used with storage images."
            )
        )
        ( () :: Constraint )

-- Only 'Sample' and 'Offset'/'ConstOffset' image operands allowed.
type family AllowedWriteOps (ops :: [OperandName]) :: Constraint where
  AllowedWriteOps '[] = ()
  AllowedWriteOps (BaseOperand Image.Sample      ': ops) = AllowedWriteOps ops
  AllowedWriteOps (BaseOperand Image.ConstOffset ': ops) = AllowedWriteOps ops
  AllowedWriteOps (BaseOperand Image.Offset      ': ops) = AllowedWriteOps ops
  AllowedWriteOps (op                            ': _  )
    = TypeError (     Text "Image operand " :<>: ShowType op
                 :<>: Text " cannot be used in conjunction \
                           \with an image write operation."
                )

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
type family ProvidedStage ( s :: Stage ) :: Constraint where
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
