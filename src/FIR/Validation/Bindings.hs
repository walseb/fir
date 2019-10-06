{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Validation.Bindings

Validation module providing checks for stateful operations
such as 'FIR.Instances.Codensity.get', 'FIR.Instances.Codensity.put',
using type families with custom type errors.

These are used, for instance, to prevent any name from being bound twice,
or accessing a binding that does not exist.

-}

module FIR.Validation.Bindings where

-- base
import Prelude
  hiding ( Integral, Floating )
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If, type (||), type (&&) )
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
import Data.Type.List
  ( Elem )
import Data.Type.Map
  ( Map, (:->)((:->))
  , Insert, Remove
  , Lookup, Union
  )
import Data.Type.Ord
  ( POrd(Compare) )
import FIR.Binding
  ( Binding, BindingsMap, FunctionType
  , Permission(Read,Write)
  , Var, Fun
  )
import FIR.Builtin
  ( ModelBuiltins )
import FIR.ASTState
  ( FunctionContext(..), ASTState(..)
  , Definedness(..), FunctionInfo(..)
  , EntryPointInfo(..), TLInterface
  )
import FIR.Prim.Image
  ( Image )
import qualified SPIRV.Stage    as SPIRV
  ( ExecutionModel, NamedExecutionModel
  , ExecutionInfo
  )

-------------------------------------------------
-- * Lookup whether a binding exists in the monadic state.

-- | Compute the type of a variable bound by a given name.
--
-- Throws a type error if no variable by that name exists.
type family Has (k :: Symbol) (i :: ASTState) :: Type where
  Has k ('ASTState bds _ _ _) = HasBinding k (Lookup k bds)

type family HasBinding (k :: Symbol) (mbd :: Maybe Binding) :: Type where
  HasBinding k 'Nothing
   = TypeError
      (  Text "No binding named " :<>: ShowType k :<>: Text " is in scope." )
  HasBinding k ('Just (Var _ a))
    = a
  HasBinding _ ('Just (Fun as b)) = FunctionType as b

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.get' and 'FIR.Instances.Codensity.use'

-- | Check whether we can 'get' a binding.
type family CanGet (k :: Symbol) (i :: ASTState) :: Constraint where
  CanGet k ('ASTState bds _ _ _) = GetBinding k (Lookup k bds)

type family GetBinding (k :: Symbol) (mbd :: Maybe Binding) :: Constraint where
  GetBinding k 'Nothing
   = TypeError
      (  Text "'get'/'use': no binding named " :<>: ShowType k :<>: Text " is in scope." )
  GetBinding k ('Just (Var _ (Image _)))
    = TypeError 
          (     Text "'get'/'use': variable named " :<>: ShowType k :<>: Text " refers to an image."
           :$$: Text "To access image data, use the 'ImageTexel' optic or the 'imageRead' function."
          )
  GetBinding k ('Just (Var perms a))
    = If 
        ( Elem 'Read perms )
        ( () :: Constraint )
        ( TypeError
          ( Text "'get'/'use': variable named " :<>: ShowType k :<>: Text " is not readable." )
        )
  GetBinding _ ('Just (Fun as b)) = ()

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.put' and 'FIR.Instances.Codensity.assign'

-- | Check whether we can write to a binding.
type family CanPut (k :: Symbol) (i :: ASTState) :: Constraint where
  CanPut k ('ASTState bds _ _ _) = PutBinding k (Lookup k bds)

type family PutBinding (k :: Symbol) (lookup :: Maybe Binding) :: Constraint where
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
        ( Elem 'Write perms )
        ( () :: Constraint )
        ( TypeError 
          ( Text "'put'/'assign': variable " :<>: ShowType k :<>: Text " is not writable." )
        )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.def'

-- | Add a new binding to the indexed state.
type family AddBinding
              ( k  :: Symbol   )
              ( bd :: Binding  )
              ( s  :: ASTState )
              :: ASTState where
  AddBinding k bd ('ASTState bds ctx funs eps)
    = 'ASTState (Insert k bd bds) ctx funs eps

-- | Check that it is valid to define a new variable with given name.
--
-- Throws a type error if a binding by this name already exists.
type family ValidDef (k :: Symbol) (i :: ASTState) :: Constraint where
  ValidDef k ('ASTState bds _ _ _) = NotAlreadyDefined k (Lookup k bds)

type family NotAlreadyDefined (k :: Symbol) (lookup :: Maybe Binding) :: Constraint where
  NotAlreadyDefined _ 'Nothing  = ()
  NotAlreadyDefined k ('Just _) = TypeError
    ( Text "'def': a binding by the name " :<>: ShowType k :<>: Text " already exists." )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.fundef'

-- | Indexed monadic state at start of function body.
--
-- Adds the required function variables to the map of in-scope bindings.
type family FunctionDefinitionStartState
              ( k  :: Symbol      )
              ( as :: BindingsMap )
              ( s  :: ASTState    )
            = ( j  :: ASTState    )
            | j -> k as
            where
  FunctionDefinitionStartState k as ('ASTState i ctx funs eps)
    = 'ASTState (Union i as) ('InFunction k as) funs eps

type family FunctionDefinitionEndState
              ( k   :: Symbol      )
              ( as  :: BindingsMap )
              ( bds :: BindingsMap )
              ( i   :: ASTState    )
            = ( j   :: ASTState    )
            | j -> k as bds
            where
  FunctionDefinitionEndState k as bds ('ASTState _ _ funs eps)
    = 'ASTState
        bds
        ( 'InFunction k as)
        funs
        eps

type family GetFunctionInfo
              ( k :: Symbol   )
              ( i :: ASTState )
            :: FunctionInfo
            where
  GetFunctionInfo k ('ASTState _ _ funs _)
    = GetFunctionInfoFromLookup k (Lookup k funs)

type family FunctionTypes
              ( k :: Symbol   )
              ( i :: ASTState )
           :: ( BindingsMap, Type )
           where
  FunctionTypes k i = FunctionTypesFromInfo (GetFunctionInfo k i)

type family FunctionTypesFromInfo
              ( info :: FunctionInfo )
            :: ( BindingsMap, Type )
            where
  FunctionTypesFromInfo ('FunctionInfo as b _ _) = '( as, b )

type family GetFunctionInfoFromLookup
              ( k :: Symbol )
              ( mbFun :: Maybe FunctionInfo )
            :: FunctionInfo
            where
  GetFunctionInfoFromLookup _ (Just info) = info
  GetFunctionInfoFromLookup k 'Nothing
    = TypeError
    (    Text "'fundef': function named " :<>: ShowType k
    :<>: Text " has not been declared (no function info)."
    )

-- | Adds a function binding to the indexed monadic state.
type family AddFunBinding (k :: Symbol) (as :: BindingsMap) (b :: Type) (s :: ASTState) :: ASTState where
  AddFunBinding k as b ('ASTState bds ctx funs eps)
  -- 'ValidFunDef' should have already checked that name 'k' is not already in use
    = 'ASTState (Insert k (Fun as b) bds) ctx (SetFunctionDefined k funs) eps

type family SetFunctionDefined
              ( k    :: Symbol                  )
              ( funs :: Map Symbol FunctionInfo )
            :: Map Symbol FunctionInfo
            where
  SetFunctionDefined k '[]
    = TypeError
    (    Text "'fundef': function named " :<>: ShowType k
    :<>: Text " has not been declared."
    )
  SetFunctionDefined k ( ( k ':-> 'FunctionInfo as b fc Declared ) ': funs )
    = ( k ':-> 'FunctionInfo as b fc Defined ) ': funs
  SetFunctionDefined k ( ( k ':-> 'FunctionInfo _  _ _  Defined  ) ': _    )
    = TypeError
    (    Text "'fundef': function named " :<>: ShowType k
    :<>: Text " has already been defined."
    )
  SetFunctionDefined k ( fun ': funs )
    = fun ': SetFunctionDefined k funs

-- | Set the monadic context: within a function body.
type family SetFunctionContext
              ( k  :: Symbol      )
              ( as :: BindingsMap )
              ( s  :: ASTState    )
              :: ASTState where
  SetFunctionContext k as ('ASTState bds TopLevel funs eps)
    = 'ASTState bds ('InFunction k as) funs eps
  SetFunctionContext k _ ('ASTState _ ('InFunction l _) _ _ )
    = TypeError
        (    Text "'fundef': unexpected nested function definition."
        :$$: Text "Function " :<>: ShowType k
        :<>: Text " declared inside the body of function " :<>: ShowType l :<>: Text "."
        )
  SetFunctionContext k _
    ( 'ASTState _ ('InEntryPoint stageName ( _ :: SPIRV.ExecutionInfo Nat em) _) _ _ )
    = TypeError
        (    Text "'fundef': unexpected function definition inside entry point."
        :$$: Text "Function " :<>: ShowType k
        :<>: Text " declared inside " :<>: Text (SPIRV.NamedExecutionModel stageName em)
        :<>: Text "."
        )

-- | Check that a function definition is valid.
--
-- Throws a type error if:
-- 
--   * function is not declared at the top level,
--   * function name is already in use,
--   * one of the function's arguments is itself a function,
--   * another function is defined inside the function,
type family ValidFunDef 
      ( k  :: Symbol      )  -- name of function to be defined
      ( as :: BindingsMap )  -- function arguments
      ( s  :: ASTState    )  -- indexed state at the point the function is definition
      ( l  :: BindingsMap )  -- total bindings at the end of the function definition
    :: Constraint
    where
  ValidFunDef k _ ('ASTState _ ('InFunction l _) _ _ ) _
    = TypeError
        (    Text "'fundef': unexpected nested function definition."
        :$$: Text "Function " :<>: ShowType k
        :<>: Text " declared inside the body of function " :<>: ShowType l :<>: Text "."
        )
  ValidFunDef k _
    ( 'ASTState _ ('InEntryPoint stageName ( _ :: SPIRV.ExecutionInfo Nat em) _) _ _ )
    _
      = TypeError
          (    Text "'fundef': unexpected function definition inside entry point."
          :$$: Text "Function " :<>: ShowType k
          :<>: Text " declared inside " :<>: Text (SPIRV.NamedExecutionModel stageName em)
          :<>: Text "."
          )
  ValidFunDef k as ('ASTState i 'TopLevel _ _) l
    = ( NoFunctionNameConflict k ( Lookup k i ) -- check that function name is not already in use
      , ValidArguments k as (Remove i (Remove as l)) )
        --     │                      └━━━━━━┬━━━━━┘
        --     │                             │
        --     │                             │
        --     │    local variables ├━━━━━━━━┘
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
  ValidArguments' k as l (_ ': as_rec) l_rec = ValidArguments' k as l as_rec l_rec
  ValidArguments' k _  l _ ((v ':-> Fun _ _) ': _    ) = TypeError
    (     Text "'fundef': unexpected nested function definition inside function " :<>: ShowType k :<>: Text ":"
     :$$: Text "local name " :<>: ShowType v :<>: Text " binds a function."
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


-- | Indexed monadic state at start of entry point body.
type family EntryPointStartState
              ( k    :: Symbol                    )
              ( nfo  :: SPIRV.ExecutionInfo Nat s )
              ( i    :: ASTState                  )
            = ( j    :: ASTState                  )
            | j -> k nfo
            where
  EntryPointStartState k nfo ('ASTState i _ funs eps)
    = 'ASTState
        ( Union i (ModelBuiltins nfo) ) -- bindings at the beginning
        ( 'InEntryPoint                 -- context: now within an entry point
             k
             nfo
             ( 'Just '( '[], '[] ) )    -- initial empty interface
        )
        funs
        eps

-- | Indexed monadic state at end of entry point body.
type family EntryPointEndState
              ( k     :: Symbol                    )
              ( nfo   :: SPIRV.ExecutionInfo Nat s )
              ( bds   :: BindingsMap               )
              ( iface :: TLInterface               )
              ( i     :: ASTState                  )
            = ( j     :: ASTState                  )
            | j -> k nfo bds iface
            where
  EntryPointEndState k nfo bds iface ('ASTState _ _ funs eps)
    = 'ASTState
        bds
        ( InEntryPoint k nfo ('Just iface) )
        funs
        eps

type family GetExecutionInfo
              ( k  :: Symbol               )
              ( em :: SPIRV.ExecutionModel )
              ( i  :: ASTState             )
            :: SPIRV.ExecutionInfo Nat em
            where
  GetExecutionInfo k em ('ASTState _ _ _ eps)
    = GetExecutionInfoOf k em eps

type family GetExecutionInfoOf
              ( k  :: Symbol               )
              ( em :: SPIRV.ExecutionModel )
              ( i  :: [EntryPointInfo]     )
            :: SPIRV.ExecutionInfo Nat em
            where
  GetExecutionInfoOf k em '[]
    = TypeError
      (    Text "'GetExecutionInfo': cannot find "
      :<>: Text (SPIRV.NamedExecutionModel k em)
      :<>: Text "."
      )
  GetExecutionInfoOf k em
    ( 'EntryPointInfo k (nfo :: SPIRV.ExecutionInfo Nat em) _ _ ': _ )
      = nfo
  GetExecutionInfoOf k em (_ ': eps)
    = GetExecutionInfoOf k em eps


type family SetInterface
              ( k     :: Symbol                    )
              ( nfo   :: SPIRV.ExecutionInfo Nat s )
              ( iface :: TLInterface               )
              ( i     :: ASTState                  )
            :: ASTState
            where
  SetInterface k (nfo :: SPIRV.ExecutionInfo Nat s) iface ('ASTState i fc funs eps)
    = 'ASTState i fc funs (SetInterfaceOf k s iface eps)

type family SetInterfaceOf
              ( k     :: Symbol               )
              ( em    :: SPIRV.ExecutionModel )
              ( iface :: TLInterface          )
              ( eps   :: [ EntryPointInfo ]   )
            :: [ EntryPointInfo]
            where
  SetInterfaceOf k em  _ '[]
    = TypeError
      (    Text "'SetInterface': cannot find "
      :<>: Text (SPIRV.NamedExecutionModel k em)
      :<>: Text "."
      )
  SetInterfaceOf k em iface
    ( 'EntryPointInfo k (nfo :: SPIRV.ExecutionInfo Nat em) _ Declared ': eps )
      = 'EntryPointInfo k nfo iface Defined ': eps
  SetInterfaceOf k em iface
    ( 'EntryPointInfo k (nfo :: SPIRV.ExecutionInfo Nat em) _ Defined ': _ )
      = TypeError
      (    Text "'entryPoint': entry point "
      :<>: Text (SPIRV.NamedExecutionModel k em)
      :<>: Text " has already been defined."
      )
  SetInterfaceOf k em iface (ep ': eps)
    = ep ': SetInterfaceOf k em iface eps

type family InsertEntryPointInfo
              ( nfo   :: EntryPointInfo     )
              ( eps   :: [ EntryPointInfo ] )
              :: [ EntryPointInfo ]
              where
  InsertEntryPointInfo nfo '[]
    = '[ nfo ]
  InsertEntryPointInfo
    ( 'EntryPointInfo k (nfo  :: SPIRV.ExecutionInfo Nat s1) iface  def)
    ( 'EntryPointInfo l (nfo2 :: SPIRV.ExecutionInfo Nat s2) iface2 def2 ': eps )
    = InsertEntryPointInfoWithComparison
          ( k  `Compare` l  )
          ( s1 `Compare` s2 )
          ( 'EntryPointInfo k nfo  iface  def  )
          ( 'EntryPointInfo l nfo2 iface2 def2 )
          eps

type family InsertEntryPointInfoWithComparison
              ( cmpName  :: Ordering           )
              ( cmpStage :: Ordering           )
              ( nfo1     :: EntryPointInfo     )
              ( nfo2     :: EntryPointInfo     )
              ( nfos     :: [ EntryPointInfo ] )
            :: [ EntryPointInfo ]
            where
  InsertEntryPointInfoWithComparison EQ EQ
      ( 'EntryPointInfo k (_ :: SPIRV.ExecutionInfo Nat s ) _ _ )
      _
      _
    = TypeError
        (    Text "'entryPoint': duplicate entry point declaration."
        :$$: Text "There already exists a " :<>: Text (SPIRV.NamedExecutionModel k s) :<>: Text "."
        )
  InsertEntryPointInfoWithComparison cmpName cmpStage nfo1 nfo2 nfos
    = If ( cmpName == LT || ( cmpName == EQ && cmpStage == LT ) )
        ( nfo1 ': nfo2 ': nfos )
        ( nfo2 ': InsertEntryPointInfo nfo1 nfos )

-- | Check that an entry point definition is valid.
--
-- Throws a type error if:
--
--   * a function is defined within the entry point,
--   * the name of a builtin for this entry point is already in use.
type family ValidEntryPoint
              ( k        :: Symbol                    )
              ( nfo      :: SPIRV.ExecutionInfo Nat s )
              ( i        :: ASTState                  )
              ( l        :: BindingsMap               )
            :: Constraint where
  ValidEntryPoint k (nfo :: SPIRV.ExecutionInfo Nat s) ('ASTState i _ _ _) l
    = ( NoEntryPointNameConflict k (Lookup k i)
      , ValidLocalBehaviour s (Remove i l)
      , BuiltinsDoNotAppearBefore s (ModelBuiltins nfo) i
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

type ValidLocalBehaviour (s :: SPIRV.ExecutionModel) (l :: BindingsMap)
  = ( ValidLocalBehaviour' s l l :: Constraint )

type family ValidLocalBehaviour'
              ( s     :: SPIRV.ExecutionModel )
              ( l     :: BindingsMap          )
              ( l_rec :: BindingsMap          )
              :: Constraint where
  ValidLocalBehaviour' _ _ '[]                         = ()
  ValidLocalBehaviour' s l ((_ ':-> Var _ _) ': l_rec) = ValidLocalBehaviour' s l l_rec
  ValidLocalBehaviour' s l ((v ':-> Fun _ _) ': _    ) = TypeError
    (     Text "'entryPoint': unexpected nested function definition inside " :<>: ShowType s :<>: Text " entry point:"
     :$$: Text "local name " :<>: ShowType v :<>: Text "binds a function."
     :$$: Text "Local bindings for " :<>: ShowType s :<>: Text " entry point are:"
     :$$: ShowType l
    )

type family BuiltinsDoNotAppearBefore
              ( s        :: SPIRV.ExecutionModel )
              ( builtins :: BindingsMap          )
              ( i        :: BindingsMap          )
              :: Constraint where
  BuiltinsDoNotAppearBefore _ '[]                _  = ()
  BuiltinsDoNotAppearBefore s ((b ':-> _) ': bs) i 
    = BuiltinDoesNotAppearBefore s b bs i (Lookup b i)

type family BuiltinDoesNotAppearBefore
              ( s   :: SPIRV.ExecutionModel )
              ( b   :: Symbol               )
              ( bs  :: BindingsMap          )
              ( i   :: BindingsMap          )
              ( lkp :: Maybe Binding        )
              :: Constraint where
  BuiltinDoesNotAppearBefore s _ bs i 'Nothing  = BuiltinsDoNotAppearBefore s bs i
  BuiltinDoesNotAppearBefore s b _  i ('Just _)
    = TypeError (     Text "'entryPoint': conflict with built-in variable for " :<>: ShowType s :<>: Text " stage."
                 :$$: Text "Variable with name " :<>: ShowType b :<>: Text " is a built-in, yet is already bound."
                 :$$: Text "Variables in scope at site of entry point definition are:"
                 :$$: ShowType i
                )

-------------------------------------------------
-- * Constraints for 'FIR.Instances.Codensity.embed'.

type family Embeddable (i :: ASTState) (j :: ASTState) :: Constraint where
  Embeddable ('ASTState i_bds i_ctx i_funs _) ('ASTState j_bds i_ctx j_funs _)
    = ( SubsetBindings  i_bds  j_bds
      , SubsetFunctions i_funs j_funs
      )
  Embeddable ('ASTState _ i_ctx _ _) ('ASTState _ j_ctx _ _)
    = TypeError
      (      Text "'embed': cannot embed computation with function context "
        :<>: ShowType i_ctx
        :<>: Text " into computation with function context "
        :<>: ShowType j_ctx
        :<>: Text "."
      )

type family SubsetBindings (is :: BindingsMap) (js :: BindingsMap) :: Constraint where
  SubsetBindings '[]       _ = ()
  SubsetBindings (i ': is) js
    = If (i `Elem` js)
        ( SubsetBindings is js )
        ( TypeError
           (     Text "'embed': cannot embed computation."
            :$$: Text "Binding " :<>: ShowType i
            :<>: Text " is missing in larger context "
            :<>: ShowType js
            :<>: Text "."
           )
        )

type family SubsetFunctions
              ( fs :: Map Symbol FunctionInfo )
              ( gs :: Map Symbol FunctionInfo )
            :: Constraint where
  SubsetFunctions '[] _ = ()
  SubsetFunctions ( (k ':-> f) ': fs) gs
    = SubsetFunctionsRec k f (Lookup k gs) fs gs

type family SubsetFunctionsRec
              ( k  :: Symbol                  )
              ( f  :: FunctionInfo            )
              ( g  :: Maybe FunctionInfo      )
              ( fs :: Map Symbol FunctionInfo )
              ( gs :: Map Symbol FunctionInfo )
            :: Constraint where
  SubsetFunctionsRec k _ Nothing _ _
    = TypeError
    (    Text "'embed': cannot embed computation."
    :$$: Text "Function named " :<>: ShowType k
    :<>: Text " is missing in larger context."
    )
  SubsetFunctionsRec _ f (Just f) fs gs
    = SubsetFunctions fs gs
  SubsetFunctionsRec k f (Just f') _ _
    = TypeError
    (    Text "'embed': cannot embed computation."
    :$$: Text "Mismatch between functions of name " :<>: ShowType k
    :<>: Text "."
    :$$: Text "  - Smaller context function info: " :<>: ShowType f :<>: Text ","
    :$$: Text "  - Larger context function info: " :<>: ShowType f' :<>: Text "."
    )
