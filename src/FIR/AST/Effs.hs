{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures -Wno-missing-signatures #-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module: FIR.AST.Effs

Effectful operations of the AST:
  * defining new variables/functions/entry-points,
  * management of indexed state.
-}

module FIR.AST.Effs where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy )
import qualified GHC.Stack
  ( HasCallStack )
import GHC.TypeLits
  ( Symbol, KnownSymbol, symbolVal )
import GHC.TypeNats
  ( Nat )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( pattern VF )

-- fir
import Data.Constraint.All
  ( All )
import Data.Type.Known
  ( Known, knownValue )
import FIR.AST.Display
  ( Display(toTreeArgs), named )
import FIR.AST.Type
  ( AugType(Val, (:-->)), Eff, FunArgs, Nullary )
import FIR.Binding
  ( BindingsMap
  , FunctionAugType, Var, Permissions
  )
import FIR.Prim.Types
  ( PrimTy, KnownVars )
import FIR.ProgramState
  ( ProgramState, TLInterface )
import FIR.Validation.Bindings
  ( ValidDef, ValidFunDef, FunctionTypes
  , AddBinding, AddFunBinding
  , FunctionDefinitionStartState, FunctionDefinitionEndState
  , ValidEntryPoint, SetInterface
  , GetExecutionInfo
  , EntryPointStartState, EntryPointEndState
  , Embeddable
  )
import qualified SPIRV.Stage  as SPIRV
  ( ExecutionModel, ExecutionInfo )

------------------------------------------------------------

pattern Let                     = VF LetF
pattern Def           k ps      = VF ( DefF           k ps )
pattern FunDef        name as b = VF ( FunDefF        name as b )
pattern FunCall       name as b = VF ( FunCallF       name as b )
pattern DefEntryPoint name info = VF ( DefEntryPointF name info )
pattern Locally                 = VF LocallyF
pattern Embed                   = VF EmbedF

-- | Let binding.
data LetF ( ast :: AugType -> Type ) ( t :: AugType ) where
  LetF :: GHC.Stack.HasCallStack => LetF ast ( Val a :--> Eff i i a )

-- | Defining a new constant/variable.
data DefF ( ast :: AugType -> Type ) ( t :: AugType ) where
  DefF
    :: forall
        ( k   :: Symbol           )
        ( ps  :: Permissions      )
        ( a   :: Type             )
        ( i   :: ProgramState     )
        ( ast :: AugType -> Type  )
    . ( GHC.Stack.HasCallStack
      , KnownSymbol k
      , Known Permissions ps
      , PrimTy a
      , ValidDef k i a
      )
    => Proxy k  -- ^ Variable name.
    -> Proxy ps -- ^ Permissions (read,write,...).
    -> DefF ast ( Val a :--> Eff i (AddBinding k (Var ps a) i) a )

-- | Defining a new function.
--
-- Meaning of type variables:
-- * name: function name,
-- * as: named function arguments (usually inferred),
-- * b: function return type (usually inferred),
-- * j_bds: bindings state at end of function definition (usually inferred),
-- * i: monadic state at function definition site (usually inferred).
data FunDefF ( ast :: AugType -> Type ) ( t :: AugType ) where
  FunDefF
    :: forall
         ( name  :: Symbol          )
         ( as    :: BindingsMap     )
         ( b     :: Type            )
         ( j_bds :: BindingsMap     )
         ( i     :: ProgramState    )
         ( ast   :: AugType -> Type )
    . ( GHC.Stack.HasCallStack
      , KnownSymbol name
      , KnownVars as
      , PrimTy b
      , ValidFunDef name as i j_bds
      , '(as,b) ~ FunctionTypes name i
      )
    => Proxy name -- ^ Funtion name.
    -> Proxy as   -- ^ Function argument types.
    -> Proxy b    -- ^ Function return type.
    -> FunDefF ast
        ( Eff 
          ( FunctionDefinitionStartState name as       i )
          ( FunctionDefinitionEndState   name as j_bds i )
          b
        :--> Eff i ( AddFunBinding name as b i ) ( FunctionHandle name as b )
        )

data FunctionHandle (name :: Symbol) (as :: BindingsMap) (b :: Type)

data FunCallF ( ast :: AugType -> Type ) ( t :: AugType ) where
  FunCallF 
    :: ( KnownSymbol name
       , KnownVars as
       , PrimTy b
       , All Nullary (FunArgs (FunctionAugType as b))
       )
    => Proxy name
    -> Proxy as
    -> Proxy b
    -> FunCallF ast ( Val (FunctionHandle name as b) :--> FunctionAugType as b )

-- | Defining a new entry point.
--
-- An entry point is like a function definition with no arguments and Unit return type.
-- Code within an entry point is given access to additional builtins.
--
-- Meaning of type variables:
-- * name: entry point name,
-- * stage: entry point stage,
-- * stageInfo: entry point stage info (usually inferred),
-- * j_bds: bindings state at end of entry point definition (usually inferred),
-- * j_iface: entry point interface (usually inferred),
-- * i: monadic state at entry point definition site (usually inferred).
data DefEntryPointF ( ast :: AugType -> Type ) ( t :: AugType ) where
  DefEntryPointF
    :: forall
        ( name      :: Symbol )
        ( stage     :: SPIRV.ExecutionModel          )
        ( stageInfo :: SPIRV.ExecutionInfo Nat stage )
        ( j_bds     :: BindingsMap  )
        ( j_iface   :: TLInterface  )
        ( i         :: ProgramState )
        ( ast       :: AugType -> Type )
    . ( GHC.Stack.HasCallStack
      , KnownSymbol name
      , Known SPIRV.ExecutionModel stage
      , Known (SPIRV.ExecutionInfo Nat stage) stageInfo
      , ValidEntryPoint name stageInfo i j_bds
      , stageInfo ~ GetExecutionInfo name stage i
      )
    => Proxy name      -- ^ Entry point name.
    -> Proxy stageInfo -- ^ Entry point stage info.
    -> DefEntryPointF ast
        (  Eff 
          ( EntryPointStartState name stageInfo               i )
          ( EntryPointEndState   name stageInfo j_bds j_iface i )
          ()
        :--> ( Eff i ( SetInterface name stageInfo j_iface i ) () )
        )


data StateF ( ast :: AugType -> Type ) ( t :: AugType ) where
  -- | Encapsulate local state.
  LocallyF :: StateF ast ( Eff i j a :--> Eff i i a )

  -- | Embed a computation into one with larger state.
  EmbedF :: Embeddable i j => StateF ast ( Eff i i a :--> Eff j j a )

------------------------------------------------------------
-- displaying

instance Display (LetF ast) where
  toTreeArgs = named \ LetF -> "Let"
instance Display (DefF ast) where
  toTreeArgs = named \(DefF k _) ->
    "Def @" ++ symbolVal k
instance Display (FunDefF ast) where
  toTreeArgs = named \(FunDefF k _ _) ->
    "FunDef @" ++ symbolVal k
instance Display (FunCallF ast) where
  toTreeArgs = named \(FunCallF k _ _) ->
    "FunCall @" ++ symbolVal k
instance Display (DefEntryPointF ast) where
  toTreeArgs = named \(DefEntryPointF _ (_ :: Proxy (stageInfo :: SPIRV.ExecutionInfo Nat stage) )) ->
    "Entry @(" ++ show (knownValue @stage) ++ ")"
instance Display (StateF ast) where
  toTreeArgs = named \ case
    LocallyF -> "Locally"
    EmbedF   -> "Embed"
