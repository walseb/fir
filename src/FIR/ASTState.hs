{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.ASTState

State for the indexed monad used to construct programs.
-}

module FIR.ASTState where

-- base
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import Data.Type.Known
  ( Demotable(Demote)
  , Known(known), knownValue
  )
import Data.Type.Map
  ( Map, (:->) )
import FIR.Binding
  ( BindingsMap
  , Permissions
  )
import FIR.Prim.Singletons
  ( KnownVars(knownVars) )
import qualified SPIRV.Control    as SPIRV
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.PrimTy     as SPIRV
  ( PrimTy )
import qualified SPIRV.Stage      as SPIRV

-------------------------------------------------------------------
-- | Data (kind) used to keep track of function context at the type-level.
data FunctionContext lit nat bindings mbIface where
  TopLevel :: FunctionContext lit nat bindings iface
  InFunction :: lit -> bindings -> FunctionContext lit nat bindings mbIface
  InEntryPoint
    :: lit
    -> SPIRV.ExecutionInfo nat em
    -> mbIface
    -> FunctionContext lit nat bindings mbIface

deriving instance ( Show lit, Show bindings, Show nat, Show mbIface )
          => Show ( FunctionContext lit nat bindings mbIface )


type InterfaceVariable nat ty = ( [SPIRV.Decoration nat], ty )
type TLInterfaceVariable = InterfaceVariable Nat    Type
type VLInterfaceVariable = InterfaceVariable Word32 SPIRV.PrimTy
type Interface lit nat ty
  = ( [ lit :-> InterfaceVariable nat ty ]
    , [ lit :-> InterfaceVariable nat ty ]
    )

type TLInterface
  = ( [ Symbol :-> TLInterfaceVariable ]
    , [ Symbol :-> TLInterfaceVariable ]
    )
type VLInterface
  = ( [ ShortText :-> VLInterfaceVariable ]
    , [ ShortText :-> VLInterfaceVariable ]
    )


-- | A type-level function context.
type TLFunctionContext
  = FunctionContext
      Symbol
      Nat
      BindingsMap
      ( Maybe TLInterface )
-- | A value-level function context.
type VLFunctionContext
  = FunctionContext
      ShortText
      Word32
      [ ( ShortText, (SPIRV.PrimTy, Permissions) ) ]
      ( Maybe VLInterface )

instance Demotable TLFunctionContext where
  type Demote TLFunctionContext = VLFunctionContext

instance Known TLFunctionContext 'TopLevel where
  known = TopLevel
instance ( KnownVars args
         , KnownSymbol fnName
         )
       => Known TLFunctionContext ('InFunction fnName args) where
  known = InFunction
            ( knownValue @fnName )
            ( knownVars  @args   )
instance ( Known (SPIRV.ExecutionInfo Nat stage) stageInfo
         , KnownSymbol stageName
       --  , KnownInterface iface
         )
      => Known TLFunctionContext
            ( 'InEntryPoint
                  stageName
                  ( stageInfo :: SPIRV.ExecutionInfo Nat stage )
                  ('Just iface)
            ) where
  known = InEntryPoint
            ( knownValue     @stageName )
            ( knownValue     @stageInfo )
            Nothing -- TODO:  ( knownInterface @iface     )

data Definedness
  = Declared
  | Defined

-- | Keeps track of a function at the type level.
data FunctionInfo where
  FunctionInfo
    :: BindingsMap
    -> Type
    -> SPIRV.FunctionControl
    -> Definedness
    -> FunctionInfo

-- | Keeps track of an entry point at the type level:
--
--   - a type level 'SPIRV.ExecutionInfo' recording the stage as well as additional
--     stage information,
--   - the entry point interface (input/output variables which it uses),
--     (or Nothing if the interface has not yet been computed).
data EntryPointInfo where
  EntryPointInfo
    :: Symbol
    -> SPIRV.ExecutionInfo Nat stage
    -> TLInterface
    -> Definedness
    -> EntryPointInfo

-- | State that is used in the user-facing indexed monad (at the type level).
-- Consists of:
--
--    - a type-level list of which bindings are in scope,
--    such as user-defined functions, user-defined variables
--    and built-in variables,
--    - a context – whether code in the current indexed monadic state
--    occurs inside a function or entry-point body – and,
--    - which functions are to be declared,
--    whether they have been declared yet,
--    and some further information (arguments, function control),
--    - which entry points are to be declared,
--    whether they have been declared yet, together with
--    some additional info pertaining to their respective execution modes,
--    and their interfaces (user defined inputs/outputs).
data ASTState
  = ASTState
      { bindings    :: BindingsMap
      , context     :: TLFunctionContext
      , functions   :: Map Symbol FunctionInfo
      , entryPoints :: [ EntryPointInfo ]
      }

type family Bindings ( s :: ASTState) :: BindingsMap where
  Bindings ('ASTState bds _ _ _) = bds

type family FunctionInfos ( s :: ASTState ) :: Map Symbol FunctionInfo where
  FunctionInfos ('ASTState _ _ fs _ ) = fs

type family EntryPointInfos ( s :: ASTState ) :: [ EntryPointInfo ] where
  EntryPointInfos ('ASTState _ _ _ eps) = eps

type family ExecutionContext ( s :: ASTState ) :: Maybe SPIRV.ExecutionModel where
  ExecutionContext ('ASTState _ ('InEntryPoint _ (info :: SPIRV.ExecutionInfo Nat stage) _) _ _)
    = Just stage
  ExecutionContext _
    = Nothing

executionContext :: VLFunctionContext -> Maybe (ShortText, SPIRV.ExecutionModel)
executionContext (InEntryPoint stageName stageInfo _) = Just (stageName, SPIRV.modelOf stageInfo)
executionContext _ = Nothing

type family ExecutionContext' ( s :: ASTState ) :: SPIRV.ExecutionModel where
  ExecutionContext' ('ASTState _ ('InEntryPoint _ (info :: SPIRV.ExecutionInfo Nat stage) _) _ _)
    = stage
  ExecutionContext' _
    = TypeError
        ( Text "Cannot access stage context: not within a stage." )

type family ExecutionInfoContext
                ( s :: ASTState )
              :: Maybe (SPIRV.ExecutionInfo Nat (ExecutionContext' s))
                where
  ExecutionInfoContext ('ASTState _ ('InEntryPoint _ info _) _ _)
    = Just info
  ExecutionInfoContext _ = 'Nothing
