{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.ASTState

State for the indexed monad used to construct programs.
-}

module FIR.ASTState where

-- base
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import Data.Type.Known
  ( Demotable(Demote)
  , Known(known), knownValue
  )
import FIR.Binding
  ( BindingsMap
  , Permissions
  )
import FIR.Prim.Singletons
  ( KnownVars(knownVars) )
import qualified SPIRV.PrimTy as SPIRV
  ( PrimTy )
import qualified SPIRV.Stage as SPIRV
  ( Stage(..), StageInfo(..), stageOf )

-------------------------------------------------------------------
-- | Data (kind) used to keep track of function context at the type-level.
data FunctionContext lit nat bindings where
  TopLevel :: FunctionContext lit nat bindings
  InFunction :: lit -> bindings -> FunctionContext lit nat bindings
  InEntryPoint :: lit -> SPIRV.StageInfo nat stage -> FunctionContext lit nat bindings

deriving instance ( Show lit, Show bindings, Show nat )
          => Show ( FunctionContext lit nat bindings )

-- | A type-level function context.
type TLFunctionContext
  = FunctionContext Symbol Nat BindingsMap
-- | A value-level function context.
type VLFunctionContext
  = FunctionContext
      Text
      Word32
      [ ( Text, (SPIRV.PrimTy, Permissions) ) ]

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
instance ( Known (SPIRV.StageInfo Nat stage) stageInfo
         , KnownSymbol stageName
         )
      => Known TLFunctionContext ('InEntryPoint stageName (stageInfo :: SPIRV.StageInfo Nat stage)) where
  known = InEntryPoint
            ( knownValue @stageName )
            ( knownValue @stageInfo )

-- | Keeps track of an entry point at the type level:
--
--   - a type level symbol for the entry point name,
--   - a type level 'SPIRV.StageInfo' recording the stage as well as additional
--     stage information.
data EntryPointInfo where
  EntryPointInfo
    :: Symbol -> SPIRV.StageInfo Nat stage -> EntryPointInfo

-- | State that is used in the user-facing indexed monad (at the type level).
-- Consists of:
--
--    - a type-level list of which bindings are in scope,
--    such as user-defined functions, user-defined variables
--    and built-in variables,
--    - a context – whether code in the current indexed monadic state
--    occurs inside a function or entry-point body – and,
--    - which entry points have been declared, together with
--    some additional info pertaining to their respective execution modes.
data ASTState
  = ASTState
      { bindings    :: BindingsMap
      , context     :: TLFunctionContext
      , entryPoints :: [ EntryPointInfo ]
      }

type family EntryPointInfos ( s :: ASTState ) :: [ EntryPointInfo ] where
  EntryPointInfos ('ASTState _ _ eps) = eps

type family StageContext ( s :: ASTState ) :: Maybe SPIRV.Stage where
  StageContext' ('ASTState _ ('InEntryPoint _ (info :: SPIRV.StageInfo Nat stage)) _)
    = Just stage
  StageContext' _
    = Nothing

stageContext :: VLFunctionContext -> Maybe (Text, SPIRV.Stage)
stageContext (InEntryPoint stageName stageInfo) = Just (stageName, SPIRV.stageOf stageInfo)
stageContext _ = Nothing

type family StageContext' ( s :: ASTState ) :: SPIRV.Stage where
  StageContext' ('ASTState _ ('InEntryPoint _ (info :: SPIRV.StageInfo Nat stage)) _)
    = stage
  StageContext' _
    = TypeError
        ( 'Text "Cannot access stage context: not within a stage." )

type family StageInfoContext
                ( s :: ASTState )
              :: Maybe (SPIRV.StageInfo Nat (StageContext' s))
                where
  StageInfoContext ('ASTState _ ('InEntryPoint _ info) _)
    = Just info
  StageInfoContext _ = 'Nothing
