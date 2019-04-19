{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.IxState

Indexed monadic state used to keep track of context in the AST.
-}

module FIR.IxState where

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
  , Permission
  )
import FIR.Prim.Singletons
  ( KnownVars(knownVars) )
import qualified SPIRV.PrimTy as SPIRV
  ( PrimTy )
import qualified SPIRV.Stage as SPIRV
  ( Stage, StageInfo )

-------------------------------------------------------------------
-- | Data (kind) used to keep track of context at the type-level.
data Context lit nat bindings where
  TopLevel :: Context lit nat bindings
  Function :: lit -> bindings -> Context lit nat bindings
  EntryPoint :: lit -> SPIRV.StageInfo nat stage -> Context lit nat bindings

-- type-level context
type TLContext
  = Context Symbol Nat BindingsMap
-- value-level context
type VLContext
  = Context
      Text
      Word32
      [ ( Text, (SPIRV.PrimTy, [Permission]) ) ]

instance Demotable TLContext where
  type Demote TLContext = VLContext

instance Known TLContext 'TopLevel where
  known = TopLevel
instance ( KnownVars args
         , KnownSymbol fnName
         )
       => Known TLContext ('Function fnName args) where
  known = Function
            ( knownValue @fnName )
            ( knownVars  @args   )
instance ( Known (SPIRV.StageInfo Nat stage) stageInfo
         , KnownSymbol stageName
         )
      => Known TLContext ('EntryPoint stageName (stageInfo :: SPIRV.StageInfo Nat stage)) where
  known = EntryPoint
            ( knownValue @stageName )
            ( knownValue @stageInfo )

data EntryPointInfo where
  EntryPointInfo
    :: Symbol -> SPIRV.StageInfo Nat stage -> EntryPointInfo

data IxState where
  IxState
    :: { bindings :: BindingsMap
       , context  :: TLContext
       , entryPoints
           :: [ EntryPointInfo ]
       }
    -> IxState

type family EntryPointInfos ( s :: IxState ) :: [ EntryPointInfo ] where
  EntryPointInfos ('IxState _ _ eps) = eps


type family StageContext ( s :: IxState ) :: Maybe SPIRV.Stage where
  StageContext' ('IxState _ ('EntryPoint _ (info :: SPIRV.StageInfo Nat stage)) _)
    = Just stage
  StageContext' _
    = Nothing


type family StageContext' ( s :: IxState ) :: SPIRV.Stage where
  StageContext' ('IxState _ ('EntryPoint _ (info :: SPIRV.StageInfo Nat stage)) _)
    = stage
  StageContext' _
    = TypeError
        ( 'Text "Cannot access stage context: not within a stage." )

type family StageInfoContext
                ( s :: IxState )
              :: Maybe (SPIRV.StageInfo Nat (StageContext' s))
                where
  StageInfoContext ('IxState _ ('EntryPoint _ info) _)
    = Just info
  StageInfoContext _ = 'Nothing
