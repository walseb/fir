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
import GHC.TypeLits
  ( Symbol, KnownSymbol )
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
import Data.Type.Map
  ( (:->), Map )
import FIR.Binding
  ( BindingsMap
  , Permission
  )
import FIR.Prim.Singletons
  ( KnownVars(knownVars) )
import qualified SPIRV.Decoration as SPIRV
  ( Decoration )
import qualified SPIRV.ExecutionMode as SPIRV
  ( ExecutionMode )
import qualified SPIRV.PrimTy as SPIRV
  ( PrimTy )
import qualified SPIRV.Stage as SPIRV
  ( Stage )


-------------------------------------------------------------------
-- | Data (kind) used to keep track of context at the type-level.
data Context s as
  = TopLevel
  | Function s as
  | EntryPoint
      s           -- ^ stage name
      SPIRV.Stage -- ^ stage (= execution model)

-- type-level context
type TLContext
  = Context Symbol BindingsMap
-- value-level context
type VLContext
  = Context
      Text
      [ ( Text, (SPIRV.PrimTy, [Permission]) ) ]

instance Demotable TLContext where
  type Demote TLContext = VLContext

instance Known TLContext 'TopLevel where
  known = TopLevel
instance ( KnownVars args
         , KnownSymbol fn
         )
       => Known TLContext (Function fn args) where
  known = Function
            ( knownValue @fn   )
            ( knownVars  @args )
instance ( Known SPIRV.Stage stage
         , KnownSymbol stageName
         )
      => Known TLContext (EntryPoint stageName stage) where
  known = EntryPoint
            ( knownValue @stageName )
            ( knownValue @stage     )

type EntryPointInfo
  = ( [ SPIRV.ExecutionMode Nat ]
    , [ Symbol :-> SPIRV.Decoration Nat ]
    )

data IxState
  = IxState
      { bindings :: BindingsMap
      , context  :: TLContext
      , entryPoints
          :: Map ( Symbol, SPIRV.Stage ) EntryPointInfo
      }

type family EntryPoints
             ( s :: IxState )
             :: Map ( Symbol, SPIRV.Stage ) EntryPointInfo
             where
  EntryPoints ('IxState _ _ eps) = eps

type family StageContext ( s :: IxState ) :: Maybe SPIRV.Stage where
  StageContext ('IxState _ ('EntryPoint _ stage) _) = Just stage
  StageContext _ = 'Nothing
