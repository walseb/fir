{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module FIR.Definition where

-- base
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Symbol )
import GHC.TypeNats
  ( Nat )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set

-- text-utf8
import Data.Text
  ( Text )

-- fir
import CodeGen.State
  ( CGContext(..), emptyContext )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.Map
  ( (:->)((:->)) )
import FIR.Binding
  ( Binding(Variable), StoragePermissions )
import FIR.Prim.Image
  ( Image, knownImage, ImageProperties )
import FIR.Prim.Singletons
  ( PrimTy, primTy )
import qualified FIR.Binding as Binding
import qualified SPIRV.Decoration      as SPIRV
import qualified SPIRV.ExecutionMode   as SPIRV
import qualified SPIRV.FunctionControl as SPIRV
import qualified SPIRV.Image           as SPIRV
  ( Image(imageUsage), ImageUsage(Sampled) )
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.Stage           as SPIRV
import qualified SPIRV.Storage         as SPIRV
  ( StorageClass )

--------------------------------------------------------------------------
-- annotating top-level definitions with necessary information
-- for instance, annotating layout information

data Definition where
  Global     :: SPIRV.StorageClass -> Type -> [ SPIRV.Decoration Nat ] -> Definition
  Function   :: SPIRV.FunctionControl -> [Symbol :-> Binding] -> Type -> Definition
  EntryPoint :: SPIRV.Stage -> [ SPIRV.ExecutionMode Nat ] -> Definition

data Annotate
  = AnnotateGlobal     ( SPIRV.PointerTy, Set (SPIRV.Decoration Word32) )
  | AnnotateFunction   SPIRV.FunctionControl
  | AnnotateEntryPoint ( SPIRV.Stage, Set (SPIRV.ExecutionMode Word32) )

instance Demotable Definition where
  type Demote Definition = Annotate

-- workaround for image types being opaque and not having a Haskell-level counterpart
instance {-# OVERLAPPING #-}
         ( Known ImageProperties props
         , Known SPIRV.StorageClass storage
         , Known [SPIRV.Decoration Nat] decs
         )
      => Known Definition ('Global storage (Image props) decs)
      where
  known = AnnotateGlobal
    ( SPIRV.PointerTy ( knownValue @storage) imgTy
    , Set.fromList ( knownValue @decs )
    )
        where imgTy :: SPIRV.PrimTy
              imgTy = case knownImage @props of
                        img -> case SPIRV.imageUsage img of
                                Just SPIRV.Sampled -> SPIRV.SampledImage img
                                _                  -> SPIRV.Image        img

instance ( PrimTy ty, Known SPIRV.StorageClass storage, Known [SPIRV.Decoration Nat] decs )
      => Known Definition ('Global storage ty decs)
      where
  known = AnnotateGlobal
    ( SPIRV.PointerTy ( knownValue @storage) (primTy @ty)
    , Set.fromList ( knownValue @decs )
    )

instance Known SPIRV.FunctionControl control
      => Known Definition ('Function control args res)
      where
  known = AnnotateFunction (knownValue @control)

instance ( Known SPIRV.Stage stage, Known [SPIRV.ExecutionMode Nat] modes )
      => Known Definition ('EntryPoint stage modes)
      where
  known = AnnotateEntryPoint
            ( knownValue @stage
            , Set.fromList ( knownValue @modes )
            )

class KnownDefinitions (defs :: [ Symbol :-> Definition ]) where
  annotations :: ( Map Text (SPIRV.PointerTy, Set (SPIRV.Decoration Word32) )
                 , Map Text SPIRV.FunctionControl
                 , Map Text (SPIRV.Stage, Set (SPIRV.ExecutionMode Word32) )
                 )

instance KnownDefinitions '[] where
  annotations = ( Map.empty, Map.empty, Map.empty )

instance (Known Symbol k, Known Definition def, KnownDefinitions defs)
      => KnownDefinitions ((k ':-> def) ': defs)
      where
  annotations
    = let (g,f,e) = annotations @defs
          k = knownValue @k
      in case knownValue @def of
           AnnotateGlobal     x -> ( Map.insert k x g, f, e )
           AnnotateFunction   x -> ( g, Map.insert k x f, e )
           AnnotateEntryPoint x -> ( g, f, Map.insert k x e )


type family StartBindings (defs :: [ Symbol :-> Definition ]) :: [ Symbol :-> Binding ] where
  StartBindings '[]
    = '[]
  StartBindings ((k ':-> Global storage ty _) ': defs)
    = (k ':-> Variable (StoragePermissions storage) ty) ': StartBindings defs
  StartBindings ((k ':-> _) ': defs)
    = StartBindings defs

type family EndBindings (defs :: [ Symbol :-> Definition ]) :: [ Symbol :-> Binding ] where
  EndBindings '[]
    = '[]
  EndBindings ((k ':-> Global storage ty _) ': defs)
    = (k ':-> Variable (StoragePermissions storage) ty) ': EndBindings defs
  EndBindings ((k ':-> Function _ as b) ': defs)
    = (k ':-> Binding.Function as b) ': EndBindings defs
  EndBindings ((k ':-> EntryPoint stage _) ': defs)
    = (k ':-> Binding.EntryPoint stage) ': EndBindings defs

--------------------------------------------------------------------------

context :: forall defs. KnownDefinitions defs => CGContext
context = let (   userGlobals
                , userFunctions
                , userEntryPoints
                ) = annotations @defs
          in emptyContext
                { userGlobals
                , userFunctions
                , userEntryPoints
                }

--------------------------------------------------------------------------
-- helpful shorthand synonyms

type Global_ s ty   = Global s ty '[]
type Function_ as b = Function SPIRV.NoFunctionControl as b
type EntryPoint_ s  = EntryPoint s '[]
