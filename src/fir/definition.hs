{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PackageImports        #-}
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
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import CodeGen.State
  ( CGContext(..), emptyContext )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.Map
  ( (:->)((:->)), Insert )
import FIR.Binding
  ( Binding(Variable), StoragePermissions )
import FIR.Instances.Bindings
  ( InsertEntryPointInfo )
import qualified FIR.Binding as Binding
import FIR.ASTState
  ( ASTState(ASTState), EntryPointInfo(EntryPointInfo) )
import qualified FIR.ASTState as ASTState
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Image
  ( Image, knownImage, ImageProperties )
import FIR.Prim.Singletons
  ( PrimTy, primTy )
import FIR.Prim.Struct
  ( Struct )
import qualified SPIRV.Decoration      as SPIRV
import qualified SPIRV.ExecutionMode   as SPIRV
import qualified SPIRV.FunctionControl as SPIRV
import qualified SPIRV.Image           as SPIRV
  ( Image(imageUsage), ImageUsage(Sampled) )
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.Stage           as SPIRV
import qualified SPIRV.Storage         as SPIRV
  ( StorageClass )
import qualified SPIRV.Storage         as Storage
  ( StorageClass(..) )

--------------------------------------------------------------------------
-- annotating top-level definitions with necessary information
-- for instance, annotating layout information

data Definition where
  Global     :: SPIRV.StorageClass -> [ SPIRV.Decoration Nat ] -> Type -> Definition
  Function   :: SPIRV.FunctionControl -> [Symbol :-> Binding] -> Type -> Definition
  EntryPoint :: [ SPIRV.ExecutionMode Nat ] -> SPIRV.Stage -> Definition

data Annotate
  = AnnotateGlobal     ( SPIRV.PointerTy, SPIRV.Decorations )
  | AnnotateFunction   SPIRV.FunctionControl
  | AnnotateEntryPoint ( SPIRV.Stage, SPIRV.ExecutionModes )

instance Demotable Definition where
  type Demote Definition = Annotate

-- workaround for image types being opaque and not having a Haskell-level counterpart
instance {-# OVERLAPPING #-}
         ( Known ImageProperties        props
         , Known SPIRV.StorageClass     storage
         , Known [SPIRV.Decoration Nat] decs
         )
      => Known Definition ('Global storage decs (Image props))
      where
  known = AnnotateGlobal
    ( SPIRV.PointerTy ( knownValue @storage ) imgTy
    , Set.fromList    ( knownValue @decs )
    )
        where imgTy :: SPIRV.PrimTy
              imgTy = case knownImage @props of
                        img -> case SPIRV.imageUsage img of
                                Just SPIRV.Sampled -> SPIRV.SampledImage img
                                _                  -> SPIRV.Image        img

instance ( PrimTy ty, Known SPIRV.StorageClass storage, Known [SPIRV.Decoration Nat] decs )
      => Known Definition ('Global storage decs ty)
      where
  known = AnnotateGlobal
    ( SPIRV.PointerTy ( knownValue @storage ) ( primTy @ty )
    , Set.fromList    ( knownValue @decs )
    )

instance Known SPIRV.FunctionControl control
      => Known Definition ('Function control args res)
      where
  known = AnnotateFunction (knownValue @control)

instance ( Known SPIRV.Stage stage, Known [SPIRV.ExecutionMode Nat] modes )
      => Known Definition ('EntryPoint modes stage)
      where
  known = AnnotateEntryPoint
            ( knownValue @stage
            , Set.fromList ( knownValue @modes )
            )

class KnownDefinitions (defs :: [ Symbol :-> Definition ]) where
  annotations :: ( Map Text                ( SPIRV.PointerTy, SPIRV.Decorations )
                 , Map Text                SPIRV.FunctionControl
                 , Map (Text, SPIRV.Stage) SPIRV.ExecutionModes
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
           AnnotateGlobal     x      -> ( Map.insert k x g, f, e )
           AnnotateFunction   x      -> ( g, Map.insert k x f, e )
           AnnotateEntryPoint (s, x) -> ( g, f, Map.insert l x e )
              where l :: ( Text, SPIRV.Stage )
                    l = (k,s)

type family StartState (defs :: [ Symbol :-> Definition ]) :: ASTState where
  StartState defs
    = 'ASTState (StartBindings defs) ASTState.TopLevel '[]

type family EndState (defs :: [ Symbol :-> Definition ]) :: ASTState where
  EndState defs
    = 'ASTState (EndBindings defs) ASTState.TopLevel (DefinitionEntryPoints defs)

type family DefinitionEntryPoints
              ( defs :: [ Symbol :-> Definition ] )
              :: [ EntryPointInfo ]
              where
  DefinitionEntryPoints '[] = '[]
  DefinitionEntryPoints ( (k ':-> EntryPoint modes stage) ': defs )
    = DefinitionInsertEntryPointInfo k
        ( SPIRV.ValidateExecutionModes k stage modes )
        ( DefinitionEntryPoints defs )
  DefinitionEntryPoints ( _ ': defs )
    = DefinitionEntryPoints defs

type family DefinitionInsertEntryPointInfo
              ( k    :: Symbol                            )
              ( nfo  :: Maybe (SPIRV.StageInfo Nat stage) )
              ( nfos :: [ EntryPointInfo ]                )
              :: [ EntryPointInfo ]
              where
  DefinitionInsertEntryPointInfo _ 'Nothing _
  -- 'SPIRV.ValidateExecutionModes' never returns Nothing, it throws a type error instead
  -- this trick forces evaluation of "SPIRV.ValidateExecutionModes"
  -- ( this fixes [issue #43](https://gitlab.com/sheaf/fir/issues/43) )
    = TypeError ( 'Text "Invalid entry point execution modes (unreachable)" )
  DefinitionInsertEntryPointInfo k ('Just nfo) nfos
    = InsertEntryPointInfo ('EntryPointInfo k nfo) nfos

type family StartBindings (defs :: [ Symbol :-> Definition ]) :: [ Symbol :-> Binding ] where
  StartBindings '[]
    = '[]
  StartBindings ((k ':-> Global storage _ ty) ': defs)
    = Insert k (Variable (StoragePermissions storage) ty) (StartBindings defs)
  StartBindings ((k ':-> _) ': defs)
    = StartBindings defs

type family EndBindings (defs :: [ Symbol :-> Definition ]) :: [ Symbol :-> Binding ] where
  EndBindings '[]
    = '[]
  EndBindings ((k ':-> Global storage _ ty) ': defs)
    = Insert k (Variable (StoragePermissions storage) ty) (EndBindings defs)
  EndBindings ((k ':-> Function _ as b) ': defs)
    = Insert k (Binding.Function as b) (EndBindings defs)
  EndBindings (_ ': defs)
    = EndBindings defs

type family ValidateGlobal
              ( storage :: SPIRV.StorageClass       )
              ( decs    :: [ SPIRV.Decoration Nat ] )
              ( ty      :: Type                     )
              :: Maybe Type -- returns 'Just ty' if block/layout decorations are needed (onto the type 'ty')
              where
  ValidateGlobal Storage.UniformConstant '[] (Image ty) = Nothing
  ValidateGlobal Storage.UniformConstant (dec ': _) (Image _)
    = TypeError
        ( 'Text "Invalid decoration " :<>: ShowType dec :<>: 'Text " applied to image." )
  ValidateGlobal Storage.UniformConstant _ nonImageTy
    = TypeError
        (    'Text "Uniform constant global expected to point to an image, but points to "
        :<>: ShowType nonImageTy :<>: 'Text " instead."
        )
  ValidateGlobal Storage.Image '[] (Image ty) = Nothing
  ValidateGlobal Storage.Image  (dec ': _) (Image _)
    = TypeError
        ( 'Text "Invalid decoration " :<>: ShowType dec :<>: 'Text " applied to image." )
  ValidateGlobal Storage.Image  _ nonImageTy
    = TypeError
        (    'Text "Image global expected to point to an image, but points to "
        :<>: ShowType nonImageTy :<>: 'Text " instead."
        )
  -- TODO: check the decorations provided
  ValidateGlobal Storage.Uniform _ (Struct as) = Just (Struct as)
  ValidateGlobal Storage.Uniform _ (Array n (Struct as)) = Just (Array n (Struct as))
  ValidateGlobal Storage.Uniform _ ty
    = TypeError
        (    'Text "Uniform buffer should be backed by a struct or array containing a struct;"
        :$$: 'Text "found type " :<>: ShowType ty :<>: 'Text " instead."
        )
  ValidateGlobal Storage.StorageBuffer _ (Struct as) = Just (Struct as)
  ValidateGlobal Storage.StorageBuffer _ (Array n (Struct as)) = Just (Array n (Struct as))
  ValidateGlobal Storage.StorageBuffer _ ty
    = TypeError
        (    'Text "Uniform buffer should be backed by a struct or array containing a struct;"
        :$$: 'Text "found type " :<>: ShowType ty :<>: 'Text " instead."
        )
  -- TODO
  ValidateGlobal Storage.Input  _ _ = Nothing
  ValidateGlobal Storage.Output _ _ = Nothing
  -- TODO
  ValidateGlobal _ _ _ = Nothing


--------------------------------------------------------------------------

initialCGContext :: forall defs. KnownDefinitions defs => CGContext
initialCGContext =
  let (   userGlobals
        , userFunctions
        , userEntryPoints
        ) = annotations @defs
  in emptyContext
        { userGlobals
        , userFunctions
        , userEntryPoints
        }

