{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Definition

In this module, a definition refers to a type-level annotation the user
can/must provide to describe the interface of a SPIR-V module.

For example, a typical fragment shader for mapping a texture could
declare the following definitions:

> type FragmentDefs =
>   '[ "in_colour"   ':-> Input      '[ Location 0 ]                 (V 4 Float)
>    , "in_uv"       ':-> Input      '[ Location 1 ]                 (V 2 Float)
>    , "texture"     ':-> Texture2D  '[ Binding 0, DescriptorSet 0 ] (RGBA8 UNorm)
>    , "out_colour"  ':-> Output     '[ Location 0 ]                 (V 4 Float)
>    , "main"        ':-> EntryPoint '[ OriginUpperLeft ]            Fragment
>    ]
>
> fragment :: ShaderStage "main" FragmentShader FragmentDefs _endState
> fragment = ...

There are three types of definitions:

  * global variables that belong to the interface (inputs, outputs, uniform constants such as textures, etc),
  * functions that are going to be defined,
  * entry points that are going to be defined.

In the above example all definitions are of the first kind,
save for the last definition which declares an entry point.

The definitions allow the user to specify, within a SPIR-V module, detailed information about the interface.

For instance, in the above example, the inputs/outputs are annotated with location information,
whereas the texture (a uniform constant) is annotated with its binding and descriptor set.
See "FIR.Layout" for further details concerning such annotations.

On the other hand, the execution model (in this case the fragment shader) is given its execution modes
(in this case, specifying that the origin for the coordinate system used by fragments is
in the upper-left corner).
-}

module FIR.Definition where

-- base
import Data.Kind
  ( Type )
import Data.Type.Bool
  ( If, type (&&) )
import GHC.Generics
  ( Generic )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- containers
import Data.Map
  ( Map )
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( fromList )

-- lens
import Control.Lens
  ( Lens', lens, at
  , set, over
  )

-- generic-monoid
import Data.Monoid.Generic
  ( GenericSemigroup(..), GenericMonoid(..) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import CodeGen.State
  ( CGContext(..), emptyContext )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import Data.Type.Map
  ( (:->)((:->)), Insert )
import FIR.ASTState
  ( ASTState(ASTState)
  , Definedness(Declared)
  , FunctionInfo(FunctionInfo)
  , EntryPointInfo(EntryPointInfo)
  )
import qualified FIR.ASTState as ASTState
  ( FunctionContext(TopLevel) )
import FIR.Binding
  ( Binding(Variable), StoragePermissions )
import FIR.Instances.Bindings
  ( InsertEntryPointInfo )
import qualified FIR.Binding as Binding
  ( Binding(Function) )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Image
  ( Image, knownImage, ImageProperties )
import FIR.Prim.Singletons
  ( PrimTy, primTy )
import FIR.Prim.Struct
  ( Struct )
import qualified SPIRV.Capability      as SPIRV
  ( Capability )
import qualified SPIRV.Control         as SPIRV
  ( FunctionControl )
import qualified SPIRV.Decoration      as SPIRV
  ( Decoration(Binding, DescriptorSet)
  , Decorations
  )
import qualified SPIRV.ExecutionMode   as SPIRV
  ( ExecutionMode, ExecutionModes
  , ValidateExecutionModes
  )
import qualified SPIRV.Extension       as SPIRV
  ( Extension )
import qualified SPIRV.Image           as SPIRV
  ( Image(imageUsage), ImageUsage(Sampled) )
import qualified SPIRV.PrimTy          as SPIRV
  ( PrimTy(Image, SampledImage)
  , PointerTy(PointerTy)
  )
import qualified SPIRV.Requirements    as SPIRV
  ( globalCapabilities, globalExtensions
  , executionModelCapabilities, executionModelExtensions
  )
import qualified SPIRV.Stage           as SPIRV
  ( ExecutionModel, ExecutionInfo )
import qualified SPIRV.Storage         as SPIRV
  ( StorageClass )
import qualified SPIRV.Storage         as Storage
  ( StorageClass(..) )

--------------------------------------------------------------------------
-- annotating top-level definitions with necessary information
-- for instance, annotating layout information

-- | Definitions are top-level type-level annotations which provide
-- the necessary information for a SPIR-V module.
data Definition where
  Global     :: SPIRV.StorageClass -> [ SPIRV.Decoration Nat ] -> Type -> Definition
  Function   :: SPIRV.FunctionControl -> [Symbol :-> Binding] -> Type -> Definition
  EntryPoint :: [ SPIRV.ExecutionMode Nat ] -> SPIRV.ExecutionModel -> Definition

data Annotate
  = AnnotateGlobal     ( SPIRV.PointerTy, SPIRV.Decorations )
  | AnnotateFunction   SPIRV.FunctionControl
  | AnnotateEntryPoint ( SPIRV.ExecutionModel, SPIRV.ExecutionModes )

data Annotations
  = Annotations
    { globalAnnotations      :: Map  ShortText                        ( SPIRV.PointerTy, SPIRV.Decorations )
    , functionAnnotations    :: Map  ShortText                        SPIRV.FunctionControl
    , executionAnnotations   :: Map (ShortText, SPIRV.ExecutionModel) SPIRV.ExecutionModes
    , annotationCapabilities :: Set                                   SPIRV.Capability
    , annotationExtensions   :: Set                                   SPIRV.Extension
    }
  deriving (Show, Eq, Generic)
  deriving Semigroup via GenericSemigroup Annotations
  deriving Monoid    via GenericMonoid    Annotations

_globalAnnotations :: Lens' Annotations (Map ShortText (SPIRV.PointerTy, SPIRV.Decorations))
_globalAnnotations = lens globalAnnotations ( \anns v -> anns { globalAnnotations = v } )

_globalAnnotation :: ShortText -> Lens' Annotations (Maybe (SPIRV.PointerTy, SPIRV.Decorations))
_globalAnnotation name = _globalAnnotations . at name

_functionAnnotations :: Lens' Annotations (Map ShortText SPIRV.FunctionControl)
_functionAnnotations = lens functionAnnotations ( \anns v -> anns { functionAnnotations = v } )

_functionAnnotation :: ShortText -> Lens' Annotations (Maybe SPIRV.FunctionControl)
_functionAnnotation name = _functionAnnotations . at name

_executionAnnotations :: Lens' Annotations (Map (ShortText, SPIRV.ExecutionModel) SPIRV.ExecutionModes)
_executionAnnotations = lens executionAnnotations ( \anns v -> anns { executionAnnotations = v } )

_executionAnnotation
  :: ShortText -> SPIRV.ExecutionModel
  -> Lens' Annotations (Maybe SPIRV.ExecutionModes)
_executionAnnotation name em = _executionAnnotations . at (name, em)

_annotationCapabilities :: Lens' Annotations (Set SPIRV.Capability)
_annotationCapabilities = lens annotationCapabilities ( \anns v -> anns { annotationCapabilities = v } )

_annotationExtensions :: Lens' Annotations (Set SPIRV.Extension)
_annotationExtensions = lens annotationExtensions ( \anns v -> anns { annotationExtensions = v } )

--------------------------------------------------------------------------
-- reification of type-level definitions/annotations

initialCGContext :: forall defs. KnownDefinitions defs => CGContext
initialCGContext =
  let Annotations {..} = annotations @defs
  in  emptyContext
        { userGlobals      = globalAnnotations
        , userFunctions    = functionAnnotations
        , userEntryPoints  = executionAnnotations
        , userCapabilities = annotationCapabilities
        , userExtensions   = annotationExtensions
        }

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
    , Set.fromList    ( knownValue @decs    )
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
    , Set.fromList    ( knownValue @decs    )
    )

instance Known SPIRV.FunctionControl control
      => Known Definition ('Function control args res)
      where
  known = AnnotateFunction ( knownValue @control )

instance ( Known SPIRV.ExecutionModel stage, Known [SPIRV.ExecutionMode Nat] modes )
      => Known Definition ('EntryPoint modes stage)
      where
  known = AnnotateEntryPoint
            ( knownValue @stage
            , Set.fromList ( knownValue @modes )
            )

class KnownDefinitions (defs :: [ Symbol :-> Definition ]) where
  annotations :: Annotations

instance KnownDefinitions '[] where
  annotations = mempty

instance (Known Symbol k, Known Definition def, KnownDefinitions defs)
      => KnownDefinitions ((k ':-> def) ': defs)
      where
  annotations
    = let anns = annotations @defs
          k    = knownValue @k
      in case knownValue @def of
           AnnotateGlobal     x@(SPIRV.PointerTy _ ty, _)
             -> set ( _globalAnnotation    k  ) (Just x)
              . over _annotationCapabilities    (SPIRV.globalCapabilities ty <>)
              . over _annotationExtensions      (SPIRV.globalExtensions   ty <>)
              $ anns
           AnnotateFunction   x
             -> set ( _functionAnnotation  k  ) (Just x) anns
           AnnotateEntryPoint (s, x)
             -> set ( _executionAnnotation k s) (Just x)
              . over _annotationCapabilities    (SPIRV.executionModelCapabilities s <>)
              . over _annotationExtensions      (SPIRV.executionModelExtensions   s <>)
              $ anns

--------------------------------------------------------------------------
-- computing type-level states from type-level definitions

type family StartState (defs :: [ Symbol :-> Definition ]) :: ASTState where
  StartState defs
    = 'ASTState
         ( StartBindings         defs )
         ASTState.TopLevel
         ( DefinitionFunctions   defs )
         ( DefinitionEntryPoints defs )

type family DefinitionFunctions
              ( defs :: [ Symbol :-> Definition ] )
              :: [Symbol :-> FunctionInfo]
              where
  DefinitionFunctions '[] = '[]
  DefinitionFunctions ( (k ':-> Function fc as b) ': defs )
    = Insert k ('FunctionInfo as b fc Declared) (DefinitionFunctions defs)
  DefinitionFunctions ( _ ': defs ) -- not a function
    = DefinitionFunctions defs

type family DefinitionEntryPoints
              ( defs   :: [ Symbol :-> Definition ] )
              :: [ EntryPointInfo ]
              where
  DefinitionEntryPoints '[] = '[]
  DefinitionEntryPoints ( (k ':-> EntryPoint modes stage) ': defs )
    = DefinitionInsertEntryPointInfo k
        ( SPIRV.ValidateExecutionModes k stage modes )
        ( DefinitionEntryPoints defs )
  DefinitionEntryPoints ( _ ': defs ) -- not an entry point
    = DefinitionEntryPoints defs

type family DefinitionInsertEntryPointInfo
              ( k    :: Symbol                                )
              ( nfo  :: Maybe (SPIRV.ExecutionInfo Nat stage) )
              ( nfos :: [ EntryPointInfo ]                    )
              :: [ EntryPointInfo ]
              where
  DefinitionInsertEntryPointInfo _ 'Nothing _
  -- 'SPIRV.ValidateExecutionModes' never returns Nothing, it throws a type error instead
  -- this trick forces evaluation of "SPIRV.ValidateExecutionModes"
  -- ( this fixes [issue #43](https://gitlab.com/sheaf/fir/issues/43) )
    = TypeError ( Text "Invalid entry point execution modes (unreachable)" )
  DefinitionInsertEntryPointInfo k ('Just nfo) nfos
    = InsertEntryPointInfo
        ( 'EntryPointInfo k nfo '( '[], '[] ) 'Declared )
        nfos

type family StartBindings (defs :: [ Symbol :-> Definition ]) :: [ Symbol :-> Binding ] where
  StartBindings '[]
    = '[]
  StartBindings ((k ':-> Global storage _ ty) ': defs)
    = Insert k (Variable (StoragePermissions storage) ty) (StartBindings defs)
  ---- ^^^^^ TODO: we probably don't want to add all of these storage classes to the list of bindings?
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
        ( Text "Invalid decoration " :<>: ShowType dec :<>: Text " applied to image." )
  ValidateGlobal Storage.UniformConstant _ nonImageTy
    = TypeError
        (    Text "Uniform constant global expected to point to an image, but points to "
        :<>: ShowType nonImageTy :<>: Text " instead."
        )
  ValidateGlobal Storage.Image '[] (Image ty) = Nothing
  ValidateGlobal Storage.Image  (dec ': _) (Image _)
    = TypeError
        ( Text "Invalid decoration " :<>: ShowType dec :<>: Text " applied to image." )
  ValidateGlobal Storage.Image  _ nonImageTy
    = TypeError
        (    Text "Image global expected to point to an image, but points to "
        :<>: ShowType nonImageTy :<>: Text " instead."
        )
  ValidateGlobal Storage.Uniform decs (Struct as)
    = If ( ValidUniformDecorations decs (Struct as))
        ( Just (Struct as) )
        Nothing -- unreachable
  ValidateGlobal Storage.Uniform decs (Array n (Struct as))
    = If ( ValidUniformDecorations decs (Array n (Struct as)) )
        ( Just (Array n (Struct as)) )
        Nothing -- unreachable
  ValidateGlobal Storage.Uniform _ ty
    = TypeError
        (    Text "Uniform buffer should be backed by a struct or array containing a struct;"
        :$$: Text "found type " :<>: ShowType ty :<>: Text " instead."
        )
  ValidateGlobal Storage.StorageBuffer _ (Struct as) = Just (Struct as)
  ValidateGlobal Storage.StorageBuffer _ (Array n (Struct as)) = Just (Array n (Struct as))
  ValidateGlobal Storage.StorageBuffer _ ty
    = TypeError
        (    Text "Uniform storage buffer should be backed by a struct or array containing a struct;"
        :$$: Text "found type " :<>: ShowType ty :<>: Text " instead."
        )
  -- TODO
  ValidateGlobal Storage.Input  _ _ = Nothing
  ValidateGlobal Storage.Output _ _ = Nothing
  -- TODO
  ValidateGlobal _ _ _ = Nothing

type family ValidUniformDecorations
              ( decs :: [ SPIRV.Decoration Nat ] )
              ( ty   :: Type                     )
            :: Bool
            where
  ValidUniformDecorations decs _
    = HasBinding decs && HasDescriptorSet decs

type family HasBinding ( decs :: [ SPIRV.Decoration Nat ] ) :: Bool where
  HasBinding ( SPIRV.Binding _ ': _ ) = 'True
  HasBinding ( _ ': decs ) = HasBinding decs
  HasBinding '[]
    = TypeError
        ( Text "Uniform buffer is missing a 'Binding' decoration." )

type family HasDescriptorSet ( decs :: [ SPIRV.Decoration Nat ] ) :: Bool where
  HasDescriptorSet ( SPIRV.DescriptorSet _ ': _ ) = 'True
  HasDescriptorSet ( _ ': decs ) = HasDescriptorSet decs
  HasDescriptorSet '[]
    = TypeError
        ( Text "Uniform buffer is missing a 'DescriptorSet' decoration." )
