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
> fragment :: ShaderModule "main" FragmentShader FragmentDefs _endState
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

See also "FIR.Validation.Definitions", which provides type families
used to validate user-provided definitions.
-}

module FIR.Definition where

-- base
import Data.Kind
  ( Type )
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
  ( (:->)((:->))
  , Insert, Union
  , Lookup
  )
import Data.Type.Maybe
  ( SequenceMaybe )
import FIR.Binding
  ( Binding(Variable), StoragePermissions )
import qualified FIR.Binding as Binding
  ( Binding(Function) )
import FIR.Prim.Image
  ( Image, knownImage, ImageProperties )
import FIR.Prim.Singletons
  ( PrimTy, primTy )
import FIR.ProgramState
  ( ProgramState(ProgramState)
  , Definedness(Declared)
  , FunctionInfo(FunctionInfo)
  , EntryPointInfo(EntryPointInfo), TLInterfaceVariable
  )
import qualified FIR.ProgramState as ProgramState
  ( FunctionContext(TopLevel) )
import FIR.Validation.Bindings
  ( InsertEntryPointInfo )
import FIR.Validation.ExecutionModes
  ( ValidateExecutionModes )
import qualified SPIRV.Capability    as SPIRV
  ( Capability )
import qualified SPIRV.Control       as SPIRV
  ( FunctionControl )
import qualified SPIRV.Decoration    as SPIRV
  ( Decoration, Decorations )
import qualified SPIRV.ExecutionMode as SPIRV
  ( ExecutionMode, ExecutionModes )
import qualified SPIRV.Extension     as SPIRV
  ( Extension )
import qualified SPIRV.Image         as SPIRV
  ( Image(imageUsage), ImageUsage(Sampled) )
import qualified SPIRV.PrimTy        as SPIRV
  ( PrimTy(Image, SampledImage)
  , PointerTy(PointerTy)
  )
import qualified SPIRV.Requirements  as SPIRV
  ( globalCapabilities, globalExtensions
  , executionModelCapabilities, executionModelExtensions
  )
import qualified SPIRV.Stage         as SPIRV
  ( ExecutionModel, ExecutionInfo )
import qualified SPIRV.Storage       as SPIRV
  ( StorageClass )
import qualified SPIRV.Storage       as Storage
  ( StorageClass(Input,Output) )

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
  deriving stock (Show, Eq, Generic)
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

instance ( Known Symbol k, Known Definition def, KnownDefinitions defs)
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

type Definitions = [ Symbol :-> Definition ]
-- recall:
-- type TLInterfaceVariable = ( [SPIRV.Decoration Nat], Type )
type TriagedDefinitions =
  ( [ Symbol :-> FunctionInfo ]
  , [ Symbol :-> (SPIRV.ExecutionModel, [ SPIRV.ExecutionMode Nat ]) ]
  , [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ]
  )

-- | Computes the starting indexed monadic state, from user-supplied
-- list of top level 'Definition's.
--
-- Performs a limited amount of validation:
--
--  * Checks there are no duplicate declarations.
--  * Validates entry point execution modes.
--
-- Validation of global variables (and their layout) is done separately,
-- see "FIR.Module".
type StartState (defs :: Definitions)
  = ( StartStateFromTriage ( TrieDefinitions defs ) :: ProgramState )

-- | Helper computation of the starting indexed monadic state, from triaged definitions.
--
--   * The insertion sorts will throw an error if there are duplicates.
--   * The computation of entry point infos performs validation of entry points.
--   * Validation of bindings is done separately, see "FIR.Module".
type family StartStateFromTriage
              ( triaged :: TriagedDefinitions )
            :: ProgramState where
  StartStateFromTriage '( funs, eps, globals )
    = 'ProgramState
        ( InsertionSortGlobals globals ) -- checks for duplicate globals
        ProgramState.TopLevel
        funs
        ( EntryPointInfos globals eps ) -- validates entry points and checks for duplicates

-- | Perform triage of user-supplied definitions,
-- so that different kinds of definition can be handled separately.
type family TrieDefinitions ( defs :: Definitions ) :: TriagedDefinitions where
  TrieDefinitions '[] = '( '[], '[], '[] )
  TrieDefinitions
    ( ( k ':-> Global storage decs ty ) ': defs )
    = TrieInsertGlobal k storage '(decs, ty) (TrieDefinitions defs)
  TrieDefinitions
    ( ( k ':-> Function fc as b ) ': defs )
    = TrieInsertFunctionInfo k ('FunctionInfo as b fc 'Declared) (TrieDefinitions defs)
  TrieDefinitions
    ( ( k ':-> EntryPoint modes em ) ': defs )
    = TrieAddEntryPoint k '(em, modes) (TrieDefinitions defs)
    -- in this case, insertion sorting (including duplicate checking)
    -- happens later (after computing entry point infos)

type family TrieInsertGlobal
              ( k :: Symbol )
              ( storage :: SPIRV.StorageClass  )
              ( global  :: TLInterfaceVariable )
              ( triaged :: TriagedDefinitions  )
            :: TriagedDefinitions
            where
  TrieInsertGlobal k storage glob '( funs, eps, globs )
    = '( funs, eps, InsertGlobal k storage glob globs )

type family InsertGlobal
              ( k :: Symbol )
              ( storage :: SPIRV.StorageClass )
              ( global  :: TLInterfaceVariable )
              ( globals :: [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ] )
            :: [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ]
            where
  InsertGlobal k storage global '[] = '[ storage ':-> '[ k ':-> global ] ]
  InsertGlobal k storage global
    ( (storage ':-> globals ) ': others )
    = ( storage ':-> ( Insert k global globals ) ) ': others
  InsertGlobal k storage global ( others ': globals )
    = others ': InsertGlobal k storage global globals

type family TrieInsertFunctionInfo
              ( k :: Symbol )
              ( nfo :: FunctionInfo )
              ( triaged :: TriagedDefinitions )
            :: TriagedDefinitions
            where
  TrieInsertFunctionInfo k nfo '( funs, eps, globs )
    = '( Insert k nfo funs, eps, globs )

type family TrieAddEntryPoint
              ( k :: Symbol )
              ( ep :: ( SPIRV.ExecutionModel, [SPIRV.ExecutionMode Nat] ) )
              ( triaged :: TriagedDefinitions )
            :: TriagedDefinitions
            where
  TrieAddEntryPoint k ep '( funs, eps, globs )
    = '( funs, ( ( k ':-> ep) ': eps ), globs )

type family EntryPointInfos
              ( globals :: [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ] )
              ( entryPoints :: [ Symbol :-> (SPIRV.ExecutionModel, [ SPIRV.ExecutionMode Nat ]) ] )
            :: [ Symbol :-> EntryPointInfo ]
            where
  EntryPointInfos _       '[] = '[]
  EntryPointInfos globals ( ( k ':-> '( em, modes ) ) ': eps )
    = InsertMaybeEntryPointInfo k globals
        ( ValidateExecutionModes k em modes )
        ( EntryPointInfos globals eps )

type family DefinitionFunctions
              ( defs :: [ Symbol :-> Definition ] )
              :: [Symbol :-> FunctionInfo]
              where
  DefinitionFunctions '[] = '[]
  DefinitionFunctions ( (k ':-> Function fc as b) ': defs )
    = Insert k ('FunctionInfo as b fc Declared) (DefinitionFunctions defs)
  DefinitionFunctions ( _ ': defs ) -- not a function
    = DefinitionFunctions defs


type family InsertMaybeEntryPointInfo
              ( k    :: Symbol                                )
              ( globals :: [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ] )
              ( nfo  :: Maybe (SPIRV.ExecutionInfo Nat stage) )
              ( nfos :: [ Symbol :-> EntryPointInfo ]         )
              :: [ Symbol :-> EntryPointInfo ]
              where
  InsertMaybeEntryPointInfo _ _ 'Nothing _
  -- 'ValidateExecutionModes' never returns Nothing, it throws a type error instead
  -- this trick forces evaluation of 'ValidateExecutionModes'
  -- ( this fixes [issue #43](https://gitlab.com/sheaf/fir/issues/43) )
    = TypeError ( Text "Invalid entry point execution modes (unreachable)" )
  InsertMaybeEntryPointInfo k globals ('Just nfo) nfos
    = InsertEntryPointInfo k
        ( 'EntryPointInfo nfo
            '( SequenceMaybe (Lookup Storage.Input  globals)
             , SequenceMaybe (Lookup Storage.Output globals)
             )
            'Declared )
        nfos

-- | Grabs the input/output variables in the given definition.
--
-- This is a workaround for the fact that shader interfaces are not kept track of
-- at the type level.
-- This is permissible as long as the interface for the entry-point consists precisely
-- of the 'Input'/'Output' variables provided in the top-level annotation.
-- See [FIR issue #51](https://gitlab.com/sheaf/fir/issues/51).
type family VariablesWithStorage
              ( storage :: SPIRV.StorageClass      )
              ( defs    :: [Symbol :-> Definition] )
            :: [ Symbol :-> TLInterfaceVariable ]
            where
  VariablesWithStorage storage '[] = '[]
  VariablesWithStorage storage ( ( varName ':-> Global storage decs ty) ': defs )
    = ( varName ':-> '(decs, ty) ) ': VariablesWithStorage storage defs
  VariablesWithStorage storage ( _ ': defs )
    = VariablesWithStorage storage defs

type family InsertionSortGlobals
               ( globals :: [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ] )
            :: [ Symbol :-> Binding ]
            where
  InsertionSortGlobals '[] = '[]
  InsertionSortGlobals ( ( storage ':-> globals ) ': others )
    = Union ( InsertionSortGlobalsWith storage globals ) ( InsertionSortGlobals others )

type family InsertionSortGlobalsWith
              ( storage :: SPIRV.StorageClass )
              ( globals :: [ Symbol :-> TLInterfaceVariable ] )
            :: [ Symbol :-> Binding ]
            where
  InsertionSortGlobalsWith _ '[] = '[]
  InsertionSortGlobalsWith storage ( ( k ':-> '( decs, ty ) ) ': globals )
    = Insert k
        ( Variable (StoragePermissions storage) ty )
        ( InsertionSortGlobalsWith storage globals )


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
