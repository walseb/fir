{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module FIR.Definition where

-- base
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import Data.Word(Word32)
import GHC.TypeLits(Symbol, KnownSymbol, symbolVal)

-- containers
import Data.Map(Map)
import qualified Data.Map.Strict as Map

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

-- fir
import CodeGen.State(CGContext(..), emptyContext)
import Data.Type.Map((:->)((:->)))
import FIR.Binding(Binding(Variable), StoragePermissions)
import FIR.Prim.Singletons(PrimTy, primTy)
import qualified FIR.Binding as Binding
import qualified SPIRV.Decoration      as SPIRV
import qualified SPIRV.ExecutionMode   as SPIRV
import qualified SPIRV.FunctionControl as SPIRV
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.Stage           as SPIRV
import qualified SPIRV.Storage         as SPIRV

{-
-- for testing
import FIR.Binding(Var,R)
import Math.Linear(V)
import SPIRV.Decoration(Decoration(Location, Component))
import SPIRV.ExecutionMode(ExecutionMode(VertexOrderCcw, OutputVertices))
import SPIRV.FunctionControl(Inlineability(..), SideEffects(..))
import SPIRV.Stage(Stage(TessellationControl))
import SPIRV.Storage(StorageClass(Input,Output))


type ExampleDefinitions --( ExampleDefinitions :: [ Symbol :-> Definition ] )
  = '[ "color"  ':-> Global Input  (V 3 Float) '[ Location 0 ]
     , "size"   ':-> Global Input  Float       '[ Location 0, Component 3 ]
     , "out"    ':-> Global Output (V 4 Float) '[ Location 0 ]
     , "main"   ':-> EntryPoint TessellationControl '[ VertexOrderCcw, OutputVertices 128 ]
     , "choose" ':-> FunctionWithControl '( Just DontInline, Nothing ) '[ "n" ':-> Var R Int, "m" ':-> Var R Int ] Int 
     ]
-}

--------------------------------------------------------------------------
-- annotating top-level definitions with necessary information
-- for instance, annotating layout information

data Definition where
  GlobalWithDecoration :: SPIRV.StorageClass -> Type -> [ SPIRV.Decoration number ] -> Definition
  FunctionWithControl  :: SPIRV.FunctionControl -> [Symbol :-> Binding] -> Type -> Definition
  EntryPointWithModes  :: SPIRV.Stage -> [ SPIRV.ExecutionMode number ] -> Definition

type Global s ty   = GlobalWithDecoration s ty '[]
type Function as b = FunctionWithControl '(Nothing, Nothing) as b
type EntryPoint s  = EntryPointWithModes s '[]

data Annotate
  = AnnotateGlobal     (SPIRV.PrimTy, SPIRV.StorageClass, [SPIRV.Decoration Word32])
  | AnnotateFunction   SPIRV.FunctionControl
  | AnnotateEntryPoint (SPIRV.Stage, [SPIRV.ExecutionMode Word32])

class KnownDefinition (def :: Definition) where
  annotation :: Annotate

instance ( PrimTy ty, SPIRV.KnownStorage storage, SPIRV.KnownDecorations decs )
      => KnownDefinition (GlobalWithDecoration storage ty decs)
      where
  annotation = AnnotateGlobal ( primTy @ty, SPIRV.storage @storage, SPIRV.decorations @_ @decs)

instance SPIRV.KnownFunctionControl control
      => KnownDefinition (FunctionWithControl control args res)
      where
  annotation = AnnotateFunction (SPIRV.functionControl @control)

instance ( SPIRV.KnownStage stage, SPIRV.KnownExecutionModes modes )
      => KnownDefinition (EntryPointWithModes stage modes)
      where
  annotation = AnnotateEntryPoint
                  ( SPIRV.stageVal   (Proxy @stage)
                  , SPIRV.executionModes @_ @modes
                  )

class KnownDefinitions (defs :: [ Symbol :-> Definition ]) where
  annotations :: ( Map Text (SPIRV.PrimTy, SPIRV.StorageClass, [ SPIRV.Decoration Word32 ])
                 , Map Text SPIRV.FunctionControl
                 , Map Text (SPIRV.Stage, [ SPIRV.ExecutionMode Word32 ])
                 )

instance KnownDefinitions '[] where
  annotations = ( Map.empty, Map.empty, Map.empty )

instance (KnownSymbol k, KnownDefinition def, KnownDefinitions defs)
      => KnownDefinitions ((k ':-> def) ': defs)
      where
  annotations
    = let (g,f,e) = annotations @defs
          k = Text.pack (symbolVal (Proxy @k))
      in case annotation @def of 
           AnnotateGlobal     x -> ( Map.insert k x g, f, e )
           AnnotateFunction   x -> ( g, Map.insert k x f, e )
           AnnotateEntryPoint x -> ( g, f, Map.insert k x e )


type family StartBindings (defs :: [ Symbol :-> Definition ]) :: [ Symbol :-> Binding ] where
  StartBindings '[]
    = '[]
  StartBindings ((k ':-> GlobalWithDecoration storage ty _) ': defs)
    = (k ':-> Variable (StoragePermissions storage) ty) ': StartBindings defs
  StartBindings ((k ':-> _) ': defs)
    = StartBindings defs

type family EndBindings (defs :: [ Symbol :-> Definition ]) :: [ Symbol :-> Binding ] where
  EndBindings '[]
    = '[]
  EndBindings ((k ':-> GlobalWithDecoration storage ty _) ': defs)
    = (k ':-> Variable (StoragePermissions storage) ty) ': EndBindings defs
  EndBindings ((k ':-> FunctionWithControl _ as b) ': defs)
    = (k ':-> Binding.Function as b) ': EndBindings defs
  -- (currently not keeping track of entry points in the indexed AST monad)
  EndBindings ((_ ':-> EntryPointWithModes _ _) ': defs)
    = EndBindings defs

--------------------------------------------------------------------------

context :: forall defs. KnownDefinitions defs => CGContext
context = let (userGlobals, userFunctions, userEntryPoints) = annotations @defs
          in emptyContext { userGlobals, userFunctions, userEntryPoints }