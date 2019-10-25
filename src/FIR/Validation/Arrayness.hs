{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Arrayness

Validation of implicit arrayness.

Non-patch inputs to tessellation and geometry shaders,
and non-patch outputs of tessellation control shaders,
have an added layer of arrayness.

This module checks for the presence of these layers of arrayness,
and provides type families to peel off such implicit arrayness layers.
-}

module FIR.Validation.Arrayness where

-- base
import Data.Kind
  ( Type )
import Data.Type.Bool
  ( If )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.List
  ( Elem )
import Data.Type.String
  ( ShowNat )
import FIR.Prim.Array
  ( Array )
import qualified SPIRV.Decoration as SPIRV
  ( Decoration(..) )
import qualified SPIRV.Stage      as SPIRV
  ( ExecutionModel
  , ExecutionInfo(..), ShaderInfo(..) 
  , NamedExecutionModel
  )
import qualified SPIRV.Storage    as SPIRV
  ( StorageClass(Input, Output) )

-------------------------------------------------------------

data IsPatchVariable
  = IsPatch
  | NotPatch

data ImplicitArrayness
  = NoImplicitArrayness IsPatchVariable
  | ImplicitArrayness   Nat

type family UnderlyingType
              ( varName :: Symbol                     )
              ( inOut   :: SPIRV.StorageClass         )
              ( decs    :: [ SPIRV.Decoration Nat ]   )
              ( emName  :: Symbol                     )
              ( ty      :: Type                       )
              ( xnfo    :: SPIRV.ExecutionInfo Nat em )
            :: Type
            where
  UnderlyingType varName inOut decs emName ty ( xnfo :: SPIRV.ExecutionInfo Nat em )
    = RemoveArrayness varName inOut emName em ty
        ( Arrayness inOut decs xnfo )

type family Arrayness
              ( inOut :: SPIRV.StorageClass         )
              ( decs  :: [ SPIRV.Decoration Nat ]   )
              ( xnfo  :: SPIRV.ExecutionInfo Nat em )
          :: ImplicitArrayness
          where
  Arrayness SPIRV.Input  decs
    ( SPIRV.ShaderExecutionInfo ( SPIRV.TessellationControlShaderInfo i _ _ ) )
      = If ( SPIRV.Patch `Elem` decs )
           ( 'NoImplicitArrayness IsPatch )
           ( 'ImplicitArrayness i )
  Arrayness SPIRV.Input  decs
    ( SPIRV.ShaderExecutionInfo ( SPIRV.TessellationEvaluationShaderInfo i _ ) )
      = If ( SPIRV.Patch `Elem` decs )
           ( 'NoImplicitArrayness IsPatch )
           ( 'ImplicitArrayness i )
  Arrayness SPIRV.Input  decs
    ( SPIRV.ShaderExecutionInfo ( SPIRV.GeometryShaderInfo i _ ) )
      = 'ImplicitArrayness i
  Arrayness SPIRV.Output decs
    ( SPIRV.ShaderExecutionInfo ( SPIRV.TessellationControlShaderInfo _ j _ ) )
      = If ( SPIRV.Patch `Elem` decs )
           ( 'NoImplicitArrayness IsPatch )
           ( 'ImplicitArrayness j )
  Arrayness _ _ _ = 'NoImplicitArrayness NotPatch


type family RemoveArrayness
              ( varName   :: Symbol )
              ( inOut     :: SPIRV.StorageClass )
              ( emName     :: Symbol )
              ( em        :: SPIRV.ExecutionModel )
              ( ty        :: Type  )
              ( arrayness :: ImplicitArrayness )
          :: Type
          where
  RemoveArrayness _       _     _      _  ty           (NoImplicitArrayness _) = ty
  -- if an implicit level of arrayness is detected,
  -- remove it (ignoring the specific array length, as Vulkan does)
  RemoveArrayness _       _     _      _  (Array _ a) ('ImplicitArrayness _) = a
  RemoveArrayness varName inOut emName em ty          ('ImplicitArrayness i)
    = TypeError
        (    ShowType inOut :<>: Text " per-vertex variable " :<>: ShowType varName
        :<>: Text " of " :<>: Text (SPIRV.NamedExecutionModel emName em)
        :<>: Text " should be an array,"
        :$$: Text "but its type is " :<>: ShowType ty :<>: Text "."
        )

type family ArraynessNote
              ( em1        :: SPIRV.ExecutionModel )
              ( name1      :: Symbol               )
              ( arrayness1 :: ImplicitArrayness    )
              ( em2        :: SPIRV.ExecutionModel )
              ( name2      :: Symbol               )
              ( arrayness2 :: ImplicitArrayness    )
            :: ErrorMessage
            where
  ArraynessNote
    em1 name1 ('ImplicitArrayness  i1)
    _   _     ('NoImplicitArrayness _)
      =    Text ""
      :$$: Text "Note that per-vertex outputs of "
      :<>: Text ( SPIRV.NamedExecutionModel name1 em1 )
      :<>: Text " must be arrays of size " :<>: Text (ShowNat i1) :<>: Text "."
  ArraynessNote
    _   _     ('NoImplicitArrayness _)
    em2 name2 ('ImplicitArrayness  i2)
      =    Text ""
      :$$: Text "Note that per-vertex inputs of "
      :<>: Text ( SPIRV.NamedExecutionModel name2 em2 )
      :<>: Text " must be arrays of size " :<>: Text (ShowNat i2) :<>: Text "."
  ArraynessNote
    em1 name1 ('ImplicitArrayness i1)
    em2 name2 ('ImplicitArrayness i2)
      =    Text ""
      :$$: Text "Note that:"
      :$$: Text "  - per-vertex outputs of "
      :<>: Text ( SPIRV.NamedExecutionModel name1 em1 )
      :<>: Text " must be arrays of size " :<>: Text (ShowNat i1) :<>: Text ","
      :$$: Text "  - per-vertex inputs of "
      :<>: Text ( SPIRV.NamedExecutionModel name2 em2 )
      :<>: Text " must be arrays of size " :<>: Text (ShowNat i2) :<>: Text "."
  ArraynessNote
    _ _ ('NoImplicitArrayness _)
    _ _ ('NoImplicitArrayness _)
      = Text ""
