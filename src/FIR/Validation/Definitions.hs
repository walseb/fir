{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Definitions

Validate individual top-level "FIR.Definition.Definition"s.

For instance, this checks that uniform buffer objects are
decorated with the required binding and descriptor set indices.

Does __not__ perform validation of entry point interfaces,
i.e. input/output variables. See "FIR.Validation.Interface".
This is because it is not possible to validate layout using only the definitions,
as the layout validity of an entry point interface depends on
the specific entry-point it is used by.
For instance, an arrayed input means something different in
a tessellation evaluation shader than in a vertex shader,
because of implicit arrayness levels. See "FIR.Validation.Arrayness".

-}

module FIR.Validation.Definitions
  ( ValidDefinitions )
  where

-- base
import Data.Kind
  ( Type, Constraint )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Map
  ( (:->)((:->)) )
import FIR.Definition
  ( Definition(..)
  , TriagedDefinitions
  , TrieDefinitions
  )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Image
  ( Image )
import FIR.Prim.Struct
  ( Struct )
import FIR.ProgramState
  ( TLInterfaceVariable )
import qualified SPIRV.Decoration as SPIRV
  ( Decoration(..) )
import qualified SPIRV.Image      as SPIRV
  ( ImageUsage )
import qualified SPIRV.Image
  ( ImageUsage(Sampled, Storage) )
import qualified SPIRV.Storage    as SPIRV
  ( StorageClass )
import qualified SPIRV.Storage    as Storage
  ( StorageClass(..) )

------------------------------------------------------------------------

-- | Validate user-supplied top-level 'FIR.Definition.Definition's.
--
-- Note that this only validates global variables and their layout,
-- as other kinds of validation are performed elsewhere.
-- See for instance 'FIR.Definition.StartState'.
type ValidDefinitions ( defs :: [ Symbol :-> Definition ] )
  = ( ValidTriagedDefinitions ( TrieDefinitions defs ) :: Constraint )


type family ValidTriagedDefinitions ( defs :: TriagedDefinitions ) :: Constraint where
  ValidTriagedDefinitions '( _, _, globs )
    = ( ValidGlobals globs )
  --   * not validating functions: no validation necessary
  --   * not validating entry points: validation is done
  --   as part of computation of entry point info

type family ValidGlobals
              ( globs :: [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ] )
              :: Constraint
              where
  ValidGlobals '[] = ()
  ValidGlobals ( ( Storage.Input ':-> _ ) ': globs )
  -- no validity checking for input variables (see "FIR.Validation.Interface")
    = ValidGlobals globs
  ValidGlobals ( ( Storage.Output ':-> _ ) ': globs )
  -- no validity checking for output variables (see "FIR.Validation.Interface")
    = ValidGlobals globs
  ValidGlobals ( ( storage ':-> vars ) ': globs )
    = ( MapValidGlobal storage vars
      , ValidGlobals   globs
      )


type family MapValidGlobal
              ( storage :: SPIRV.StorageClass )
              ( globals :: [ Symbol :-> TLInterfaceVariable ] )
            :: Constraint
            where
  MapValidGlobal _ '[] = ()
  MapValidGlobal storage ( ( k ':-> '( decs, ty ) ) ': globals )
    = ( ValidGlobal  k storage decs ty
      , MapValidGlobal storage globals
      )

type family ValidGlobal
              ( name    :: Symbol                   )
              ( storage :: SPIRV.StorageClass       )
              ( decs    :: [ SPIRV.Decoration Nat ] )
              ( ty      :: Type                     )
              :: Constraint
              where
  ValidGlobal name Storage.UniformConstant decs (Image ty)
    = ( ValidUniformDecorations
        ( Text "Image named " :<>: ShowType name )
        decs
        (Image ty)
      , ValidImageDecorations name SPIRV.Image.Sampled decs
      )
  ValidGlobal name Storage.UniformConstant _ nonImageTy
    = TypeError
        (    Text "Uniform constant global named " :<>: ShowType name
        :<>: Text " expected to point to an image, but points to "
        :<>: ShowType nonImageTy :<>: Text " instead."
        )
  ValidGlobal name Storage.Image decs (Image ty)
    = ( ValidUniformDecorations
        ( Text "Storage image named " :<>: ShowType name )
        decs
        (Image ty)
      , ValidImageDecorations name SPIRV.Image.Storage decs
      )
  ValidGlobal name Storage.Image  _ nonImageTy
    = TypeError
        (    Text "Image global named " :<>: ShowType name
        :<>: Text " expected to point to an image, but points to "
        :<>: ShowType nonImageTy :<>: Text " instead."
        )
  ValidGlobal name Storage.Uniform decs (Struct as)
    = ValidUniformDecorations
        ( Text "Uniform buffer named " :<>: ShowType name )
        decs
        (Struct as)
  ValidGlobal name Storage.Uniform decs (Array n (Struct as))
    = ValidUniformDecorations
        ( Text "Uniform buffer named " :<>: ShowType name )
        decs
        (Array n (Struct as))
  ValidGlobal name Storage.Uniform _ ty
    = TypeError
        (    Text "Uniform buffer named " :<>: ShowType name
        :<>: Text " should be backed by a struct or array containing a struct;"
        :$$: Text "found type " :<>: ShowType ty :<>: Text " instead."
        )
  ValidGlobal name Storage.StorageBuffer decs (Struct as)
      = ValidUniformDecorations
          ( Text "Uniform storage buffer named " :<>: ShowType name )
          decs
          (Struct as)
  ValidGlobal name Storage.StorageBuffer decs (Array n (Struct as))
      = ValidUniformDecorations
          ( Text "Uniform storage buffer named " :<>: ShowType name )
          decs
          (Array n (Struct as))
  ValidGlobal name Storage.StorageBuffer _ ty
    = TypeError
        (    Text "Uniform storage buffer named " :<>: ShowType name
        :<>: Text " should be backed by a struct or array containing a struct;"
        :$$: Text "found type " :<>: ShowType ty :<>: Text " instead."
        )
  ValidGlobal _ _ _ _ = ()

type family ValidUniformDecorations
              ( name :: ErrorMessage             )
              ( decs :: [ SPIRV.Decoration Nat ] )
              ( ty   :: Type                     )
            :: Constraint
            where
  ValidUniformDecorations name decs _
    = ( HasBinding name decs, HasDescriptorSet name decs )

type family HasBinding
              ( name :: ErrorMessage             )
              ( decs :: [ SPIRV.Decoration Nat ] )
            :: Constraint
            where
  HasBinding _    ( SPIRV.Binding _ ': _ ) = ()
  HasBinding name ( _ ': decs ) = HasBinding name decs
  HasBinding name '[]
    = TypeError
        ( name :<>: Text " is missing a 'Binding' decoration." )

type family HasDescriptorSet
              ( name :: ErrorMessage             )
              ( decs :: [ SPIRV.Decoration Nat ] )
            :: Constraint
            where
  HasDescriptorSet _    ( SPIRV.DescriptorSet _ ': _ ) = ()
  HasDescriptorSet name ( _ ': decs ) = HasDescriptorSet name decs
  HasDescriptorSet name '[]
    = TypeError
        ( name :<>: Text " is missing a 'DescriptorSet' decoration." )

-- TODO: I'm not sure what the valid decorations are here...
type family ValidImageDecorations
              ( name    :: Symbol                  )
              ( imageTy :: SPIRV.ImageUsage        )
              ( decs    :: [ SPIRV.Decoration Nat] )
            :: Constraint
            where
  ValidImageDecorations _    _ '[]          = ()
  ValidImageDecorations name ty ( SPIRV.DescriptorSet _ ': decs )
    = ValidImageDecorations name ty decs
  ValidImageDecorations name ty ( SPIRV.Binding _ ': decs )
    = ValidImageDecorations name ty decs
  ValidImageDecorations name ty ( SPIRV.NonReadable ': decs )
    = ValidImageDecorations name ty decs
  ValidImageDecorations name ty ( SPIRV.NonWritable ': decs )
    = ValidImageDecorations name ty decs
  ValidImageDecorations name _ ( dec ': _ )
    = TypeError
        (    Text "Unexpected decoration " :<>: ShowType dec
        :<>: Text " applied to image named " :<>: ShowType name
        :<>: Text "."
        )
