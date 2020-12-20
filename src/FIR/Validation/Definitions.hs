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
import Data.Type.Maybe
import Data.Type.Map
  (Map, UnionWithAppend, (:->)((:->)), InsertWithAppend )
import FIR.Definition
  ( Definition(..)
  , TriagedDefinitions
  , TrieDefinitions
  )
import FIR.Prim.Array
  ( Array, RuntimeArray )
import FIR.Prim.Image
  ( Image )
import FIR.Prim.RayTracing
  ( AccelerationStructure )
import FIR.Prim.Singletons
  ( HasOpaqueType )
import FIR.Prim.Struct
  ( Struct )
import FIR.ProgramState
  ( TLInterfaceVariable )
import FIR.Validation.Images
  ( ImageUsageFromProperties )
import qualified SPIRV.Decoration as SPIRV
  ( Decoration(..) )
import qualified SPIRV.Image      as SPIRV
  ( ImageUsage )
import qualified SPIRV.Image
  ( ImageUsage(..) )
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
  = ( ValidTriagedDefinitions ( TrieDefinitions defs ) :: Constraint)

-- | Validate that descriptor set and bindings don't overlap
type family NonOverlappingDescriptors ( globs :: [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ] )
              :: Constraint
              where
  NonOverlappingDescriptors list = EnsureNoDuplicates (BuildDescriptorList list)

type family BuildDescriptorList  ( globs :: [ SPIRV.StorageClass :-> [ Symbol :-> TLInterfaceVariable ] ] ) :: Map (Nat, Nat) [Symbol]
  where
    BuildDescriptorList '[] = '[]
    BuildDescriptorList  ( (storage ':-> vars) ': globs ) = UnionWithAppend (GetDescriptor vars)  (BuildDescriptorList globs)

type family AppendMaybeDescriptor ( mbDescNo :: Maybe Nat ) ( mbBdNo :: Maybe Nat ) ( name :: Symbol ) ( bds :: Map (Nat,Nat) [Symbol] ) :: Map (Nat,Nat) [Symbol] where
  AppendMaybeDescriptor ( Just descNo ) mbBdNo name bds = InsertWithAppend '( descNo, FromMaybe mbBdNo 0 ) name bds
  -- Ignore bindings which don't have a "DescriptorSet" decoration.
  AppendMaybeDescriptor _ _ _ bds = bds

type family GetDescriptor  ( globs :: [ Symbol :-> TLInterfaceVariable ] ) :: Map (Nat, Nat) [Symbol]
  where
    GetDescriptor '[] = '[]
    GetDescriptor  ( (k ':-> '( decs, ty )) ': globs ) =
      AppendMaybeDescriptor ( ExtractDescriptorSet decs ) ( ExtractBinding decs ) k ( GetDescriptor globs )

type family ExtractDescriptorSet  ( decs :: [ SPIRV.Decoration Nat ] ) :: Maybe Nat
  where
    ExtractDescriptorSet '[] = 'Nothing
    ExtractDescriptorSet  (SPIRV.DescriptorSet n ': globs) = 'Just n
    ExtractDescriptorSet  (_ ': globs) = ExtractDescriptorSet globs

type family ExtractBinding  ( decs :: [ SPIRV.Decoration Nat ] ) :: Maybe Nat
  where
    ExtractBinding '[] = 'Nothing
    ExtractBinding  (SPIRV.Binding n ': globs) = 'Just n
    ExtractBinding  (_ ': globs) = ExtractBinding globs    

type family EnsureNoDuplicates (as :: Map (Nat,Nat) [Symbol]) :: Constraint where
  EnsureNoDuplicates '[] = ()
  EnsureNoDuplicates ( ( '( descNo, bdNo ) ':-> names ) ': bds ) = ( NoDuplicates descNo bdNo names, EnsureNoDuplicates bds )

type family NoDuplicates ( descNo :: Nat ) ( bdNo :: Nat ) ( names :: [Symbol] ) :: Constraint where
  NoDuplicates _ _ '[] = ()
  NoDuplicates _ _ '[_] = ()
  NoDuplicates descNo bdNo names = TypeError
    (    Text "More than one descriptor with DescriptorSet " :<>: ShowType descNo
    :<>: Text " and Binding " :<>: ShowType bdNo :<>: Text ":"
    :$$: ShowType names
    )


type family ValidTriagedDefinitions ( defs :: TriagedDefinitions ) :: Constraint where
  ValidTriagedDefinitions '( _, _, globs )
    = ( ValidGlobals globs, NonOverlappingDescriptors globs)
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
  ValidGlobal name Storage.UniformConstant decs (Image props)
    = ( ValidUniformDecorations
        ( Text "Image named " :<>: ShowType name )
        decs
        (Image props)
      , ValidImageDecorations name (ImageUsageFromProperties props) decs
      )
  ValidGlobal name Storage.UniformConstant decs AccelerationStructure
    = ValidUniformDecorations
        ( Text "Acceleration structure named " :<>: ShowType name )
        decs
        AccelerationStructure
  ValidGlobal name Storage.UniformConstant _ otherTy
    = TypeError
        (    Text "Uniform constant global named " :<>: ShowType name
        :<>: Text " points to unexpected type "
        :<>: ShowType otherTy :<>: Text "."
        )
  ValidGlobal name Storage.Image decs (Image props)
    = ( ValidUniformDecorations
        ( Text "Storage image named " :<>: ShowType name )
        decs
        (Image props)
      , ValidImageDecorations name (ImageUsageFromProperties props) decs
      )
  ValidGlobal name Storage.Image  _ nonImageTy
    = TypeError
        (    Text "Image global named " :<>: ShowType name
        :<>: Text " expected to point to an image, but points to "
        :<>: ShowType nonImageTy :<>: Text " instead."
        )
  ValidGlobal name Storage.Uniform decs (Struct as)
    = ( ValidUniformDecorations
          ( Text "Uniform buffer named " :<>: ShowType name )
          decs
          (Struct as)
      , NoOpaqueTypes name ( Text "buffer" ) as
      )
  ValidGlobal name Storage.Uniform decs (Array n (Struct as))
    = ( ValidUniformDecorations
          ( Text "Uniform buffer named " :<>: ShowType name )
          decs
          (Array n (Struct as))
      , NoOpaqueTypes name ( Text "buffer" ) as
      )
  ValidGlobal name Storage.Uniform _ ty
    = TypeError
        (    Text "Uniform buffer named " :<>: ShowType name
        :<>: Text " should be backed by a struct or a fixed-size array containing a struct;"
        :$$: Text "found type " :<>: ShowType ty :<>: Text " instead."
        )
  ValidGlobal name Storage.StorageBuffer decs (Struct as)
      = ( ValidUniformDecorations
            ( Text "Uniform storage buffer named " :<>: ShowType name )
            decs
            (Struct as)
        , NoOpaqueTypesExceptRuntimeArrayLast name as
        )
  ValidGlobal name Storage.StorageBuffer decs (Array n (Struct as))
      = ( ValidUniformDecorations
            ( Text "Uniform storage buffer named " :<>: ShowType name )
            decs
            (Array n (Struct as))
        , NoOpaqueTypes name ( Text "storage buffer" ) as
        )
  ValidGlobal name Storage.StorageBuffer decs (RuntimeArray a)
    = TypeError
        (    Text "Uniform storage buffer named " :<>: ShowType name
        :<>: Text " should be backed by a struct or a fixed-size array containing a struct;"
        :$$: Text "found type " :<>: ShowType (RuntimeArray a) :<>: Text " instead."
        :$$: Text "To use a dynamically sized array in a storage buffer, define a structure"
        :<>: Text " whose last member is a RuntimeArray."
        )
  ValidGlobal name Storage.StorageBuffer _ ty
    = TypeError
        (    Text "Uniform storage buffer named " :<>: ShowType name
        :<>: Text " should be backed by a struct or fixed-size array containing a struct;"
        :$$: Text "found type " :<>: ShowType ty :<>: Text " instead."
        )
  -- TODO: more validation for ray-tracing storage classes
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

type family HasLocation
              ( name :: Symbol                   )
              ( decs :: [ SPIRV.Decoration Nat ] )
              :: Constraint
              where
  HasLocation _    ( SPIRV.Location _ ': _ ) = ()
  HasLocation name ( _ ': decs ) = HasLocation name decs
  HasLocation name '[]
    = TypeError
        ( ShowType name :<>: Text " is missing a 'Location' decoration." )

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
  ValidImageDecorations name SPIRV.Image.Storage ( SPIRV.NonWritable ': decs )
    = ValidImageDecorations name SPIRV.Image.Storage decs
  ValidImageDecorations name _ ( dec ': _ )
    = TypeError
        (    Text "Unexpected decoration " :<>: ShowType dec
        :<>: Text " applied to image named " :<>: ShowType name
        :<>: Text "."
        )

type family NoOpaqueTypes
              ( name    :: Symbol              )
              ( loc     :: ErrorMessage        )
              ( as      :: [ Symbol :-> Type ] )
            :: Constraint
            where
  NoOpaqueTypes _    _     '[] = ()
  NoOpaqueTypes name loc ((k ':-> a) ': as) =
    ErrorIfOpaque (HasOpaqueType a) name loc k a

type family ErrorIfOpaque (hasOpaque :: Bool) (name :: Symbol) (loc :: ErrorMessage) (k :: Symbol) (a :: Type) :: Constraint where
  ErrorIfOpaque True name loc k a =
    TypeError
      (    Text "Structure member " :<>: ShowType k :<>: Text " cannot appear"
      :<>: Text " in " :<>: loc :<>: Text " named " :<>: ShowType name :<>: Text "."
      :$$: Text "Its type " :<>: ShowType a 
      :$$: Text "is opaque (or it contains an opaque type)."
      )
  ErrorIfOpaque _ _ _ _ _ = ()

type family NoOpaqueTypesExceptRuntimeArrayLast ( name :: Symbol ) ( as :: [ Symbol :-> Type ] ) :: Constraint where
  NoOpaqueTypesExceptRuntimeArrayLast _   '[] = ()
  NoOpaqueTypesExceptRuntimeArrayLast name ( ( k ':-> RuntimeArray a ) ': '[] ) =
    ErrorIfOpaqueRuntimeArray (HasOpaqueType a) name k a
  NoOpaqueTypesExceptRuntimeArrayLast name ( ( k ':-> RuntimeArray a ) ': _ ) =
    TypeError
      (    Text "Unexpected runtime array named " :<>: ShowType k
      :<>: Text " within storage buffer named " :<>: Text name :<>: Text "."
      :$$: Text "Runtime arrays must appear last in the structure."
      )
  NoOpaqueTypesExceptRuntimeArrayLast name ( ( k ':-> a ) ': as ) =
    ( NoOpaqueTypes name ( Text "storage buffer" ) '[ k ':-> a ]
    , NoOpaqueTypesExceptRuntimeArrayLast name as
    )

type family ErrorIfOpaqueRuntimeArray (hasOpaque :: Bool) (name :: Symbol) (k :: Symbol) (a :: Type) :: Constraint where
  ErrorIfOpaqueRuntimeArray True name k a =
    TypeError
      (    Text "Structure member " :<>: ShowType k :<>: Text " cannot appear"
      :<>: Text " in storage buffer named " :<>: ShowType name :<>: Text "."
      :$$: Text "It is a runtime array whose element type"
      :$$: ShowType a
      :$$: Text "is opaque, or contains an opaque type."
      )
  ErrorIfOpaqueRuntimeArray _ _ _ _ = ()
