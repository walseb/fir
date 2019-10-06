{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Definitions

Validate top-level "FIR.Definition.Definition"s.

For instance, this checks that uniform buffer objects are
decorated with the required binding and descriptor set indices.

-}

module FIR.Validation.Definitions
  ( )
  where

-- base
import Data.Kind
  ( Type )
import Data.Type.Bool
  ( If, type (&&) )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat )

-- fir
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Image
  ( Image )
import FIR.Prim.Struct
  ( Struct )
import qualified SPIRV.Decoration as SPIRV
  ( Decoration(..) )
import qualified SPIRV.Storage    as SPIRV
  ( StorageClass )
import qualified SPIRV.Storage    as Storage
  ( StorageClass(..) )

-------------------------------------------------

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
