{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module FIR.Prim.Image where

-- base
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat )

-- text-utf8
import qualified Data.Text as Text

-- fir
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( ScalarTy, scalarTy )
import Math.Linear
  ( V )
import SPIRV.Image

--------------------------------------------------

newtype ImageHandle input res = ImageHandle Text.Text
  deriving ( Eq, Show )

data ImageProperties where
  Properties
     :: Type -- component type for accessing image data
     -> Type -- result component type
     -> Dimensionality
     -> Maybe Depth
     -> Arrayness
     -> MultiSampling
     -> Maybe ImageUsage
     -> ImageFormat Nat
     -> ImageProperties

instance Demotable ImageProperties where
  type Demote ImageProperties = ImageTy

instance ( ScalarTy                 component
         , Known Dimensionality     dimensionality
         , Known (Maybe Depth)      depth
         , Known Arrayness          arrayness
         , Known MultiSampling      multiSampling
         , Known (Maybe ImageUsage) imageUsage
         , Known (ImageFormat Nat)  imageFormat
         )
  => Known ImageProperties
      ( 'Properties component res dim mbDepth arrayness ms mbUsage fmt )
  where
  known = ImageTy
    { component      = scalarTy   @component
    , dimensionality = knownValue @dimensionality
    , depth          = knownValue @depth
    , arrayness      = knownValue @arrayness
    , multiSampling  = knownValue @multiSampling
    , imageUsage     = knownValue @imageUsage
    , imageFormat    = knownValue @imageFormat
    }

type family ImageFetchType (a :: Type) (dim :: Dimensionality) (arr :: Arrayness) :: Type where
  ImageFetchType a OneD        NonArrayed =     a
  ImageFetchType a TwoD        NonArrayed = V 2 a
  ImageFetchType a ThreeD      NonArrayed = V 3 a
  ImageFetchType a Cube        NonArrayed = V 4 a
  ImageFetchType a Rect        NonArrayed = V 2 a
  ImageFetchType a Buffer      NonArrayed =     a
  ImageFetchType a SubpassData NonArrayed = V 2 a
  ImageFetchType a OneD        Arrayed    = V 2 a
  ImageFetchType a TwoD        Arrayed    = V 3 a
  ImageFetchType a Cube        Arrayed    = V 3 a -- weird stuff going on here
  ImageFetchType _ dim         Arrayed
    = TypeError ( Text "Unsupported arrayed image format \
                       \with dimensionality "
                  :<>: ShowType dim
                )
