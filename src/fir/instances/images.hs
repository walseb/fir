{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module FIR.Instances.Images where

-- base
import Prelude
  hiding ( Floating, Integral )
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol, KnownSymbol )

-- fir
import Control.Type.Optic
  ( Optic(Name_, RTOptic_, ComposeO)
  , Gettable, Settable
  )
import Data.Type.Known
  ( Known )
import FIR.Binding
  ( BindingsMap )
import FIR.Prim.Image
  ( ImageProperties
  , Image, ImageData, ImageCoordinates
  , OperandName, ImageOperands
  )
import FIR.Instances.Bindings
  ( LookupImageProperties
  , ValidImageRead, ValidImageWrite
  )

-- testing optics
instance {-# OVERLAPPING #-} 
         forall 
           ( k       :: Symbol      )
           ( i       :: BindingsMap )
           ( props   :: ImageProperties )
           ( ops     :: [OperandName] )
           ( empty   :: [Type] )
           ( is      :: [Type] )
           ( imgData :: Type )
         .
         ( KnownSymbol k
         , LookupImageProperties k i ~ props
         , Known ImageProperties props
         , ValidImageRead props ops
         , empty ~ '[]
         , is ~ '[ImageOperands props ops, ImageCoordinates props ops]
         , imgData ~ ImageData props ops
         )
      => Gettable ( ( ( Name_ k :: Optic empty i (Image props))
                    `ComposeO`
                      ( RTOptic_ :: Optic is (Image props) imgData )
                    ) :: Optic is i imgData
                  )
      where

instance {-# OVERLAPPING #-} 
         forall 
           ( k       :: Symbol      )
           ( i       :: BindingsMap )
           ( props   :: ImageProperties )
           ( ops     :: [OperandName] )
           ( empty   :: [Type] )
           ( is      :: [Type] )
           ( imgData :: Type )
         .
         ( KnownSymbol k
         , LookupImageProperties k i ~ props
         , Known ImageProperties props
         , ValidImageWrite props ops
         , empty ~ '[]
         , is ~ '[ImageOperands props ops, ImageCoordinates props ops]
         , imgData ~ ImageData props ops
         )
      => Settable ( ( ( Name_ k :: Optic empty i (Image props))
                    `ComposeO`
                      ( RTOptic_ :: Optic is (Image props) imgData )
                    ) :: Optic is i imgData
                  )
      where


type family ImageTexel k :: Optic
                              '[ ImageOperands props ops, ImageCoordinates props ops]
                              i (ImageData props ops)
                  where
  ImageTexel k
    = ( ( ( Name_  k :: Optic '[] i (Image props) )
          `ComposeO`
          ( RTOptic_ :: Optic '[ ImageOperands props ops, ImageCoordinates props ops] (Image props) (ImageData props ops) )
        ) :: Optic '[ ImageOperands props ops, ImageCoordinates props ops] i (ImageData props ops)
      )
