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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Instances.Images 

This module allows for the operations 'FIR.Instances.Codensity.use'
and 'FIR.Instances.Codensity.assign' to work with images, by providing the 'ImageTexel' lens.

This allows accessing image data in a shader like so:

@ use \@(ImageTexel "imageName") imageOperands coords @

provided that @"imageName"@ binds an image in the monadic context.

@coords@ are the coordinates at which to access the image.
The specific type of these coordinates depends on:

  * whether we are sampling or reading directly,
  * the image dimensionality,
  * whether the image is arrayed,
  * the type of coordinates being used (affine or projective).

The return type of this operation depends on:

  * the image format (for instance a normalised integer image format
    will return floating-point types),
  * whether a depth-comparison is being performed: the return type
    will be a scalar in that case, and otherwise a vector with 4 components.


These properties are specified either at the type level,
with a top-level type annotation declaring the image properties,
or by the 'FIR.Prim.Image.ImageOperands' @imageOperands@ (optional
unless a depth-comparison value is required, but can be used
e.g. to set an explicit level of detail).



As images in @SPIR-V@ form an opaque type, the corresponding
'FIR.Prim.Image.Image' data type used by this library is
uninhabited, meaning it is impossible to construct or obtain
a value of this type. Instead, 'FIR.Prim.Image.Image'
is used only indirectly.
Attempting to directly obtain an 'FIR.Prim.Image.Image'
results in an error:

> img <- use @(Name "imgName")
>
>   • Variable named "imgName" refers to an image.
>     To access image data, use the 'ImageTexel' optic or the 'imageRead' function.


-}

module FIR.Instances.Images
  ( -- * Lens focusing on an image texel    
    ImageTexel
  )
where

-- base
import Prelude
  hiding ( Floating, Integral )
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol, KnownSymbol )

-- fir
import Control.Type.Optic
  ( Optic(Field_, RTOptic_, ComposeO)
  , Gettable, Settable
  )
import Data.Type.Known
  ( Known )
import FIR.ASTState
  ( ASTState )
import FIR.Prim.Image
  ( ImageProperties
  , Image, ImageData, ImageCoordinates
  , OperandName, ImageOperands
  )
import FIR.Validation.Images
  ( LookupImageProperties
  , ValidImageRead, ValidImageWrite
  )

-----------------------------------------------------------------------

-- | Lens for focusing on texels of an image.
--
-- This is a /phantom/ composite @ Name k :.: Texel @,
-- where @Texel@ denotes a (hypothetical) run-time optic
-- which focuses on the texel at a given coordinate (with specified image operands).
--
-- As it is impossible to obtain actual values of type 'FIR.Prim.Image.Image',
-- it is only the composite optic @ImageTexel k@, understood atomically,
-- which allows focusing on image texels.
type family ImageTexel
              ( k :: Symbol )
            = ( optic
                :: Optic
                    '[ ImageOperands props ops, ImageCoordinates props ops]
                    ( i :: ASTState )
                    ( ImageData props ops )
              )
            | optic -> k
            where
  ImageTexel k
    = ( ( ( Field_ (k :: Symbol) :: Optic '[] i (Image props) )
          `ComposeO`
          ( RTOptic_
              :: Optic
                  '[ ImageOperands props ops, ImageCoordinates props ops]
                  ( Image props )
                  ( ImageData props ops )
          )
        ) :: Optic
              '[ ImageOperands props ops, ImageCoordinates props ops]
              i
              (ImageData props ops)
      )

----------------------------------------------------
-- instances for this particular 'ghost' composite
-- (leverages that normal runtime optics only ever have 1 run-time index, this has 2)

instance {-# OVERLAPPING #-} 
         forall 
           ( k       :: Symbol          )
           ( i       :: ASTState        )
           ( props   :: ImageProperties )
           ( ops     :: [OperandName]   )
           ( empty   :: [Type]          )
           ( imgOps  :: Type            )
           ( imgCds  :: Type            )
           ( imgData :: Type            )
         .
         ( KnownSymbol k
         , LookupImageProperties k i ~ props
         , Known ImageProperties props
         , ValidImageRead props ops
         , empty ~ '[]
         , imgOps ~ ImageOperands props ops
         , imgCds ~ ImageCoordinates props ops
         , imgData ~ ImageData props ops
         )
      => Gettable ( ( ( Field_ k :: Optic empty i (Image props))
                    `ComposeO`
                      ( RTOptic_ :: Optic '[imgOps, imgCds] (Image props) imgData )
                    ) :: Optic '[imgOps, imgCds] i imgData
                  )
      where

instance {-# OVERLAPPING #-} 
         forall 
           ( k       :: Symbol          )
           ( i       :: ASTState        )
           ( props   :: ImageProperties )
           ( ops     :: [OperandName]   )
           ( empty   :: [Type]          )
           ( imgOps  :: Type            )
           ( imgCds  :: Type            )
           ( imgData :: Type            )
         .
         ( KnownSymbol k
         , LookupImageProperties k i ~ props
         , Known ImageProperties props
         , ValidImageWrite props ops
         , empty ~ '[]
         , imgOps ~ ImageOperands props ops
         , imgCds ~ ImageCoordinates props ops
         , imgData ~ ImageData props ops
         )
      => Settable ( ( ( Field_ k :: Optic empty i (Image props))
                    `ComposeO`
                      ( RTOptic_ :: Optic '[imgOps, imgCds] (Image props) imgData )
                    ) :: Optic '[imgOps, imgCds] i imgData
                  )
      where
