{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Formats

Computes the image formats underlying a given layout.

-}

module FIR.Validation.Formats
  ( ComputeFormats )
  where

-- base
import Data.Kind
  ( Type )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( AppendSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, type (+), Div )

-- fir
import Data.Type.List
  ( Replicate )
import Data.Type.Map
  ( Map, (:->)((:->)) )
import Data.Type.Ord
  ( POrd(Max) )
import FIR.Prim.Singletons
  ( SKPrimTy, ScalarFromSScalar )
import FIR.Prim.Struct
  ( LocationSlot(LocationSlot)
  , ShowLocationSlot
  )
import FIR.Validation.Layout
  ( SlotProvenance(Provenance) )
import qualified SPIRV.Image
import qualified SPIRV.Image    as SPIRV
  ( ImageFormat(ImageFormat) )
import qualified SPIRV.ScalarTy as SPIRV

--------------------------------------------------------------------------

type family ComputeFormats
              ( slots :: [ LocationSlot Nat :-> SlotProvenance ] )
            :: [ Nat :-> SPIRV.ImageFormat Nat ]
            where
  ComputeFormats '[] = '[]
  ComputeFormats ( ( loc ':-> Provenance loc ( _ :: SKPrimTy ty ) scalar _ ) ': slots )
    = ComputeFormatsFromBaseSlot loc ty ( ScalarFromSScalar scalar ) 1 slots
  ComputeFormats ( ( loc ':-> Provenance loc' ( _ :: SKPrimTy ty ) _ _ ) ': _ )
    = TypeError
    (    Text "Internal error: slot provenance has not been encountered."
    :$$: Text ( "When computing format at " `AppendSymbol` ShowLocationSlot loc `AppendSymbol` "," )
    :$$: Text "based off " :<>: ShowType ty
    :<>: Text ( " at " `AppendSymbol` ShowLocationSlot loc `AppendSymbol` "." )
    )

-- | Accumulates all the locations until it finds a new base component
-- with component 0.
--
-- TODO: I think this is currently incorrect; check e.g. with slot components: xxxx Float Float xxxx
type family ComputeFormatsFromBaseSlot
              ( baseLoc    :: LocationSlot Nat )
              ( baseTy     :: Type             )
              ( baseScalar :: SPIRV.ScalarTy   )
              ( nbComps    :: Nat              )
              ( slots      :: Map (LocationSlot Nat) SlotProvenance )
            :: Map Nat (SPIRV.ImageFormat Nat)
            where
  ComputeFormatsFromBaseSlot baseLoc _ baseScalar nbComps '[]
    = '[ FormatFromComponents baseLoc baseScalar nbComps ]
  ComputeFormatsFromBaseSlot
    baseLoc _ baseScalar nbComps
    ( ( 'LocationSlot l 0 ':-> Provenance ('LocationSlot l 0 ) ( _ :: SKPrimTy newTy ) newScalar _ ) ': slots )
  -- found new base slot with component 0
      = FormatFromComponents baseLoc baseScalar nbComps
        ': ComputeFormatsFromBaseSlot
            ( 'LocationSlot l 0 )
            newTy
            ( ScalarFromSScalar newScalar )
            1
            slots
  ComputeFormatsFromBaseSlot
    baseLoc baseTy baseScalar nbComps
    ( ( loc ':-> Provenance otherBase ( _ :: SKPrimTy otherTy ) otherScalar _ ) ': slots )
      = ComputeFormatsFromBaseSlot
          baseLoc baseTy baseScalar
          ( SuccNbComponentsIfMatching
              baseLoc   baseTy  baseScalar nbComps
              otherBase otherTy ( ScalarFromSScalar otherScalar )
              loc
              ( baseScalar == ScalarFromSScalar otherScalar )
          )
          slots

type family SuccNbComponentsIfMatching
              ( baseLoc      :: LocationSlot Nat )
              ( baseTy       :: Type             )
              ( baseScalar   :: SPIRV.ScalarTy   )
              ( nbComps      :: Nat              )
              ( otherBase    :: LocationSlot Nat )
              ( otherTy      :: Type             )
              ( otherScalar  :: SPIRV.ScalarTy   )
              ( loc          :: LocationSlot Nat )
              ( equalScalars :: Bool             )
            :: Nat
            where
  SuccNbComponentsIfMatching
    baseLoc   baseTy  baseScalar _
    otherBase otherTy otherScalar
    loc
    'False
    -- TODO: this error should be redundant, as Validation.Layout should catch this
      = TypeError
      (    Text ( "Mismatched components at " `AppendSymbol` ShowLocationSlot loc `AppendSymbol` ":" )
      :$$: Text "  - object of type " :<>: ShowType baseTy
      :$$: Text ( "    based at " `AppendSymbol` ShowLocationSlot baseLoc `AppendSymbol` ":" )
      :$$: Text "    uses components of type " :<>: ShowType baseScalar :<>: Text ","
      :$$: Text "  - object of type " :<>: ShowType otherTy
      :$$: Text ( "    based at " `AppendSymbol` ShowLocationSlot otherBase `AppendSymbol` ":" )
      :$$: Text "    uses components of type " :<>: ShowType otherScalar :<>: Text "."
      :$$: Text ""
      :$$: Text "Note that all components within a given location must use the same component type."
      )
  SuccNbComponentsIfMatching
    _ _ _ nbComps
    _ _ _
    _
    _
      = nbComps + 1

type family FormatFromComponents
              ( baseLoc    :: LocationSlot Nat )
              ( baseScalar :: SPIRV.ScalarTy   )
              ( nbComps    :: Nat              )
            :: ( Nat :-> SPIRV.ImageFormat Nat )
            where
  FormatFromComponents
    ('LocationSlot l c) scalarTy nbComps
      = l ':->
        FormatForComponentsOfWidth
          ( nbComps + c )
          ( ComponentOf scalarTy )
          ( SPIRV.WidthToNat (SPIRV.ScalarWidth scalarTy ) )

type family ComponentOf (scalar :: SPIRV.ScalarTy) :: SPIRV.Image.Component where
  ComponentOf (SPIRV.Integer SPIRV.Unsigned _) = SPIRV.Image.UI
  ComponentOf (SPIRV.Integer SPIRV.Signed   _) = SPIRV.Image.I
  ComponentOf (SPIRV.Floating _              ) = SPIRV.Image.F

type family FormatForComponentsOfWidth
              ( nbComponents :: Nat                   )
              ( component    :: SPIRV.Image.Component )
              ( width        :: Nat                   )
            :: ( SPIRV.ImageFormat Nat ) where
  FormatForComponentsOfWidth n comp w
    = 'SPIRV.ImageFormat comp ( Replicate (n `Div` (Max w 32 `Div` 32)) w )
