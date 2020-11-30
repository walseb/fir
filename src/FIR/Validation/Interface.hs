{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Interface

Validates the layout of individual entry point input/output interfaces.

For instance:

  * inputs/outputs in entry point interfaces must not overlap,
  * types within a location must use a consistent base type,
    e.g. a 'Float' and a 'Word32' cannot share a location.

-}

module FIR.Validation.Interface
  ( ValidInterface )
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
  ( (:->)((:->)), Lookup )
import Data.Type.Maybe
  ( SequenceMaybe )
import FIR.Prim.Struct
  ( LocationSlot(LocationSlot) )
import FIR.ProgramState
  ( TLInterfaceVariable
  , EntryPointInfo(EntryPointInfo)
  )
import FIR.Validation.Arrayness
  ( IsPatchVariable(..), ImplicitArrayness(..)
  , Arrayness, RemoveArrayness
  )
import FIR.Validation.Layout
  ( ValidLayout )
import qualified SPIRV.Decoration as SPIRV
  ( Decoration(..) )
import qualified SPIRV.Stage      as SPIRV
  ( ExecutionModel(..), ExecutionInfo(..) )
import qualified SPIRV.Storage    as SPIRV
  ( StorageClass(Input,Output) )

------------------------------------------------------------------------

type family ValidInterface ( epName :: Symbol ) ( nfo :: EntryPointInfo ) :: Constraint where
  ValidInterface epName ( 'EntryPointInfo xNfo globals _ )
    = ( ValidLayout ( LocationSlotsFromDecorations epName xNfo SPIRV.Input  ( SequenceMaybe ( Lookup SPIRV.Input  globals ) ) )
      , ValidLayout ( LocationSlotsFromDecorations epName xNfo SPIRV.Output ( SequenceMaybe ( Lookup SPIRV.Output globals ) ) )
      )

type family LocationSlotsFromDecorations
              ( emName  :: Symbol                     )
              ( xNfo    :: SPIRV.ExecutionInfo Nat em )
              ( inOut   :: SPIRV.StorageClass         )
              ( globals :: [ Symbol :-> TLInterfaceVariable ] )
            :: [ LocationSlot Nat :-> Type ]
            where
  LocationSlotsFromDecorations _      _    _     '[] = '[]
  LocationSlotsFromDecorations
    emName ( xNfo :: SPIRV.ExecutionInfo Nat em )
    inOut  ( ( varName ':-> '(decs, ty) ) ': globals )
    = AddLocationSlotIfNecessary varName ty decs emName em inOut
        ( Arrayness inOut decs xNfo )
        ( LocationSlotsFromDecorations emName xNfo inOut globals )

type family AddLocationSlotIfNecessary
              ( varName   :: Symbol                        )
              ( ty        :: Type                          )
              ( decs      :: [ SPIRV.Decoration Nat ]      )
              ( emName    :: Symbol                        )
              ( em        :: SPIRV.ExecutionModel          )
              ( inOut     :: SPIRV.StorageClass            )
              ( arrayness :: ImplicitArrayness             )
              ( slots     :: [ LocationSlot Nat :-> Type ] )
            :: [ LocationSlot Nat :-> Type ] 
            where
  AddLocationSlotIfNecessary _ _ _ _ _ _ ('NoImplicitArrayness 'IsPatch) slots
  -- ignore patch variables
    = slots
  -- otherwise, add the located type (after removing the implicit arrayness)
  AddLocationSlotIfNecessary varName ty decs emName em inOut arrayness slots
    = AddLocationSlot varName
        ( RemoveArrayness varName inOut emName em ty arrayness )
        ( LocationDecorations decs )
        slots

type family AddLocationSlot
              ( k       :: Symbol )
              ( ty      :: Type   )
              ( locInfo :: ( [Nat], [Nat] ) )
              ( slots   :: [ LocationSlot Nat :-> Type ] )
           :: [ LocationSlot Nat :-> Type ]
           where
  AddLocationSlot k ty locInfo slots
    = ( LocationSlotFromDecorations k locInfo ':-> ty ) ': slots

type family LocationSlotFromDecorations
              (  k      :: Symbol )
              ( locInfo :: ( [Nat], [Nat] ) )
          :: LocationSlot Nat
          where
  LocationSlotFromDecorations _ '( '[l] , '[c]  )
    = 'LocationSlot l c
  LocationSlotFromDecorations _ '( '[l] , '[]   )
    = 'LocationSlot l 0
  LocationSlotFromDecorations k '( '[l] ,  cs   )
    = TypeError
    (    Text "Global named " :<>: ShowType k
    :<>: Text " has more than one 'Component' decoration:"
    :$$: ShowType cs
    )
  LocationSlotFromDecorations k '( '[]  ,  _    )
    = TypeError
    (    Text "Global named " :<>: ShowType k
    :<>: Text " is missing a 'Location' decoration."
    )
  LocationSlotFromDecorations k '(  ls ,  _     )
    = TypeError
    (    Text "Global named " :<>: ShowType k
    :<>: Text " has more than one 'Location' decoration:"
    :$$: ShowType ls
    )

type family LocationDecorations
                ( decs :: [SPIRV.Decoration Nat] )
             :: ( [Nat], [Nat] )
             where
  LocationDecorations '[] = '( '[], '[] )
  LocationDecorations ( SPIRV.Location l ': decs )
    = CombineLocations '( Just l, 'Nothing ) ( LocationDecorations decs )
  LocationDecorations ( SPIRV.Component c ': decs )
    = CombineLocations '( 'Nothing, 'Just c ) ( LocationDecorations decs )
  LocationDecorations ( _ ': decs ) = LocationDecorations decs

type family CombineLocations
              ( lc  :: ( Maybe Nat, Maybe Nat ) )
              ( lcs :: ( [Nat], [Nat] ) )
            :: ( [Nat], [Nat] ) 
            where
  CombineLocations '(Just l, Just c) '(ls, cs) = '( l ': ls, c ': cs )
  CombineLocations '(Just l, _     ) '(ls, cs) = '( l ': ls, cs )
  CombineLocations '(_     , Just c) '(ls, cs) = '( ls, c ': cs )
  CombineLocations _                 lcs       = lcs
