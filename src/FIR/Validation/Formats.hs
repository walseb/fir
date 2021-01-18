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
import Data.Type.Bool
  ( If, type (&&), type (||) )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeNats
  ( Nat
  , type (+), type (-), Div
  )

-- fir
import Data.Type.List
  ( Length, Replicate, type (:++:) )
import Data.Type.Map
  ( Map, (:->)((:->)) )
import Data.Type.Ord
  ( POrd(..) )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Struct
  ( LocationSlot(LocationSlot) )
import FIR.Prim.Types
  ( SKPrimTy(..), SKPrimTyMap(..)
  , PrimTySing
  , ScalarFromSScalar
  )
import Math.Linear
  ( V, M )
import qualified SPIRV.Image
import qualified SPIRV.Image    as SPIRV
  ( ImageFormat(ImageFormat) )
import qualified SPIRV.ScalarTy as SPIRV

--------------------------------------------------------------------------
-- computing image formats for vertex input backed by a structure type

type family ComputeFormats
              ( slots :: Map (LocationSlot Nat) Type )
            :: Map Nat ( SPIRV.ImageFormat Nat )
            where
  ComputeFormats '[] = '[]
  ComputeFormats ( (loc ':-> ty) ': slots )
    = InsertFormatsCombining
        ( ComputeSlotFormats loc (PrimTySing ty) )
        ( ComputeFormats slots )

--------------------------------------------------------------------------
-- computing image formats for a given type

type family ComputeSlotFormats
              ( loc  :: LocationSlot Nat )
              ( skTy :: SKPrimTy ty      )
            :: [ Nat :-> SPIRV.ImageFormat Nat ]
            where
  -- scalars
  ComputeSlotFormats ('LocationSlot l c) ( SKScalar s :: SKPrimTy ty )
    = '[ l ':-> ScalarFormat ('LocationSlot l c) 1 ( ScalarFromSScalar s ) ]
  -- vectors
  ComputeSlotFormats ('LocationSlot l c) ( SKVector s :: SKPrimTy (V m ty) )
    = '[ l ':-> ScalarFormat ('LocationSlot l c) m ( ScalarFromSScalar s ) ]
  -- matrices
  ComputeSlotFormats ('LocationSlot l c) ( SKMatrix s :: SKPrimTy (M m n ty) )
    = MatrixSlotFormats l m
        ( ComponentOf ( ScalarFromSScalar s ) )
        ( SPIRV.WidthToNat (SPIRV.ScalarWidth ( ScalarFromSScalar s ) ) )
  -- arrays
  ComputeSlotFormats ('LocationSlot l c) ( SKArray elt :: SKPrimTy (Array 0 ty) )
    = '[ ]
  ComputeSlotFormats ('LocationSlot l c) ( SKArray elt :: SKPrimTy (Array 1 ty) )
    = ComputeSlotFormats ('LocationSlot l c) ( elt :: SKPrimTy ty )
  ComputeSlotFormats ('LocationSlot l c) ( SKArray elt :: SKPrimTy (Array n ty) )
    = CompoundSlotFormats
        ( 'LocationSlot l c )
        ( ComputeSlotFormats ('LocationSlot l c) elt )
        ( SKArray elt :: SKPrimTy (Array (n-1) ty) )
  -- structs
  ComputeSlotFormats ('LocationSlot l c) ( SKStruct SKNil ) = '[]
  ComputeSlotFormats ('LocationSlot l c) ( SKStruct (elt `SKCons` elts) )
    = CompoundSlotFormats
        ( 'LocationSlot l c )
        ( ComputeSlotFormats ('LocationSlot l c) elt )
        ( SKStruct elts )
  -- unsupported types: unit, bool, runtime array
  ComputeSlotFormats _  SKUnit             = '[]
  ComputeSlotFormats _  SKBool             = '[]
  ComputeSlotFormats _ (SKRuntimeArray _ ) = '[]

type family ScalarFormat
              ( loc      :: LocationSlot Nat )
              ( elems    :: Nat              )
              ( scalarTy :: SPIRV.ScalarTy   )
            :: SPIRV.ImageFormat Nat
            where
  ScalarFormat loc elems scalarTy
    = FormatForComponentsOfWidth
        loc elems
        ( ComponentOf scalarTy )
        ( SPIRV.WidthToNat (SPIRV.ScalarWidth scalarTy ) )

type family MatrixSlotFormats
              ( loc   :: Nat                   )
              ( n     :: Nat                   )
              ( comp  :: SPIRV.Image.Component )
              ( width :: Nat                   )
           :: [ Nat :-> SPIRV.ImageFormat Nat ]
           where
  MatrixSlotFormats _ 0 _    _ = '[]
  MatrixSlotFormats l 1 comp w = '[ l ':-> 'SPIRV.ImageFormat comp ( Replicate 4 w ) ]
  MatrixSlotFormats l n comp w
    = ( l ':-> 'SPIRV.ImageFormat comp ( Replicate 4 w ) )
    ': MatrixSlotFormats ( l + ( w `Div` 32 ) ) ( n-1 ) comp w

type family CompoundSlotFormats
              ( loc  :: LocationSlot Nat                  )
              ( fmts :: [ Nat :-> SPIRV.ImageFormat Nat ] )
              ( skTy :: SKPrimTy ty                       )
           :: [ Nat :-> SPIRV.ImageFormat Nat ]
           where
  CompoundSlotFormats ('LocationSlot l c) fmts skTy
    = fmts
    :++:
    ComputeSlotFormats ('LocationSlot (NextAvailableLocation l fmts) c) skTy

type family NextAvailableLocation
              ( loc  :: Nat                               )
              ( fmts :: [ Nat :-> SPIRV.ImageFormat Nat ] )
            :: Nat
            where
  NextAvailableLocation l '[]
    = l
  NextAvailableLocation _ ( (l ':-> _) ': ls )
    = NextAvailableLocation (l + 1) ls

type family FormatForComponentsOfWidth
              ( loc          :: LocationSlot Nat      )
              ( elems        :: Nat                   )
              ( component    :: SPIRV.Image.Component )
              ( width        :: Nat                   )
            :: ( SPIRV.ImageFormat Nat ) where
  FormatForComponentsOfWidth ('LocationSlot _ c) elems comp w
    = 'SPIRV.ImageFormat comp ( Replicate ( elems + ( c `Div` ( Max w 32 `Div` 32 ) ) ) w )

type family ComponentOf (scalar :: SPIRV.ScalarTy) :: SPIRV.Image.Component where
  ComponentOf (SPIRV.Integer SPIRV.Unsigned _) = SPIRV.Image.UI
  ComponentOf (SPIRV.Integer SPIRV.Signed   _) = SPIRV.Image.I
  ComponentOf (SPIRV.Floating _              ) = SPIRV.Image.F

--------------------------------------------------------------------------
-- combining formats

type family InsertFormatsCombining
             ( fmts :: [ Nat :-> SPIRV.ImageFormat Nat ] )
             ( nxt  :: Map Nat ( SPIRV.ImageFormat Nat ) )
          :: Map Nat ( SPIRV.ImageFormat Nat )
          where
  InsertFormatsCombining '[] nxt = nxt
  InsertFormatsCombining ( fmt ': fmts ) nxt
    = InsertFormatCombining fmt ( InsertFormatsCombining fmts nxt )

type family InsertFormatCombining
              ( fmt  :: ( Nat :-> SPIRV.ImageFormat Nat ) )
              ( fmts :: Map Nat ( SPIRV.ImageFormat Nat ) )
            :: Map Nat ( SPIRV.ImageFormat Nat )
            where
  InsertFormatCombining fmt '[]
    = '[ fmt ]
  InsertFormatCombining ( i  ':-> fmt1 ) ( ( i  ':-> fmt2 ) ': fmts )
    = ( i ':-> LargestFormat fmt1 fmt2 ) ': fmts
  InsertFormatCombining ( i1 ':-> fmt1 ) ( ( i2 ':-> fmt2 ) ': fmts )
    = MaybeInsertCombination i1 fmt1 i2 fmt2
        (Combination i1 fmt2 i2 fmt2)
        fmts

type family MaybeInsertCombination
                ( i1   :: Nat                                 )
                ( fmt1 :: SPIRV.ImageFormat Nat               )
                ( i2   :: Nat                                 )
                ( fmt2 :: SPIRV.ImageFormat Nat               )
                ( comb :: Maybe ( Nat, SPIRV.ImageFormat Nat) )
                ( fmts :: Map Nat ( SPIRV.ImageFormat Nat )   )
              :: Map Nat ( SPIRV.ImageFormat Nat )
              where
  MaybeInsertCombination _  _    _  _    ( Just '(i, fmt) ) fmts
    = ( ( i ':-> fmt ) ': fmts )
  MaybeInsertCombination i1 fmt1 i2 fmt2 'Nothing           fmts
    = If ( i1 :< i2 )
        ( ( i1 ':-> fmt1 ) ': ( i2 ':-> fmt2 ) ': fmts )
        ( ( i2 ':-> fmt2 ) ': InsertFormatCombining ( i1 ':-> fmt1 ) fmts )

type family Combination
              ( i1   :: Nat                   )
              ( fmt1 :: SPIRV.ImageFormat Nat )
              ( j2   :: Nat                   )
              ( fmt2 :: SPIRV.ImageFormat Nat )
            :: Maybe ( Nat, SPIRV.ImageFormat Nat )
            where
  Combination i fmt1 i fmt2
    = Just '( i, LargestFormat fmt1 fmt2 )
  Combination i1 ( 'SPIRV.ImageFormat comp widths1 ) i2 ( 'SPIRV.ImageFormat _ widths2 )
    = CrossLocationCombine comp i1 widths1 (Length widths2) i2 widths2 (Length widths2)

type family CrossLocationCombine
              ( comp    :: SPIRV.Image.Component )
              ( i1      ::  Nat                  )
              ( widths1 :: [Nat]                 )
              ( lg1     ::  Nat                  )
              ( i2      ::  Nat                  )
              ( widths2 :: [Nat]                 )
              ( lg2     ::  Nat                  )
            :: Maybe ( Nat, SPIRV.ImageFormat Nat )
            where
  CrossLocationCombine comp i1 widths1 lg1 i2 widths2 lg2
    = If (  ( i2 == i1 + 1 && ComponentsUsed widths1 :> 4 && Length widths2 :<= 2 )
         || ( i1 == i2 + 1 && ComponentsUsed widths2 :> 4 && Length widths1 :<= 2 )
         )
        ( Just
           '( Min i1 i2
            , 'SPIRV.ImageFormat comp
                ( Replicate
                  ( Max lg1 ( Max lg2 ( Min lg1 lg2 + 1 ) ) ) -- sorry
                  64
                )
            )
        )
        Nothing

type family ComponentsUsed ( widths :: [Nat] ) :: Nat where
  ComponentsUsed '[] = 0
  ComponentsUsed (w ': ws) = ( w `Div` 32 ) + ComponentsUsed ws

type family LargestFormat
              ( fmt1 :: SPIRV.ImageFormat Nat )
              ( fmt2 :: SPIRV.ImageFormat Nat )
           :: SPIRV.ImageFormat Nat
           where
  LargestFormat ( 'SPIRV.ImageFormat comp1 widths1 ) ( 'SPIRV.ImageFormat comp2 widths2 )
    = If ( Length widths2 :> Length widths1 )
        ( 'SPIRV.ImageFormat comp2 widths2 )
        ( 'SPIRV.ImageFormat comp1 widths1 )
  -- simply picking the largest of the two formats
  --
  -- trying to not throw any errors even when results don't make sense,
  -- as layout validation should prevent anything silly from happening
  -- (and we don't want to duplicate error messages)
