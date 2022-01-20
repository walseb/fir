
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Validation.Bounds

Custom error messages for out-of bounds indexing operations.

-}

module FIR.Validation.Bounds
  ( VectorIndexInBounds, MatrixColumnIndexInBounds
  , ArrayIndexInBounds
  , StructFieldFromIndex, StructIndexFromName
  )
  where

-- base
import Data.Kind
  ( Constraint )
import GHC.TypeLits
  ( TypeError
  , ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, CmpNat, type (+) )

-- fir
import Data.Type.List
  ( AtIndex, Length )
import Data.Type.Map
  ( (:->)((:->)) )
import Data.Type.Maybe
  ( FromJust )

---------------------------------------------------------------------------------------------

type family ErrorIfNotLT (n :: Nat) (i :: Nat) (msg :: ErrorMessage) :: Constraint where
  ErrorIfNotLT n i msg = ErrorIfNotLT_Helper msg (CmpNat i n)

type family ErrorIfNotLT_Helper (msg :: ErrorMessage) (cmp :: Ordering) :: Constraint where
  ErrorIfNotLT_Helper _  LT = ( () :: Constraint )
  ErrorIfNotLT_Helper msg _ = TypeError msg

type family VectorIndexInBounds (n :: Nat) (i :: Nat) :: Constraint where
  VectorIndexInBounds n i
    = ErrorIfNotLT n i
        (    Text "Vector index "
        :<>: ShowType i
        :<>: Text " is out of bounds."
        :$$: Text "Vector dimension is "
        :<>: ShowType n :<>: Text "."
        :$$: Text "Note: indexing starts from 0."
        )

type family MatrixColumnIndexInBounds (m :: Nat) (n :: Nat) (i :: Nat) :: Constraint where
  MatrixColumnIndexInBounds m n i
    = ErrorIfNotLT n i
        (    Text "Matrix column index "
        :<>: ShowType i
        :<>: Text " is out of bounds."
        :$$: Text "This matrix has "
        :<>: ShowType m :<>: Text " rows, "
        :<>: ShowType n :<>: Text " columns."
        :$$: Text "Note: indexing starts from 0."
        )

type family ArrayIndexInBounds (n :: Nat) (i :: Nat) :: Constraint where
  ArrayIndexInBounds n i
    = ErrorIfNotLT n i
        (    Text "Array index "
        :<>: ShowType i
        :<>: Text " is out of bounds."
        :$$: Text "Array size is "
        :<>: ShowType n :<>: Text "."
        :$$: Text "Note: indexing starts from 0."
        )

type family StructFieldFromIndex (i :: Nat) (as :: [fld :-> ty]) :: ( fld :-> ty ) where
  StructFieldFromIndex i as =
    FromJust (AtIndex i as)
      (    Text "Index "
      :<>: ShowType i
      :<>: Text " out of bounds when accessing structure with fields"
      :$$: ShowType as
      :$$: Text "Structure has only " :<>: ShowType (Length as)
      :<>: Text " fields."
      :$$: Text "Note: indexing starts from 0."
      )

type family StructIndexFromName (k :: fld) (as :: [fld :-> ty]) :: ( Nat :-> ty ) where
  StructIndexFromName k as = StructIndexFromNameRec k as 0 as

type family StructIndexFromNameRec
                  (k :: fld) (as :: [fld :-> ty])
                  (i :: Nat) (as_rec :: [fld :-> ty])
                :: ( Nat :-> ty )
                  where
  StructIndexFromNameRec k _  i ((k ':-> a)': _) = (i ':-> a)
  StructIndexFromNameRec k as i ( _ ': as_rec) = StructIndexFromNameRec k as (i+1) as_rec
  StructIndexFromNameRec k as _ '[]
    = ( TypeError
          (    Text "Structure has no field with name "
          :<>: ShowType k :<>: Text "."
          :$$: Text "This structure has the following fields:"
          :$$: ShowType as
          )
      )
