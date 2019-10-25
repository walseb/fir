{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.Layout

Validation module for GPU memory layout, e.g. shader location/component assignments.

To specify the layout of vertex input data, the user specifies the type of inhabitants
in the available memory locations. Each location holds four components,
with each component of size 32 bits (4 bytes), i.e. the size of a 'Float'.
The type-system checks the compliance of any user-supplied layout with the SPIR-V and Vulkan specifications,
in particular checking that the assignment of slots is non-overlapping.

For instance, suppose we want to lay out objects of the following types:

  [@A@] @Array 3 (V 2 Float)@

  [@B@] @Array 2 Float@

  [@C@] @Float@

  [@D@] @V 3 Double@

  [@E@] @Array 2 Double@

  [@F@] @M 2 3 Float@

We can lay out this data by specifying which location slots each type uses.
Consider for instance the following specification:

>  Struct
>    '[ Slot 0 0 ':-> Array 3 (V 2 Float)
>     , Slot 0 2 ':-> Array 2 Float
>     , Slot 1 3 ':-> Float
>     , Slot 3 0 ':-> V 3 Double
>     , Slot 4 2 ':-> Array 2 Double
>     , Slot 6 0 ':-> M 2 3 Float
>     ]

We can visualise this layout as follows,
each row representing a location (each consisting of 4 components):

\[
\begin{matrix}
  & 0                & 1                & 2                     & 3                     \\
0 & \color{brown}{A} & \color{brown}{A} & \color{RoyalBlue}{B}  & \circ                 \\
1 & \color{brown}{A} & \color{brown}{A} & \color{RoyalBlue}{B}  & \color{purple}{C}     \\
2 & \color{brown}{A} & \color{brown}{A} & \circ                 & \circ                 \\
3 & \color{Plum}{D}  & \color{Plum}{D}  & \color{Plum}{D}       & \color{Plum}{D}       \\
4 & \color{Plum}{D}  & \color{Plum}{D}  & \color{orange}{E}     & \color{orange}{E}     \\
5 & \circ            & \circ            & \color{orange}{E}     & \color{orange}{E}     \\
6 & \color{green}{F} & \color{green}{F} & \color{green}{\times} & \color{green}{\times} \\
7 & \color{green}{F} & \color{green}{F} & \color{green}{\times} & \color{green}{\times} \\
8 & \color{green}{F} & \color{green}{F} & \color{green}{\times} & \color{green}{\times}
\end{matrix}
\]

Here \( \circ \) denotes unused components that are still available for other data,
whereas \( \times \) denotes unused components that /cannot/ be filled with other data.

Note in particular that matrices use as many locations as they have columns, consuming each location in its entirety.

The specification guarantees that locations 0 to 15 will be available. The availability of further locations
will depend on the GPU and its implementation of Vulkan.

For further reference, see:

  - SPIR-V specification, 2.16.2 "Validation Rules for Shader Capabilities", bullet point 3,
  - SPIR-V specification, 2.18.1 "Memory Layout",
  - Vulkan specification, 14.1.4 "Location Assignment", and 14.1.5 "Component Assignment",
  - Vulkan specification, 14.5.2 "Descriptor Set Interface", and 14.5.4 "Offset and Stride Assignment".

See also "FIR.Layout" for further information concerning memory layout.

-}

module FIR.Validation.Layout where

-- base
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If, type (&&), type (||) )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat
  , type (+), type (-)
  , type (*), Div, Mod
  )

-- fir
import Data.Type.Error
  ( IsRight )
import Data.Type.List
  ( Elem )
import Data.Type.Map
  ( (:->)((:->)) )
import Data.Type.Maybe
  ( IfNothingThen )
import Data.Type.Ord
  ( POrd(..)
  , PEnum(EnumFromOfCount)
  )
import Data.Type.String
  ( ShowNat )
import FIR.Layout
  ( Components )
import FIR.Prim.Array
  ( Array(..) )
import FIR.Prim.Singletons
  ( SScalarTy, ScalarWidth
  , ScalarFromSScalar
  , SKPrimTy(..), PrimTySing
  )
import FIR.Prim.Struct
  ( LocationSlot(LocationSlot)
  , ShowLocationSlot
  )
import Math.Linear
  ( V, M )
import qualified SPIRV.ScalarTy as SPIRV
  ( ScalarTy(Integer, Floating) )

-----------------------------------------------------------------------------------------------------
-- Layout validation.

-- | Validate the layout of a collection of located types.
type family ValidLayout (as :: [LocationSlot Nat :-> Type]) :: Constraint where
  ValidLayout '[] = ( () :: Constraint )
  ValidLayout ( ( loc ':-> ty ) ': slots )
    = ( IsRight ( ValidateSlot loc ( PrimTySing ty ) ) -- validate individual slot
      , CheckOverlaps ( loc ':-> ty ) slots -- pairwise overlap checks
      , ValidLayout slots
      )

-----------------------------------------------------------------------------------------------------
-- Individual validation of slots.

-- | Validate an individual location slot.
type family ValidateSlot
               ( loc  :: LocationSlot Nat )
               ( sing :: SKPrimTy ty      )
             :: Either ErrorMessage ()
             where
  ValidateSlot _ SKUnit
    = Left
        ( Text "Unit type not supported here." )
  ValidateSlot _ SKBool
    = Left
        ( Text "Booleans not supported here. Suggestion: use 'Word32' instead." )
  ValidateSlot ('LocationSlot l c) (SKScalar s :: SKPrimTy ty)
    = If ( c :<= 3 && ( c `Mod` 2 == 0 || ScalarWidth s :<= 32 ) )
        ( Right '() )
        ( Left
            (    Text "Cannot position scalar of type " :<>: ShowType ty
            :$$: Text " at " :<>: Text (ShowLocationSlot ('LocationSlot l c)) :<>: Text "."
            )
        )
  ValidateSlot ('LocationSlot l c) (SKVector s :: SKPrimTy (V n a))
    = If ( n :> 4 || n :< 2 )
        ( Left
          (    Text "Unexpected vector dimensions "
          :<>: ShowType (V n a)
          :$$: Text " at " :<>: Text (ShowLocationSlot ('LocationSlot l c)) :<>: Text "."
          )
        )
        ( If ( c == 0 || ( c `Mod` 2 == 0 && c :<= 3 && n :<= 2 && ScalarWidth s :<= 32 ) )
          ( Right '() )
          ( Left
              (    Text "Cannot position vector of type "
              :<>: ShowType (V n a)
              :$$: Text " at " :<>: Text (ShowLocationSlot ('LocationSlot l c)) :<>: Text "."
              )
          )
        )
  ValidateSlot ('LocationSlot l 0) (SKMatrix s :: SKPrimTy (M m n a))
    = If ( n :<= 4 && m :<= 4 && n :> 1 && m :> 1 )
        ( Right '() )
        ( Left
            (    Text "Unexpected matrix dimensions "
            :<>: ShowType (M m n a)
            :$$: Text " at " :<>: Text (ShowLocationSlot ('LocationSlot l 0)) :<>: Text "."
            )
        )
  ValidateSlot ('LocationSlot l c) (SKMatrix s :: SKPrimTy (M m n a))
    = Left
        (    Text "Cannot position matrix at location " :<>: ShowType l
        :<>: Text " with non-zero component " :<>: ShowType c :<>: Text "."
        )
  ValidateSlot loc (SKArray _ :: SKPrimTy (Array 0 a))
    = Left ( Text "Unexpected size 0 array at " :<>: Text (ShowLocationSlot loc) :<>: Text "." )
  ValidateSlot loc (SKArray elt :: SKPrimTy (Array l a))
    = ValidateArraySlot loc (Array l a) (ValidateSlot loc elt)
  ValidateSlot loc (SKRuntimeArray _)
    = Left ( Text "Unexpected runtime array at " :<>: Text (ShowLocationSlot loc) :<>: Text "." )
  ValidateSlot loc (SKStruct _)
    = Left
        (    Text "Unexpected struct at " :<>: Text (ShowLocationSlot loc) :<>: Text "."
        :$$: Text "Structs not (yet?) supported in inputs/outputs."
        )

type family ValidateArraySlot
              ( loc :: LocationSlot Nat )
              ( arr :: Type             )
              ( ok  :: Either ErrorMessage () )
            :: Either ErrorMessage ()
            where
  ValidateArraySlot loc arr (Left err)
    = Left
        (    Text "Attempting to position " :<>: ShowType arr
        :<>: Text " at " :<>: Text (ShowLocationSlot loc)
        :<>: Text " has caused the following error:"
        :$$: err
        )
  ValidateArraySlot _ _ _ = Right '()

-----------------------------------------------------------------------------------------------------
-- Check overlap within a collection of located types.

-- | Data (kind) recording problematic overlap between located types.
data Overlap
  = OverlappingSlot (LocationSlot Nat) -- ^ Location + component overlap.
  | OverlappingLocation Nat Type Type  -- ^ Location overlap with incompatible underlying scalar types.

type family CheckOverlaps
              ( slot  :: ( LocationSlot Nat :-> Type ) )
              ( slots :: [ LocationSlot Nat :-> Type ] )
            :: Constraint
            where
  CheckOverlaps _ '[] = ( () :: Constraint )
  CheckOverlaps slot ( s ': ss ) = ( CheckOverlap slot s, CheckOverlaps slot ss )

type family CheckOverlap
              ( slot1 :: ( LocationSlot Nat :-> Type ) )
              ( slot2 :: ( LocationSlot Nat :-> Type ) )
            :: Constraint
            where
  CheckOverlap ( loc1 ':-> ty1 ) ( loc2 ':-> ty2 )
    = ValidateOverlap
        loc1 ty1
        loc2 ty2
        ( ComputeFirstOverlap
            loc1 ( PrimTySing ty1 )
            loc2 ( PrimTySing ty2 )
        )

type family ValidateOverlap
              ( loc1    :: LocationSlot Nat )
              ( ty1     :: Type             )
              ( loc2    :: LocationSlot Nat )
              ( ty2     :: Type             )
              ( overlap :: Maybe Overlap    )
          :: Constraint
          where
  ValidateOverlap
    _ _
    _ _
    'Nothing
      = ( () :: Constraint )
  ValidateOverlap
    slot1 ty1
    slot2 ty2
    ( Just (OverlappingSlot overlapSlot) )
      = TypeError
        (    Text "Overlap at " :<>: Text (ShowLocationSlot overlapSlot) :<>: Text ":"
        :$$: Text "  - " :<>: ShowType ty1 :<>: Text " with "
        :<>: Text (ShowLocationSlot slot1) :<>: Text ","
        :$$: Text "  - " :<>: ShowType ty2 :<>: Text " with "
        :<>: Text (ShowLocationSlot slot2) :<>: Text "."
        )
  ValidateOverlap
    slot1 ty1
    slot2 ty2
    ( Just (OverlappingLocation loc scalar1 scalar2 ) )
      = TypeError
          (    Text "Incompatible scalars within Location " :<>: Text (ShowNat loc) :<>: Text ":"
          :$$: Text "  - " :<>: ShowType ty1 :<>: Text " with " :<>: Text (ShowLocationSlot slot1)
          :<>: Text " requires " :<>: ShowType scalar1 :<>: Text ","
          :$$: Text "  - " :<>: ShowType ty2 :<>: Text " with " :<>: Text (ShowLocationSlot slot2)
          :<>: Text " requires " :<>: ShowType scalar2 :<>: Text "."
          )

-- | Compute the first problematic overlap between any two pairs of located types.
--
-- This computation is done explicitly, to avoid building up any large
-- intermediate structures.
--
-- Only needs to cover pairs of the following types:
--
--  * scalars,
--  * vectors,
--  * matrices,
--  * arrays of any of the above (but not arrays of arrays).
--
-- Other types are not (currently) allowed in inputs/outputs.
type family ComputeFirstOverlap
              ( loc1  :: LocationSlot Nat )
              ( skty1 :: SKPrimTy ty1     )
              ( loc2  :: LocationSlot Nat )
              ( skty2 :: SKPrimTy ty2     )
            :: Maybe Overlap
            where
  -- scalar - scalar
  ComputeFirstOverlap
    ( 'LocationSlot l c1 ) ( SKScalar s1 :: SKPrimTy ty1 )
    ( 'LocationSlot l c2 ) ( SKScalar s2 :: SKPrimTy ty2 )
      = FirstOverlapFromEnum l
          ( 'LocationSlot l c1 ) ty1 s1
          ( 'LocationSlot l c2 ) ty2 s2
    -- (scalars are always contained within a single location)
  ComputeFirstOverlap
    ( 'LocationSlot l1 _ ) ( SKScalar _ )
    ( 'LocationSlot l2 _ ) ( SKScalar _ )
      = Nothing
  -- scalar - vector
  ComputeFirstOverlap
    ( 'LocationSlot l1 c1 ) ( SKScalar s1 :: SKPrimTy ty1        )
    ( 'LocationSlot l2 c2 ) ( SKVector s2 :: SKPrimTy (V m2 ty2) )
      = If ( l1 :< l2 || l1 :>= ( l2 + VectorLocations m2 s2 ) )
          Nothing -- no overlap possible
          ( FirstOverlapFromEnum l1
              ( 'LocationSlot l1 c1 ) ty1        s1
              ( 'LocationSlot l2 c2 ) (V m2 ty2) s2
          )
  -- vector - scalar: reduce to scalar - vector
  ComputeFirstOverlap
    ( 'LocationSlot l1 c1 ) ( SKVector s1 :: SKPrimTy (V m1 ty1) )
    ( 'LocationSlot l2 c2 ) ( SKScalar s2 :: SKPrimTy       ty2  )
      = FmapSwapOverlap
          ( ComputeFirstOverlap
            ( 'LocationSlot l2 c2 ) ( SKScalar s2 :: SKPrimTy       ty2  )
            ( 'LocationSlot l1 c1 ) ( SKVector s1 :: SKPrimTy (V m1 ty1) )
          )
  -- vector - vector
  ComputeFirstOverlap
    ( 'LocationSlot l1 c1 ) ( SKVector s1 :: SKPrimTy (V m1 ty1) )
    ( 'LocationSlot l2 c2 ) ( SKVector s2 :: SKPrimTy (V m2 ty2) )
      = If (  l1 :>= ( l2 + VectorLocations m2 s2 )
           || l2 :>= ( l1 + VectorLocations m1 s1 )
           )
          Nothing -- no overlap possible
          ( -- in this case, a unique location is used by both vectors: Max l1 l2
            FirstOverlapFromEnum (Max l1 l2)
              ( 'LocationSlot l1 c1 ) (V m1 ty1) s1
              ( 'LocationSlot l2 c2 ) (V m2 ty2) s2
          )
  -- scalar - matrix
  --   only need to check location overlap;
  --   no need to worry about components, as matrices consume locations entirely
  ComputeFirstOverlap
    ( 'LocationSlot l1 c1 ) ( SKScalar s1 :: SKPrimTy          ty1  )
    ( 'LocationSlot l2 _  ) ( SKMatrix s2 :: SKPrimTy (M m2 n2 ty2) )
      = If ( l1 :< l2 || l1 :>= ( l2 + MatrixLocations n2 s2 ) )
          Nothing
          ( Just ( 'OverlappingSlot ( 'LocationSlot l1 c1 ) ) )
  -- matrix - scalar: reduce to scalar - matrix
  ComputeFirstOverlap
    loc1 ( SKMatrix s1 :: SKPrimTy (M m1 n1 ty1) )
    loc2 ( SKScalar s2 :: SKPrimTy          ty2  )
      = FmapSwapOverlap
        ( ComputeFirstOverlap
            loc2 ( SKScalar s2 :: SKPrimTy          ty2  )
            loc1 ( SKMatrix s1 :: SKPrimTy (M m1 n1 ty1) )
        )
  -- vector - matrix
  --    essentially the same as scalar - matrix,
  --    except that the vector might take up two locations
  ComputeFirstOverlap
    ( 'LocationSlot l1 c1 ) ( SKVector s1 :: SKPrimTy (V m1    ty1) )
    ( 'LocationSlot l2 _  ) ( SKMatrix s2 :: SKPrimTy (M m2 n2 ty2) )
      = If (  ( l2 :>= ( l1 + VectorLocations m1 s1 ) )
           || ( l1 :>= ( l2 + MatrixLocations n2 s2 ) )
           )
          Nothing
          ( If ( l1 :< l2 )
              ( Just ( 'OverlappingSlot ( 'LocationSlot (l1 + 1) 0  ) ) )
              ( Just ( 'OverlappingSlot ( 'LocationSlot l1       c1 ) ) )
          )
  -- matrix - vector: reduce to vector - matrix
  ComputeFirstOverlap
    loc1 ( SKMatrix s1 :: SKPrimTy (M m1 n1 ty1) )
    loc2 ( SKVector s2 :: SKPrimTy (V m2    ty2) )
      = FmapSwapOverlap
        ( ComputeFirstOverlap
            loc2 ( SKVector s2 :: SKPrimTy (V m2    ty2) )
            loc1 ( SKMatrix s1 :: SKPrimTy (M m1 n1 ty1) )
        )
  -- matrix - matrix
  ComputeFirstOverlap
    ( 'LocationSlot l1 _ ) ( SKMatrix s1 :: SKPrimTy (M m1 n1 ty1) )
    ( 'LocationSlot l2 _ ) ( SKMatrix s2 :: SKPrimTy (M m2 n2 ty2) )
      = If (  ( l2 :>= ( l1 + MatrixLocations n1 s1 ) )
           || ( l1 :>= ( l2 + MatrixLocations n2 s2 ) )
           )
          Nothing
          ( Just ( OverlappingSlot ( 'LocationSlot ( Max l1 l2 ) 0 ) ) )
  -- arrays
  ComputeFirstOverlap
    _ ( SKArray _ :: SKPrimTy (Array 0 ty1) )
    _ _
      = Nothing
  ComputeFirstOverlap
    loc1 ( SKArray elt :: SKPrimTy (Array 1 ty1) )
    loc2 sing2
      = ComputeFirstOverlap loc1 elt loc2 sing2
  ComputeFirstOverlap
    ( 'LocationSlot l1 c1 ) ( SKArray elt1 :: SKPrimTy (Array n1 ty1) )
    ( 'LocationSlot l2 c2 ) ( sing2        :: SKPrimTy           ty2  )
      = If ( ( l1 + n1 * ( Components ty1 `Div` 4 ) ) :< l2 )
          Nothing
          ( ( ComputeFirstOverlap
                ( 'LocationSlot l1 c1 ) elt1
                ( 'LocationSlot l2 c2 ) sing2
            )
            `IfNothingThen`
            ( ComputeFirstOverlap
                ( 'LocationSlot
                    ( l1 + 1 + ( Components ty1 `Div` 4 ) )
                    c1
                )
                ( SKArray elt1 :: SKPrimTy ( Array (n1-1) ty1) )
                ( 'LocationSlot l2 c2 ) sing2
            )
          )
  ComputeFirstOverlap
    loc1 sing1
    loc2 ( SKArray elt2 :: SKPrimTy (Array n2 ty2) )
      = FmapSwapOverlap
        ( ComputeFirstOverlap
            loc2 ( SKArray elt2 :: SKPrimTy (Array n2 ty2) )
            loc1 sing1
        )

-- | Helper function to accomodate for switching arguments.
type family FmapSwapOverlap ( overlap :: Maybe Overlap ) :: Maybe Overlap where
  FmapSwapOverlap Nothing = Nothing
  FmapSwapOverlap (Just (OverlappingSlot slot)) = Just (OverlappingSlot slot)
  FmapSwapOverlap (Just (OverlappingLocation i ty1 ty2))
    = Just (OverlappingLocation i ty2 ty1)

type family VectorLocations (m :: Nat) (s :: SScalarTy ty) :: Nat where
  VectorLocations m s = If ( ScalarWidth s :<= 32 || m :<= 2 ) 1 2

type family MatrixLocations (n :: Nat) (s :: SScalarTy ty) :: Nat where
  MatrixLocations n s = n * VectorLocations 4 s

type family FirstOverlapFromEnum
              ( loc  :: Nat              ) -- location used for 'OverlappingLocation'
              ( loc1 :: LocationSlot Nat )
              ( ty1  :: Type             )
              ( s1   :: SScalarTy scal1  )
              ( loc2 :: LocationSlot Nat )
              ( ty2  :: Type             )
              ( s2   :: SScalarTy scal2  )
           :: Maybe Overlap
           where
  FirstOverlapFromEnum loc
    loc1 ty1 (s1 :: SScalarTy scal1)
    loc2 ty2 (s2 :: SScalarTy scal2)
    = ( FirstOverlappingSlot
        ( EnumConsecutiveSlots loc1 ty1 )
        ( EnumConsecutiveSlots loc2 ty2 )
      )
      `IfNothingThen`
      ( If ( CompatibleSScalars s1 s2 )
          Nothing
          ( Just ('OverlappingLocation loc scal1 scal2 ) )
      )

type family FirstOverlappingSlot
              ( slots1 :: [ LocationSlot Nat ] ) -- ordered
              ( slots2 :: [ LocationSlot Nat ] ) -- ordered
           :: Maybe Overlap
           where
  FirstOverlappingSlot '[]               _      = Nothing
  FirstOverlappingSlot (slot1 ': slots1) slots2
    = If ( slot1 `Elem` slots2 )
        ( Just (OverlappingSlot slot1) )
        ( FirstOverlappingSlot slots1 slots2 )

type CompatibleSScalars
              ( s1 :: SScalarTy ty1 )
              ( s2 :: SScalarTy ty2 )
  = ( CompatibleScalars ( ScalarFromSScalar s1 ) ( ScalarFromSScalar s2 ) :: Bool )

type family CompatibleScalars
              ( s1 :: SPIRV.ScalarTy )
              ( s2 :: SPIRV.ScalarTy )
            :: Bool
            where
  CompatibleScalars ( SPIRV.Integer _ w ) ( SPIRV.Integer _ w ) = True
  CompatibleScalars ( SPIRV.Floating  w ) ( SPIRV.Floating  w ) = True
  CompatibleScalars _                     _                     = False

-- | Simple enumeration of consecutive slots,
-- as many as the size of the type.
--
-- Should not be used for compound types,
-- where the locations used are not necessarily consecutive.
type family EnumConsecutiveSlots
              ( startLoc :: LocationSlot Nat )
              ( ty       :: Type )
            :: [ LocationSlot Nat ]
              where
  EnumConsecutiveSlots loc ty
    = EnumFromOfCount loc (Components ty)
