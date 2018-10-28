{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module SPIRV.PrimTy where

-- base
import Data.Kind(Type)
import Data.Proxy(Proxy)
import Data.Word(Word32)
import Numeric.Natural(Natural)
--import GHC.TypeLits(TypeError,ErrorMessage(..))
import GHC.TypeNats(Nat, KnownNat, natVal)
import Prelude hiding (Integer, Floating)
import Unsafe.Coerce(unsafeCoerce)

-- fir
import SPIRV.Operation hiding(Function)
import SPIRV.Storage(StorageClass)

--------------------------------------------------
-- SPIR-V types

data Width
  = W8
  | W16
  | W32
  | W64
  deriving ( Show, Eq, Ord, Enum, Bounded )

width :: Width -> Word32
width W8  = 8
width W16 = 16
width W32 = 32
width W64 = 64

data Signedness
  = Unsigned
  | Signed
  deriving ( Show, Eq, Ord, Enum, Bounded )

signedness :: Signedness -> Word32
signedness Unsigned = 0
signedness Signed   = 1

data Dim
  = D2
  | D3
  | D4
{-
  |D8
  |D16
-} 
  deriving ( Show, Eq, Ord, Enum, Bounded )

dim :: Dim -> Word32
dim D2 = 2
dim D3 = 3
dim D4 = 4

toDim :: Natural -> Dim
toDim 2 = D2
toDim 3 = D3
toDim 4 = D4
toDim n = error $ "dimension must be 2, 3 or 4 (given: " ++ show n ++ ")"

type family ToDim (n :: Nat) = (d :: Dim) | d -> n where
  ToDim 2 = D2
  ToDim 3 = D3
  ToDim 4 = D4
  {- see GHC trac #12119
  ToDim n = TypeError 
    ( Text "Error: dimension must be 2, 3 or 4 (given: "
     :<>: ShowType n :<>: Text ")" ) -}

data ScalarTy where
  Integer  :: Signedness -> Width -> ScalarTy
  Floating ::               Width -> ScalarTy
  deriving ( Show, Eq, Ord )

data PrimTy where
  Unit     ::                           PrimTy -- known as Void in the SPIR-V specification
  Boolean  ::                           PrimTy
  Scalar   ::               ScalarTy -> PrimTy
  Vector   :: Dim        -> ScalarTy -> PrimTy
  Matrix   :: Dim -> Dim -> ScalarTy -> PrimTy
  Function :: [ PrimTy ]   -> PrimTy -> PrimTy
  Pointer  :: StorageClass -> PrimTy -> PrimTy
  -- todo: records, arrays, opaque types, ...
  deriving ( Show, Eq, Ord )

tyAndStaticTyConArgs :: PrimTy -> (Operation, [Word32])
tyAndStaticTyConArgs Unit    = ( TypeVoid    , [ ] )
tyAndStaticTyConArgs Boolean = ( TypeBool    , [ ] )
tyAndStaticTyConArgs (Scalar (Integer  s w)) = ( TypeInt     , [ width w, signedness s ] )
tyAndStaticTyConArgs (Scalar (Floating   w)) = ( TypeFloat   , [ width w ] )
tyAndStaticTyConArgs (Vector n   _) = ( TypeVector  , [ dim n ] ) -- element type ID is provided separately
tyAndStaticTyConArgs (Matrix _ m _) = ( TypeMatrix  , [ dim m ] ) -- only number of columns... column type ID is provided separately
tyAndStaticTyConArgs (Function _ _) = ( TypeFunction, [] ) -- can't know return type and parameter type IDs statically
tyAndStaticTyConArgs (Pointer _ _ ) = ( TypePointer , [] ) -- can't statically know the ID of type the pointer points to
                                                  --  ^^-- not putting the storage class because of weird ordering
                                                  -- the arguments are: first storage class, then ID of type
                                                  -- this is inconvenient when writing an instruction, easier to not put anything

ty :: PrimTy -> Operation
ty = fst . tyAndStaticTyConArgs


------------------------------------------------------------
-- singletons

data SSignedness :: Signedness -> Type where
  SUnsigned :: SSignedness 'Unsigned
  SSigned   :: SSignedness 'Signed

data SWidth :: Width -> Type where
  SW8  :: SWidth 'W8
  SW16 :: SWidth 'W16
  SW32 :: SWidth 'W32
  SW64 :: SWidth 'W64

data SDim :: Dim -> Type where
  SD2 :: SDim 'D2
  SD3 :: SDim 'D3
  SD4 :: SDim 'D4


sDim :: SDim n -> Dim
sDim SD2 = D2
sDim SD3 = D3
sDim SD4 = D4

-- GHC.TypeNats does not export the 'natSing' method of 'KnownNat'
-- so we have to work around it
natSDim :: KnownNat n => Proxy n -> SDim (ToDim n)
natSDim px = case natVal px of
             2 -> unsafeCoerce SD2
             3 -> unsafeCoerce SD3
             4 -> unsafeCoerce SD4
             _ -> error "stuck"

data SScalarTy :: ScalarTy -> Type where
  SInteger  :: SSignedness s -> SWidth w -> SScalarTy (Integer  s w)
  SFloating ::                  SWidth w -> SScalarTy (Floating   w)

-- some singleton types for PrimTy
-- (only those corresponding to primitive AST types, e.g. no pointers or functions)
data SPrimTy :: PrimTy -> Type where
  SUnit     ::                                    SPrimTy Unit
  SBoolean  ::                                    SPrimTy Boolean
  SScalar   ::                     SScalarTy a -> SPrimTy (Scalar     a)
  SVector   :: SDim n ->           SScalarTy a -> SPrimTy (Vector n   a)
  SMatrix   :: SDim m -> SDim n -> SScalarTy a -> SPrimTy (Matrix m n a)

-- reification

fromSSignedness :: SSignedness s -> Signedness
fromSSignedness SUnsigned = Unsigned
fromSSignedness SSigned   = Signed

fromSWidth :: SWidth w -> Width
fromSWidth SW8  = W8
fromSWidth SW16 = W16
fromSWidth SW32 = W32
fromSWidth SW64 = W64

fromSDim :: SDim d -> Dim
fromSDim SD2 = D2
fromSDim SD3 = D3
fromSDim SD4 = D4

fromSScalarTy :: SScalarTy a -> ScalarTy
fromSScalarTy (SInteger  s w) = Integer (fromSSignedness s) (fromSWidth w)
fromSScalarTy (SFloating   w) = Floating                    (fromSWidth w)

fromSPrimTy :: SPrimTy ty -> PrimTy
fromSPrimTy SUnit            = Unit
fromSPrimTy SBoolean         = Boolean
fromSPrimTy (SScalar     a ) = Scalar (fromSScalarTy a)
fromSPrimTy (SVector n   a ) = Vector (fromSDim n)              (fromSScalarTy a)
fromSPrimTy (SMatrix m n a ) = Matrix (fromSDim m) (fromSDim n) (fromSScalarTy a)