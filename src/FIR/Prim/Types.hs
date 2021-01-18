{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
--{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Prim.Types

This module defines the types used internally, including singletons machinery that is necessary for code generation.

See for instance 'CodeGen.IDs.constID' for a crucial application of these singletons.

For situations where we only need one-way reification, i.e. being able to turn type-level data into value-level data
without the need to be able to go back the other way, see "Data.Type.Known".

-}

module FIR.Prim.Types where

-- base
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.Kind
  ( Type )
import Data.List
  ( intercalate )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Bool
  ( type (&&) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Type.List
  ( type (:++:) )
import Data.Typeable
  ( Typeable, eqT )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(Text, ShowType, (:$$:), (:<>:))
  )
import GHC.TypeNats
  ( Nat, KnownNat, natVal
  , type (+)
  )

-- containers
import qualified Data.Set as Set

-- half
import Numeric.Half
  ( Half )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import {-# SOURCE #-} Control.Type.Optic
  ( IndexChain, IndexInfo(ThisIndex, AnyIndex) )
import Data.Binary.Class.Put
  ( Put )
import Data.Constraint.All
  ( All(allDict), AllDict(NilDict, ConsDict) )
import Data.Function.Variadic
  ( ListVariadic )
import Data.Type.Known
  ( Known, knownValue )
import Data.Type.Map
  ( (:->)((:->)) )
import {-# SOURCE #-} FIR.AST
  ( Syntactic(Internal) )
import FIR.AST.Type
  ( AugType(Val) )
import FIR.Binding
  ( Binding, BindingsMap, Var
  , Permission(Read,Write), Permissions
  )
import FIR.Prim.Array
  ( Array, RuntimeArray )
import {-# SOURCE #-} FIR.Prim.Image
  ( Image )
import FIR.Prim.RayTracing
  ( AccelerationStructure, RayQuery )
import FIR.Prim.Struct
  ( Struct, FieldKind(NamedField), StructFieldKind(fieldKind) )
import Math.Algebra.Class
  ( Ring )
import Math.Linear
  ( V, M )
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.PrimTy     as SPIRV
import SPIRV.ScalarTy
  ( Signedness(Unsigned, Signed)
  , Width(W8,W16,W32,W64)
  )
import qualified SPIRV.ScalarTy   as SPIRV
import qualified SPIRV.Storage    as SPIRV
  ( StorageClass )
import qualified SPIRV.Storage    as Storage

------------------------------------------------------------
-- singletons for primitive types
-- this allows us to pattern match on the type when necessary

data SScalarTy :: Type -> Type where
  SWord8  :: SScalarTy Word8
  SWord16 :: SScalarTy Word16
  SWord32 :: SScalarTy Word32
  SWord64 :: SScalarTy Word64
  SInt8   :: SScalarTy Int8
  SInt16  :: SScalarTy Int16
  SInt32  :: SScalarTy Int32
  SInt64  :: SScalarTy Int64
  SHalf   :: SScalarTy Half
  SFloat  :: SScalarTy Float
  SDouble :: SScalarTy Double

showSScalarTy :: SScalarTy ty -> String
showSScalarTy SWord8  = "Word8"
showSScalarTy SWord16 = "Word16"
showSScalarTy SWord32 = "Word32"
showSScalarTy SWord64 = "Word64"
showSScalarTy SInt8   = "Int8"
showSScalarTy SInt16  = "Int16"
showSScalarTy SInt32  = "Int32"
showSScalarTy SInt64  = "Int64"
showSScalarTy SHalf   = "Half"
showSScalarTy SFloat  = "Float"
showSScalarTy SDouble = "Double"

type ScalarFromTy (ty :: Type)
  = ( ScalarFromSScalar ( ScalarTySing ty ) :: SPIRV.ScalarTy )

type family ScalarFromSScalar (sTy :: SScalarTy ty) :: SPIRV.ScalarTy where
  ScalarFromSScalar SWord8  = SPIRV.Integer Unsigned W8
  ScalarFromSScalar SWord16 = SPIRV.Integer Unsigned W16
  ScalarFromSScalar SWord32 = SPIRV.Integer Unsigned W32
  ScalarFromSScalar SWord64 = SPIRV.Integer Unsigned W64
  ScalarFromSScalar SInt8   = SPIRV.Integer Signed   W8
  ScalarFromSScalar SInt16  = SPIRV.Integer Signed   W16
  ScalarFromSScalar SInt32  = SPIRV.Integer Signed   W32
  ScalarFromSScalar SInt64  = SPIRV.Integer Signed   W64
  ScalarFromSScalar SHalf   = SPIRV.Floating         W16
  ScalarFromSScalar SFloat  = SPIRV.Floating         W32
  ScalarFromSScalar SDouble = SPIRV.Floating         W64

type family TypeFromScalar ( scalar :: SPIRV.ScalarTy ) :: Type where
  TypeFromScalar (SPIRV.Integer Unsigned W8 ) = Word8
  TypeFromScalar (SPIRV.Integer Unsigned W16) = Word16
  TypeFromScalar (SPIRV.Integer Unsigned W32) = Word32
  TypeFromScalar (SPIRV.Integer Unsigned W64) = Word64
  TypeFromScalar (SPIRV.Integer Signed   W8 ) = Int8
  TypeFromScalar (SPIRV.Integer Signed   W16) = Int16
  TypeFromScalar (SPIRV.Integer Signed   W32) = Int32
  TypeFromScalar (SPIRV.Integer Signed   W64) = Int64
  TypeFromScalar (SPIRV.Floating         W16) = Half
  TypeFromScalar (SPIRV.Floating         W32) = Float
  TypeFromScalar (SPIRV.Floating         W64) = Double

type family ScalarWidth (sTy :: SScalarTy ty) :: Nat where
  ScalarWidth sTy
   = SPIRV.WidthToNat
        ( SPIRV.ScalarWidth ( ScalarFromSScalar sTy ) )

-- singleton data type
data SPrimTy :: Type -> Type where
  SUnit   :: SPrimTy ()
  SBool   :: SPrimTy Bool
  SScalar :: ScalarTy a => SPrimTy a
  SVector :: (KnownNat n, PrimTy a)
          => SPrimTy (V n a)
  SMatrix :: (KnownNat m, KnownNat n, ScalarTy a, Ring a)
          => SPrimTy (M m n a)
  SArray  :: (KnownNat n, PrimTy a)
          => SPrimTy (Array n a)
  SRuntimeArray
          :: PrimTy a
          => SPrimTy (RuntimeArray a)
  SStruct :: forall (fld :: Type) (as :: [fld :-> Type])
          .  ( StructFieldKind fld, PrimTyMap as )
          => SPrimTy (Struct as)
  SAccelerationStructure
    :: SPrimTy AccelerationStructure

data SPrimTyMap :: [fld :-> Type] -> Type where
  SNil  :: SPrimTyMap '[]
  SCons :: (StructFieldKind fld, Known fld k, PrimTy a, PrimTyMap as)
        => SPrimTyMap ((k ':-> a) ': as)

type family HasOpaqueType (a :: Type) :: Bool where
  HasOpaqueType (Image _)                    = True
  HasOpaqueType AccelerationStructure        = True
  HasOpaqueType RayQuery                     = True
  HasOpaqueType (RuntimeArray _)             = True
  HasOpaqueType (Array _ a)                  = HasOpaqueType a
  HasOpaqueType (Struct ((_ ':-> a) ': as )) = HasOpaqueType a && HasOpaqueType (Struct as)
  HasOpaqueType _                            = False

instance Show (SPrimTy ty) where
  show :: SPrimTy ty -> String
  show SUnit = "()"
  show SBool = "Bool"
  show (SScalar :: SPrimTy a) = showSScalarTy (scalarTySing @a)
  show sVector@SVector
    = case sVector of
        ( _ :: SPrimTy (V n a) ) ->
          "V " ++ show (natVal (Proxy @n)) ++ " " ++ show (primTySing @a)
  show sMatrix@SMatrix
    = case sMatrix of
        ( _ :: SPrimTy (M m n a) ) ->
          "M " ++ show (natVal (Proxy @m)) ++ " "
          ++ show (natVal (Proxy @n))
          ++ " " ++ showSScalarTy (scalarTySing @a)
  show sArray@SArray
    = case sArray of
        ( _ :: SPrimTy (Array n a) ) ->
          "Array " ++ show (natVal (Proxy @n))
          ++ " " ++ show (primTySing @a)
  show sRuntimeArray@SRuntimeArray
    = case sRuntimeArray of
        ( _ :: SPrimTy (RuntimeArray a) ) ->
          "RuntimeArray " ++ show (primTySing @a)
  show sStruct@SStruct
    = case sStruct of
        ( _ :: SPrimTy (Struct as) ) ->
          "Struct '["
          ++ intercalate ", " ( showSPrimTyMap ( primTyMapSing @_ @as ) )
          ++ "]"
  show SAccelerationStructure = "AccelerationStructure"

showSPrimTyMap :: SPrimTyMap as -> [String]
showSPrimTyMap SNil = []
showSPrimTyMap sCons@SCons
  = case sCons of
      ( _ :: SPrimTyMap ((k ':-> a) ': as) ) ->
        ( show (knownValue @k) ++ " :-> " ++ show ( primTySing @a ) )
        : showSPrimTyMap ( primTyMapSing @_ @as )

-- singleton data kind, without unpromotable contexts
data SKPrimTy :: Type -> Type where
  SKUnit   :: SKPrimTy ()
  SKBool   :: SKPrimTy Bool
  SKScalar :: SScalarTy a -> SKPrimTy a
  SKVector :: SScalarTy a -> SKPrimTy (V n a)
  SKMatrix :: SScalarTy a -> SKPrimTy (M m n a)
  SKArray  :: SKPrimTy a -> SKPrimTy (Array n a)
  SKRuntimeArray
           :: SKPrimTy a -> SKPrimTy (RuntimeArray a)
  SKStruct :: SKPrimTyMap as -> SKPrimTy (Struct as)
  SKAccelerationStructure :: SKPrimTy AccelerationStructure

data SKPrimTyMap :: [fld :-> Type] -> Type where
  SKNil :: SKPrimTyMap '[]
  SKCons :: SKPrimTy a -> SKPrimTyMap as -> SKPrimTyMap ((k ':-> a) ': as)

type family ScalarTySing (ty :: Type) :: SScalarTy ty where
  ScalarTySing Word8  = SWord8
  ScalarTySing Word16 = SWord16
  ScalarTySing Word32 = SWord32
  ScalarTySing Word64 = SWord64
  ScalarTySing Int8   = SInt8
  ScalarTySing Int16  = SInt16
  ScalarTySing Int32  = SInt32
  ScalarTySing Int64  = SInt64
  ScalarTySing Half   = SHalf
  ScalarTySing Float  = SFloat
  ScalarTySing Double = SDouble
  ScalarTySing a
    = TypeError
      ( Text "Type " :<>: ShowType a :<>: Text " is not a valid scalar type." )

type family PrimTySing (ty :: Type) :: SKPrimTy ty where
  PrimTySing ()     = SKUnit
  PrimTySing Bool   = SKBool
  PrimTySing Word8  = SKScalar SWord8
  PrimTySing Word16 = SKScalar SWord16
  PrimTySing Word32 = SKScalar SWord32
  PrimTySing Word64 = SKScalar SWord64
  PrimTySing Int8   = SKScalar SInt8
  PrimTySing Int16  = SKScalar SInt16
  PrimTySing Int32  = SKScalar SInt32
  PrimTySing Int64  = SKScalar SInt64
  PrimTySing Half   = SKScalar SHalf
  PrimTySing Float  = SKScalar SFloat
  PrimTySing Double = SKScalar SDouble
  PrimTySing (V n a) = SKVector (ScalarTySing a)
  PrimTySing (M m n a) = SKMatrix (ScalarTySing a)
  PrimTySing (Array n a) = SKArray (PrimTySing a)
  PrimTySing (RuntimeArray a) = SKRuntimeArray (PrimTySing a)
  PrimTySing (Struct as) = SKStruct (MapPrimTySing as)
  PrimTySing AccelerationStructure = SKAccelerationStructure
  PrimTySing ty
    = TypeError
        ( Text "Type " :<>: ShowType ty :<>: Text " is not a valid primitive type." )

type family MapPrimTySing (as :: [fld :-> Type]) :: SKPrimTyMap as where
  MapPrimTySing '[] = SKNil
  MapPrimTySing ( ( k ':-> a) ': as)
    = SKCons (PrimTySing a) (MapPrimTySing as)

------------------------------------------------

class ( Show ty                    -- for convenience
      , Eq ty, Ord ty, Typeable ty -- to keep track of lists of constants
      , ty ~ ListVariadic '[] ty   -- ty is not a function type
      )
    => PrimTy ty where
  -- associated type used for computing overlap with "OfType" optic
  type FieldsOfType ty fldTy :: [ IndexChain ]
  primTySing :: SPrimTy ty

class ( PrimTy ty
      , Put ty                     -- for serialisation
      , HasOpaqueType ty ~ False
      ) => ScalarTy ty where
  scalarTySing :: SScalarTy ty

type family MonolithicFields (a :: Type) (ty :: Type) :: [ IndexChain ] where
  MonolithicFields a a = '[ '[] ]
  MonolithicFields _ _ = '[]

instance PrimTy ()   where
  type FieldsOfType () _ = '[]
  primTySing = SUnit
instance PrimTy Bool where
  type FieldsOfType Bool a = MonolithicFields Bool a
  primTySing = SBool
instance PrimTy AccelerationStructure where
  type FieldsOfType AccelerationStructure a = MonolithicFields AccelerationStructure a
  primTySing = SAccelerationStructure

instance ScalarTy Word8  where
  scalarTySing = SWord8
instance ScalarTy Word16 where
  scalarTySing = SWord16
instance ScalarTy Word32 where
  scalarTySing = SWord32
instance ScalarTy Word64 where
  scalarTySing = SWord64
instance ScalarTy Int8   where
  scalarTySing = SInt8
instance ScalarTy Int16  where
  scalarTySing = SInt16
instance ScalarTy Int32  where
  scalarTySing = SInt32
instance ScalarTy Int64  where
  scalarTySing = SInt64
instance ScalarTy Half   where
  scalarTySing = SHalf
instance ScalarTy Float  where
  scalarTySing = SFloat
instance ScalarTy Double where
  scalarTySing = SDouble

class (ScalarTy ty, Integral ty) => IntegralTy ty where
instance IntegralTy Word8  where
instance IntegralTy Word16 where
instance IntegralTy Word32 where
instance IntegralTy Word64 where
instance IntegralTy Int8   where
instance IntegralTy Int16  where
instance IntegralTy Int32  where
instance IntegralTy Int64  where

instance PrimTy Word8  where
  type FieldsOfType Word8  a = MonolithicFields Word8  a
  primTySing = SScalar
instance PrimTy Word16 where
  type FieldsOfType Word16 a = MonolithicFields Word16 a
  primTySing = SScalar
instance PrimTy Word32 where
  type FieldsOfType Word32 a = MonolithicFields Word32 a
  primTySing = SScalar
instance PrimTy Word64 where
  type FieldsOfType Word64 a = MonolithicFields Word64 a
  primTySing = SScalar
instance PrimTy Int8   where
  type FieldsOfType Int8   a = MonolithicFields Int8   a
  primTySing = SScalar
instance PrimTy Int16  where
  type FieldsOfType Int16  a = MonolithicFields Int16  a
  primTySing = SScalar
instance PrimTy Int32  where
  type FieldsOfType Int32  a = MonolithicFields Int32  a
  primTySing = SScalar
instance PrimTy Int64  where
  type FieldsOfType Int64  a = MonolithicFields Int64  a
  primTySing = SScalar
instance PrimTy Half   where
  type FieldsOfType Half   a = MonolithicFields Half   a
  primTySing = SScalar
instance PrimTy Float  where
  type FieldsOfType Float  a = MonolithicFields Float  a
  primTySing = SScalar
instance PrimTy Double where
  type FieldsOfType Double a = MonolithicFields Double a
  primTySing = SScalar


instance ( TypeError
             ( Text "Use a specific width unsigned integer type \
                    \instead of 'Word' (recommended: 'Word32')."
              )
         , Put Word
         )
    => PrimTy Word where
  type FieldsOfType Word a = MonolithicFields Word a
  primTySing = error "unreachable" 
instance ( TypeError
             ( Text "Use a specific width signed integer type \
                    \instead of 'Int' (recommended: 'Int32')."
              )
         , Put Int
         )
    => PrimTy Int where
  type FieldsOfType Int a = MonolithicFields Int a
  primTySing = error "unreachable"
instance ( TypeError
             ( Text "Use a specific width unsigned integer type \
                    \instead of 'Word' (recommended: 'Word32')."
              )
         , Put Word
         )
    => ScalarTy Word where
  scalarTySing = error "unreachable" 
instance ( TypeError
             ( Text "Use a specific width signed integer type \
                    \instead of 'Int' (recommended: 'Int32')."
              )
         , Put Int
         )
    => ScalarTy Int where
  scalarTySing = error "unreachable"


type family HomogeneousFields (c :: Type -> Type) (a :: Type) (b :: Type) :: [ IndexChain ] where
  HomogeneousFields c a (c a) = '[ '[] ]
  HomogeneousFields _ a b     = MapCons AnyIndex ( FieldsOfType a b )

type family MapCons (x :: k) (xss :: [[k]]) :: [[k]] where
  MapCons _ '[]         = '[]
  MapCons x (ys ': yss) = ( x ': ys ) ': MapCons x yss

instance ( PrimTy a
         , KnownNat n
         ) => PrimTy (V n a) where
  type FieldsOfType (V n a) ty = HomogeneousFields (V n) a ty
  primTySing = SVector

type family MatrixFields (m :: Nat) (a :: Type) (b :: Type) :: [ IndexChain ] where
  MatrixFields m a (V m a) = '[ '[ AnyIndex ] ]
  MatrixFields _ a a       = '[ '[ AnyIndex, AnyIndex ] ]
  MatrixFields _ _ _       = '[] -- no need to recurse as matrices can only contain scalars

instance ( PrimTy a, ScalarTy a
         , Ring a
         , KnownNat m--, 2 <= m, m <= 4
         , KnownNat n--, 2 <= n, n <= 4
         ) => PrimTy (M m n a) where
  type FieldsOfType (M m n a) ty = MatrixFields m a ty
  primTySing = SMatrix

instance (PrimTy a, KnownNat l) => PrimTy (Array l a) where
  type FieldsOfType (Array l a) ty = HomogeneousFields (Array l) a ty
  primTySing = SArray

instance PrimTy a => PrimTy (RuntimeArray a) where
  type FieldsOfType (RuntimeArray a) ty = HomogeneousFields RuntimeArray a ty
  primTySing = SRuntimeArray

class    (Typeable as, StructFieldKind fld) => PrimTyMap (as :: [fld :-> Type]) where
  primTyMapSing :: SPrimTyMap as

instance StructFieldKind fld => PrimTyMap ('[] :: [fld :-> Type]) where
  primTyMapSing = SNil

instance forall ( fld :: Type           )
                ( k   :: fld            )
                ( a   :: Type           )
                ( as  :: [fld :-> Type] )
      . ( StructFieldKind fld
        , Typeable k
        , Known fld k
        , PrimTy a
        , PrimTyMap as
        )
       => PrimTyMap ((k ':-> a) ': as) where
  primTyMapSing = SCons

instance PrimTyMap as => PrimTy (Struct (as :: [fld :-> Type])) where
  type FieldsOfType (Struct as) ty = StructFieldsStartingAt 0 as ty
  primTySing = SStruct

type family StructFieldsStartingAt
              ( i  :: Nat              )
              ( as :: [ fld :-> Type ] )
              ( ty :: Type             )
            :: [ IndexChain ]
            where
  StructFieldsStartingAt _ '[]                    _
    = '[]
  StructFieldsStartingAt i ( ( _ ':-> a ) ': as ) ty
    = ( MapCons ( ThisIndex i ) (FieldsOfType a ty) )
    :++:
    StructFieldsStartingAt (i+1) as ty

primTy :: forall ty. PrimTy ty => SPIRV.PrimTy
primTy = sPrimTy ( primTySing @ty )

primTys :: forall as. All PrimTy as => [SPIRV.PrimTy]
primTys = case allDict @PrimTy @as of
  NilDict -> []
  consDict@ConsDict ->
    case consDict of
      ( _ :: AllDict PrimTy (b ': bs) ) ->
        primTy @b : primTys @bs

primTyMap :: forall (fld :: Type) (flds :: [fld :-> Type])
          .  PrimTyMap flds
          => [ (Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations) ]
primTyMap = sPrimTyMap ( primTyMapSing @fld @flds )

sPrimTy :: SPrimTy ty -> SPIRV.PrimTy
sPrimTy SUnit = SPIRV.Unit
sPrimTy SBool = SPIRV.Boolean
sPrimTy (SScalar :: SPrimTy a)
  = SPIRV.Scalar (scalarTy @a)
sPrimTy vec@SVector = case vec of
  ( _ :: SPrimTy (V n a) )
    -> SPIRV.Vector (knownValue @n) (primTy @a)
sPrimTy mat@SMatrix = case mat of
  ( _ :: SPrimTy (M m n a) )
    -> SPIRV.Matrix (knownValue @m) (knownValue @n) (scalarTy @a)
sPrimTy arr@SArray = case arr of
  ( _ :: SPrimTy (Array l a) )
    -> SPIRV.Array  (knownValue @l) (primTy @a) Set.empty SPIRV.NotForBuiltins
sPrimTy rtArr@SRuntimeArray = case rtArr of
  ( _ :: SPrimTy (RuntimeArray a) )
    -> SPIRV.RuntimeArray (primTy @a) Set.empty SPIRV.NotForBuiltins
sPrimTy struct@SStruct = case struct of
  ( _ :: SPrimTy (Struct as) )
    -> SPIRV.Struct (sPrimTyMap (primTyMapSing @_ @as)) Set.empty SPIRV.NotForBuiltins
sPrimTy SAccelerationStructure = SPIRV.AccelerationStructure

sPrimTyMap :: forall (fld :: Type) (flds :: [fld :-> Type])
           .  SPrimTyMap flds
           -> [ (Maybe ShortText, SPIRV.PrimTy, SPIRV.Decorations) ]
sPrimTyMap SNil = []
sPrimTyMap cons@SCons = case cons of
  ( _ :: SPrimTyMap (( f ':-> a ) ': as) )
    ->  let
          mbFieldName :: Maybe ShortText
          mbFieldName =
            case fieldKind @fld of
              NamedField -> Just ( knownValue @f )
              _          -> Nothing
        in ( mbFieldName, primTy @a, Set.empty )
           : sPrimTyMap (primTyMapSing @_ @as)


scalarTy :: forall ty. ScalarTy ty => SPIRV.ScalarTy
scalarTy = sScalarTy ( scalarTySing @ty )

sScalarTy :: SScalarTy ty -> SPIRV.ScalarTy
sScalarTy SWord8  = SPIRV.Integer Unsigned W8
sScalarTy SWord16 = SPIRV.Integer Unsigned W16
sScalarTy SWord32 = SPIRV.Integer Unsigned W32
sScalarTy SWord64 = SPIRV.Integer Unsigned W64
sScalarTy SInt8   = SPIRV.Integer Signed   W8
sScalarTy SInt16  = SPIRV.Integer Signed   W16
sScalarTy SInt32  = SPIRV.Integer Signed   W32
sScalarTy SInt64  = SPIRV.Integer Signed   W64
sScalarTy SHalf   = SPIRV.Floating         W16
sScalarTy SFloat  = SPIRV.Floating         W32
sScalarTy SDouble = SPIRV.Floating         W64

------------------------------------------------------------
-- statically known list of variables (for function definitions)

class KnownVar (bd :: (Symbol :-> Binding)) where
  knownVar :: ( ShortText, (SPIRV.PrimTy, Permissions) )

instance (KnownSymbol k, Known Permissions ps, PrimTy a)
       => KnownVar (k ':-> Var ps a) where
  knownVar =
    ( knownValue @k
    , ( primTy @a
      , knownValue @ps
      )
    )

class KnownVars (bds :: BindingsMap) where
  knownVars :: [ (ShortText, (SPIRV.PrimTy, Permissions)) ]
instance KnownVars '[] where
  knownVars = []
instance (KnownVar bd, KnownVars bds)
      => KnownVars (bd ': bds) where
  knownVars = knownVar @bd : knownVars @bds

------------------------------------------------------------
-- statically known interfaces (for entry points)

class KnownInterfaceBinding (bd :: (Symbol :-> Binding)) where
  knownInterfaceBinding :: ( ShortText, (SPIRV.PrimTy, SPIRV.StorageClass) )

instance (KnownSymbol k, PrimTy a)
       => KnownInterfaceBinding (k ':-> Var '[ 'Read ] a) where
  knownInterfaceBinding
    = ( knownValue @k
      , ( primTy @a
        , Storage.Input
        )
      )
instance (KnownSymbol k, PrimTy a)
       => KnownInterfaceBinding (k ':-> Var '[ 'Write ] a) where
  knownInterfaceBinding
    = ( knownValue @k
      , ( primTy @a
        , Storage.Output
        )
      )

instance
  TypeError
    (    Text "Interface binding " :<>: ShowType k :<>: Text " is both readable and writable."
    :$$: Text "Interface variables must be of either 'Input' or 'Output' type, not both." )
  => KnownInterfaceBinding (k ':-> Var '[ 'Read, 'Write ] a) where
  knownInterfaceBinding = error "unreachable"
instance
  TypeError
    (    Text "Interface binding " :<>: ShowType k :<>: Text " is both readable and writable."
    :$$: Text "Interface variables must be of either 'Input' or 'Output' type, not both." )
  => KnownInterfaceBinding (k ':-> Var '[ 'Write, 'Read ] a) where
  knownInterfaceBinding = error "unreachable"

class KnownInterface (bds :: BindingsMap) where
  knownInterface :: [ ( ShortText, (SPIRV.PrimTy, SPIRV.StorageClass) ) ]
instance KnownInterface '[] where
  knownInterface = []
instance (KnownInterfaceBinding bd, KnownInterface bds)
      => KnownInterface (bd ': bds) where
  knownInterface = knownInterfaceBinding @bd : knownInterface @bds

------------------------------------------------------------
-- functors

data SPrimFunc :: (Type -> Type) -> Type where
  SFuncVector  :: KnownNat n => SPrimFunc (V n)
  SFuncMatrix  :: (KnownNat m, KnownNat n) => SPrimFunc (M m n)
  SFuncArray   :: KnownNat n => SPrimFunc (Array n)

-- explicit dictionary passing workaround
-- would like to use a quantified constraint instead
-- (see (GHC issue #17226)[https://gitlab.haskell.org/ghc/ghc/issues/17226])
data DistDict f a where
  DistDict
    :: ( Syntactic a, Internal a ~ Val va
       , Syntactic (f a), Internal (f a) ~ Val (f va)
       )
    => DistDict f a

class ( Applicative f
    --, forall a. (Syntactic a, Internal a ~ Val va, PrimTy va) => Syntactic (f a)
      ) => PrimFunc f where
  primFuncSing :: SPrimFunc f
  distDict     :: ( Syntactic a, Internal a ~ Val va, PrimTy va )
               => DistDict f a

primFuncName :: forall f. PrimFunc f => String
primFuncName = case primFuncSing @f of

  sFuncVector@SFuncVector ->
    case sFuncVector of
      ( _ :: SPrimFunc (V n) ) ->
        "V " ++ show (natVal (Proxy @n))

  sFuncMatrix@SFuncMatrix ->
    case sFuncMatrix of
      ( _ :: SPrimFunc (M m n) ) ->
        "M " ++ show (natVal (Proxy @m))
             ++ " "
             ++ show (natVal (Proxy @n))
  sFuncArray@SFuncArray ->
    case sFuncArray of
      ( _ :: SPrimFunc (Array n) ) ->
        "Array " ++ show (natVal (Proxy @n))

------------------------------------------------------------
-- dependent pair

data AConstant :: Type where
  AConstant :: PrimTy ty => ty -> AConstant

aConstant :: PrimTy ty => ty -> AConstant
aConstant = AConstant

instance Show AConstant where
  show (AConstant (a :: ty))
    = "Constant " ++ show a ++ " of type " ++ show (primTy @ty)

instance Eq AConstant where
  (AConstant (a1 :: ty1)) == (AConstant (a2 :: ty2))
    = case eqT @ty1 @ty2 of
        Just Refl -> a1 == a2
        _         -> False

instance Ord AConstant where
  (AConstant (a1 :: ty1)) `compare` (AConstant (a2 :: ty2))
    = case eqT @ty1 @ty2 of
         Just Refl -> compare a1 a2
         _         -> compare (primTy @ty1)
                              (primTy @ty2)
