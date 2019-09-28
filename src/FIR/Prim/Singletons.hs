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
Module: FIR.Prim.Singletons

This module contains singletons machinery that is necessary for code generation.

See for instance 'CodeGen.IDs.constID' for a crucial application of these singletons.

-}

module FIR.Prim.Singletons where

-- base
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Typeable
  ( Typeable, eqT )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(Text, ShowType, (:$$:), (:<>:))
  )
import GHC.TypeNats
  ( Nat, KnownNat, natVal )

-- containers
import qualified Data.Set as Set

-- half
import Numeric.Half
  ( Half )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import Data.Binary.Class.Put
  ( Put )
import Data.Function.Variadic
  ( ListVariadic )
import Data.Type.Known
  ( Known, knownValue )
import Data.Type.Map
  ( (:->)((:->)) )
import {-# SOURCE #-} FIR.AST
  ( Syntactic(Internal) )
import FIR.Binding
  ( Binding, BindingsMap, Var
  , Permission(Read,Write), Permissions
  )
import FIR.Prim.Array
  ( Array,RuntimeArray )
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

type family Integrality (sTy :: SScalarTy ty) :: SPIRV.ScalarTy where
  Integrality SWord8  = SPIRV.Integer Unsigned W8
  Integrality SWord16 = SPIRV.Integer Unsigned W16
  Integrality SWord32 = SPIRV.Integer Unsigned W32
  Integrality SWord64 = SPIRV.Integer Unsigned W64
  Integrality SInt8   = SPIRV.Integer Signed   W8
  Integrality SInt16  = SPIRV.Integer Signed   W16
  Integrality SInt32  = SPIRV.Integer Signed   W32
  Integrality SInt64  = SPIRV.Integer Signed   W64
  Integrality SHalf   = SPIRV.Floating         W16
  Integrality SFloat  = SPIRV.Floating         W32
  Integrality SDouble = SPIRV.Floating         W64

type family ScalarWidth (sTy :: SScalarTy ty) :: Nat where
  ScalarWidth sTy
   = SPIRV.WidthToNat
        ( SPIRV.ScalarWidth ( Integrality sTy ) )

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

data SPrimTyMap :: [fld :-> Type] -> Type where
  SNil  :: SPrimTyMap '[]
  SCons :: (StructFieldKind fld, Known fld k, PrimTy a, PrimTyMap as)
        => SPrimTyMap ((k ':-> a) ': as)

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
  PrimTySing ty
    = TypeError
        ( Text "Type " :<>: ShowType ty :<>: Text " is not a valid primitive type." )

type family MapPrimTySing (as :: [fld :-> Type]) :: SKPrimTyMap as where
  MapPrimTySing '[] = SKNil
  MapPrimTySing ( ( k ':-> a) ': as)
    = SKCons (PrimTySing a) (MapPrimTySing as)

------------------------------------------------
-- function arity

data Arity a where
  ZeroArity :: PrimTy a => Arity a
  SuccArity :: Arity b -> Arity (a -> b)

class KnownArity a where
  arity :: Arity a

instance {-# OVERLAPPABLE #-} PrimTy a => KnownArity a where
  arity = ZeroArity

instance {-# OVERLAPPING #-} KnownArity b => KnownArity (a -> b) where
  arity = SuccArity (arity @b)

------------------------------------------------

class HasField (k :: Symbol) (as :: [ Symbol :-> Type ]) where
  fieldIndex :: Word32

instance HasField k ( (k ':-> v) ': as ) where
  fieldIndex = 0

instance {-# OVERLAPPABLE #-} HasField k as
       => HasField k ( (l ':-> v) ': as)
       where
  fieldIndex = succ ( fieldIndex @k @as )

------------------------------------------------

class ( Show ty                    -- for convenience
      , Eq ty, Ord ty, Typeable ty -- to keep track of lists of constants
      , ty ~ ListVariadic '[] ty   -- ty is not a function type... useful for optics
      )
    => PrimTy ty where
  primTySing :: SPrimTy ty

class ( PrimTy ty
      , Put ty                     -- for serialisation
      ) => ScalarTy ty where
  scalarTySing :: SScalarTy ty

instance PrimTy ()   where
  primTySing = SUnit
instance PrimTy Bool where
  primTySing = SBool

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
  primTySing = SScalar
instance PrimTy Word16 where
  primTySing = SScalar
instance PrimTy Word32 where
  primTySing = SScalar
instance PrimTy Word64 where
  primTySing = SScalar
instance PrimTy Int8   where
  primTySing = SScalar
instance PrimTy Int16  where
  primTySing = SScalar
instance PrimTy Int32  where
  primTySing = SScalar
instance PrimTy Int64  where
  primTySing = SScalar
instance PrimTy Half   where
  primTySing = SScalar
instance PrimTy Float  where
  primTySing = SScalar
instance PrimTy Double where
  primTySing = SScalar


instance ( TypeError
             ( Text "Use a specific width unsigned integer type \
                    \instead of 'Word' (recommended: 'Word32')."
              )
         , Put Word
         )
    => PrimTy Word where
  primTySing = error "unreachable" 
instance ( TypeError
             ( Text "Use a specific width signed integer type \
                    \instead of 'Int' (recommended: 'Int32')."
              )
         , Put Int
         )
    => PrimTy Int where
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


instance ( PrimTy a
         , KnownNat n
         ) => PrimTy (V n a) where
  primTySing = SVector

instance ( PrimTy a, ScalarTy a
         , Ring a
         , KnownNat m--, 2 <= m, m <= 4
         , KnownNat n--, 2 <= n, n <= 4
         ) => PrimTy (M m n a) where
  primTySing = SMatrix

instance (PrimTy a, KnownNat l) => PrimTy (Array l a) where
  primTySing = SArray

instance PrimTy a => PrimTy (RuntimeArray a) where
  primTySing = SRuntimeArray

class StructFieldKind fld => PrimTyMap (as :: [fld :-> Type]) where
  primTyMapSing :: SPrimTyMap as

instance StructFieldKind fld => PrimTyMap ('[] :: [fld :-> Type]) where
  primTyMapSing = SNil

instance forall ( fld :: Type           )
                ( k   :: fld            )
                ( a   :: Type           )
                ( as  :: [fld :-> Type] )
      . ( StructFieldKind fld
        , Known fld k
        , PrimTy a
        , PrimTyMap as
        )
       => PrimTyMap ((k ':-> a) ': as) where
  primTyMapSing = SCons

instance ( Typeable fld, Typeable as, PrimTyMap as )
       => PrimTy (Struct (as :: [fld :-> Type])) where
  primTySing = SStruct


primTy :: forall ty. PrimTy ty => SPIRV.PrimTy
primTy = sPrimTy ( primTySing @ty )

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
    :: ( Syntactic (f a), Internal (f a) ~ f (Internal a) )
    => DistDict f a

class ( Applicative f
    --, forall a. (Syntactic a, PrimTy (Internal a)) => Syntactic (f a)
      ) => PrimFunc f where
  primFuncSing :: SPrimFunc f
  distDict     :: ( Syntactic a, PrimTy (Internal a) )
               => DistDict f a

primFuncName :: forall f. PrimFunc f => String
primFuncName
  = case primFuncSing @f of
      sFuncVector@SFuncVector
        -> case sFuncVector of
            ( _ :: SPrimFunc (V n) )
              -> "V " ++ show (natVal (Proxy @n))
      sFuncMatrix@SFuncMatrix
        -> case sFuncMatrix of
            ( _ :: SPrimFunc (M m n) )
              -> "M " ++ show (natVal (Proxy @m))
                      ++ " "
                      ++ show (natVal (Proxy @n))
      sFuncArray@SFuncArray
        -> case sFuncArray of
            ( _ :: SPrimFunc (Array n) )
              -> "Array " ++ show (natVal (Proxy @n))

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
