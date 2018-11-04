{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.PrimTy where

-- base
import Control.Applicative(liftA2)
import Data.Int(Int8, Int16, Int32, Int64)
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import Data.Type.Equality((:~:)(Refl))
import Data.Typeable(Typeable, eqT)
import Data.Word(Word8, Word16, Word32, Word64)
import GHC.TypeLits( Symbol, KnownSymbol, symbolVal
                   , TypeError, ErrorMessage(Text)
                   )
import GHC.TypeNats(Nat, KnownNat, natVal)

-- half
import Numeric.Half(Half)

-- text-utf8
import qualified Data.Text as Text

-- fir
import Data.Binary.Class.Put(Put)
import Data.Type.Bindings( Binding
                         , Assignment, type (:->)
                         , SymbolMap, BindingsMap
                         , Var
                         , Permission, permissions
                         , KnownPermissions
                         )
import Math.Algebra.Class(Ring)
import Math.Linear(V, M)
import qualified SPIRV.PrimTy as SPIRV
import SPIRV.PrimTy ( Signedness(Unsigned, Signed)
                    , Width(W8,W16,W32,W64)
                    )

------------------------------------------------------------
-- primitive types, which can be internalised in the AST
------------------------------------------------------------

-- basic types:
-- Unit ()
-- Bool
-- Word8, ..., Word64
-- Int8, ..., Int64
-- vectors
-- matrices
-- arrays
-- structs (ordered records)

-- importantly, function types are not "primitive"

------------------------------------------------------------
-- arrays and structs

data Array :: Nat -> Type -> Type where
  MkArray :: forall n a. KnownNat n => [a] -> Array n a

deriving instance Eq   a => Eq   (Array l a)
deriving instance Ord  a => Ord  (Array l a)
deriving instance Show a => Show (Array l a)
deriving instance Functor     (Array n)
deriving instance Foldable    (Array n)
deriving instance Traversable (Array n)

newtype RuntimeArray a = RuntimeArray [a]

deriving instance Eq   a => Eq   (RuntimeArray a)
deriving instance Ord  a => Ord  (RuntimeArray a)
deriving instance Show a => Show (RuntimeArray a)
deriving instance Functor     RuntimeArray
deriving instance Foldable    RuntimeArray
deriving instance Traversable RuntimeArray

-- order *matters* for structs (memory layout!)
data Struct :: [Assignment Symbol Type] -> Type where
  End  :: Struct '[]
  (:&) :: forall k a as. (KnownSymbol k, PrimTy a)
       => a -> Struct as -> Struct ((k :-> a) ': as)

deriving instance Eq   (Struct as)
deriving instance Ord  (Struct as)
deriving instance Show (Struct as)

foldrStruct
  :: forall as b.
  (forall a. PrimTy a => a -> b -> b)
  -> b -> Struct as -> b
foldrStruct f b = go @as
  where go :: forall xs. Struct xs -> b
        go End       = b
        go (a :& as) = f a (go as)

traverseStruct :: Applicative f =>
  ( forall a. PrimTy a => a -> f b )
  -> Struct as
  -> f [b]
traverseStruct f
   = foldrStruct ( \a bs -> liftA2 (:) (f a) bs )
       ( pure [] )

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

data SPrimTy :: Type -> Type where
  SUnit   :: SPrimTy ()
  SBool   :: SPrimTy Bool
  SScalar :: ScalarTy a
          => SScalarTy a -> SPrimTy a
  SVector :: (KnownNat n, PrimTy a)
          => Proxy n -> SPrimTy a -> SPrimTy (V n a)
  SMatrix :: (KnownNat m, KnownNat n, ScalarTy a, Ring a)
          => Proxy m -> Proxy n -> SScalarTy a -> SPrimTy (M m n a)
  SArray  :: (KnownNat n, PrimTy a)
          => Proxy n -> SPrimTy a -> SPrimTy (Array n a)
  SRuntimeArray
          :: PrimTy a
          => SPrimTy a -> SPrimTy (RuntimeArray a)
  SStruct :: SPrimTyBindings as -> SPrimTy (Struct as)

data SPrimTyBindings :: SymbolMap Type -> Type where
  SNilBindings  :: SPrimTyBindings '[]
  SConsBindings :: (KnownSymbol k, PrimTy a)
                => Proxy k
                -> SPrimTy a
                -> SPrimTyBindings as
                -> SPrimTyBindings ((k :-> a) ': as)


class ( Show ty                    -- for convenience
      , Eq ty, Ord ty, Typeable ty -- to keep track of lists of constants
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

class ScalarTy ty => IntegralTy ty where
instance IntegralTy Word8  where
instance IntegralTy Word16 where
instance IntegralTy Word32 where
instance IntegralTy Word64 where
instance IntegralTy Int8   where
instance IntegralTy Int16  where
instance IntegralTy Int32  where
instance IntegralTy Int64  where

instance PrimTy Word8  where
  primTySing = SScalar SWord8
instance PrimTy Word16 where
  primTySing = SScalar SWord16
instance PrimTy Word32 where
  primTySing = SScalar SWord32
instance PrimTy Word64 where
  primTySing = SScalar SWord64
instance PrimTy Int8   where
  primTySing = SScalar SInt8
instance PrimTy Int16  where
  primTySing = SScalar SInt16
instance PrimTy Int32  where
  primTySing = SScalar SInt32
instance PrimTy Int64  where
  primTySing = SScalar SInt64
instance PrimTy Half   where
  primTySing = SScalar SHalf
instance PrimTy Float  where
  primTySing = SScalar SFloat
instance PrimTy Double where
  primTySing = SScalar SDouble


instance TypeError
  ( Text "Use a specific width unsigned integer type instead of 'Word' (recommended: 'Word32')." )
    => PrimTy Word where
  primTySing = error "unreachable" 
instance TypeError
  ( Text "Use a specific width signed integer type instead of 'Int' (recommended: 'Int32')." )
    => PrimTy Int where
  primTySing = error "unreachable"
instance TypeError
  ( Text "Use a specific width unsigned integer type instead of 'Word' (recommended: 'Word32')." )
    => ScalarTy Word where
  scalarTySing = error "unreachable" 
instance TypeError
  ( Text "Use a specific width signed integer type instead of 'Int' (recommended: 'Int32')." )
    => ScalarTy Int where
  scalarTySing = error "unreachable"


instance ( PrimTy a
         , KnownNat n--, 2 <= n, n <= 4
         ) => PrimTy (V n a) where
  primTySing = SVector (Proxy @n) (primTySing @a)

instance ( PrimTy a, ScalarTy a
         , Ring a
         , KnownNat m--, 2 <= m, m <= 4
         , KnownNat n--, 2 <= n, n <= 4
         ) => PrimTy (M m n a) where
  primTySing = SMatrix (Proxy @m) (Proxy @n) (scalarTySing @a)


instance (PrimTy a, KnownNat l) => PrimTy (Array l a) where
  primTySing = SArray (Proxy @l) (primTySing @a)

instance PrimTy a => PrimTy (RuntimeArray a) where
  primTySing = SRuntimeArray (primTySing @a)




primTy :: forall ty. PrimTy ty => SPIRV.PrimTy
primTy = sPrimTy ( primTySing @ty )

primTyVal :: forall ty. PrimTy ty => Proxy ty -> SPIRV.PrimTy
primTyVal _ = primTy @ty

val :: KnownNat n => Proxy n -> Word32
val = fromIntegral . natVal

sPrimTy :: SPrimTy ty -> SPIRV.PrimTy
sPrimTy SUnit             = SPIRV.Unit
sPrimTy SBool             = SPIRV.Boolean
sPrimTy (SScalar       a) = SPIRV.Scalar                 (sScalarTy a)
sPrimTy (SVector n     a) = SPIRV.Vector (val n)         (sPrimTy   a)
sPrimTy (SMatrix m n   a) = SPIRV.Matrix (val m) (val n) (sScalarTy a)
sPrimTy (SArray l      a) = SPIRV.Array  (val l)         (sPrimTy   a)
sPrimTy (SRuntimeArray a) = SPIRV.RuntimeArray           (sPrimTy   a)
sPrimTy (SStruct      as) = SPIRV.Struct (sPrimTyBindings as)

sPrimTyBindings :: SPrimTyBindings ty -> [SPIRV.PrimTy]
sPrimTyBindings SNilBindings           = []
sPrimTyBindings (SConsBindings _ a as) = sPrimTy a : sPrimTyBindings as

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


class KnownVar (bd :: Assignment Symbol Binding) where
  knownVar :: Proxy bd -> (Text.Text, (SPIRV.PrimTy, [Permission]))

instance (KnownSymbol k, KnownPermissions ps, PrimTy a)
       => KnownVar (k :-> Var ps a) where
  knownVar _ = ( Text.pack . symbolVal $ Proxy @k
               , ( primTy @a
                 , permissions (Proxy @ps)
                 )
               )

-- doing this by hand...
class KnownVars (bds :: BindingsMap) where
  knownVars :: Proxy bds -> [(Text.Text, (SPIRV.PrimTy, [Permission]))]
instance KnownVars '[] where
  knownVars _ = []
instance (KnownVar bd, KnownVars bds)
      => KnownVars (bd ': bds) where
  knownVars _ = knownVar (Proxy @bd) : knownVars (Proxy @bds)

------------------------------------------------------------
-- functors

data SPrimFunc :: (Type -> Type) -> Type where
  SFuncVector :: KnownNat n
              => Proxy n -> SPrimFunc (V n)
  SFuncMatrix :: (KnownNat m, KnownNat n)
              => Proxy m -> Proxy n -> SPrimFunc (M m n)

primFuncName :: SPrimFunc f -> String
primFuncName (SFuncVector n) 
  = "V " ++ show (natVal n)
primFuncName (SFuncMatrix m n)
  = "M " ++ show (natVal m)
         ++ " "
         ++ show (natVal n)


------------------------------------------------------------
-- dependent pair

data AConstant :: Type where
  AConstant :: PrimTy ty => Proxy ty -> ty -> AConstant
  

aConstant :: PrimTy ty => ty -> AConstant
aConstant = AConstant Proxy

eqTProx :: forall s t. (Typeable s, Typeable t) => Proxy s -> Proxy t -> Maybe (s :~: t)
eqTProx _ _ = eqT @s @t

instance Show AConstant where
  show (AConstant ty a) = "Constant " ++ show a ++ " of type " ++ show (primTyVal ty)

instance Eq AConstant where
  (AConstant ty1 a1) == (AConstant ty2 a2)
    = case eqTProx ty1 ty2 of
        Just Refl -> a1 == a2
        _         -> False

instance Ord AConstant where
  (AConstant ty1 a1) `compare` (AConstant ty2 a2)
    = case eqTProx ty1 ty2 of
         Just Refl -> compare a1 a2
         _         -> compare (primTyVal ty1)
                              (primTyVal ty2)