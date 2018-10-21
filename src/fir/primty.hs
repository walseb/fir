{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.PrimTy where

-- base
import Data.Int(Int8, Int16, Int32, Int64)
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import Data.Type.Equality((:~:)(Refl))
import Data.Typeable(Typeable, eqT)
import Data.Word(Word8, Word16, Word32, Word64)
import GHC.TypeLits(TypeError, ErrorMessage(Text))
import GHC.TypeNats(KnownNat)

-- binary
import Data.Binary(Binary(get,put))

-- half
import Numeric.Half(Half)

-- fir
import Math.Algebra.Class(Ring)
import Math.Linear(V, M)
import qualified SPIRV.PrimTy as SPIRV
import SPIRV.PrimTy ( Signedness(Unsigned, Signed)
                    , SSignedness(SUnsigned, SSigned)
                    , Width(W8,W16,W32,W64)
                    , SWidth(SW8,SW16,SW32,SW64)
                    , ToDim, natSDim
                    )

------------------------------------------------------------
-- primitive types, which can be internalised in the AST

-- names for the primitive types which can be represented internally
type family TyName (ty :: Type) = (r :: SPIRV.PrimTy) | r -> ty where
  TyName ()        = SPIRV.Unit
  TyName Bool      = SPIRV.Boolean
  TyName Word8     = SPIRV.Integer Unsigned W8
  TyName Word16    = SPIRV.Integer Unsigned W16
  TyName Word32    = SPIRV.Integer Unsigned W32
  TyName Word64    = SPIRV.Integer Unsigned W64
  TyName Int8      = SPIRV.Integer Signed   W8
  TyName Int16     = SPIRV.Integer Signed   W16
  TyName Int32     = SPIRV.Integer Signed   W32
  TyName Int64     = SPIRV.Integer Signed   W64
  TyName Half      = SPIRV.Floating         W16
  TyName Float     = SPIRV.Floating         W32
  TyName Double    = SPIRV.Floating         W64
  TyName (V n   a) = SPIRV.Vector (ToDim n)           (TyName a)
  TyName (M m n a) = SPIRV.Matrix (ToDim m) (ToDim n) (TyName a)

-- corresponding singletons
data SPrimTy :: Type -> Type where
  SUnit   :: SPrimTy ()
  SBool   :: SPrimTy Bool
  SWord8  :: SPrimTy Word8
  SWord16 :: SPrimTy Word16
  SWord32 :: SPrimTy Word32
  SWord64 :: SPrimTy Word64
  SInt8   :: SPrimTy Int8
  SInt16  :: SPrimTy Int16
  SInt32  :: SPrimTy Int32
  SInt64  :: SPrimTy Int64
  SHalf   :: SPrimTy Half
  SFloat  :: SPrimTy Float
  SDouble :: SPrimTy Double
  SVector :: (KnownNat n, PrimScalarTy a)
          => Proxy n -> SPrimTy a -> SPrimTy (V n a)
  SMatrix :: (KnownNat m, KnownNat n, PrimScalarTy a, Ring a)
          => Proxy m -> Proxy n -> SPrimTy a -> SPrimTy (M m n a)

-- associated function on singletons (should not need to write this)
sTyName :: SPrimTy a -> SPIRV.SPrimTy (TyName a)
sTyName SUnit           = SPIRV.SUnit
sTyName SBool           = SPIRV.SBoolean
sTyName SWord8          = SPIRV.SInteger SUnsigned SW8
sTyName SWord16         = SPIRV.SInteger SUnsigned SW16
sTyName SWord32         = SPIRV.SInteger SUnsigned SW32
sTyName SWord64         = SPIRV.SInteger SUnsigned SW64
sTyName SInt8           = SPIRV.SInteger SSigned   SW8
sTyName SInt16          = SPIRV.SInteger SSigned   SW16
sTyName SInt32          = SPIRV.SInteger SSigned   SW32
sTyName SInt64          = SPIRV.SInteger SSigned   SW64
sTyName SHalf           = SPIRV.SFloating          SW16
sTyName SFloat          = SPIRV.SFloating          SW32
sTyName SDouble         = SPIRV.SFloating          SW64
sTyName (SVector n   a) = SPIRV.SVector (natSDim n) (sTyName a)
sTyName (SMatrix m n a) = SPIRV.SMatrix (natSDim m) (natSDim n) (sTyName a)


class ( Show ty                    -- for convenience
      , Eq ty, Ord ty, Typeable ty -- to keep track of lists of constants
      , Binary ty                  -- for serialisation
      ) 
    => PrimTy ty where
  primTySing :: SPrimTy ty

instance PrimTy ()     where
  primTySing = SUnit
instance PrimTy Bool   where
  primTySing = SBool
instance PrimTy Word8 where
  primTySing = SWord8
instance PrimTy Word16 where
  primTySing = SWord16
instance PrimTy Word32 where
  primTySing = SWord32
instance PrimTy Word64 where
  primTySing = SWord64
instance PrimTy Int8  where
  primTySing = SInt8
instance PrimTy Int16  where
  primTySing = SInt16
instance PrimTy Int32  where
  primTySing = SInt32
instance PrimTy Int64  where
  primTySing = SInt64
instance PrimTy Half   where
  primTySing = SHalf
instance PrimTy Float  where
  primTySing = SFloat
instance PrimTy Double where
  primTySing = SDouble


instance Binary Half where
  get = error "todo"
  put = error "todo"


instance TypeError 
  ( Text "Use a specific width unsigned integer type instead of 'Word' (recommended: 'Word32')." )
    => PrimTy Word where
  primTySing = error "unreachable" 
instance TypeError 
  ( Text "Use a specific width signed integer type instead of 'Int' (recommended: 'Int32')." )
    => PrimTy Int where
  primTySing = error "unreachable"

primTy :: forall ty. PrimTy ty => SPIRV.PrimTy
primTy = SPIRV.fromSPrimTy $ sTyName ( primTySing @ty )

sPrimTy :: SPrimTy a -> SPIRV.PrimTy
sPrimTy = SPIRV.fromSPrimTy . sTyName

primTyPx :: forall ty. PrimTy ty => Proxy ty -> SPIRV.PrimTy
primTyPx _ = primTy @ty

class PrimTy a => PrimScalarTy a where

-- not making Bool a scalar type, contrary to SPIR-V spec
instance PrimScalarTy Word8  where
instance PrimScalarTy Word16 where
instance PrimScalarTy Word32 where
instance PrimScalarTy Word64 where
instance PrimScalarTy Int8   where
instance PrimScalarTy Int16  where
instance PrimScalarTy Int32  where
instance PrimScalarTy Int64  where
instance PrimScalarTy Half   where
instance PrimScalarTy Float  where
instance PrimScalarTy Double where

instance TypeError 
  ( Text "Use a specific width unsigned integer type instead of 'Word' (recommended: 'Word32')." )
    => PrimScalarTy Word where
instance TypeError 
  ( Text "Use a specific width signed integer type instead of 'Int' (recommended: 'Int32')." )
    => PrimScalarTy Int where

instance ( PrimScalarTy a
         , KnownNat n--, 2 <= n, n <= 4
         ) => PrimTy (V n a) where
  primTySing = SVector (Proxy @n) (primTySing @a)

instance ( PrimScalarTy a
         , Ring a
         , KnownNat m--, 2 <= m, m <= 4
         , KnownNat n--, 2 <= n, n <= 4
         ) => PrimTy (M m n a) where
  primTySing = SMatrix (Proxy @m) (Proxy @n) (primTySing @a)

------------------------------------------------------------
-- dependent pair

data AConstant :: Type where
  AConstant :: PrimTy ty => Proxy ty -> ty -> AConstant
  

aConstant :: PrimTy ty => ty -> AConstant
aConstant = AConstant Proxy

eqTProx :: forall a b. (Typeable a, Typeable b) => Proxy a -> Proxy b -> Maybe (a :~: b)
eqTProx _ _ = eqT @a @b

instance Eq AConstant where
  (AConstant px1 v1) == (AConstant px2 v2) 
    = case eqTProx px1 px2 of
        Just Refl -> v1 == v2
        _         -> False

instance Ord AConstant where
  (AConstant px1 v1) `compare` (AConstant px2 v2)
    = case eqTProx px1 px2 of
         Just Refl -> compare v1 v2
         _         -> compare (primTyPx px1)
                              (primTyPx px2)