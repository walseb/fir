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

module FIR.PrimTy where

-- base
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import Data.Type.Equality((:~:)(Refl))
import Data.Typeable(Typeable, eqT)
import GHC.TypeNats(KnownNat)

-- binary
import Data.Binary(Binary)

-- fir
import Math.Algebra.Class(Ring)
import Math.Linear(V, M)
import qualified SPIRV.PrimTy as SPIRV
import SPIRV.PrimTy ( Signedness(Unsigned, Signed), SSignedness(SUnsigned, SSigned)
                    , Width(W32, W64), SWidth(SW32,SW64)
                    , ToDim, natSDim
                    )


------------------------------------------------------------
-- primitive types, which can be internalised in the AST

-- internal name for the primitive types which can be represented internally
type family TyName (ty :: Type) = (r :: SPIRV.PrimTy) | r -> ty where
  TyName ()        = SPIRV.Unit
  TyName Bool      = SPIRV.Boolean
  TyName Word      = SPIRV.Integer Unsigned W32
  TyName Int       = SPIRV.Integer Signed   W32
  TyName Float     = SPIRV.Floating         W32
  TyName Double    = SPIRV.Floating         W64
  TyName (V n   a) = SPIRV.Vector (ToDim n)           (TyName a)
  TyName (M m n a) = SPIRV.Matrix (ToDim m) (ToDim n) (TyName a)

-- corresponding singletons
data SPrimTy :: Type -> Type where
  SUnit   :: SPrimTy ()
  SBool   :: SPrimTy Bool
  SWord   :: SPrimTy Word
  SInt    :: SPrimTy Int
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
sTyName SWord           = SPIRV.SInteger SUnsigned SW32
sTyName SInt            = SPIRV.SInteger SSigned   SW32
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
instance PrimTy Word   where
  primTySing = SWord
instance PrimTy Int    where
  primTySing = SInt
instance PrimTy Float  where
  primTySing = SFloat
instance PrimTy Double where
  primTySing = SDouble

primTy :: forall ty. PrimTy ty => SPIRV.PrimTy
primTy = SPIRV.fromSPrimTy $ sTyName ( primTySing @ty )

sPrimTy :: SPrimTy a -> SPIRV.PrimTy
sPrimTy = SPIRV.fromSPrimTy . sTyName

primTyPx :: forall ty. PrimTy ty => Proxy ty -> SPIRV.PrimTy
primTyPx _ = primTy @ty

class PrimTy a => PrimScalarTy a where
instance PrimScalarTy Word   where
instance PrimScalarTy Int    where
instance PrimScalarTy Float  where
instance PrimScalarTy Double where

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