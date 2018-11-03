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
import GHC.TypeLits( Symbol, KnownSymbol, symbolVal
                   , TypeError, ErrorMessage(Text)
                   )
import GHC.TypeNats(KnownNat, natVal)

-- half
import Numeric.Half(Half)

-- text-utf8
import qualified Data.Text as Text

-- fir
import Data.Binary.Class.Put(Put)
import Data.Type.Bindings( Binding
                         , Assignment, type (:->)
                         , BindingsMap
                         , Var
                         , Permission, permissions
                         , KnownPermissions
                         )
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

type family ScalarName (ty :: Type) = (r :: SPIRV.ScalarTy ) | r -> ty where
  ScalarName Word8     = SPIRV.Integer Unsigned W8
  ScalarName Word16    = SPIRV.Integer Unsigned W16
  ScalarName Word32    = SPIRV.Integer Unsigned W32
  ScalarName Word64    = SPIRV.Integer Unsigned W64
  ScalarName Int8      = SPIRV.Integer Signed   W8
  ScalarName Int16     = SPIRV.Integer Signed   W16
  ScalarName Int32     = SPIRV.Integer Signed   W32
  ScalarName Int64     = SPIRV.Integer Signed   W64
  ScalarName Half      = SPIRV.Floating         W16
  ScalarName Float     = SPIRV.Floating         W32
  ScalarName Double    = SPIRV.Floating         W64

-- names for the primitive types which can be represented internally
type family TyName (ty :: Type) = (r :: SPIRV.PrimTy) | r -> ty where
  TyName ()        = SPIRV.Unit
  TyName Bool      = SPIRV.Boolean
  TyName Word8     = SPIRV.Scalar ( ScalarName Word8  )
  TyName Word16    = SPIRV.Scalar ( ScalarName Word16 )
  TyName Word32    = SPIRV.Scalar ( ScalarName Word32 )
  TyName Word64    = SPIRV.Scalar ( ScalarName Word64 )
  TyName Int8      = SPIRV.Scalar ( ScalarName Int8   )
  TyName Int16     = SPIRV.Scalar ( ScalarName Int16  )
  TyName Int32     = SPIRV.Scalar ( ScalarName Int32  )
  TyName Int64     = SPIRV.Scalar ( ScalarName Int64  )
  TyName Half      = SPIRV.Scalar ( ScalarName Half   )
  TyName Float     = SPIRV.Scalar ( ScalarName Float  )
  TyName Double    = SPIRV.Scalar ( ScalarName Double )
  TyName (V n   a) = SPIRV.Vector (ToDim n)           (TyName a)
  TyName (M m n a) = SPIRV.Matrix (ToDim m) (ToDim n) (ScalarName a)

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

-- corresponding singletons
data SPrimTy :: Type -> Type where
  SUnit   :: SPrimTy ()
  SBool   :: SPrimTy Bool
  SScalar :: ScalarTy a
          => SScalarTy a -> SPrimTy a
  SVector :: (KnownNat n, PrimTy a)
          => Proxy n -> SPrimTy a -> SPrimTy (V n a)
  SMatrix :: (KnownNat m, KnownNat n, ScalarTy a, Ring a)
          => Proxy m -> Proxy n -> SScalarTy a -> SPrimTy (M m n a)


-- associated functions on singletons

sScalarName :: SScalarTy ty -> SPIRV.SScalarTy (ScalarName ty)
sScalarName SWord8  = SPIRV.SInteger SUnsigned SW8
sScalarName SWord16 = SPIRV.SInteger SUnsigned SW16
sScalarName SWord32 = SPIRV.SInteger SUnsigned SW32
sScalarName SWord64 = SPIRV.SInteger SUnsigned SW64
sScalarName SInt8   = SPIRV.SInteger SSigned   SW8
sScalarName SInt16  = SPIRV.SInteger SSigned   SW16
sScalarName SInt32  = SPIRV.SInteger SSigned   SW32
sScalarName SInt64  = SPIRV.SInteger SSigned   SW64
sScalarName SHalf   = SPIRV.SFloating          SW16
sScalarName SFloat  = SPIRV.SFloating          SW32
sScalarName SDouble = SPIRV.SFloating          SW64

sTyName :: SPrimTy ty -> SPIRV.SPrimTy (TyName ty)
sTyName SUnit             = SPIRV.SUnit
sTyName SBool             = SPIRV.SBoolean
sTyName (SScalar SWord8 ) = SPIRV.SScalar ( sScalarName SWord8  )
sTyName (SScalar SWord16) = SPIRV.SScalar ( sScalarName SWord16 )
sTyName (SScalar SWord32) = SPIRV.SScalar ( sScalarName SWord32 )
sTyName (SScalar SWord64) = SPIRV.SScalar ( sScalarName SWord64 )
sTyName (SScalar SInt8  ) = SPIRV.SScalar ( sScalarName SInt8   )
sTyName (SScalar SInt16 ) = SPIRV.SScalar ( sScalarName SInt16  )
sTyName (SScalar SInt32 ) = SPIRV.SScalar ( sScalarName SInt32  )
sTyName (SScalar SInt64 ) = SPIRV.SScalar ( sScalarName SInt64  )
sTyName (SScalar SHalf  ) = SPIRV.SScalar ( sScalarName SHalf   )
sTyName (SScalar SFloat ) = SPIRV.SScalar ( sScalarName SFloat  )
sTyName (SScalar SDouble) = SPIRV.SScalar ( sScalarName SDouble )
sTyName (SVector n   a  ) = SPIRV.SVector (natSDim n) (sTyName a)
sTyName (SMatrix m n a  ) = SPIRV.SMatrix (natSDim m) (natSDim n) (sScalarName a)


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


primTy :: forall ty. PrimTy ty => SPIRV.PrimTy
primTy = SPIRV.fromSPrimTy $ sTyName ( primTySing @ty )

sPrimTy :: SPrimTy a -> SPIRV.PrimTy
sPrimTy = SPIRV.fromSPrimTy . sTyName

scalarTy :: forall ty. ScalarTy ty => SPIRV.ScalarTy
scalarTy = SPIRV.fromSScalarTy $ sScalarName ( scalarTySing @ty )

sScalarTy :: SScalarTy a -> SPIRV.ScalarTy
sScalarTy = SPIRV.fromSScalarTy . sScalarName

primTyVal :: forall ty. PrimTy ty => Proxy ty -> SPIRV.PrimTy
primTyVal _ = primTy @ty

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