{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module SPIRV.Types where

-- base
import Data.Proxy   ( Proxy(Proxy) )
import GHC.TypeLits ( KnownNat, natVal )

--------------------------------------------------
-- SPIR-V types

data Width
  = W8
  | W16
  | W32
  deriving ( Show, Eq )

data Signedness
  = Unsigned
  | Signed
  deriving ( Show, Eq )


class KnownSign s where
  signVal :: Proxy s -> Signedness
instance KnownSign 'Unsigned where
  signVal _ = Unsigned
instance KnownSign 'Signed where
  signVal _ = Signed


class KnownWidth w where
  widthVal :: Proxy w -> Width
instance KnownWidth W8 where
  widthVal _ = W8
instance KnownWidth W16 where
  widthVal _ = W16
instance KnownWidth W32 where
  widthVal _ = W32


class KnownScalar a where
  scalarVal :: Proxy a -> PrimTy
instance KnownScalar 'Boolean where -- yes
  scalarVal _ = Boolean
instance (KnownSign s, KnownWidth w) => KnownScalar ('Integer s w) where
  scalarVal _ = Integer (signVal (Proxy @s)) (widthVal (Proxy @w))
instance (KnownWidth w) => KnownScalar ('Floating w) where
  scalarVal _ = Floating (widthVal (Proxy @w))


data PrimTy where
  Unit     :: PrimTy -- known as Void in the SPIR-V specification
  Boolean  :: PrimTy
  Integer  :: Signedness -> Width -> PrimTy
  Floating :: Width -> PrimTy
  Vec      :: (KnownNat n, KnownScalar a) => Proxy n -> Proxy a -> PrimTy
  Mat      :: (KnownNat n, KnownNat m, KnownScalar a) => Proxy n -> Proxy m -> Proxy a -> PrimTy
  -- todo: records, arrays, opaque types, ...

instance Show PrimTy where
  show Unit           = "Unit"
  show Boolean        = "Boolean"
  show (Integer s w  ) = "Integer "  ++ show s ++ " " ++ show w
  show (Floating  w  ) = "Floating " ++ show w
  show (Vec     n a  ) = "Vec "      ++ show (natVal n) ++ " " ++ show (scalarVal a)
  show (Mat     n m a) = "Mat "      ++ show (natVal n) ++ " " ++ show (natVal m) ++ " " ++ show (scalarVal a)


--------------------------------------------------
-- SPIR-V type constructors

data OpType
  = Void
  | Bool
  | Int
  | Float
  | Vector
  | Matrix
  -- | Image
  -- | Sampler
  -- | SampledImage
  -- | Array
  -- | RuntimeArray
  -- | Struct
  -- | Opaque
  -- | Pointer
  | Function
  -- | Event
  -- | DeviceEvent
  -- | ReserveId
  -- | Queue
  -- | Pipe
  -- | ForwardPointer
  deriving Show