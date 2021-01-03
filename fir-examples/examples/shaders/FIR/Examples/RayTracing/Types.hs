{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Examples.RayTracing.Types where

-- base
import qualified Prelude
  ( Show, Eq, Ord
  , toInteger
  )
import Data.Proxy
  ( Proxy(..) )
import Data.Type.Bool
  ( If )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, KnownNat, Mod, type (<=), natVal )

-- fir
import FIR
  hiding ( Triangle )
import Math.Linear

-- fir-examples
import FIR.Examples.RayTracing.Camera
  ( CameraCoordinates )

--------------------------------------------------------------------------
-- UBO.

type UBO =
  Struct
    '[ "camera" ':-> CameraCoordinates
     , "reset"  ':-> Int32
     ]

--------------------------------------------------------------------------
-- Hit types.

type HitType = Int32

pattern EndRay, Miss, Specular, Diffuse :: HitType
pattern EndRay   = (-2)
pattern Miss     = (-1)
pattern Specular = 0
pattern Diffuse  = 1

--------------------------------------------------------------------------
-- Sizes.

type Width      = 1280 `WithDivisor` LocalSizeX
type Height     = 720  `WithDivisor` LocalSizeY
type LocalSizeX = 16
type LocalSizeY = 8

width, height, localSizeX, localSizeY, mipWidth, mipHeight :: Semiring a => a
width      = fromInteger ( Prelude.toInteger $ natVal ( Proxy @Width  ) )
height     = fromInteger ( Prelude.toInteger $ natVal ( Proxy @Height ) )
localSizeX = fromInteger ( Prelude.toInteger $ natVal ( Proxy @LocalSizeX ) )
localSizeY = fromInteger ( Prelude.toInteger $ natVal ( Proxy @LocalSizeY ) )
mipWidth   = 1
mipHeight  = 1

infixl 7 `WithDivisor`
type family n `WithDivisor` d where
  n `WithDivisor` d =
    If ( n `Mod` d == 0 )
      n
      ( TypeError ( ShowType d :<>: Text " does not divide " :<>: ShowType n ) )

--------------------------------------------------------------------------
-- Ray payloads.

type PrimaryPayload =
  Struct
    '[ "quasiRandomConstants" ':-> V 4 Float
     , "quasiRandomState"     ':-> V 4 Float
     , "hitType"              ':-> HitType
     , "extinction"           ':-> V 4 Float -- extinction coefficients per wavelength
     , "worldRayOrigin"       ':-> V 3 Float
     , "worldRayDirection"    ':-> V 3 Float
     , "wavelengths"          ':-> V 4 Float
     , "throughput"           ':-> V 4 Float
     , "radiance"             ':-> V 4 Float
     ]

type OcclusionPayload =
  Struct
    '[ "primitiveID" ':-> Int32     -- value of "gl_PrimitiveID" returned on an intersection, or -1 if no intersection
     , "instanceID"  ':-> Int32     -- value of "gl_InstanceID"                       --- '' ---
     , "hitT"        ':-> Float     -- value of "gl_RayTMax"                          --- '' ---
     , "normal"      ':-> V 3 Float -- normal vector at ray hit intersection
     ]

initialOcclusionPayload :: Code OcclusionPayload
initialOcclusionPayload = Struct ( Lit (-1) :& Lit (-1) :& Lit (-1) :& Vec3 0 0 0 :& End )

--------------------------------------------------------------------------
-- Geometry, Luminaire, Material

class ( KnownNat ( BindingNo x ), 3 <= BindingNo x ) => Bindable ( x :: k ) where
  type BindingNo ( x :: k ) = ( bd :: Nat ) | bd -> k x -- injectivity annotation ensures no overlap of resources
type GeometryBindingNo  ( geom :: GeometryKind  ) = BindingNo geom
type LuminaireBindingNo ( lum  :: LuminaireKind ) = BindingNo lum
type MaterialBindingNo  ( mat  :: MaterialKind  ) = BindingNo mat

data IndexBuffer = TriangleIndexBuffer

-- | Data-kind used to parametrise different geometries.
data GeometryKind
  = Triangle
  | Sphere
  deriving stock ( Prelude.Show, Prelude.Eq, Prelude.Ord )

-- Distinguishing between triangles and non-triangles.
-- This accounts for the fact that:
--  - triangle geometry does not need an intersection shader,
--  - non-triangle geometry must have corresponding AABBs.
type family IsTriangle ( geom :: GeometryKind ) :: Bool where
  IsTriangle Triangle = True
  IsTriangle _        = False
data STriangleQ ( geom :: GeometryKind ) where
  STriangle    :: STriangleQ Triangle
  SNotTriangle :: IsTriangle geom ~ False => STriangleQ geom

-- | Data-kind used to parametrise different luminaires.
data LuminaireKind
  = Blackbody -- ^ Blackbody radiator.
  deriving stock ( Prelude.Show, Prelude.Eq, Prelude.Ord )

-- | Data-kind used to parametrise different materials.
data MaterialKind
  = Lambertian   -- ^ Lambertian diffuse material.
  | Fresnel      -- ^ Fresnel material.
  | RoughFresnel -- ^ Fresnel material with microfacet roughness.
  deriving stock ( Prelude.Show, Prelude.Eq, Prelude.Ord )

instance Bindable TriangleIndexBuffer where
  type BindingNo TriangleIndexBuffer = 3
instance Bindable Triangle where
  type BindingNo Triangle = 4
instance Bindable Sphere where
  type BindingNo Sphere = 5
instance Bindable Blackbody where
  type BindingNo Blackbody = 6
instance Bindable Lambertian where
  type BindingNo Lambertian = 7
instance Bindable Fresnel where
  type BindingNo Fresnel = 8
instance Bindable RoughFresnel where
  type BindingNo RoughFresnel = 9

type ShaderRecord =
  Struct
    '[ "geometryOffset"    ':-> Word32  -- offset into the geometry array appropriate to this shader group
     , "emitterCallable"   ':-> Int32   -- index of emitter callable shader in SBT, -1 if given object is not an emitter
     , "emitterInfoIndex"  ':-> Word32  -- index into the corresponding emitter info array (ignored if callable is -1)
     , "matSampleCallable" ':-> Word32  -- index of material sample callable shader in SBT
     , "matQueryCallable"  ':-> Word32  -- index of material query callable shader in SBT
     , "matPropsIndex"     ':-> Word32  -- offset into the corresponding material property array
     ]

type LuminaireID =
  Struct
    '[ "luminaireWeight"     ':-> Float
     , "emitterInfoIndex"    ':-> Word32
     , "geometryInfoIndex"   ':-> Word32
     , "primitiveID"         ':-> Word32
     , "instanceID"          ':-> Word32
     , "emitterCallable"     ':-> Word32
     , "lightSampleCallable" ':-> Word32
     ]

type EmitterCallableData =
  Struct
    '[ "emitterInfoIndex" ':-> Word32
     , "rayDirection"     ':-> V 3 Float
     , "normal"           ':-> V 3 Float
     , "mainData"         ':-> V 4 Float -- Incoming: wavelengths. Outgoing: spectral radiances.
     ]

type LightSamplingCallableData =
  Struct
    '[ "quasiRandomConstants" ':-> V 4 Float
     , "quasiRandomState"     ':-> V 4 Float
     , "geometryInfoIndex"    ':-> Word32
     , "emitterInfoIndex"     ':-> Word32
     , "normal"               ':-> V 3 Float -- Normal to the surface from where we are sampling lights.
     , "rayOrigin"            ':-> V 3 Float -- Input: ray origin on surface. Output: light sample point.
     , "psa_correction"       ':-> Float     -- Correction factor to account for not sampling with respect to projected solid angle.
     ]

--------------------------------------------------------------------------
-- Utility tracing functions.

tracePrimaryRay
  :: forall ( primaryPayloadName :: Symbol ) ( s :: ProgramState )
  .  ( KnownSymbol primaryPayloadName, CanTraceRay primaryPayloadName s )
  => Code AccelerationStructure -> Code ( V 3 Float ) -> Code ( V 3 Float )
  -> Program s s ( Code () )
tracePrimaryRay accel rayOrigin rayDirection = do
  let
    rayInfo :: RayInfo
    rayInfo = RayInfo
      { rayFlags     = Lit RayFlagsOpaque
      , rayOrigin
      , rayDirection
      , rayTMin      = 7e-2 -- this is terrible, fixes needed
      , rayTMax      = 1e10
      , cullMask     = 0xff
      }
    rayShaderInfo :: RayShaderInfo
    rayShaderInfo = RayShaderInfo
      { bindingTableOffset = 0
      , bindingTableStride = 2
      , missShaderIndex    = 0
      }
  traceRay @primaryPayloadName accel rayInfo rayShaderInfo

traceOcclusionRay
  :: forall ( occlusionPayloadName :: Symbol ) ( s :: ProgramState )
  .  ( KnownSymbol occlusionPayloadName, CanTraceRay occlusionPayloadName s )
  => Code AccelerationStructure -> Code ( V 3 Float ) -> Code ( V 3 Float )
  -> Program s s ( Code () )
traceOcclusionRay accel rayOrigin rayDirection = do
  let
    rayInfo :: RayInfo
    rayInfo = RayInfo
      { rayFlags     = Lit RayFlagsOpaque
      , rayOrigin
      , rayDirection
      , rayTMin      = 7e-2
      , rayTMax      = 1e10
      , cullMask     = 0xff -- could use a different cull mask
      }
    rayShaderInfo :: RayShaderInfo
    rayShaderInfo = RayShaderInfo
      { bindingTableOffset = 1
      , bindingTableStride = 2
      , missShaderIndex    = 1
      }
  traceRay @occlusionPayloadName accel rayInfo rayShaderInfo
