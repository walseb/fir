{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BinaryLiterals         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE NumericUnderscores     #-}
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
  ( Nat, KnownNat, Mod, natVal )

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
    '[ "camera"          ':-> CameraCoordinates
     , "reset"           ':-> Int32
     , "missShaderIndex" ':-> Word32
     ]

--------------------------------------------------------------------------
-- Hit types.

type HitType = Word32

-- | Whether the bounce was diffuse (continuous PDF) or specular (discrete probability).
data BounceDistribution = Diffuse | Specular
-- | Whether the ray traversed the surface or remained on the same side.
data BounceType = Reflect  | Refract
-- | Which side the ray came in from, relative to the surface normal vector.
-- 'Positive' means the ray came from the side the normal points towards,
-- i.e. the outside of the surface (assuming outward pointing normals).
data BounceSide = Positive | Negative

pattern EndRay, Miss :: HitType
pattern EndRay = 0b_0_0_0_00
pattern Miss   = 0b_0_0_0_01

bounce :: BounceDistribution -> BounceType -> BounceSide -> HitType
bounce Diffuse  Reflect Positive = 0b_0_0_0_10
bounce Diffuse  Reflect Negative = 0b_0_0_1_10
bounce Diffuse  Refract Positive = 0b_0_1_0_10
bounce Diffuse  Refract Negative = 0b_0_1_1_10
bounce Specular Reflect Positive = 0b_1_0_0_10
bounce Specular Reflect Negative = 0b_1_0_1_10
bounce Specular Refract Positive = 0b_1_1_0_10
bounce Specular Refract Negative = 0b_1_1_1_10

rayFinished :: Code HitType -> Code Bool
rayFinished h = h < 2

rayComingFromInside :: Code HitType -> Code Bool
rayComingFromInside h = h .&. 0b_0_0_1_00 > 0

flipBounceSign :: Code HitType -> Code Bool
flipBounceSign h =
  if refracted h then not ( rayComingFromInside h ) else rayComingFromInside h

refracted :: Code HitType -> Code Bool
refracted h = h .&. 0b_0_1_0_00 > 0

specular :: Code HitType -> Code Bool
specular h = h .&. 0b_1_0_0_00 > 0

--------------------------------------------------------------------------
-- Sizes.

type Width      = 1920 `WithDivisor` LocalSizeX
type Height     = 1080 `WithDivisor` LocalSizeY
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

class KnownNat ( BindingNo x ) => Bindable ( x :: k ) where
  type BindingNo ( x :: k ) = ( bd :: Nat ) | bd -> k x -- injectivity annotation ensures no overlap of resources
type GeometryBindingNo  ( geom :: GeometryKind  ) = BindingNo geom
type LuminaireBindingNo ( lum  :: LuminaireKind ) = BindingNo lum
type MaterialBindingNo  ( mat  :: MaterialKind  ) = BindingNo mat

data IndexBuffer = TriangleIndexBuffer

-- | Data-kind used to parametrise (primary) miss shaders
data MissKind
  = EnvironmentBlackbody
  | Factor
  | Sky

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
  | Sun       -- ^ Sun (seen through an atmosphere).
  deriving stock ( Prelude.Show, Prelude.Eq, Prelude.Ord )

-- | Data-kind used to parametrise different materials.
data MaterialKind
  = Lambertian   -- ^ Lambertian diffuse material.
  | Fresnel      -- ^ Fresnel material.
  | RoughFresnel -- ^ Fresnel material with microfacet roughness.
  deriving stock ( Prelude.Show, Prelude.Eq, Prelude.Ord )


instance Bindable UBO where
  type BindingNo UBO = 0
instance Bindable AccelerationStructure where
  type BindingNo AccelerationStructure = 1
instance Bindable LuminaireID where
  type BindingNo LuminaireID = 2
instance Bindable MissKind where
  type BindingNo MissKind = 3
instance Bindable TriangleIndexBuffer where
  type BindingNo TriangleIndexBuffer = 4
instance Bindable Triangle where
  type BindingNo Triangle = 5
instance Bindable Sphere where
  type BindingNo Sphere = 6
instance Bindable Blackbody where
  type BindingNo Blackbody = 7
instance Bindable Lambertian where
  type BindingNo Lambertian = 8
instance Bindable Fresnel where
  type BindingNo Fresnel = 9
instance Bindable RoughFresnel where
  type BindingNo RoughFresnel = 10

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

type MieParams = Struct
  '[ "v"            ':-> Float
   , "directionalG" ':-> Float
   , "zenithLength" ':-> Float
   , "turbidity"    ':-> Float
   , "weight"       ':-> Float
   , "k_array"      ':-> Array 82 Float
   ]

type RayleighParams = Struct
  '[ "depolarisation" ':-> Float
   , "zenithLength"   ':-> Float
   , "intensity"      ':-> Float
   , "numMolecules"   ':-> Float
   , "ior"            ':-> Float
   ]

type SunParams = Struct
  '[ "position"  ':-> V 3 Float
   , "intensity" ':-> Float
   , "falloff"   ':-> Float
   ]

type MissData = Struct
  '[ "blackbody" ':-> Struct '[ "temperature" ':-> Float, "intensity" ':-> Float ]
   , "factor"    ':-> Float
   , "sky"       ':-> Struct
      [ "mie"      ':-> MieParams
      , "rayleigh" ':-> RayleighParams
      , "sun"      ':-> SunParams
      ]
   ]

--------------------------------------------------------------------------
-- Utility functions.

-- Machine epsilon for 32 bit floating pointer numbers, 2^-24.
ε :: Float
ε = 0b0_01100111_00000000000000000000000

γ :: Float -> Float
γ n = n * ε / ( 1 - n * ε )

tracePrimaryRay
  :: forall ( primaryPayloadName :: Symbol ) ( s :: ProgramState )
  .  ( KnownSymbol primaryPayloadName, CanTraceRay primaryPayloadName s )
  => Code AccelerationStructure
  -> Code Word32
  -> Code ( V 3 Float )
  -> Code ( V 3 Float )
  -> Program s s ( Code () )
tracePrimaryRay accel missIndex rayOrigin rayDirection = do
  let
    rayInfo :: RayInfo
    rayInfo = RayInfo
      { rayFlags     = Lit RayFlagsOpaque
      , rayOrigin
      , rayDirection
      , rayTMin      = 0
      , rayTMax      = 1e10
      , cullMask     = 0xff
      }
    rayShaderInfo :: RayShaderInfo
    rayShaderInfo = RayShaderInfo
      { bindingTableOffset = 0
      , bindingTableStride = 2
      , missShaderIndex    = missIndex
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
      , rayTMin      = 0
      , rayTMax      = 1e10
      , cullMask     = 0xff -- could use a different cull mask
      }
    rayShaderInfo :: RayShaderInfo
    rayShaderInfo = RayShaderInfo
      { bindingTableOffset = 1
      , bindingTableStride = 2
      , missShaderIndex    = 0
      }
  traceRay @occlusionPayloadName accel rayInfo rayShaderInfo
