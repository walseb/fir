{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module FIR.Examples.RayTracing.Luminaire
  (
  -- * Luminaires
    LuminaireKind(..), Luminaire(..)
  -- * Hit emitter
  , Emitter(..)
  -- * Light sampling
  , LightSamplingMethod(..), SampleableGeometry(..)
  )
  where

-- base
import qualified Prelude
  ( Show, Eq, Ord )
import Data.Kind
  ( Type )
import Data.Typeable
  ( Typeable )
import GHC.TypeNats
  ( Nat, KnownNat )

-- fir
import FIR
  hiding ( Geometry, Triangle )
import Math.Linear

-- fir-examples
import FIR.Examples.RayTracing.Colour
  ( blackbodySpectrum )
import FIR.Examples.RayTracing.Geometry
  ( Geometry(..) ) -- + instances
import FIR.Examples.RayTracing.QuasiRandom
  ( QuasiRandom, random01s )
import FIR.Examples.RayTracing.Types
  ( GeometryKind(..)
  , LuminaireKind(..)
  , EmitterCallableData, LightSamplingCallableData
  )

--------------------------------------------------------------------------
-- Luminaires.

class    ( KnownNat ( LuminaireBindingNo  lum )
         , PrimTy   ( LuminaireProperties lum )
         , Typeable lum
         )
      => Luminaire ( lum :: LuminaireKind )
      where

  type LuminaireBindingNo  lum = ( bdNo :: Nat ) | bdNo -> lum
  type LuminaireProperties lum :: Type
  luminaireKind :: LuminaireKind

instance Luminaire Blackbody where
  type LuminaireBindingNo  Blackbody = 5
  type LuminaireProperties Blackbody = Struct '[ "temperature" ':-> Float, "intensity" ':-> Float ]
  luminaireKind = Blackbody

--------------------------------------------------------------------------
-- Emitter callable shaders: query emission properties of a luminaire,
-- in the given direction and at the given wavelengths.


type EmitterCallableDefs ( lum :: LuminaireKind ) =
  '[ "callableData"   ':-> CallableDataIn '[] EmitterCallableData
   , "luminaireProps" ':-> StorageBuffer '[ DescriptorSet 0, Binding ( LuminaireBindingNo lum ), NonWritable ]
                              ( Struct '[ "propsArray" ':-> RuntimeArray ( LuminaireProperties lum ) ] )
   , "main"           ':-> EntryPoint '[]
                              Callable
   ]


class Luminaire lum => Emitter lum where
  emitterCallableShader :: Module ( EmitterCallableDefs lum )

instance Emitter Blackbody where

  emitterCallableShader = Module $ entryPoint @"main" @Callable do
    emitterInfoIndex <- use @( Name "callableData" :.: Name "emitterInfoIndex" )
    emitterInfo      <- use @( Name "luminaireProps" :.: Name "propsArray" :.: AnIndex Word32 ) emitterInfoIndex
    emitterTemp      <- let' $ view @( Name "temperature" ) emitterInfo
    emitterIntensity <- let' $ view @( Name "intensity"   ) emitterInfo

    rayDir <- ( let' . normalise ) =<< use @( Name "callableData" :.: Name "rayDirection" )
    normal <- ( let' . normalise ) =<< use @( Name "callableData" :.: Name "normal" )
    ~( Vec4 λ0 λ1 λ2 λ3 ) <- use @( Name "callableData" :.: Name "mainData" )
    r0 <- let' =<< blackbodySpectrum emitterTemp λ0
    r1 <- let' =<< blackbodySpectrum emitterTemp λ1
    r2 <- let' =<< blackbodySpectrum emitterTemp λ2
    r3 <- let' =<< blackbodySpectrum emitterTemp λ3
    let
      radiances :: Code ( V 4 Float )
      radiances = ( 1e-13 * emitterIntensity * abs ( rayDir ^.^ normal ) )
                *^ Vec4 r0 r1 r2 r3
    assign @( Name "callableData" :.: Name "mainData" ) radiances

--------------------------------------------------------------------------
-- Light sampling callable shaders.

data LightSamplingMethod
   = SurfaceArea
   | SolidAngle
   | ProjectedSolidAngle
   deriving stock ( Prelude.Show, Prelude.Eq, Prelude.Ord )

type LightSampleCallableDefs ( geom :: GeometryKind ) =
  '[ "callableData" ':-> CallableDataIn '[] LightSamplingCallableData
   , "geometries"   ':-> StorageBuffer '[ DescriptorSet 0, Binding ( GeometryBindingNo geom ), NonWritable ]
                           ( Struct '[ "geometryArray" ':-> RuntimeArray ( GeometryData geom ) ] )
   , "main"         ':-> EntryPoint '[]
                          Callable
   ]

class SampleableGeometry ( geom :: GeometryKind ) ( meth :: LightSamplingMethod ) where

  lightSampleCallableShader :: Module ( LightSampleCallableDefs geom )

instance SampleableGeometry Triangle SurfaceArea where

  lightSampleCallableShader = Module $ entryPoint @"main" @Callable do

    triangleIndex <- let' =<< use @( Name "callableData" :.: Name "geometryInfoIndex" )
    triangle      <- let' =<< use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 ) triangleIndex
    p0            <- let' $ view @( Name "p0"     ) triangle
    p1            <- let' $ view @( Name "p1"     ) triangle
    p2            <- let' $ view @( Name "p2"     ) triangle
    triNormal     <- let' $ view @( Name "normal" ) triangle
    -- assumes the geometry is not transformed

    rayOrig       <- use @( Name "callableData" :.: Name "rayOrigin" )
    surfaceNormal <- use @( Name "callableData" :.: Name "normal"    )

    _ <- def @"quasiRandomConstants" @R  =<< use @( Name "callableData" :.: Name "quasiRandomConstants" )
    _ <- def @"quasiRandomState"     @RW =<< use @( Name "callableData" :.: Name "quasiRandomState"     )

    trianglePt <- randomPointOnTriangle p0 p1 p2
    rayDir     <- let' $ normalise ( trianglePt ^-^ rayOrig )
    psa_correction <-
      let' $ abs ( rayDir ^.^ surfaceNormal ) * abs ( rayDir ^.^ triNormal )
           / quadrance rayOrig trianglePt

    assign @( Name "callableData" :.: Name "rayOrigin"        ) trianglePt
    assign @( Name "callableData" :.: Name "psa_correction"   ) psa_correction
    assign @( Name "callableData" :.: Name "quasiRandomState" ) =<< get @"quasiRandomState"


instance SampleableGeometry Sphere SurfaceArea where

  lightSampleCallableShader = Module $ entryPoint @"main" @Callable do

    sphereIndex <- let' =<< use @( Name "callableData" :.: Name "geometryInfoIndex" )
    sphere      <- let' =<< use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 ) sphereIndex
    c           <- let' $ view @( Name "center" ) sphere
    r           <- let' $ view @( Name "radius" ) sphere
    -- assumes the geometry is not transformed

    rayOrig       <- use @( Name "callableData" :.: Name "rayOrigin" )
    surfaceNormal <- use @( Name "callableData" :.: Name "normal"    )

    _ <- def @"quasiRandomConstants" @R  =<< use @( Name "callableData" :.: Name "quasiRandomConstants" )
    _ <- def @"quasiRandomState"     @RW =<< use @( Name "callableData" :.: Name "quasiRandomState"     )

    spherePt       <- randomPointOnSphere c r
    sphereNormal   <- let' $ normalise ( spherePt ^-^ c )  
    rayDir         <- let' $ normalise ( spherePt ^-^ rayOrig )
    psa_correction <-
      let' $ abs ( rayDir ^.^ surfaceNormal ) * abs ( rayDir ^.^ sphereNormal )
           / quadrance rayOrig spherePt

    assign @( Name "callableData" :.: Name "rayOrigin"        ) spherePt
    assign @( Name "callableData" :.: Name "psa_correction"   ) psa_correction
    assign @( Name "callableData" :.: Name "quasiRandomState" ) =<< get @"quasiRandomState"

--------------------------------------------------------------------------

-- | Compute a random point on a triangle, uniformly with respect to surface area.
randomPointOnTriangle
  :: QuasiRandom s
  => Code ( V 3 Float )
  -> Code ( V 3 Float )
  -> Code ( V 3 Float )
  -> Program s s ( Code ( V 3 Float ) )
randomPointOnTriangle p1 p2 p3 = do

  ~( Vec4 t0 s _ _ ) <- random01s
  t <- let' $ sqrt ( 1 - t0 )

  v1 <- let' $ p2 ^-^ p1
  v2 <- let' $ p3 ^-^ p1

  let' $ p1 ^+^ ( t * s ) *^ v1 ^+^ ( 1 - t ) *^ v2

-- | Compute a random point on a sphere, uniformly with respect to surface area.
randomPointOnSphere
  :: QuasiRandom s
  => Code ( V 3 Float )
  -> Code Float
  -> Program s s ( Code ( V 3 Float ) )
randomPointOnSphere center radius = do

  ~( Vec4 c f _ _ ) <- random01s
  cosθ <- let' ( 2 * c - 1 )
  sinθ <- let' ( 2 * sqrt ( c * ( 1 - c ) ) )
  φ    <- let' ( 2 * pi * f )

  -- Use spherical coordinates to return point on sphere.
  -- Physicist's convention:
  --   - φ: azimuthal angle,
  --   - θ: polar angle.
  let' $ center ^+^ radius *^ Vec3 ( cos φ * sinθ ) ( sin φ * sinθ ) cosθ
