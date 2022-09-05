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
{-# LANGUAGE UndecidableInstances   #-}

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
import Data.Typeable
  ( Typeable )

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
import FIR.Examples.RayTracing.Sky
  ( skyScatter, sunIntensity )
import FIR.Examples.RayTracing.Types
  ( Bindable(..), GeometryBindingNo, LuminaireBindingNo
  , IndexBuffer(..), GeometryKind(..), MissKind(..), MissData
  , LuminaireKind(..)
  , EmitterCallableData, LightSamplingCallableData
  )

--------------------------------------------------------------------------
-- Luminaires.

class    ( PrimTy   ( LuminaireProperties lum )
         , Typeable lum
         )
      => Luminaire ( lum :: LuminaireKind )
      where

  type LuminaireProperties lum :: Type
  luminaireKind :: LuminaireKind

instance Luminaire Blackbody where
  type LuminaireProperties Blackbody = Struct '[ "temperature" ':-> Float, "intensity" ':-> Float ]
  luminaireKind = Blackbody

instance Luminaire Sun where
  type LuminaireProperties Sun = () -- sun properties read from miss shader data
  luminaireKind = Sun

--------------------------------------------------------------------------
-- Emitter callable shaders: query emission properties of a luminaire,
-- in the given direction and at the given wavelengths.


type family EmitterCallableDefs ( lum :: LuminaireKind ) where
  EmitterCallableDefs Sun =
    '[ "callableData" ':-> CallableDataIn '[] EmitterCallableData
     , "skyMissData"  ':-> Uniform   '[ DescriptorSet 0, Binding ( BindingNo MissKind ) ] MissData
     , "main"         ':-> EntryPoint '[] Callable
     ]
  EmitterCallableDefs lum =
    '[ "callableData"   ':-> CallableDataIn '[] EmitterCallableData
     , "luminaireProps" ':-> StorageBuffer '[ DescriptorSet 0, Binding ( LuminaireBindingNo lum ), NonWritable ]
                                ( Struct '[ "propsArray" ':-> RuntimeArray ( LuminaireProperties lum ) ] )
     , "main"           ':-> EntryPoint '[] Callable
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

instance Emitter Sun where

  emitterCallableShader = Module $ entryPoint @"main" @Callable do

    rayleighParams <- let' =<< use @( Name "skyMissData" :.: Name "sky" :.: Name "rayleigh" )
    mieParams      <- let' =<< use @( Name "skyMissData" :.: Name "sky" :.: Name "mie"      )
    sunParams      <- let' =<< use @( Name "skyMissData" :.: Name "sky" :.: Name "sun"      )

    sunDir   <- let' . normalise $ view @( Name "position" ) sunParams
    sunDir_y <- let' . negate $ view @( Swizzle "y" ) sunDir

    rayDir   <- ( let' . normalise ) =<< use @( Name "callableData" :.: Name "rayDirection" )
    normal   <- ( let' . normalise ) =<< use @( Name "callableData" :.: Name "normal" )
    λs       <- use @( Name "callableData" :.: Name "mainData" )

    f_exs    <- let' . view @( Name "f_exs" ) =<< skyScatter rayleighParams mieParams sunParams rayDir λs
    ξ        <- let' . acos $ sunDir_y
    sun_e    <- sunIntensity sunParams ξ

    let
      radiances :: Code ( V 4 Float )
      radiances = ( abs ( rayDir ^.^ normal ) * sun_e ) *^ f_exs

    assign @( Name "callableData" :.: Name "mainData" ) radiances

--------------------------------------------------------------------------
-- Light sampling callable shaders.

data LightSamplingMethod
   = SurfaceArea
   | SolidAngle
   | ProjectedSolidAngle
   deriving stock ( Prelude.Show, Prelude.Eq, Prelude.Ord )

type LightSampleCallableDefs_ ( geom :: GeometryKind ) =
  '[ "callableData" ':-> CallableDataIn '[] LightSamplingCallableData
   , "geometries"   ':-> StorageBuffer '[ DescriptorSet 0, Binding ( GeometryBindingNo geom ), NonWritable ]
                           ( Struct '[ "geometryArray" ':-> RuntimeArray ( GeometryData geom ) ] )
   , "main"         ':-> EntryPoint '[]
                          Callable
   ]

type family LightSampleCallableDefs ( geom :: GeometryKind ) where
  LightSampleCallableDefs Triangle =
    ( "triangleIndices" ':->
        StorageBuffer '[ DescriptorSet 0, Binding ( BindingNo TriangleIndexBuffer ), NonWritable ]
          ( Struct '[ "indices" ':-> RuntimeArray ( Struct '[ "i0" ':-> Word32, "i1" ':-> Word32, "i2" ':-> Word32 ] ) ] )
    ) : LightSampleCallableDefs_ Triangle
  LightSampleCallableDefs geom = LightSampleCallableDefs_ geom

class SampleableGeometry ( geom :: GeometryKind ) ( meth :: LightSamplingMethod ) where

  lightSampleCallableShader :: Module ( LightSampleCallableDefs geom )

instance SampleableGeometry Triangle SurfaceArea where

  lightSampleCallableShader = Module $ entryPoint @"main" @Callable do

    triangleIndex <- let' =<< use @( Name "callableData" :.: Name "geometryInfoIndex" )
    is  <- use @( Name "triangleIndices" :.: Name "indices" :.: AnIndex Word32 ) triangleIndex
    i0  <- let' $ view @( Name "i0" ) is
    i1  <- let' $ view @( Name "i1" ) is
    i2  <- let' $ view @( Name "i2" ) is
    pn0 <- let' =<< use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 ) i0
    pn1 <- let' =<< use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 ) i1
    pn2 <- let' =<< use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 ) i2
    p0  <- let' $ view @( Name "vertex" ) pn0
    p1  <- let' $ view @( Name "vertex" ) pn1
    p2  <- let' $ view @( Name "vertex" ) pn2
    n0  <- let' $ view @( Name "normal" ) pn0
    n1  <- let' $ view @( Name "normal" ) pn1
    n2  <- let' $ view @( Name "normal" ) pn2
    -- assumes the geometry is not transformed

    rayOrig       <- use @( Name "callableData" :.: Name "rayOrigin" )
    surfaceNormal <- use @( Name "callableData" :.: Name "normal"    )

    _ <- def @"quasiRandomConstants" @R  =<< use @( Name "callableData" :.: Name "quasiRandomConstants" )
    _ <- def @"quasiRandomState"     @RW =<< use @( Name "callableData" :.: Name "quasiRandomState"     )

    ~( Vec2 u v ) <- randomBarycentric
    trianglePt <- let' $ p0 ^+^ u *^ ( p1 ^-^ p0 ) ^+^ v *^ ( p2 ^-^ p0 )
    triNormal  <- let' $ ( 1 - u - v ) *^ n0 ^+^ u *^ n1 ^+^ v *^ n2
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

-- | Compute barycentric coordinates of a random point on a triangle, uniformly with respect to surface area.
randomBarycentric
  :: QuasiRandom s
  => Program s s ( Code ( V 2 Float ) )
randomBarycentric = do
  ~( Vec4 ξ1 ξ2 _ _ ) <- random01s
  s <- let' $ sqrt ξ1
  let' $ Vec2 ( 1 - s ) ( ξ2 * s )

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
