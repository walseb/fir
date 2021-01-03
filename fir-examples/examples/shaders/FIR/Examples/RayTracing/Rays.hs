{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE NegativeLiterals      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module FIR.Examples.RayTracing.Rays
  ( raygenShader
  , MissData, MissShader(..), occlusionMissShader
  ) where

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.RayTracing.Camera
  ( Camera(..), cameraRay )
import FIR.Examples.RayTracing.Colour
  ( blackbodySpectrum, wavelengthToXYZ )
import FIR.Examples.RayTracing.Luminaire
  ( Luminaire(LuminaireProperties) )
import FIR.Examples.RayTracing.QuasiRandom
  ( initialiseQuasiRandomState, random01s )
import FIR.Examples.RayTracing.Types
  ( LuminaireKind(Blackbody), MissKind(..)
  , PrimaryPayload, OcclusionPayload, UBO
  , type BindingNo
  , pattern Specular, pattern Miss
  , height, width
  , tracePrimaryRay
  )

--------------------------------------------------------------------------
-- Ray generation shader:
--
--  - generate initial ray data (position, direction, wavelength)
--    using quasi-random sampling,
--  - main loop:
--      - recursively shoot ray into the scene until a closest hit or a miss shader
--        writes updated data to the ray payload
--      - result says whether to continue or not
--  - finally, convert the resulting spectral radiance information
--    into XYZ colour data, and write that to the output image.


type RayGenDefs =
  '[ "ubo"      ':-> Uniform         '[ DescriptorSet 0, Binding ( BindingNo UBO ) ] UBO
   , "accel"    ':-> UniformConstant '[ DescriptorSet 0, Binding ( BindingNo AccelerationStructure ) ] AccelerationStructure
   , "in_data"  ':-> Image2D         '[ DescriptorSet 1, Binding 0, NonWritable ] ( RGBA32 F )
   , "out_data" ':-> Image2D         '[ DescriptorSet 2, Binding 0, NonReadable ] ( RGBA32 F )
   , "loglumis" ':-> Image2D         '[ DescriptorSet 2, Binding 1 ] ( RGBA32 F )
   , "payload"  ':-> RayPayload      '[] PrimaryPayload
   , "main"     ':-> EntryPoint      '[] RayGeneration
   ]

raygenShader :: Module RayGenDefs
raygenShader = Module $ entryPoint @"main" @RayGeneration do
  cameraCoords <- use @( Name "ubo" :.: Name "camera"          )
  reset        <- use @( Name "ubo" :.: Name "reset"           )
  missIndex    <- use @( Name "ubo" :.: Name "missShaderIndex" )
  accel        <- get @"accel"
  let
    frame :: Code Float
    frame = view @( Name "position" :.: Index 3 ) cameraCoords

  ~( Vec3 i_x i_y _i_s ) <- get @"gl_LaunchID"
  ~( Vec3 n_x n_y _n_s ) <- get @"gl_LaunchSize"

  -- Initialise quasi-random number generator state.
  initialiseQuasiRandomState ( Vec3 i_x i_y ( round frame ) )

  -- Get the ray origin and direction from the camera data (using quasi-random jittering).
  ~( Vec4 dx dy _ _ ) <- random01s
  screen_x <- let' $ ( fromIntegral i_x + ( dx - 0.5 ) - 0.5 * fromIntegral n_x ) / ( 0.5 * ( fromIntegral n_x + 1 ) )
  screen_y <- let' $ ( fromIntegral i_y + ( dy - 0.5 ) - 0.5 * fromIntegral n_y ) / ( 0.5 * ( fromIntegral n_y + 1 ) )
  initialRay <- cameraRay Pinhole cameraCoords screen_x ( -screen_y * height / width )
  let
    initialOrigin, initialDirection :: Code ( V 3 Float )
    initialOrigin    = view @( Name "worldRayOrigin"    ) initialRay
    initialDirection = view @( Name "worldRayDirection" ) initialRay


  -- Use quasi-random sampling to generate initial wavelengths.
  -- Using a uniform distribution across the visible spectrum for now;
  -- would be better to focus on important wavelengths (more sensitive to the human eye, more present in the scene).
  ls <- random01s
  initialWavelengths <- let' ( ( \ l -> 380 + 400 * l ) <$$> ls )

  _ <- def @"bounceNo" @RW @Int32 $ 0
  _ <- def @"continue" @RW @Bool  $ Lit True

  quasiRandomConstants <- get @"quasiRandomConstants"
  quasiRandomState     <- get @"quasiRandomState"

  put @"payload"
    ( Struct
    $  quasiRandomConstants
    :& quasiRandomState
    :& Lit Specular       -- count direct light hits when shooting from camera
    :& Vec4 0 0 0 0       -- outside any media (air)
    :& initialOrigin
    :& initialDirection
    :& initialWavelengths
    :& Vec4 1 1 1 1       -- throughput per wavelength
    :& Vec4 0 0 0 0       -- accumulated radiance per wavelength
    :& End
    )

  -- Path tracing loop.
  while ( get @"continue" ) do

    -- Shoot primary ray.
    worldRayOrigin    <- use @( Name "payload" :.: Name "worldRayOrigin"    )
    worldRayDirection <- use @( Name "payload" :.: Name "worldRayDirection" )
    tracePrimaryRay @"payload" accel missIndex worldRayOrigin worldRayDirection

    -- New ray payload data has now been written to,
    -- by a closest hit shader or a miss shader.

    -- Continue until ray has been terminated
    hitType  <- use @( Name "payload" :.: Name "hitType" )
    bounceNo <- get @"bounceNo"
    if hitType < 0 || bounceNo > 5 -- Ray should end: stop.
    then put @"continue" ( Lit False )
    else put @"bounceNo" ( bounceNo + 1 )

  -- Done with path tracing loop: accumulate results.

  -- Gather final spectral radiances.
  ~( Vec4 λ1 λ2 λ3 λ4 ) <- use @( Name "payload" :.: Name "wavelengths" ) -- these won't have changed (no support for fluorescence currently)
  ~( Vec4 r1 r2 r3 r4 ) <- use @( Name "payload" :.: Name "radiance"    )

  -- Compute XYZ colorimetric values for each (wavelength, radiance) pair, and add them up.
  xyz1 <- wavelengthToXYZ λ1
  xyz2 <- wavelengthToXYZ λ2
  xyz3 <- wavelengthToXYZ λ3
  xyz4 <- wavelengthToXYZ λ4
  ~( Vec3 x y z ) <- let'
    $   ( r1 *^ xyz1 )
    ^+^ ( r2 *^ xyz2 )
    ^+^ ( r3 *^ xyz3 )
    ^+^ ( r4 *^ xyz4 )

  -- Write this data to output, noting that XYZ is a linear colour space
  -- so it makes sense to simply add the values.
  old <-
    if reset > 0
    then pure ( Vec4 0 0 0 0 )
    else imageRead @"in_data" ( Vec2 i_x i_y )
  new@( ~( Vec4 x' y' z' n' ) ) <- let' $
    if   isNaN x || isNaN y || isNaN z
    then old
    else ( old ^+^ Vec4 x y z 4 )
  imageWrite @"out_data" ( Vec2 i_x i_y ) new

  -- Write log luminance data for tone mapping.
  logLumi <- let' $ min 10 ( log ( y' / n' + 1e-3 ) / log 10 )
  imageWrite @"loglumis" ( Vec2 i_x i_y ) ( Vec4 x' y' z' logLumi )

--------------------------------------------------------------------------
-- Miss shaders:
--
-- - a single occlusion miss shader
-- - several different primary miss shaders

type OcclusionMissDefs =
  '[ "payload" ':-> RayPayloadIn '[] OcclusionPayload
   , "main"    ':-> EntryPoint   '[] Miss
   ]

occlusionMissShader :: Module OcclusionMissDefs
occlusionMissShader = Module $ entryPoint @"main" @Miss do
  assign @( Name "payload" ) ( Struct $ (-1) :& (-1) :& (-1) :& Vec3 0 0 0 :& End )

type PrimaryMissDefs =
  '[ "payload"  ':-> RayPayloadIn '[] PrimaryPayload
   , "main"     ':-> EntryPoint   '[] Miss
   , "missData" ':-> Uniform      '[ DescriptorSet 0, Binding ( BindingNo MissKind ) ] MissData
   ]


type MissData = Struct
  '[ "blackbody" ':-> MissProperties EnvironmentBlackbody
   , "factor"    ':-> MissProperties Factor
--   , "sky"       ':-> MissProperties Sky
   ]

defaultMissData :: MissData
defaultMissData = defaultMissProps @EnvironmentBlackbody :& defaultMissProps @Factor :& End --  :& defaultMissProps @Sky :& End

class MissShader ( miss :: MissKind ) where
  type MissProperties miss

  defaultMissProps  :: MissProperties miss
  missData          :: MissProperties miss -> MissData
  primaryMissShader :: Module PrimaryMissDefs

instance MissShader EnvironmentBlackbody where
  type MissProperties EnvironmentBlackbody = LuminaireProperties Blackbody
  defaultMissProps = 6.5e3 :& 1 :& End
  missData props = set @( Name "blackbody" ) props defaultMissData

  primaryMissShader = Module $ entryPoint @"main" @Miss do
    emitterInfo      <- use @( Name "missData" :.: Name "blackbody" )
    emitterTemp      <- let' $ view @( Name "temperature" ) emitterInfo
    emitterIntensity <- let' $ view @( Name "intensity"   ) emitterInfo
    ~( Vec4 λ0 λ1 λ2 λ3 ) <- use @( Name "payload" :.: Name "wavelengths" )
    r0 <- let' =<< blackbodySpectrum emitterTemp λ0
    r1 <- let' =<< blackbodySpectrum emitterTemp λ1
    r2 <- let' =<< blackbodySpectrum emitterTemp λ2
    r3 <- let' =<< blackbodySpectrum emitterTemp λ3
    let
      radiances :: Code ( V 4 Float )
      radiances = ( 1e-13 * emitterIntensity ) *^ Vec4 r0 r1 r2 r3
    throughput <- use @( Name "payload" :.: Name "throughput" )
    modifying @( Name "payload" :.: Name "radiance" ) ( ^+^ ( (*) <$$> throughput <**> radiances ) )
    assign @( Name "payload" :.: Name "hitType" ) ( Lit Miss )

instance MissShader Factor where
  type MissProperties Factor = Float
  defaultMissProps = 0.9
  missData props = set @( Name "factor" ) props defaultMissData

  primaryMissShader = Module $ entryPoint @"main" @Miss do
    f <- use @( Name "missData" :.: Name "factor" )
    throughput <- use @( Name "payload" :.: Name "throughput" )
    modifying @( Name "payload" :.: Name "radiance" ) ( ^+^ ( (*) <$$> throughput <**> Vec4 f f f f ) )
    assign @( Name "payload" :.: Name "hitType" ) ( Lit Miss )

{-
instance MissShader Sky where
  type MissProperties Sky =
    Struct 
      '[ "depolarisation"            ':-> Float
       , "mieCoefficient"            ':-> Float
       , "mieDirectionalG"           ':-> Float
       , "mieV"                      ':-> Float

       , "mieKCoefficient"           ':-> V 3 Float
       , "mieZenithLength"           ':-> Float

       , "primaries"                 ':-> V 3 Float
       , "turbidity"                 ':-> Float

       , "rayleigh"                  ':-> Float
       , "rayleighZenithLength"      ':-> Float
       , "refractiveIndex"           ':-> Float
       , "sumAngularDiameterDegrees" ':-> Float

       , "sunPosition"               ':-> V 3 Float
       , "sumIntensityFactor"        ':-> Float

       , "sumIntensityFalloff"       ':-> Float

       , "numMolecules"              ':-> Word32

       ]
  defaultMissProps
    =  0.035
    :& 0.005
    :& 0.8
    :& 4

    :& V3 0.686 0.678 0.666
    :& 1.25e3

    :& V3 6.8e-7 5.5e-7 4.5e-7
    :& 2

    :& 1
    :& 8.4e3
    :& 1.0003
    :& 0.0093333

    :& V3 0 -1e6 0
    :& 1000.0

    :& 1.5

    :& ( 2542 * 10 Prelude.^ ( 22 :: Word32 ) )
    :& End
  missData props = set @( Name "sky" ) props defaultMissData


  primaryMissShader = Module $ entryPoint @"main" @Miss do
    -- TODO
    assign @( Name "payload" :.: Name "hitType" ) ( Lit Miss )
-}