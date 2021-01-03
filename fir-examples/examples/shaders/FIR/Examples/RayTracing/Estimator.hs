{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module FIR.Examples.RayTracing.Estimator
  ( estimateRadiance )
  where

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.RayTracing.Material
  ( MaterialSampleCallableData, MaterialQueryCallableData )
import FIR.Examples.RayTracing.QuasiRandom
  ( QuasiRandom, random01s )
import FIR.Examples.RayTracing.Types
  ( LuminaireID, ShaderRecord
  , EmitterCallableData, LightSamplingCallableData
  , pattern EndRay, pattern Miss, pattern Specular
  , PrimaryPayload, OcclusionPayload
  , initialOcclusionPayload
  , traceOcclusionRay
  )

--------------------------------------------------------------------------

type HasRW s name ty = ( Has name s ~ ty, CanGet name s, CanPut name s )

estimateRadiance
  :: forall s
  .  ( HasRW s "payload"         PrimaryPayload
     , HasRW s "occPayload"      OcclusionPayload
     , HasRW s "emitterData"     EmitterCallableData
     , HasRW s "matSampleData"   MaterialSampleCallableData
     , HasRW s "matQueryData"    MaterialQueryCallableData
     , HasRW s "lightSampleData" LightSamplingCallableData
     , Has     "luminaires" s ~  ( Struct '[ "luminaireArray" ':-> RuntimeArray LuminaireID ] )
     , CanGet  "luminaires" s
     , CanTraceRay        "occPayload"      s
     , CanExecuteCallable "emitterData"     s
     , CanExecuteCallable "matSampleData"   s
     , CanExecuteCallable "matQueryData"    s
     , CanExecuteCallable "lightSampleData" s
     , QuasiRandom s
     , _
     )
  => Code AccelerationStructure
  -> Code ShaderRecord
  -> Code ( V 3 Float )  -- ^ World position of hit.
  -> Code ( V 3 Float )  -- ^ Normal vector to surface at hit point.
  -> Program s s ( Code () )
estimateRadiance accel shaderRecord hitPos normal = do

  prevHitType  <- use @( Name "payload" :.: Name "hitType"           )
  rayOrigin    <- use @( Name "payload" :.: Name "worldRayOrigin"    )
  rayDirection <- use @( Name "payload" :.: Name "worldRayDirection" )
  wavelengths  <- use @( Name "payload" :.: Name "wavelengths"       )

  ------------------------------------------------------
  -- Get transmittance of path from ray origin to current position,
  -- and use it to update throughput.

  extinctionCoeffs <- use @( Name "payload" :.: Name "extinction" )
  d <- let' $ distance rayOrigin hitPos
  -- Lambert's law of absorption.
  transmittance <- let' $ ( \ k λ -> exp ( - d * 4 * pi * k * 1e9 / λ ) ) <$$> extinctionCoeffs <**> wavelengths
  modifying @( Name "payload" :.: Name "throughput" ) ( (*) <$$> transmittance <**> )
  throughput    <- use @( Name "payload" :.: Name "throughput" )

  ------------------------------------------------------
  -- Add contribution from hitting an emitter
  -- after a specular bounce
  -- (not accounted for by next-event estimation).

  emitterCallable <- let' $ view @( Name "emitterCallable" ) shaderRecord
  when ( prevHitType == Lit Specular && emitterCallable >= 0 ) do
  
    -- Pass the data that the callable shader needs.
    emitterInfoIndex <- let' $ view @( Name "emitterInfoIndex" ) shaderRecord
    put @"emitterData" ( Struct $ emitterInfoIndex :& rayDirection :& normal :& wavelengths :& End )

    -- Execute the callable shader to figure out the emitter contribution.
    executeCallable @"emitterData" ( fromIntegral emitterCallable :: Code Word32 )

    -- Add the resulting spectral radiances to the ray payload.
    emitterRadiances <- use @( Name "emitterData" :.: Name "mainData" )
    let
      adjustedEmitterRadiances :: Code ( V 4 Float )
      adjustedEmitterRadiances = (*) <$$> throughput <**> emitterRadiances
    modifying @( Name "payload" :.: Name "radiance" ) ( ^+^ adjustedEmitterRadiances )

  ------------------------------------------------------
  -- Sample a bounce direction using the callable shader
  -- corresponding to the material of the current object.

  -- Choice of bounce direction determined by hero wavelength.
  hero <- heroWavelength wavelengths throughput
  λ    <- let' $ view @( Name "wavelength"  ) hero
  p_λ  <- let' $ view @( Name "probability" ) hero

  -- Pass the data that the callable shader needs.
  materialSampleCallable <- let' $ view @( Name "matSampleCallable" ) shaderRecord
  materialInfoIndex      <- let' $ view @( Name "matPropsIndex"     ) shaderRecord
  quasiRandomConstants   <- use @( Name "payload" :.: Name "quasiRandomConstants" )
  quasiRandomState       <- use @( Name "payload" :.: Name "quasiRandomState"     )
  put @"matSampleData"
    ( Struct $ materialInfoIndex :& quasiRandomConstants :& quasiRandomState :& normal :& λ :& rayDirection :& Lit Miss :& End )

  -- Execute the callable shader.
  executeCallable @"matSampleData" materialSampleCallable

  -- Obtain results.
  bounceDir        <- use @( Name "matSampleData" :.: Name "inOutRayDir"      )
  bounceType       <- use @( Name "matSampleData" :.: Name "sampleType"       )
  quasiRandomState <- use @( Name "matSampleData" :.: Name "quasiRandomState" )

  -- Update ray payload for the bounce, and update the quasi-random state.
  assign @( Name "payload" :.: Name "worldRayOrigin"    ) hitPos
  assign @( Name "payload" :.: Name "worldRayDirection" ) bounceDir
  assign @( Name "payload" :.: Name "hitType"           ) bounceType
  assign @( Name "payload" :.: Name "quasiRandomState"  ) quasiRandomState
  put @"quasiRandomState" quasiRandomState

  ------------------------------------------------------
  -- Query the material at the given bounce direction
  -- in order to obtain BSDF & probabilities
  -- for all of the wavelengths.

  -- Pass the data that the callable shader needs.
  materialQueryCallable <- let' $ view @( Name "matQueryCallable" ) shaderRecord
  put @"matQueryData"
    ( Struct $ materialInfoIndex :& normal :& rayDirection :& bounceDir :& wavelengths :& extinctionCoeffs :& Vec4 1 1 1 1 :& Vec4 0 0 0 0 :& End )

  -- Execute the callable shader.
  executeCallable @"matQueryData" materialQueryCallable

  -- Obtain results.
  bounceDirBSDF <- use @( Name "matQueryData" :.: Name "bsdf"  )
  bounceDirProb <- use @( Name "matQueryData" :.: Name "probs" )
  assign @( Name "payload" :.: Name "extinction" ) =<< use @( Name "matQueryData" :.: Name "extinction" )

  ------------------------------------------------------
  -- Light sampling.
  --
  -- If this was a specular bounce: no explicit light sampling to be done.
  -- Otherwise: sample direct illumination using multiple importance sampling.

  unless ( bounceType == Lit Specular ) do

    ---------------------------------------------
    -- Start by picking a random light source.

    lg <- arrayLength @( Name "luminaires" :.: Index 0 )
    unless ( lg < 1 ) do
      luminaire              <- randomLuminaire lg
    --lumWeight              <- let' $ view @( Name "luminaireWeight"     ) luminaire
      lightPrimitiveID       <- let' $ view @( Name "primitiveID"         ) luminaire
      lightInstanceID        <- let' $ view @( Name "instanceID"          ) luminaire
      lightEmitterCallable   <- let' $ view @( Name "emitterCallable"     ) luminaire
      lightSampleCallable    <- let' $ view @( Name "lightSampleCallable" ) luminaire
      lightEmitterInfoIndex  <- let' $ view @( Name "emitterInfoIndex"    ) luminaire
      lightGeometryInfoIndex <- let' $ view @( Name "geometryInfoIndex"   ) luminaire

      ---------------------------------------------
      -- Compute the bounce contribution.

      -- Trace an occlusion ray along the bounce ray direction.
      put @"occPayload" initialOcclusionPayload
      traceOcclusionRay @"occPayload" accel hitPos bounceDir
      bounceOccPayload <- get @"occPayload"
      bounceVisFactor  <- visibilityFactor lightPrimitiveID lightInstanceID bounceOccPayload

      -- Query the chosen light source for spectral radiances.
      bounceEmitterRadiances <-
        if bounceVisFactor == 0
        then pure ( pureAST 0 :: Code ( V 4 Float ) )
        else do
          bounceLightNormal <- let' $ view @( Name "normal" ) bounceOccPayload
          put @"emitterData" ( Struct $ lightEmitterInfoIndex :& bounceDir :& bounceLightNormal :& wavelengths :& End )
          executeCallable @"emitterData" lightEmitterCallable
          use @( Name "emitterData" :.: Name "mainData" )

      ---------------------------------------------
      -- Compute the direct light contribution.

      -- Pick a random point on the light,
      -- using the callable shader corresponding to the light type.
      --
      -- Assumes that the geometry info obtained from the index,
      -- for the geometry type corresponding to the callable shader,
      -- does not have any transformation matrix applied to it.
      put @"lightSampleData"
        ( Struct $ quasiRandomConstants :& quasiRandomState :& lightGeometryInfoIndex :& lightEmitterInfoIndex :& normal :& hitPos :& Lit 1 :& End )
      executeCallable @"lightSampleData" lightSampleCallable
      lightPt          <- use @( Name "lightSampleData" :.: Name "rayOrigin"        )
      psa_correction   <- use @( Name "lightSampleData" :.: Name "psa_correction"   )
      quasiRandomState <- use @( Name "lightSampleData" :.: Name "quasiRandomState" )
      assign @( Name "payload" :.: Name "quasiRandomState" ) quasiRandomState

      -- Trace an occlusion ray towards the chosen light point.
      lightDirection <- let' ( normalise $ lightPt ^-^ hitPos )
      put @"occPayload" initialOcclusionPayload
      traceOcclusionRay @"occPayload" accel hitPos lightDirection
      lightSampleOccPayload <- get @"occPayload"
      lightSampleVisFactor  <- visibilityFactor lightPrimitiveID lightInstanceID lightSampleOccPayload

      -- Query the chosen light source for spectral radiances.
      lightSampleEmitterRadiances <-
        if lightSampleVisFactor == 0
        then pure ( pureAST 0 :: Code ( V 4 Float ) )
        else do
          lightSampleLightNormal <- let' $ view @( Name "normal" ) lightSampleOccPayload
          put @"emitterData" ( Struct $ lightEmitterInfoIndex :& lightDirection :& lightSampleLightNormal :& wavelengths :& End )
          executeCallable @"emitterData" lightEmitterCallable
          use @( Name "emitterData" :.: Name "mainData" )

      -- Query the material BSDF along the sampled light direction.
      put @"matQueryData"
        ( Struct $ materialInfoIndex :& normal :& rayDirection :& lightDirection :& wavelengths :& extinctionCoeffs :& Vec4 1 1 1 1 :& Vec4 0 0 0 0 :& End )
      executeCallable @"matQueryData" materialQueryCallable
  
      -- Obtain results.
      lightDirBSDF <- use @( Name "matQueryData" :.: Name "bsdf"  )
      lightDirProb <- use @( Name "matQueryData" :.: Name "probs" )

      ---------------------------------------------
      -- Accumulate radiances using multiple importance sampling (balance heuristic).

      estimatedRadiance <-
        mis
          ( (*) <$$> bounceDirBSDF <**>      bounceEmitterRadiances ) ( bounceDirProb ^*   p_λ )
          ( (*) <$$>  lightDirBSDF <**> lightSampleEmitterRadiances ) (  lightDirProb ^* ( p_λ * psa_correction ) )
  
      modifying @( Name "payload" :.: Name "radiance" ) ( ^+^ ( (*) <$$> throughput <**> estimatedRadiance ) )

  -- Update the ray throughput using BSDF values.
  assign @( Name "payload" :.: Name "throughput" ) ( (*) <$$> bounceDirBSDF <**> throughput )

  -- Use Russian roulette to decide whether to continue.
  rrAdjustment <- russianRoulette =<< use @( Name "payload" :.: Name "throughput" )
  if   rrAdjustment < 0
  then assign    @( Name "payload" :.: Name "hitType"    ) ( Lit EndRay )
  else modifying @( Name "payload" :.: Name "throughput" ) ( rrAdjustment *^ )

--------------------------------------------------------------------------
-- Helper functions.

-- | Check whether the result of tracing an occlusion ray is as we expected,
-- i.e. the occlusion ray did end up hitting the light it should have.
visibilityFactor :: Code Word32 -> Code Word32 -> Code OcclusionPayload -> Program s s ( Code Float )
visibilityFactor expectedPrimitiveID expectedInstanceID occPayload = do
  hitPrimitiveID <- let' $ view @( Name "primitiveID" ) occPayload
  hitInstanceID  <- let' $ view @( Name "instanceID"  ) occPayload
  let' $
    if    ( hitPrimitiveID == fromIntegral expectedPrimitiveID )
       && ( hitInstanceID  == fromIntegral expectedInstanceID  )
    -- (could also check whether hitT is reasonable)
    then 1
    else 0

-- | Probabilistically returns whether to continue, given spectral throughput.
--
-- Negative result: end ray.
-- Positive result: continue, updating the throughput with the given factor.
russianRoulette
  :: forall s
  .  ( QuasiRandom s )
  => Code ( V 4 Float )
  -> Program s s ( Code Float )
russianRoulette ( Vec4 t1 t2 t3 t4 ) = do
  continueProb <- let' $ min 0.99 ( max t1 ( max t2 ( max t3 t4 ) ) )
  ~( Vec4 rr _ _ _ ) <- random01s
  pure $
    if rr <= continueProb
    then recip continueProb
    else (-1)

-- | Randomly choose a scene light, with probability
-- proportional to its weight in the given array of all scene lights.
--
-- The total weight (i.e. sum of each light's weight) must be 1.
randomLuminaire
  :: ( QuasiRandom s, _ )
  => Code Word32 -> Program s s ( Code LuminaireID )
randomLuminaire nbLuminaires = do
  ~( Vec4 r _ _ _ ) <- random01s
  locally do
    _  <- def @"i"        @RW @Word32      0
    _  <- def @"acc"      @RW @Float       0
    _  <- def @"res"      @RW @LuminaireID =<< use @( Name "luminaires" :.: Name "luminaireArray" :.: AnIndex Word32 ) 0
    while ( (< nbLuminaires) <<$>> get @"i" ) do
      i    <- get @"i"
      acc  <- get @"acc"
      lum  <- let' @( Code LuminaireID ) =<< use @( Name "luminaires" :.: Name "luminaireArray" :.: AnIndex Word32 ) i
      acc' <- let' $ acc + view @( Name "luminaireWeight" ) lum
      if acc' >= r
      then do
        put @"i"   nbLuminaires -- end loop
        put @"res" lum
      else do
        put @"i"   (i+1)
        put @"acc" acc'
    get @"res"

-- | Spectral multiple importance sampling.
mis
  :: Code ( V 4 Float ) -- ^ BSDF values in bounce direction (per wavelength).
  -> Code ( V 4 Float ) -- ^ material sampling probabilities in bounce direction (per wavelength), relative to projected solid angle.
  -> Code ( V 4 Float ) -- ^ BSDF values in light direction (per wavelength).
  -> Code ( V 4 Float ) -- ^ material sampling probabilities in light direction (per wavelength), relative to projected solid angle.
  -> Program s s ( Code ( V 4 Float ) )
mis bounceVal bounceProb lightVal lightProb = do
  balance <- let' $ max 1e-15 $ sumV bounceProb + sumV lightProb
  let' $ recip balance *^ ( bounceVal ^+^ lightVal )
    where
      sumV :: Code ( V 4 Float ) -> Code Float
      sumV ( Vec4 x0 x1 x2 x3 ) = x0 + x1 + x2 + x3

-- | Choose a hero wavelength, prioritising a wavelength with high throughput.
heroWavelength
  :: ( QuasiRandom s, _ )
  => Code ( V 4 Float )
  -> Code ( V 4 Float )
  -> Program s s ( Code ( Struct '[ "wavelength" ':-> Float, "probability" ':-> Float ] ) )
heroWavelength ( Vec4 λ0 λ1 λ2 λ3 ) throughput@( Vec4 t0 t1 t2 t3 ) = do
  ~( Vec4 r _ _ _ ) <- random01s
  totalT <- let' $ t0 + t1 + t2 + t3
  ~( Vec4 p0 p1 p2 p3 ) <- let' $ ( recip totalT ) *^ throughput
  pure $
    if r <= p0
    then Struct ( λ0 :& p0 :& End )
    else
      if r <= p0 + p1
      then Struct ( λ1 :& p1 :& End )
      else
        if r <= p0 + p1 + p2
        then Struct ( λ2 :& p2 :& End )
        else Struct ( λ3 :& p3 :& End )
