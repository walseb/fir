{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UnicodeSyntax              #-}

module FIR.Examples.Kerr.Event
  ( rayTraceUntilEvent, eventColour )
  where

-- fir
import FIR
import Math.Linear

-- fir-examples
--import Examples.Kerr.Coordinates
--  ( spatialMetric, normaliseSpatialComponents  )
import FIR.Examples.Kerr.Colour
  ( blackbodyColour, wavelengthColour, starTemperature )
import FIR.Examples.Kerr.Doppler
  ( localGravitationalDopplerFactor, specialDopplerFactor )
import FIR.Examples.Kerr.Info
  ( KerrInfo, DiskInfo )
import FIR.Examples.Kerr.Motion
  ( MotionConstants, geodesicEquations
  , Canonical, pattern XP
  )
import FIR.Examples.Kerr.Noise
  ( hash
--, brownianNoise
  )
import qualified FIR.Examples.Kerr.RungeKutta as RK
  ( dormandPrince
  , Parameters(..), StepData(..), StepResult
  )

------------------------------------------------
-- Events.

type EventData
  = Struct
  '[ "flag"      ':-> Int32
   , "value"     ':-> Float
   , "intensity" ':-> Float
   ]
newtype Event = Event ( Code Float, Code EventData )
  deriving newtype Syntactic



black :: Code EventData
black = Struct ( 0 :& 0 :& 1 :& End )
temperature :: Code Float -> Code Float -> Code EventData
temperature t i = Struct ( 1 :& t :& i :& End )
--wavelength :: Code Float -> Code Float -> Code EventData
--wavelength λ i = Struct ( 2 :& λ :& i :& End )
noEvent :: Event
noEvent = Event ( Lit (1 / 0), black )

eventColour
  :: Code KerrInfo
  -> Code MotionConstants
  -> Code (V 4 Float)
  -> Code EventData
  -> Program i i (Code (V 4 Float))
eventColour kerrInfo constants x₀ evtData
  = purely do
      flag <- def @"flag" @R $ view @(Field "flag") evtData
      if flag < 1 -- black
      then pure (Vec4 0 0 0 (Lit (1/0)))
      else do
        value     <- def @"value"     @R $ view @(Field "value"    ) evtData
        intensity <- def @"intensity" @R $ view @(Field "intensity") evtData
        -- Compute Doppler shift factor.
        -- Doppler shift factors for the emitter have already been incorporated,
        -- it remains to account for the observer.
        dopp <- localGravitationalDopplerFactor kerrInfo constants x₀
        -- A doppler shift affects frequency and temperature the same,
        -- as doppler shifting a blackbody spectrum by a given factor
        -- simply corresponds to multiplying the temperature by this same factor.
        col <- def @"col" @R =<<
          if flag == 1
          then blackbodyColour  ( value * dopp )
          else wavelengthColour ( value / dopp )
        pure ( over @(Index 3) (* intensity) col )

------------------------------------------------
-- Integrate the geodesic equations for a photon in Kerr space-time,
-- until one of the following events occurs:
--  - photon enters black hole event horizon,
--  - photon hits the accretion disk,
--  - photon escapes from the gravitational attraction of the black hole.

type CanonicalStep = RK.StepData Float Canonical

-- Call out to the Runge–Kutta integrator,
-- passing it a stepping function which asks integration to stop
-- when an event occurs.
rayTraceUntilEvent
  :: Code KerrInfo
  -> Code DiskInfo
  -> Code MotionConstants
  -> Code Canonical
  -> Code Float
  -> Program i i (Code EventData)
rayTraceUntilEvent kerrInfo diskInfo constants x₀p₀ clock =
  RK.dormandPrince
    RK.Parameters
      { RK.start         = ( 0, x₀p₀, black )
      , RK.function      = geodesicEquations kerrInfo constants
      , RK.tolerances    = tolerances
      , RK.stepper       = checkStepEvent
      , RK.startStepSize = 1e-3
      , RK.minStepSize   = \ _ _ -> 1e-7
      , RK.maxStepSize   = maxStepSize
      , RK.maxIterations = 1000
      }
    where
      checkStepEvent
        :: CanonicalStep
        -> Code EventData
        -> Program i i (Code (RK.StepResult EventData))
      checkStepEvent stepData _
      -- TODO: for the moment this ignores the past,
      -- and simply outputs the colour associated to a discrete event.
      -- For volumetric rendering we would want to keep track of the past.
        = purely do
            dλ <- def @"dλ" @R $ fst ( RK.stepSize stepData )
            Event (dλₑ, dat) <- earliestStepEvent kerrInfo diskInfo constants stepData clock
            pure . Struct $ (dλₑ < 0 || dλₑ > dλ) :& dat :& End

      tolerances :: Code Float -> Code Canonical -> Program s s (Code Canonical)
      tolerances _ (XP (Vec4 _ r₀ cosθ₀ _) _ ) = purely do
        a²    <- def @"a²"    @R $ view @(Field "a²" ) kerrInfo
        r     <- def @"r"     @R $ r₀
        cosθ  <- def @"cosθ"  @R $ cosθ₀
        cos²θ <- def @"cos²θ" @R $ cosθ * cosθ
        δ     <- def @"δ"     @R $ r * ( r - 2 ) + a²
        pure $ XP
          ( Vec4 1 ( 1e-3 * δ ) 1e-4 ( 1e-3 + 1e0 * cos²θ ) )
          ( Vec2 ( 1e-3 * δ ) ( 1e-3 + 1e0 * cos²θ ) )

      maxStepSize :: Code Float -> Code Canonical -> Program s s (Code Float)
      maxStepSize _ (XP (Vec4 _ r₀ _ _) _ )
        = pure $
            if r₀ < 20
            then 5e-2
            else 5e-1

-- Compute the first event that occurs (if any)
-- within the step (λ = geodesic affine parameter).
earliestStepEvent
  :: Code KerrInfo
  -> Code DiskInfo
  -> Code MotionConstants
  -> CanonicalStep
  -> Code Float
  -> Program i i Event
earliestStepEvent kerrInfo diskInfo constants stepData clock = do
  dλ_eh  <- crossedEventHorizon  kerrInfo                    stepData
  dλ_ad  <- crossedAccretionDisk kerrInfo diskInfo constants stepData clock
  dλ_esc <- rayEscaped           kerrInfo                    stepData
  pure (earliestEvent dλ_eh [dλ_ad, dλ_esc])

-- Return the event with the smallest positive "time" difference.
earliestEvent :: Event -> [ Event ] -> Event
earliestEvent ev [ ] = ev
earliestEvent ( Event (dλ₁, v₁) ) ( Event (dλ₂,v₂) : evs )
  = if dλ₁ >= 0 && dλ₁ < dλ₂
    then earliestEvent ( Event (dλ₁,v₁) ) evs
    else earliestEvent ( Event (dλ₂,v₂) ) evs


-- Check whether ray has crossed event horizon.
--
-- Note that, by the very definition of an event horizon,
-- if we integrate a geodesic backwards in space-time,
-- starting from outside the event horizon,
-- we are never going to cross the event horizon.
-- Hence we stop the simulation early when
-- the adaptive Runge–Kutta step size becomes too small.
crossedEventHorizon
  :: Code KerrInfo -> CanonicalStep -> Program i i Event
crossedEventHorizon
  kerrInfo
  RK.StepData
    { RK.stepOrigin = ( _ , XP ( x@(Vec4 t r  _ _ )) _ )
    , RK.stepSize   = ( dλ, XP (x'@(Vec4 t' r' _ _ )) _ )
    }
  = purely do
      if t + dλ * t' > 5e3 || dλ < 1.1e-7 -- needs to be greater than min step size, as step size never goes below that
      then do
        evData <- eventHorizon x
        pure ( Event (dλ, evData) )
      else do
        -- photon sphere calculation
        --r_ph <- def @"r_ph" @R $ view @(Field "r_ph") kerrInfo
        --dλₑ  <- def @"dλₑ" @R $ ( r_ph - r ) / ( r' * dλ )
        --pure ( Event ( dλₑ, Vec4 0 0 0 1 ) )

        -- get event horizon radius
        r_h <- def @"r_h" @R $ view @(Field "r_h") kerrInfo
        if r + dλ * r' < r_h + 1e-5
        then do
          -- compute relative Mino time of crossing
          dλₑ <- def @"dλₑ" @R $ ( r_h - r ) / ( r' * dλ )
          -- compute the colour given the 4-position of crossing
          xₑ  <- def @"xₑ"  @R $ x ^+^ dλₑ *^ x'
          evData <- eventHorizon xₑ
          pure ( Event ( dλₑ, evData ) )
        else
          pure noEvent

eventHorizon :: Code (V 4 Float) -> Program i i (Code EventData)
eventHorizon _ = pure black

crossedAccretionDisk
  :: Code KerrInfo
  -> Code DiskInfo
  -> Code MotionConstants
  -> CanonicalStep
  -> Code Float
  -> Program i i Event
crossedAccretionDisk
  kerrInfo
  diskInfo
  constants
  RK.StepData
    { RK.stepOrigin = (  λ, XP ( x@(Vec4 _ r  cosθ  φ  )) _ )
    , RK.stepSize   = ( dλ, XP (x'@(Vec4 _ r' cosθ' _  )) _ )
    }
  clock
  = purely do

      r_inner <- def @"r_inner" @R $ view @(Field "r_inner"   ) diskInfo
      r_outer <- def @"r_outer" @R $ view @(Field "r_outer"   ) diskInfo
      θ_disk  <- def @"θ_disk"  @R $ view @(Field "thickness" ) diskInfo
      ω_prec  <- def @"ω_prec"  @R $ view @(Field "precession") diskInfo

      -- Linear approximation of disk tilt around r.
      σ    <- def @"σ"    @R $ acos cosθ - pi / 2
      sinθ <- def @"sinθ" @R $ sqrt ( 1 - cosθ * cosθ )
      cosψ <- def @"cosφ" @R $ cos ( φ + ( clock - λ ) * ω_prec )

      r_min <- def @"r_min" @R $ max r_inner ( min r (r + 1.1 * dλ * r') )
      r_max <- def @"r_max" @R $ min r_outer ( max r (r + 1.1 * dλ * r') )

      τ_min <- def @"τ_min" @R =<< tilt diskInfo r_min
      τ_max <- def @"τ_max" @R =<< tilt diskInfo r_max

      -- AABB intersection test
      invDir <- def @"invDir" @R $ recip @(Code Float) <$$> Vec2 r' ( - cosθ' / sinθ )
      t1 <- def @"t1" @R
        $ ( \ s p i -> ( s - p ) * i :: Code Float )
            <$$> Vec2 r_min ( cosψ * τ_min - θ_disk )
            <**> Vec2 r σ
            <**> invDir
      t2 <- def @"t2" @R
        $ ( \ s p i -> ( s - p ) * i :: Code Float )
            <$$> Vec2 r_max ( cosψ * τ_max + θ_disk )
            <**> Vec2 r σ
            <**> invDir
      tMin <- def @"tMin" @R $ maxV2 ( min @(Code Float) <$$> t1 <**> t2 )
      tMax <- def @"tMax" @R $ minV2 ( max @(Code Float) <$$> t1 <**> t2 )

      if   ( tMin > tMax || tMax < 0 )
      then pure noEvent
      else do
        dλₑ <- def @"dλₑ" @R $ tMin
        xₑ  <- def @"xₑ"  @R $ x ^+^ dλₑ *^ x'
        τ   <- def @"τ"   @R =<< tilt diskInfo ( view @(Index 1) xₑ )
        evData <- accretionDisk kerrInfo diskInfo τ constants xₑ x'
        pure ( Event ( dλₑ, evData ) )

minV2 :: Code (V 2 Float) -> Code Float
minV2 (Vec2 x y) = min x y

maxV2 :: Code (V 2 Float) -> Code Float
maxV2 (Vec2 x y) = max x y

accretionDisk
  :: Code KerrInfo
  -> Code DiskInfo
  -> Code Float
  -> Code MotionConstants
  -> Code (V 4 Float)
  -> Code (V 4 Float)
  -> Program i i (Code EventData)
accretionDisk kerrInfo diskInfo τ constants x@(Vec4 _ r cosθ _) v = purely do
  a <- def @"a" @R $ view @(Field "a") kerrInfo
  energy <- def @"energy" @R $ view @(Field "energy"     ) diskInfo
  orient <- def @"orient" @R $ view @(Field "orientation") diskInfo
  sinθ <- def @"sinθ" @R $ sqrt ( 1 - cosθ * cosθ )
  sinτ <- def @"sinτ" @R $ sin τ
  cosτ <- def @"cosτ" @R $ cos τ
  r³ <- def @"r³" @R $ r * r * r
  -- Very basic accretion disk temperature model: assumed to follow a -3/4 power law.
  temp <- def @"temp"  @R $ energy * sqrt ( invSqrt r³ ) -- r^(-3/4)

  -- Gravitational Doppler effect: computation of local factor.
  -- The total gravitational Doppler effect is obtained
  -- as the ratio of the observer factor by the emitter factor.
  dopp_g <- def @"dopp_g" @R =<< localGravitationalDopplerFactor kerrInfo constants x

  -- Special-relativistic Doppler effect.
  ω <- def @"ω" @R $ signum a * orient * recip ( sqrt r³ + abs a ) -- ≈ r^(-3/2)
  let v_disk = Vec4 0 0 ( - sinτ * sinθ * ω ) ( cosτ * ω )
  dopp_s <- def @"dopp_s" @R =<< specialDopplerFactor kerrInfo x v v_disk

  {-
  -- Lambert's cosine law. (TODO: Incorrect at the moment.)
  v_tilde <- def @"v_tilde" @R =<< normaliseSpatialComponents kerrInfo x v
  v_disk  <- def @"v_disk"  @R =<< normaliseSpatialComponents kerrInfo x (Vec4 0 0 cosτ sinτ)
  lambertianFactor
     <- ( def @"c" @R . abs ) =<< spatialMetric kerrInfo x v_tilde v_disk
  -}

  pure ( temperature ( temp / ( dopp_s * dopp_g ) ) 1 )

-- | Arbitrary disk tilt function, used to interpolate from 0 (within the Bardeen–Petterson radius)
-- to the outer inclination angle, with a small dip in the opposite direction.
-- Not physically accurate, but I don't want to involve Novikov–Thorne thin disk theory.
tilt :: Code DiskInfo -> Code Float -> Program i i (Code Float)
tilt diskInfo r = purely do
  dip    <- def @"dip"    @R $ 0.58
  τ      <- def @"τ"      @R $ view @(Field "inclination") diskInfo
  r_BP   <- def @"r_BP"   @R $ view @(Field "r_BP"       ) diskInfo
  s      <- def @"s"      @R $ r - r_BP
  s²_p_1 <- def @"1_p_r²" @R $ s * s + 1
  let res =
        if r < r_BP
        then 0
        else τ * ( exp ( 1 - recip s²_p_1 ) - dip * exp ( 1 - recip ( s²_p_1 * s²_p_1 ) ) - ( 1 - dip ) )
             / ( ( exp 1 - 1 ) * ( 1 - dip ) )
  pure res

rayEscaped :: Code KerrInfo -> CanonicalStep -> Program i i Event
rayEscaped _
  RK.StepData
    { RK.stepOrigin = (  λ, XP (Vec4 _ r  cosθ  φ ) _ )
    , RK.stepSize   = ( dλ, XP (Vec4 _ r' cosθ' φ') _ )
    }
  -- TODO: find a good way to check when a photon has escaped
  -- the sphere of influence of the black hole.
  -- At the moment we assume photon trajectories are straight lines
  -- outside a given radius.
  = purely do
      if r' > 0 && r + dλ * r' > 60
      then do
        -- Compute spherical coordinates at infinity, supposing
        -- that the photon moves in a straight line.
        -- This amounts to computing the Jacobian matrix for
        -- a translation in spherical coordinates. A bit messy.
        sinθ <- def @"sinθ" @R $ sqrt ( 1 - cosθ * cosθ )
        dθ_by_dr <- def @"dθ_by_dr" @R $ - cosθ' / ( r' * sinθ )
        dφ_by_dr <- def @"dφ_by_dr" @R $   φ'    /   r'
        sinθdφ_by_dr <- def @"sinθdφ_by_dr" @R $ sinθ * dφ_by_dr
        cosθ_inf <- def @"cosθ_inf" @R $ ( cosθ - r * sinθ * dθ_by_dr )
                                       / sqrt ( 1 + r * r * ( dθ_by_dr * dθ_by_dr + sinθdφ_by_dr * sinθdφ_by_dr ) )
        φ_inf    <- def @"φ_inf"    @R $ φ + atan2 ( r * sinθdφ_by_dr ) ( sinθ + r * cosθ * dθ_by_dr )
        far <- def @"far" @R =<< farField cosθ_inf φ_inf
        pure $ Event ( dλ, far )
      else
        if λ + dλ > 1e3
        then pure $ Event (dλ, black)
        else pure noEvent

-- Far field image.
farField :: Code Float -> Code Float -> Program i i (Code EventData)
farField cosθ φ_ = purely do
  φ <- def @"φ" @R $ φ_ `mod` (2 * pi)
  --cos²θ <- def @"cos²θ" @R $ cosθ * cosθ
  --sin²θ <- def @"sin²θ" @R $ 1 - cos²θ

  _ <- def @"farFieldData" @RW black

  {-
  ----------------------------------
  -- galaxy and dust
  _ <- def @"d" @RW $ sin²θ
  u <- def @"u" @R  $ ( max 0 $ 15 * ( sin²θ - 1 ) + 1 )
  -- add noise on outer edges of galaxy
  modify @"d" ( \v -> v * ( v + (1 - v) * 0.5 * ( hash (12.7 * (φ `mod` 0.002)) (40 * (cosθ `mod` 0.002)) + 1 ) ) )
  -- make galaxy thinner
  modify @"d" ( \v -> max 0 ( 15 * (v - 1) + 1 ) )
  base <- get @"d"
  -- add randomness
  n <- brownianNoise ( Vec2 ( 5.7 * cosθ ) ( 3.2 * φ ) )
  modify @"d" ( \v -> v * ( 0.72 * v + (1 - 0.72 * v) * ( 4 * n - 0.15 ) ) )
  modify @"d" ( \v -> v * v )
  d <- get @"d"
  put @"farFieldData" $ temperature ( 4e2 + 8e3 * d * u ) 1
  -}

  ----------------------------------
  -- stars
  off_cosθ <- def @"off_cosθ" @R $ ( ( cosθ `mod` 0.002 ) - 0.001 )
  off_φ    <- def @"off_φ"    @R $ ( ( φ    `mod` 0.002 ) - 0.001 )
  x  <- def @"x"  @R $ cosθ - off_cosθ
  y  <- def @"y"  @R $ φ    - off_φ
  h1 <- def @"h1" @R $ hash (x-y) (y+x)
  h2 <- def @"h2" @R $ hash  x    (y-x)
  when ( h1 > 0.999 ) $ do
    let dist² = 1e0 + 1e2 * h2 * h2
    put @"farFieldData" $ temperature ( starTemperature (1000 * (h1 - 0.999)) ) ( recip dist² )

  get @"farFieldData"
