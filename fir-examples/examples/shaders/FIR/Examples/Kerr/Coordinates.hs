{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE ViewPatterns          #-}

module FIR.Examples.Kerr.Coordinates where

-- base
import Prelude
  ( map )

-- fir
import FIR
import Math.Linear
  hiding ( normalise, proj, projC, gramSchmidt )

-- fir-examples
import FIR.Examples.Kerr.Info
  ( KerrInfo )
import qualified FIR.Examples.Kerr.RungeKutta as RK
  ( Integrable(..) )

----------------------------------------------------------

-- Canonical coordinates: 4-position, and conjugate momentum in the r and θ coordinates.
type Canonical =
  Struct
    '[ "x" ':-> V 4 Float
     , "p" ':-> V 2 Float
     ]
{-# COMPLETE XP #-}
pattern XP :: Code (V 4 Float) -> Code (V 2 Float) -> Code Canonical
pattern XP x p <- ( ( \xp -> ( view @(Name "x") xp, view @(Name "p") xp ) ) -> (x,p) )
  where XP x p = Struct ( x :& p :& End )

instance RK.Integrable Float Canonical where
  (XP x₁ p₁) ^+^ (XP x₂ p₂) = XP ( x₁ ^+^ x₂ ) ( p₁ ^+^ p₂ )
  s *^ (XP x p) = XP ( s *^ x ) ( s *^ p )
  absV (XP x p) = XP ( abs @(Code Float) <$$> x ) ( abs @(Code Float) <$$> p )
  maxAdaptiveStepFactor (XP εx εp) (XP tol_x tol_p) δ
    = let
        ~(Vec4 δt δr δχ δφ) = δ <$$> εx <**> tol_x
        ~(Vec2 δp_r δp_χ)   = δ <$$> εp <**> tol_p
      in
        min δt ( min δr ( min δχ ( min δφ ( min δp_r δp_χ ) ) ) )

----------------------------------------------------------
-- Conversions.

-- Internally (e.g. to solve the geodesic equations),
-- we use (oblate spheroidal) Boyer–Lindquist coordinates (t,r,θ,φ),
-- which we modify by using χ = cos θ instead of θ
-- (algebraic form of the Boyer–Lindquist coordinates).
--
-- Camera orientation is handled by cartesian Boyer–Lindquist coordinates,
-- given by:
--
--  x = sqrt(r²+a²) sin θ cos φ
--  y = sqrt(r²+a²) sin θ sin φ
--  z = r cos θ
--
-- This module computes the conversion from cartesian into spheroidal coordinates,
-- both for 4-positions and for tangent vectors.


-- Express a position in modified Boyer–Lindquist coordinates.
boyerLindquistPosition :: KerrInfo -> V 3 Float -> V 4 Float
boyerLindquistPosition kerrInfo (V3 x y z) = V4 0 r cosθ φ
  where
    a² = view @(Field "a²") kerrInfo
    x² = x * x
    y² = y * y
    z² = z * z
    d² = 0.5 * ( x² + y² + z² - a² )
    r = sqrt ( d² + sqrt ( a² * z² + d² * d² ) )
    φ = atan2 y x
    cosθ = z / r


-- Express a tangent vector in Boyer–Lindquist coordinates.
-- This consists in multiplication by the inverse Jacobian of the above equations.
boyerLindquistTangent :: KerrInfo -> V 4 Float -> V 3 Float -> V 4 Float
boyerLindquistTangent kerrInfo (V4 _ r cosθ φ) (V3 vx vy vz)
  = V4 0 vr vθ vφ
    where
      a² = view @(Name "a²") kerrInfo
      r² = r * r
      cos²θ = cosθ * cosθ
      sin²θ = 1 - cos²θ
      sinθ  = sqrt sin²θ
      cosφ = cos φ
      sinφ = sin φ
      ρ² = r² + a² * cos²θ
      d² = a² + r²
      d = sqrt d²

      vr = ( r * d * sinθ / ρ² ) * ( cosφ * vx + sinφ * vy )
         + ( cosθ * d² / ρ² ) * vz
      vθ = ( d * cosθ / ρ² ) * ( cosφ * vx + sinφ * vy )
         - ( r * sinθ / ρ² ) * vz
      vφ = ( - sinφ * vx + cosφ * vy ) / ( sinθ * d )


metric :: KerrInfo -> V 4 Float -> V 4 Float -> V 4 Float -> Float
metric kerrInfo (V4 _ r cosθ _) (V4 dt₁ dr₁ dθ₁ dφ₁) (V4 dt₂ dr₂ dθ₂ dφ₂)
  = - ( 1 - 2 * r / ρ² ) * dt₁ * dt₂
  + ( ρ² / δ ) * dr₁ * dr₂
  + ρ² * dθ₁ * dθ₂
  + sin²θ * ( a² + r² + 2 * a² * r * sin²θ / ρ² ) * dφ₁ * dφ₂
  - ( 2 * a * r * sin²θ / ρ² ) * ( dt₁ * dφ₂ + dt₂ * dφ₁ )
    where
      r² = r * r
      δ  = r * (r - 2) + a²
      ρ² = r² + a² * cos²θ
      a  = view @(Name "a" ) kerrInfo
      a² = view @(Name "a²") kerrInfo
      cos²θ = cosθ * cosθ
      sin²θ = 1 - cos²θ

gramSchmidt :: KerrInfo -> V 4 Float -> [V 4 Float] -> [V 4 Float]
gramSchmidt _        _   []     = []
gramSchmidt kerrInfo pos (u:vs)
  = u' : gramSchmidt kerrInfo pos ( map (\v -> v ^-^ proj kerrInfo pos v u') vs)
      where
        u' = normalise kerrInfo pos u

normalise :: KerrInfo -> V 4 Float -> V 4 Float -> V 4 Float
normalise kerrInfo pos v
  = invSqrt ( abs ( metric kerrInfo pos v v ) ) *^ v

-- assumes last argument is normalised
proj :: KerrInfo -> V 4 Float -> V 4 Float -> V 4 Float -> V 4 Float
proj kerrInfo pos u v
  = projC *^ v
      where
        projC = metric kerrInfo pos u v

-- Normalise a vector with zero "t" component.
normaliseSpatialComponents :: Code KerrInfo -> Code ( V 4 Float ) -> Code ( V 4 Float ) -> Program i i ( Code (V 4 Float ) )
normaliseSpatialComponents kerrInfo pos v = purely do
  nm <- def @"nm" @R =<< spatialNorm kerrInfo pos v
  pure $ invSqrt (abs nm) *^ v

-- Compute the spatial part of the metric (3+1 formalism).
spatialMetric :: Code KerrInfo -> Code (V 4 Float) -> Code (V 4 Float) -> Code (V 4 Float) -> Program i i (Code Float)
spatialMetric kerrInfo (Vec4 _ r cosθ _) (Vec4 _ dr₁ dθ₁ dφ₁) (Vec4 _ dr₂ dθ₂ dφ₂) = purely do
  -- black hole info
  a  <- def @"a"  @R $ view @(Field "a" ) kerrInfo
  a² <- def @"a²" @R $ view @(Field "a²") kerrInfo

  -- 4-position
  r²    <- def @"r²"    @R $ r * r
  cos²θ <- def @"cos²θ" @R $ cosθ * cosθ

  δ       <- def @"δ"       @R $ r * (r - 2) + a²
  ρ²      <- def @"ρ²"      @R $ r² + a² * cos²θ
  sin²θ   <- def @"sin²θ"   @R $ 1 - cos²θ

  two_a_r_sin²θ_by_ρ² <- def @"two_a_r_sin²θ_by_ρ²" @R $ 2 * a * r * sin²θ / ρ²

  dr² <- def @"dr²"    @R $ dr₁ * dr₂ * ρ² / δ
  dθ² <- def @"dcosθ²" @R $ dθ₁ * dθ₂ * ρ²
  dφ² <- def @"dφ²"    @R $ dφ₁ * dφ₂ * sin²θ * ( r² + a² + a * two_a_r_sin²θ_by_ρ² )

  def @"spatialMetric" @R $ dr² + dθ² + dφ²

spatialNorm :: Code KerrInfo -> Code (V 4 Float) -> Code (V 4 Float) -> Program i i (Code Float)
spatialNorm kerrInfo pos v = spatialMetric kerrInfo pos v v
  
{-
-- Squared norm of a tangent vector with respect to the Kerr metric,
-- in Boyer–Lindquist coordinates.
ds² :: Code KerrInfo -> Code (V 4 Float) -> Code (V 4 Float) -> Program i i (Code Float)
ds² kerrInfo (Vec4 _ r₀ cosθ₀ _) (Vec4 dt dr dθ dφ) = purely do

  -- black hole info
  a  <- def @"a"  @R $ view @(Field "a" ) kerrInfo
  a² <- def @"a²" @R $ view @(Field "a²") kerrInfo

  -- 4-position
  r     <- def @"r"     @R $ r₀
  r²    <- def @"r²"    @R $ r * r
  cosθ  <- def @"cosθ"  @R $ cosθ₀
  cos²θ <- def @"cos²θ" @R $ cosθ * cosθ

  δ       <- def @"δ"       @R $ r * (r - 2) + a²
  ρ²      <- def @"ρ²"      @R $ r² + a² * cos²θ
  sin²θ   <- def @"sin²θ"   @R $ 1 - cos²θ

  two_a_r_sin²θ_by_ρ² <- def @"two_a_r_sin²θ_by_ρ²" @R $ 2 * a * r * sin²θ / ρ²

  dt²  <- def @"dt²"  @R $ dt * dt * ( (2 * r / ρ²) - 1)
  dr²  <- def @"dr²"  @R $ dr * dr * ρ² / δ
  dθ²  <- def @"dθ²"  @R $ dθ * dθ * ρ²
  dφ²  <- def @"dφ²"  @R $ dφ * dφ * sin²θ * ( r² + a² + a * two_a_r_sin²θ_by_ρ² )
  dtdφ <- def @"dtdφ" @R $ - dt * dφ * 2 * two_a_r_sin²θ_by_ρ²

  def @"ds²" @R $ dt² + dr² + d² + dφ² + dtdφ
-}