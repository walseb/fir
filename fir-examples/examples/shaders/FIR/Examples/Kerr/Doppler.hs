{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnicodeSyntax    #-}

module FIR.Examples.Kerr.Doppler where

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.Kerr.Info
  ( KerrInfo )
import FIR.Examples.Kerr.Coordinates
  ( spatialMetric, spatialNorm )
import FIR.Examples.Kerr.Motion
  ( MotionConstants )

----------------------------------------------------
-- Computing Doppler frequency shift factors.

-- Compute the local gravitational Doppler factor.
-- 
-- The total gravitational Doppler factor is the ratio of two such factors,
-- between emitter and observer.
--
-- See
--   - "The gravitational light shift and the Sachs–Wolfe effect" (p. 5, eq. 3.8)
--       -- Cesar Merlín, Marcelo Salgado
localGravitationalDopplerFactor
  :: Code KerrInfo
  -> Code MotionConstants
  -> Code (V 4 Float) -- position
  -> Program s s (Code Float)
localGravitationalDopplerFactor kerrInfo constants (Vec4 _ r cosθ _)
  = purely do
    -- black hole info
    a   <- let' $ view @(Field "a" ) kerrInfo
    a²  <- let' $ view @(Field "a²") kerrInfo

    -- constants of motion
    p_φ <- let' $ view @(Field "p_φ") constants

    -- 4-position
    r²    <- let' $ r * r
    cos²θ <- let' $ cosθ * cosθ
    sin²θ <- let' $ 1 - cos²θ

    -- useful expressions
    r²_p_a² <- let' $ r² + a²
    ρ²      <- let' $ r² + a² * cos²θ
    δ       <- let' $ r * (r - 2) + a²
    σ²      <- let' $ r²_p_a² * r²_p_a² - a² * δ * sin²θ

    -- ADM formalism: lapse function and shift vector field
    -- reciprocal of lapse
    _α <- let' $ sqrt σ² * invSqrt δ * invSqrt ρ²
    -- φ-component of shift vector (its only non-zero component)
    βφ <- let' $ - 2 * a * r / σ²

                 -- ( 1 - βφ * p_φ / ξ ) * _α
    let' $ ( 1 - βφ * p_φ     ) * _α
    -- Note that, in the Schwarzschild metric, we have:
    --  α = sqrt ( 1 - 2 / r )
    --  β = 0
    -- Thus the local gravitational Doppler factor
    -- simplifies to: 1 / sqrt ( 1 - 2 / r )

-- Compute the Doppler factor due to special relativistic effects,
-- i.e. due to motion of the observer/emitter.
specialDopplerFactor
  :: Code KerrInfo
  -> Code (V 4 Float) -- position
  -> Code (V 4 Float) -- particle velocity
  -> Code (V 4 Float) -- observer velocity
  -> Program i i (Code Float)
specialDopplerFactor kerrInfo pos v v_obs = purely do
  num    <- ( let' . (1-) ) =<< spatialMetric kerrInfo pos v_obs v
  denom² <- ( let' . (1-) ) =<< spatialNorm   kerrInfo pos v_obs
  let' $ num * invSqrt denom²
