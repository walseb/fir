{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Examples.Kerr.Doppler where

-- fir
import FIR
import Math.Linear

-- fir-examples
import Examples.Kerr.Info
  ( KerrInfo )
import Examples.Kerr.Coordinates
  ( spatialMetric, spatialNorm )
import Examples.Kerr.Motion
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
  :: AST KerrInfo
  -> AST MotionConstants
  -> AST (V 4 Float) -- position
  -> Program s s (AST Float)
localGravitationalDopplerFactor kerrInfo constants (Vec4 _ r cosθ _)
  = purely do
    -- black hole info
    a   <- def @"a"   @R $ view @(Field "a" ) kerrInfo
    a²  <- def @"a²"  @R $ view @(Field "a²") kerrInfo

    -- constants of motion
    p_φ <- def @"p_φ" @R $ view @(Field "p_φ") constants

    -- 4-position
    r²    <- def @"r²"    @R $ r * r
    cos²θ <- def @"cos²θ" @R $ cosθ * cosθ
    sin²θ <- def @"sin²θ" @R $ 1 - cos²θ

    -- useful expressions
    r²_p_a² <- def @"r²_p_a²" @R $  r² + a²
    ρ² <- def @"ρ²" @R $ r² + a² * cos²θ
    δ  <- def @"δ"  @R $ r * (r - 2) + a²
    σ² <- def @"σ²" @R $ r²_p_a² * r²_p_a² - a² * δ * sin²θ

    -- ADM formalism: lapse function and shift vector field
    -- reciprocal of lapse
    _α <- def @"_α" @R $ sqrt σ² * invSqrt δ * invSqrt ρ²
    -- φ-component of shift vector (its only non-zero component)
    βφ <- def @"βφ" @R $ - 2 * a * r / σ²

                 -- ( 1 - βφ * p_φ / ξ ) * _α
    def @"res" @R $ ( 1 - βφ * p_φ     ) * _α
    -- Note that, in the Schwarzschild metric, we have:
    --  α = sqrt ( 1 - 2 / r )
    --  β = 0
    -- Thus the local gravitational Doppler factor
    -- simplifies to: 1 / sqrt ( 1 - 2 / r )

-- Compute the Doppler factor due to special relativistic effects,
-- i.e. due to motion of the observer/emitter.
specialDopplerFactor
  :: AST KerrInfo
  -> AST (V 4 Float) -- position
  -> AST (V 4 Float) -- particle velocity
  -> AST (V 4 Float) -- observer velocity
  -> Program i i (AST Float)
specialDopplerFactor kerrInfo pos v v_obs = purely do
  num    <- ( def @"num"    @R . (1-) ) =<< spatialMetric kerrInfo pos v_obs v
  denom² <- ( def @"denom²" @R . (1-) ) =<< spatialNorm   kerrInfo pos v_obs
  def @"res" @R $ num * invSqrt denom²
