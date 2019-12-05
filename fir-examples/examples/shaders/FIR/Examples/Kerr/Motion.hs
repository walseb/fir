{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE ViewPatterns     #-}

module FIR.Examples.Kerr.Motion
  ( MotionConstants, initialiseMotion
  , geodesicEquations
  , Canonical, pattern XP
  )
  where

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.Kerr.Coordinates
  ( Canonical, pattern XP )
import FIR.Examples.Kerr.Info
  ( KerrInfo )

----------------------------------------------------
-- Computing initial conditions for a given photon.

-- Constants of motion.
type MotionConstants
  = Struct
  '[ "p_φ" ':-> Float -- specific angular momentum about the z-axis p_φ = L_z
   , "ϰ"   ':-> Float -- Carter's constant Κ ( = Q + L_z² + a² ( E² - μ² ) )
   ]

-- Compute constants of motion for a photon setting off in a particular direction.
initialiseMotion
  :: AST KerrInfo
  -> AST (V 4 Float) -- initial 4-position, in modified Boyer–Lindquist coordinates
  -> AST (V 4 Float) -- initial photon direction vector, in Boyer–Lindquist coordinates
  -> Program i i ( AST MotionConstants, AST (V 2 Float) )
initialiseMotion kerrInfo (Vec4 _ r cosθ _) (Vec4 vt vr vθ vφ_)
  = purely do

      -- black hole info
      a  <- def @"a"  @R $ view @(Field "a" ) kerrInfo
      a² <- def @"a²" @R $ view @(Field "a²") kerrInfo

      -- 4-position
      r²    <- def @"r²"    @R $ r * r
      cos²θ <- def @"cos²θ" @R $ cosθ * cosθ
      sin²θ <- def @"sin²θ" @R $ 1 - cos²θ
      csc²θ <- def @"csc²θ" @R $ recip sin²θ
      
      vφ  <- def @"vφ"  @R $ - vφ_ -- I have a sign error somewhere

      δ   <- def @"δ"   @R $ r * (r - 2) + a²
      _δ  <- def @"_δ"  @R $ recip δ
      ρ²  <- def @"ρ²"  @R $ r² + a² * cos²θ
      _ρ² <- def @"_ρ²" @R $ recip ρ²

      -- inner product of 4-momentum with -∂/∂t
      two_a_r_sin²θ_by_ρ² <- def @"two_a_r_sin²θ_by_ρ²" @R $ 2 * a * r * sin²θ * _ρ²
      ξ  <- def @"ξ"  @R $ vt * (1 - 2 * r * _ρ²) + vφ * two_a_r_sin²θ_by_ρ²

      -- inner product of 4-momentum with ∂/∂φ
      p_φ  <- def @"p_φ" @R $ ( vφ * sin²θ * ( a² + r² + a * two_a_r_sin²θ_by_ρ² ) - vt * two_a_r_sin²θ_by_ρ² ) / ξ

      -- inner product of 4-momentum with ∂/∂θ
      p_θ <- def @"p_θ" @R $ ρ² * vθ / ξ

      -- inner product of 4-momentum with ∂/∂r
      p_r <- def @"p_r" @R $ vr * ρ² * _δ / ξ

      -- Carter's constant
      ϰ <- def @"ϰ" @R $ p_θ * p_θ + a² * sin²θ + p_φ * p_φ * csc²θ

      pure $
        ( Struct ( p_φ :& ϰ :& End ), Vec2 p_r p_θ )

------------------------------------------------
-- Geodesic equations for a photon in Kerr space-time, in Hamiltonian form.
--
-- Coordinate system:
--    - position uses modified Boyer–Lindquist coordinates (t,r,χ,φ), χ = cos θ
--    - conjugate momenta (p_r,p_θ) (note: not p_χ).

-- Geodesic equations taken from
--   - "Radiation Transfer of Emission Lines in Curved Space-Time" (pp. 3–4)
--        -- Steven Fuerst, Kinwah Wu


geodesicEquations
  :: AST KerrInfo
  -> AST MotionConstants
  -> AST Float
  -> AST Canonical
  -> Program _i _i (AST Canonical)
geodesicEquations kerrInfo constants _
  ( XP (Vec4 _ r cosθ _) (Vec2 p_r p_θ) ) = purely do

    -- black hole constants
    a  <- def @"a"  @R $ view @(Field "a"  ) kerrInfo
    a² <- def @"a²" @R $ view @(Field "a²" ) kerrInfo

    -- constants of motion
    p_φ <- def @"p_φ" @R $ view @(Field "p_φ") constants
    ϰ   <- def @"ϰ"   @R $ view @(Field "ϰ"  ) constants

    -- derived expressions involving canonical coordinates
    r²    <- def @"r²"    @R $ r * r
    cos²θ <- def @"cos²θ" @R $ cosθ * cosθ
    sin²θ <- def @"sin²θ" @R $ 1 - cos²θ
    sinθ  <- def @"sinθ"  @R $ sqrt  sin²θ
    csc²θ <- def @"csc²θ" @R $ recip sin²θ

    p_r²  <- def @"p_r²"  @R $ p_r * p_r

    -- some common expressions
    r²_p_a²   <- def @"r²_p_a²"   @R $ r² + a²
    δ         <- def @"δ"         @R $ r²_p_a² - 2 * r
    ρ²        <- def @"ρ²"        @R $ r² + a² * cos²θ
    _δ        <- def @"_δ"        @R $ recip δ
    _ρ²       <- def @"_ρ²"       @R $ recip ρ²
    p_φ_csc²θ <- def @"p_φ_csc²θ" @R $ p_φ * csc²θ

    -- geodesic equations, in Hamiltonian form
    -- ξ = specific energy (energy at infinity), assumed to be 1 here by previous normalisation
    -- μ = invariant mass, assumed to be 0 here (as solving geodesic equations for a photon)

                           --  ρ² * ξ + ( 2 * r * r²_p_a² * ξ - 2 * a * p_φ ) * _δ
    t'    <- def @"t'"    @R $ ρ²     + ( 2 * r * r²_p_a²     - 2 * a * p_φ ) * _δ

    r'    <- def @"r'"    @R $ p_r * δ

    cosθ' <- def @"cosθ'" @R $ - sinθ * p_θ

                            -- ( 2 * a * r * ξ  + ( ρ² - 2 * r ) * p_φ_csc²θ ) * _δ
    φ'    <- def @"φ'"    @R $ ( 2 * a * r      + ( ρ² - 2 * r ) * p_φ_csc²θ ) * _δ

                                  -- 2 * r * r²_p_a² * ξ² - 2 * a * p_φ * ξ + ( 1 - r ) * ( ϰ - μ² * r² )
    pr'_term <- def @"pr'_term" @R $ 2 * r * r²_p_a²      - 2 * a * p_φ     + ( 1 - r ) *   ϰ

                            -- ( - r * μ² + pr'_term ) * _δ + 2 * p_r² * ( 1 - r )
    pr'   <- def @"pr'"   @R $              pr'_term   * _δ + 2 * p_r² * ( 1 - r )

                            -- sinθ * cosθ * ( p_φ_csc²θ * p_φ_csc²θ - a² * ( ξ² - μ² ) )
    pθ'   <- def @"pθ'"   @R $ sinθ * cosθ * ( p_φ_csc²θ * p_φ_csc²θ - a²               )

    pure $
      XP
        ( (-_ρ²) *^ Vec4 t' r' cosθ' φ' )
        ( (-_ρ²) *^ Vec2 pr' pθ' )
