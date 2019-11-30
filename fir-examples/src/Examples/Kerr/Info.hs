
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Examples.Kerr.Info
  ( KerrInfo, defaultKerrInfo, computeKerrInfo
  , DiskInfo, defaultDiskInfo, computeDiskInfo
  ) where

-- fir
import FIR

------------------------------------------------
-- Black hole information, such as mass, angular momentum,
-- and information about its accretion disk.

-- This information is computed statically,
-- at the Haskell level (not on the GPU).

type KerrInfo =
  Struct
    '[ "r_h"  ':-> Float -- (outer) event horizon radius
     , "r_ph" ':-> Float -- innermost photon orbit radius
     , "a"    ':-> Float -- signed Kerr parameter, - M ≤ a ≤ M, |a| = J / M c
     , "a²"   ':-> Float -- square of the above
     ]

defaultKerrInfo :: KerrInfo
defaultKerrInfo = computeKerrInfo (-0.6)

defaultDiskInfo :: DiskInfo
defaultDiskInfo = computeDiskInfo defaultKerrInfo 1.2e4 14 True 0.015 0.27 5

type DiskInfo =
  Struct
    '[ "energy"      ':-> Float -- disk energy, derived from accretion rate M': 3 G M M' / 8 π σ
     , "orientation" ':-> Float -- +1 or -1 depending on orientation of disk rotation relative to black hole rotation
     , "r_inner"     ':-> Float -- inner disk radius ( computed to be the innermost stable circular (equatorial) orbit ( = r_ISCO ) )
     , "r_outer"     ':-> Float -- outer disk radius
     , "thickness"   ':-> Float -- disk angular thickness (aspect ratio)
     , "inclination" ':-> Float -- inclination of disk relative to equator
     , "r_BP"        ':-> Float -- Bardeen–Petterson radius
     , "precession"  ':-> Float -- precession angular frequency (relative to gravitational time)
     ]

computeKerrInfo :: Float -> KerrInfo
computeKerrInfo a = r_h :& r_ph :& a :& a² :& End
  where
    a² = a * a
    r_h = 1 + sqrt ( 1 - a² )
    -- r_ergo cosθ = ( 1 + sqrt ( 1 - a² * cos²θ ) )
    r_ph = 2 * ( 1 + cos ( (2/3) * acos ( abs a ) ) )
    -- r_mb  = 2 * r_G * ( 1 + sqrt (1 - a) - 0.5 * a )
    -- Omega_BH = a / ( r_h² + a² )


computeDiskInfo :: KerrInfo -> Float -> Float -> Bool -> Float -> Float -> Float -> DiskInfo
computeDiskInfo kerrInfo energy r_outer same thickness inclination r_BP
  = energy :& sign :& r_inner :& r_outer :& thickness :& inclination :& r_BP :& 1e-2 :& End
    where
      a  = view @(Field "a" ) kerrInfo
      a² = view @(Field "a²") kerrInfo
      z₁  = 1 + ( (1 - a²)**(1/3) ) * ( (1 + a)**(1/3) + (1 - a)**(1/3) )
      z₂  = sqrt $ 3 * a² + z₁ * z₁
      r_inner = ( 3 + z₂ - sign * sqrt ( ( 3 - z₁ ) * ( 3 + z₁ + 2 * z₂ ) ) )
      sign = if same then 1 else -1
