{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FIR.Examples.Kerr.Colour
  ( blackbodyColour, wavelengthColour, starTemperature, toneMap )
  where

-- base
import qualified Prelude
import Data.Maybe
  ( fromJust )
import Data.Proxy
  ( Proxy(Proxy) )
import GHC.TypeNats
  ( KnownNat, natVal )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- fir
import FIR
import Math.Linear

------------------------------------------------------------------------

-- Really basic logarithmic tone mapping.
toneMap :: AST (V 4 Float) -> AST (V 4 Float)
toneMap (Vec4 r g b i) =
  let l = max 0 . min 1 $ ( log i - 25 ) / 10
  in Vec4 (r * l) (g * l) (b * l) 1

blackbodyColour :: AST Float -> Program s s (AST (V 4 Float))
blackbodyColour t = purely do
  t' <- def @"t'" @R $ ( max 0 . min 20000 $ t ) / 20000
  pure (gradient t' (Lit blackbodyTable))

wavelengthColour :: AST Float -> Program s s (AST (V 4 Float))
wavelengthColour λ = purely do
  t <- def @"t" @R $ ( λ - 365 ) / 400
  pure ( gradient t (Lit wavelengthTable) )

wavelengthTable :: Array 21 (V 4 Float)
wavelengthTable
  = MkArray . fromJust . Vector.fromList
  $ [ V4 0     0     0     1  -- 365 nm
    , V4 0.022 0     0.128 1  -- 385 nm
    , V4 0.141 0     0.432 1  -- 405 nm
    , V4 0.426 0     1     1  -- 425 nm
    , V4 0.447 0     1     1  -- 445 nm
    , V4 0     0     1     1  -- 465 nm
    , V4 0     0.573 0.922 1  -- 485 nm
    , V4 0     0.899 0.49  1  -- 505 nm
    , V4 0     1     0     1  -- 525 nm
    , V4 0     1     0     1  -- 545 nm
    , V4 0.763 1     0     1  -- 565 nm
    , V4 1     0.801 0     1  -- 585 nm
    , V4 1     0.278 0     1  -- 605 nm
    , V4 1     0     0     1  -- 625 nm
    , V4 0.958 0     0     1  -- 645 nm
    , V4 0.591 0     0     1  -- 665 nm
    , V4 0.321 0     0     1  -- 685 nm
    , V4 0.155 0     0     1  -- 705 nm
    , V4 0.063 0     0     1  -- 725 nm
    , V4 0.016 0     0     1  -- 745 nm
    , V4 0     0     0     1  -- 765 nm
    ]

blackbodyTable :: Array 21 (V 4 Float)
blackbodyTable
  = MkArray . fromJust . Vector.fromList
  $ [ V4 0      0      0      0         --     0 K
    , V4 1.0000 0.0401 0.0000 2.525e+06 --  1000 K
    , V4 1.0000 0.2484 0.0061 4.431e+11 --  2000 K
    , V4 1.0000 0.4589 0.1483 2.939e+13 --  3000 K
    , V4 1.0000 0.6354 0.3684 2.496e+14 --  4000 K
    , V4 1.0000 0.7792 0.6180 9.170e+14 --  5000 K
    , V4 1.0000 0.8952 0.8666 2.208e+15 --  6000 K
    , V4 0.9102 0.9000 1.0000 4.174e+15 --  7000 K
    , V4 0.7644 0.8139 1.0000 6.783e+15 --  8000 K
    , V4 0.6693 0.7541 1.0000 9.964e+15 --  9000 K
    , V4 0.6033 0.7106 1.0000 1.364e+16 -- 10000 K
    , V4 0.5551 0.6776 1.0000 1.773e+16 -- 11000 K
    , V4 0.5187 0.6519 1.0000 2.217e+16 -- 12000 K
    , V4 0.4904 0.6314 1.0000 2.691e+16 -- 13000 K
    , V4 0.4677 0.6146 1.0000 3.190e+16 -- 14000 K
    , V4 0.4493 0.6007 1.0000 3.710e+16 -- 15000 K
    , V4 0.4341 0.5890 1.0000 4.248e+16 -- 16000 K
    , V4 0.4212 0.5791 1.0000 4.802e+16 -- 17000 K
    , V4 0.4103 0.5705 1.0000 5.369e+16 -- 18000 K
    , V4 0.4009 0.5630 1.0000 5.947e+16 -- 19000 K
    , V4 0.3928 0.5565 1.0000 6.535e+16 -- 20000 K
    ]

gradient :: forall n. KnownNat n
         => AST Float
         -> AST (Array n (V 4 Float))
         -> AST (V 4 Float)
gradient t colors
  =   ( (1-s) *^ ( view @(AnIndex _)  i    colors ) )
  ^+^ (    s  *^ ( view @(AnIndex _) (i+1) colors ) )
  where n :: Semiring a => a
        n = fromInteger . Prelude.fromIntegral $ natVal ( Proxy @n )
        i :: AST Int32
        i = max 0 . min (n-1) $ floor ( (n-1) * t )
        s :: AST Float
        s = (n-1) * t - fromIntegral i

-- | Cumulative distribution function of stellar temperatures.
starTemperatureCDF :: [ ( AST Float, AST Float ) ]
starTemperatureCDF
  = [ (0     ,   2400)
    , (0.7645,   3700) -- 76.45% of stars have perceived temperature in the 2400K - 3700K range
    , (0.8855,   5200) -- etc
    , (0.9615,   6000)
    , (0.9914,   7500)
    , (0.9984,  10000)
    , (0.9971,  30000)
    , (1     , 200000)
    ]

piecewiseLinear :: [ ( AST Float, AST Float ) ] -> AST Float -> AST Float
piecewiseLinear ( ( t1, v1 ) : ( t2, v2 ) : tvs ) t
  = if t < t2
    then v1 + ( v2 - v1 ) * ( t - t1 ) / ( t2 - t1 )
    else piecewiseLinear ( ( t2, v2) : tvs ) t
piecewiseLinear [ (_,v) ] _ = v
piecewiseLinear []        t = t

starTemperature :: AST Float -> AST Float
starTemperature = piecewiseLinear starTemperatureCDF
