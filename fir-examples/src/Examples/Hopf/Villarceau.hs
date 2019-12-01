{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Examples.Hopf.Villarceau where

-- base
import Data.Maybe
  ( fromJust )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( KnownNat )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- fir
import FIR
  ( Struct(..)
  , Array(MkArray)
  , AnIndex
  , knownValue
  )
import qualified FIR
import Math.Linear
import qualified Math.Quaternion as Quaternion

-- fir-examples
import Examples.Hopf.Shaders

----------------------------------------------------------------------------
-- Hopf fibration

villarceauRadii
  :: Float            -- absolute value
  -> ( Float, Float ) -- minor and major radii
villarceauRadii r
  = ( r, sqrt $ 1 + r * r )

villarceauCenter
  :: Float     -- minor torus radius
  -> Float     -- angle
  -> V 3 Float
villarceauCenter a theta
  = ( a * cos theta ) *^ V3 0 0 1 ^+^ ( a * sin theta ) *^ V3 1 0 0

villarceauNormal
  :: Float -- major torus radius
  -> Float -- minor torus radius
  -> Float -- angle
  -> V 3 Float
villarceauNormal c a phi = baseNormal ^-^ perp ^+^ rotatedPerp
  where theta = atan ( a / sqrt ( c*c - a*a ) )
        baseNormal  = V3 (sin theta) (cos theta) 0
        torusNormal = V3 0 (-1) 0
        dp   = baseNormal ^.^ torusNormal
        perp = baseNormal ^-^ dp *^ torusNormal
        rotatedPerp = Quaternion.rotate (Quaternion.axisAngle torusNormal (-phi)) perp

villarceauCircle
  :: Float     -- major torus radius
  -> Float     -- minor torus radius
  -> Float     -- size of circle thickening
  -> Float     -- angle
  -> V 4 Float -- colour
  -> Struct VertexInput
villarceauCircle c a circleThickening theta col
  =  villarceauCenter a theta
  :& c
  :& villarceauNormal c a theta
  :& circleThickening
  :& col
  :& End

villarceauCircles
  :: KnownNat n
  => Float               -- radius
  -> Float               -- radius
  -> Float               -- size of circle thickening
  -> Int                 -- number of tori
  -> Array n (V 4 Float) -- gradient
  -> Float               -- angular offset
  -> [Struct VertexInput]
villarceauCircles c a circleThickening n grad offset
  = [ villarceauCircle c a circleThickening (theta + offset) col
    | k <- [0 .. (n-1)]
    , let theta = fromIntegral k * (2 * pi) / fromIntegral n
    , let col   = gradient ( fromIntegral k / fromIntegral n ) grad
    ]

----------------------------------------------------------------------------
-- gradients

gradient
  :: forall n. KnownNat n
  => Float
  -> Array n (V 4 Float)
  -> V 4 Float
gradient t colors
  =   ( (1-s) *^ ( FIR.view @(AnIndex _)  i    colors ) )
  ^+^ (    s  *^ ( FIR.view @(AnIndex _) (i+1) colors ) )
  where
    n :: Float
    n =  fromIntegral $ knownValue @n
    i :: Word32
    i = floor ( (n-1) * t )
    s :: Float
    s = (n-1) * t - fromIntegral i


sunset :: Array 3 (V 4 Float)
sunset = MkArray . fromJust . Vector.fromList $ fmap (fmap (/255))
  [ V4 224  28  11 255
  , V4 224 154  11 255
  , V4 224  28  11 255
  ]

blues :: Array 3 (V 4 Float)
blues = MkArray . fromJust . Vector.fromList  $ fmap (fmap (/255))
  [ V4 138  43 226 255
  , V4  30 144 255 255
  , V4 138  43 226 255
  ]

greens :: Array 3 (V 4 Float)
greens = MkArray . fromJust . Vector.fromList  $ fmap (fmap (/255))
  [ V4  29 173  51 255
  , V4  86 188  35 255
  , V4 138 193  36 255
  ]
