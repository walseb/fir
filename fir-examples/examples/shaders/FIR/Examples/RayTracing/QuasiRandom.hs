{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Examples.RayTracing.QuasiRandom
  ( QuasiRandom
  , initialiseQuasiRandomState, random01s
  )
  where

-- fir
import FIR
import Math.Linear

--------------------------------------------------------------------------

type QuasiRandom s =
  ( Has "quasiRandomConstants" s ~ V 4 Float, CanGet "quasiRandomConstants" s
  , Has "quasiRandomState"     s ~ V 4 Float, CanGet "quasiRandomState"     s, CanPut "quasiRandomState" s
  )

-- | Positive real solution to x^5 + x^4 = 1.
α :: Code Float
α = 0.85667488385450287485

-- | Initialise quasi-random generator.
initialiseQuasiRandomState :: ( QuasiRandom _t, _ ) => Code ( V 3 Word32 ) -> Program _s _t ( Code () )
initialiseQuasiRandomState ( Vec3 i j t ) = do
  h  <- let' $ ( 741103597 * ( ( i + 39847 * t ) * 0x3504f335 `xor` ( j + 14867 * t ) * 0x8fc1ecd5 ) ) `mod` 5000
  β  <- let' $ ( fromIntegral h * α ) `mod` 1
  β² <- let' $ β * β
  cs <- def @"quasiRandomConstants" @R  @( V 4 Float ) $ Vec4 β β² ( β * β² ) ( β² * β² )
  _  <- def @"quasiRandomState"     @RW @( V 4 Float ) $ ( `mod` 1 ) <$$> ( ( 20 * fromIntegral t ) *^ cs )
  pure ( Lit () )

-- | Generate 4 quasi-random numbers using the quasi-random generator state (which this function updates).
random01s :: QuasiRandom s => Program s s ( Code ( V 4 Float ) )
random01s = do
  cs <- get @"quasiRandomConstants"
  st <- get @"quasiRandomState"
  rs <- let' $ ( `mod` 1 ) <$$> ( cs ^+^ st )
  put @"quasiRandomState" rs
  pure rs
