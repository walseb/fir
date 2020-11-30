{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.RayTracing.RayGeneration where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs =
  '[ "payload"        ':-> RayPayload '[ Location 0 ] (V 4 Float)
   , "accelStructure" ':-> Uniform    '[ DescriptorSet 0, Binding 0 ] AccelerationStructure
   , "output"         ':-> Image2D    '[ DescriptorSet 0, Binding 1 ] ( RGBA8 UNorm )
   , "main"           ':-> EntryPoint '[] RayGeneration
   ]

program :: Module Defs
program = Module $ entryPoint @"main" @RayGeneration do
  accelStructure <- get @"accelStructure"
  launchID@( ~( Vec2 ix iy ) ) <- use @( Name "gl_LaunchID" :.: Swizzle "xy" )
  let
    rayDirection :: Code ( V 3 Float )
    rayDirection = Vec3 ( ( fromIntegral ix - 320 ) / 640 ) ( ( fromIntegral iy - 240 ) / 480 ) 1
  traceRay @"payload" accelStructure ( defaultRayInfo { rayDirection } )
  payload <- get @"payload"
  imageWrite @"output" launchID payload

