{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Examples.Kerr.Shaders where

-- base
import qualified Prelude
import Control.Monad
  ( void )
import Data.Proxy
  ( Proxy(Proxy) )
import GHC.TypeLits
  ( Div, natVal )

-- filepath
import System.FilePath
  ( (</>) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import FIR
  hiding ( Triangle )
import Math.Linear

-- fir-examples
import FIR.Examples.Kerr.Colour
  ( toneMap )
import FIR.Examples.Kerr.Coordinates
  ( normaliseSpatialComponents )
import FIR.Examples.Kerr.Event
  ( rayTraceUntilEvent, eventColour )
import FIR.Examples.Kerr.Info
  ( KerrInfo, DiskInfo
  , defaultKerrInfo, defaultDiskInfo
  )
import FIR.Examples.Kerr.Motion
  ( initialiseMotion, pattern XP )
import FIR.Examples.Paths
  ( shaderDir )

------------------------------------------------
-- Compute shader: relativistic ray-tracing in Kerr space-time.

type Camera =
  ( Struct
     '[ "position" ':-> V 4 Float
      , "time"     ':-> V 4 Float
      , "right"    ':-> V 4 Float
      , "up"       ':-> V 4 Float
      , "forward"  ':-> V 4 Float
      , "clock"    ':-> Float
      ]
  )

type Width  = 960
type Height = 600
width, height :: Semiring a => a
width  = fromInteger ( Prelude.toInteger $ natVal ( Proxy @Width ) )
height = fromInteger ( Prelude.toInteger $ natVal ( Proxy @Height) )

type ComputeDefs =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 0, Binding 0 ] Camera
   , "kerr" ':-> Uniform '[ DescriptorSet 0, Binding 1 ] KerrInfo
   , "disk" ':-> Uniform '[ DescriptorSet 0, Binding 2 ] DiskInfo
   , "img"  ':-> Image2D '[ DescriptorSet 0, Binding 3 ] ( RGBA8 UNorm )
   -- global size: 60 * 75 * 1
   , "main" ':-> EntryPoint '[ LocalSize ( Width `Div` 60 ) ( Height `Div` 75 ) 1 ] Compute
   ]

computeShader :: Module ComputeDefs
computeShader = Module $ entryPoint @"main" @Compute do

  -- Obtain camera position and orientation, in Boyer–Lindquist coordinates.
  -- We assume that:
  --   - 'time' is a normalised time-like vector,
  --   - 'fwd', 'up', and 'right' are space-like and orthogonal to 'time'.
  x₀    <- use @(Name "ubo" :.: Name "position")
  time  <- use @(Name "ubo" :.: Name "time"    )
  right <- use @(Name "ubo" :.: Name "right"   )
  up    <- use @(Name "ubo" :.: Name "up"      )
  fwd   <- use @(Name "ubo" :.: Name "forward" )
  clock <- use @(Name "ubo" :.: Name "clock"   )

  -- Obtain black hole information (mass, angular momentum, accretion disk info).
  --kerrInfo <- use @(Name "kerr")
  --diskInfo <- use @(Name "disk")
  let
    kerrInfo = Lit defaultKerrInfo
    diskInfo = Lit defaultDiskInfo

  -- Compute pixel coordinates.
  ~(Vec3 i_x i_y _) <- get @"gl_GlobalInvocationID"
  let
    x,y :: Code Float
    x = ( fromIntegral i_x - ( width  / 2 ) ) / ( width / 2 )
    y = ( fromIntegral i_y - ( height / 2 ) ) / ( width / 2 )

  -- Initialise result colour.
  _ <- def @"col" @RW (Vec4 0 0 0 0)

  -- Anti-aliasing outer loops.
  _ <- def @"i" @RW @Float 0
  _ <- def @"j" @RW @Float 0
  while (get @"i" < xSamples) do
    while (get @"j" < ySamples) do

      -- Main loop.

      -- Get the camera ray direction 4-vector for a given (anti-aliased) pixel.
      i <- get @"i"
      j <- get @"j"
      c_right <- let' $ x + ( ( i + 0.5 ) / xSamples - 0.5 ) / 960
      c_up    <- let' $ y + ( ( j + 0.5 ) / ySamples - 0.5 ) / 960

      s₀ <- let' =<< normaliseSpatialComponents kerrInfo x₀ ( fwd ^+^ (c_right *^ right) ^-^ (c_up *^ up) )
      v₀ <- let' $ time ^-^ s₀ -- (light-like vector because of assumptions and normalisation)
      -- Note that the direction is negated to obtain the direction of the photon
      -- instead of the direction the camera is pointing in.

      -- Get constants of motion for a photon coming from a given direction.
      ( constants, p₀ ) <- initialiseMotion kerrInfo x₀ v₀

      -- Trace the ray backwards in time,
      -- by numerical integration of the geodesic equations for Kerr space-time.
      --
      -- Returns the colour/wavelength/temperature associated to one of the following events:
      --  - ray reaches black hole event horizon,
      --  - ray hits the accretion disk,
      --  - ray escapes from the gravitational attraction of the black hole.
      evt <- rayTraceUntilEvent kerrInfo diskInfo constants (XP x₀ p₀) clock
      col <- toneMap <<$>> eventColour kerrInfo constants x₀ evt
      modify @"col" ( ^+^ ( weight *^ col ) )

      modify @"j" (+1)
    put    @"j" 0
    modify @"i" (+1)

  imageWrite @"img"
    ( Vec2 i_x i_y )
    =<< get @"col"

------------------------------------------------
-- rendering parameters

-- samples for anti-aliasing
xSamples, ySamples, samples :: Semiring a => a
xSamples = 1
ySamples = 1
samples = xSamples * ySamples

weight :: Code Float
weight = Lit (1 / samples)

------------------------------------------------
-- Compiling.

compPath :: FilePath
compPath = shaderDir </> "kerr_comp.spv"

compileComputeShader :: IO ( Either ShortText ModuleRequirements )
compileComputeShader = compileTo compPath [Debug, Assert] computeShader

compileAllShaders :: IO ()
compileAllShaders = void compileComputeShader
