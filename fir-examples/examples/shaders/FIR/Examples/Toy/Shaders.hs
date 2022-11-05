{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedWildCards           #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE RebindableSyntax         #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module FIR.Examples.Toy.Shaders where

-- base
import Data.Foldable
  ( sequence_ )
import Data.Kind
  ( Type )
import Data.Maybe
  ( fromJust )
import GHC.TypeNats
  ( KnownNat )
import Prelude
  ( Int )
import qualified Prelude

-- filepath
import System.FilePath
  ( (</>) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

-- fir-examples
import FIR.Examples.DearImGui
  ( Controller(..), ControllerRef(..), ControllerData
  , controllerInitValues
  )
import FIR.Examples.Paths
  ( shaderDir )

------------------------------------------------
-- user data

type InputData :: ControllerRef -> Type
type InputData ref =
  Struct
    '[ "map_mode"  ':-> Word32
     , "screen"    ':-> V 2 Float
     , "zoom"      ':-> Float
     , "origin"    ':-> V 2 Float
     , "seed"      ':-> V 2 Float
     , "scancodes" ':-> Array 512 Word32
     , "imGuiData" ':-> ImGuiData ref
     ]

type ImGuiData :: ControllerRef -> Type
type ImGuiData ref =
  Struct
    '[ "inverse"  ':-> ControllerData ref ()             Int32
     , "color"    ':-> ControllerData ref (Float, Float) Float
     , "max_iter" ':-> ControllerData ref (Int,   Int)   Int32
     ]

initImGuiData :: ImGuiData InitValue
initImGuiData
  =  ( "Inverse Axis", Toggle, (), 1 )
  :& ( "Itensity",     Slider, (0, 1), 0.5 )
  :& ( "Iterations",   DiscreteSlider, (1, 256), 42 )
  :& End

initInputData :: InputData Value
initInputData = 0 :& V2 0 0 :& 0 :& V2 0 0 :& V2 0 0 :& Prelude.pure 0 :& controllerInitValues initImGuiData :& End

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]

-------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   , "main"         ':-> EntryPoint '[            ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
    ~(Vec3 x y z) <- get @"in_position"
    put @"gl_Position" (Vec4 x y z 1)

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "out_colour"  ':-> Output '[ Location 0 ]
                          ( V 4 Float )
   , "ubo"         ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( InputData Value )
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ]
                          Fragment
   ]

pixel2Coord :: Code (V 2 Float) -> Code Float -> Code (V 2 Float) -> Code (V 2 Float) -> Code (V 2 Float)
pixel2Coord (Vec2 scrX scrY) range (Vec2 centerX centerY) (Vec2 pixX pixY) =
  let
      (uvX, uvY) = (pixX / scrX, pixY / scrY)
      coordX = (scrX / scrY) * (uvX - 0.5)
      coordY = (-1) * (uvY - 0.5)
      x = centerX + coordX * range
      y = centerY + coordY * range
   in Vec2 x y

-- | Compute the pixel color using the Duck set fractal:
--   http://www.algorithmic-worlds.net/blog/blog.php?Post=20110227
quackColor :: Code Word32 -> Code (V 2 Float) -> Code (V 2 Float) -> Program _ _ (Code Float)
quackColor max_iter initZ c = purely do
  -- Define stateful variable for the loop
  _ <- def @"iter" @RW ( 0 :: Code Word32 )
  _ <- def @"z" @RW initZ
  _ <- def @"mean" @RW ( 0 :: Code Float )

  loop do
    -- Get the current z value
    ~(Vec2 zR zI) <- get @"z"
    -- Compute next z value :              log( zR+abs(zI)i + c )
    z <- let' $ complexLog $ CodeComplex ( Vec2 zR (abs zI) ^+^ c )

    modulus <- let' (magnitude z)
    mean <- get @"mean"
    put @"mean" $ mean + modulus
    put @"z" $ codeComplex z

    iter <- get @"iter"
    if iter >= max_iter || modulus > escape
      then break @1
      else put @"iter" $ iter + 1

  iter <- get @"iter"
  mean <- get @"mean"
  pure $
      if iter == max_iter
        then 1 - (0.3 * mean / fromIntegral iter)
        else 0

  where
    escape = 4242


fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
  -- Get CPU data
  ~(Vec4 px py _ _) <- #gl_FragCoord
  ~(Vec2 sx sy)     <- use @(Name "ubo" :.: Name "screen")
  intensity         <- use @(Name "ubo" :.: Name "imGuiData" :.: Name "color")
  max_iter'         <- use @(Name "ubo" :.: Name "imGuiData" :.: Name "max_iter")
  inverse_coord     <- use @(Name "ubo" :.: Name "imGuiData" :.: Name "inverse")
  map_mode'         <- use @(Name "ubo" :.: Name "map_mode")
  seed              <- use @(Name "ubo" :.: Name "seed")
  range             <- use @(Name "ubo" :.: Name "zoom")
  origin            <- use @(Name "ubo" :.: Name "origin")

  -- Adapt the values
  map_mode  <- let' $ map_mode' /= 0
  inversed  <- let' $ not map_mode && (inverse_coord /= 0)
  pixel     <- let' $ if inversed then Vec2 py px else Vec2 px py
  screen    <- let' $ if inversed then Vec2 sy sx else Vec2 sx sy
  coord     <- let' $ pixel2Coord screen range origin pixel

  max_iter  <- let' $ fromIntegral max_iter'
  z         <- let' $ if map_mode then Lit (V2 0 0) else coord
  c         <- let' $ if map_mode then coord else seed

  -- Compute the color
  color <- quackColor max_iter z c

  let col = if map_mode && nearBy (range / 100) coord seed
              then Lit seedColor
              else gradient (color * intensity) (Lit sunset)

  #out_colour .= col

nearBy :: Code Float -> Code (V 2 Float) -> Code (V 2 Float) -> Code Bool
nearBy dist (Vec2 x y) (Vec2 x' y') =
  if abs (x - x') < dist && abs (y - y') < dist
    then Lit True
    else Lit False

mkRescaledComplex :: Code (V 2 Float) -> CodeComplex Float
mkRescaledComplex (Vec2 x y) =
  ( (x - 960) / 250 ) :+: ( (y - 540) / 250 )

gradient :: forall n. KnownNat n
         => Code Float
         -> Code (Array n (V 4 Float))
         -> Code (V 4 Float)
gradient t colors
  =   ( (1-s) *^ ( view @(AnIndex _)  i    colors ) )
  ^+^ (    s  *^ ( view @(AnIndex _) (i+1) colors ) )
  where n :: Code Float
        n = Lit . fromIntegral $ knownValue @n
        i :: Code Word32
        i = floor ( (n-1) * t )
        s :: Code Float
        s = (n-1) * t - fromIntegral i

seedColor :: V 4 Float
seedColor = V4 0.9843 0.2823 0.7686 1.0

sunset :: Array 9 (V 4 Float)
sunset = MkArray . fromJust . Vector.fromList $
       [ V4 0    0    0    0
       , V4 0.28 0.1  0.38 1
       , V4 0.58 0.2  0.38 1
       , V4 0.83 0.3  0.22 1
       , V4 0.98 0.45 0.05 1
       , V4 0.99 0.62 0.2  1
       , V4 1    0.78 0.31 1
       , V4 1    0.91 0.6  1
       , V4 1    1    1    1
       ]

------------------------------------------------
-- compiling

vertPath, fragPath :: FilePath
vertPath = shaderDir </> "toy_vert.spv"
fragPath = shaderDir </> "toy_frag.spv"

compileVertexShader :: IO ( Either ShortText ModuleRequirements )
compileVertexShader = compileTo vertPath [SPIRV $ Version 1 0] vertex

compileFragmentShader :: IO ( Either ShortText ModuleRequirements )
compileFragmentShader = compileTo fragPath [SPIRV $ Version 1 0] fragment

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileVertexShader
  , compileFragmentShader
  ]

shaderPipeline :: ShaderPipeline FilePath
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(Triangle List)
  :>-> (vertex  , vertPath)
  :>-> (fragment, fragPath)
