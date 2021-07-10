{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE RebindableSyntax         #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE NamedWildCards           #-}
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
     , "zoom"      ':-> Float
     , "origin"    ':-> V 2 Float
     , "seed"      ':-> V 2 Float
     , "scancodes" ':-> Array 512 Word32
     , "imGuiData" ':-> ImGuiData ref
     ]

type ImGuiData :: ControllerRef -> Type
type ImGuiData ref =
  Struct
    '[ "color"    ':-> ControllerData ref Float
     , "max_iter" ':-> ControllerData ref Float
     ]

initImGuiData :: ImGuiData InitValue
initImGuiData
  =  ( "Color", Slider, 0 )
  :& ( "Iterations", Slider, 0 )
  :& End

initInputData :: InputData Value
initInputData = 0 :& 0 :& V2 0 0 :& V2 0 0 :& Prelude.pure 0 :& controllerInitValues initImGuiData :& End

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

maxDepth :: Code Word32
maxDepth = 256

xSamples, ySamples :: Code Word32
xSamples = 4
ySamples = 4

xWidth, yWidth :: Code Float
xWidth = recip . fromIntegral $ xSamples
yWidth = recip . fromIntegral $ ySamples

pixel2Coord :: Code Float -> Code (V 2 Float) -> Code (V 4 Float) -> Code (V 2 Float)
pixel2Coord range (Vec2 centerX centerY) (Vec4 pixX' pixY' _ _) =
  let (pixX, pixY) = if inverseCoord then (pixY', pixX') else (pixX', pixY')
      (uvX, uvY) = (pixX / screenXF, pixY / screenYF)
      coordX = (screenXF / screenYF) * (uvX - 0.5)
      coordY = (-1) * (uvY - 0.5)
      x = centerX + coordX * range
      y = centerY + coordY * range
   in Vec2 x y

-- Params begins

inverseCoord :: Bool
inverseCoord = False

screenX, screenY :: Word32
(screenX, screenY) = (800, 600)

screenXF, screenYF :: Code Float
screenXF =  Lit (fromIntegral screenX)
screenYF =  Lit (fromIntegral screenY)

grad_freq :: Code Float
grad_freq = 0.6

-- Params ends
fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
  gl_FragCoord <- #gl_FragCoord
  color <- use @(Name "ubo" :.: Name "imGuiData" :.: Name "color")
  max_iter' <- use @(Name "ubo" :.: Name "imGuiData" :.: Name "max_iter")
  map_mode' <- use @(Name "ubo" :.: Name "map_mode")
  seed <- use @(Name "ubo" :.: Name "seed")
  range <- use @(Name "ubo" :.: Name "zoom")
  origin <- use @(Name "ubo" :.: Name "origin")

  let escape = 4242
      map_mode = map_mode' /= 0
  let max_iter :: Code Word32
      max_iter = 100 + (250 * round max_iter')

  #modulus #= (0 :: Code Float)
  #mean #= (0 :: Code Float)

  #iter #= (0 :: Code Word32)

  let pixelCoord = pixel2Coord range origin gl_FragCoord

  #depth #= (0 :: Code Word32)

  #z #=   (if map_mode then Lit (V2 0 0) else pixelCoord)
  let c = (if map_mode then pixelCoord else seed)

  loop do
    iter <- #iter
    modulus <- #modulus
    z <- #z
    if iter > max_iter || modulus > escape
      then break @1
      else do
        let Vec2 zR zI = z
            newZ = Vec2 zR (abs zI) ^+^ c

            newZLog = complexLog (CodeComplex newZ)

            newModulus = magnitude newZLog

        #modulus .= newModulus

        mean <- #mean
        #mean .= (mean + newModulus)

        #iter .= (iter + 1)
        #z .= codeComplex newZLog

  iter <- #iter
  mean <- #mean
  modulus <- #modulus
  let iterF = fromIntegral iter

  t <-
    let' @(Code Float) $
      if iter == (max_iter + 1)
        then 1 - (0.3 * mean / iterF)
        else
          let ml = iterF - log (log (grad_freq * modulus)) / log 2 + log (log escape) / log 2
              res = ml / fromIntegral max_iter
           in res

  let col = if map_mode && nearBy (range / 100) pixelCoord seed
              then Lit seedColor
              else gradient ((1 + 7 * color) * t) (Lit sunset)

  --let col' = Vec4 t 0.2 0.1 0.5

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
