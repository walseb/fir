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
    '[ "mousePos"  ':-> V 2 Float
     , "scancodes" ':-> Array 512 Word32
     , "imGuiData" ':-> ImGuiData ref
     ]

type ImGuiData :: ControllerRef -> Type
type ImGuiData ref =
  Struct
    '[ "slider1" ':-> ControllerData ref Float
     , "slider2" ':-> ControllerData ref Float
     ]

initImGuiData :: ImGuiData InitValue
initImGuiData
  =  ( "Slider 1", Slider, 0 )
  :& ( "Slider 2", Slider, 0 )
  :& End

initInputData :: InputData Value
initInputData = V2 0 0 :& Prelude.pure 0 :& controllerInitValues initImGuiData :& End

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

pixel2Coord :: Code Float -> Code (V 4 Float) -> Code (V 2 Float)
pixel2Coord range (Vec4 pixX' pixY' _ _) =
  let (pixX, pixY) = if inverseCoord then (pixY', pixX') else (pixX', pixY')
      (uvX, uvY) = (pixX / screenX, pixY / screenY)
      x = uvX * range - centerX
      y = uvY * range - centerY
   in Vec2 x y

-- Params begins

inverseCoord :: Bool
inverseCoord = True

screenX, screenY :: Code Float
(screenX, screenY) = (500, 500)

centerX, centerY :: Code Float
(centerX, centerY) = (0, 0)

grad_freq :: Code Float
grad_freq = 0.6

max_iter' :: Word32
max_iter' = 100

seed :: Code (V 2 Float)
seed = Vec2 (-0.7477055835083013) (-2.692868835794263)

-- Params ends
fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
  gl_FragCoord <- #gl_FragCoord
  range' <- use @(Name "ubo" :.: Name "imGuiData" :.: Name "slider1")

  let range = 1000 * range'
  let escape = 4242
  let max_iter = 100

  #modulus #= (0 :: Code Float)
  #mean #= (0 :: Code Float)

  #iter #= (0 :: Code Word32)

  #z #= pixel2Coord range gl_FragCoord
  #depth #= (0 :: Code Word32)

  -- let Vec2 mouseX mouseY = pixel2Coord (Vec4 mx my 0 0)

  -- let Vec2 x y = seed
  let c = seed -- Vec2 (x + mouseX) (y + mouseY)
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
  let col = gradient t (Lit sunset)

  --let col' = Vec4 t 0.2 0.1 0.5

  #out_colour .= col

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
