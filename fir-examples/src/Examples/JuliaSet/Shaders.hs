{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Examples.JuliaSet.Shaders where

-- base
import Data.Maybe
  ( fromJust )
import GHC.TypeNats
  ( KnownNat )

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
  '[ "out_colour"  ':-> Output     '[ Location 0      ] (V 4 Float)
   , "ubo"         ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mousePos" ':-> V 2 Float ] )
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]


complexSquare :: AST (V 2 Float) -> AST (V 2 Float)
complexSquare (Vec2 x y) = Vec2 ( x * x - y * y ) ( 2 * x * y )

gradient :: forall n. KnownNat n
         => AST Float
         -> AST (Array n (V 4 Float))
         -> AST (V 4 Float)
gradient t colors
  =   ( (1-s) *^ ( view @(AnIndex _)  i    colors ) )
  ^+^ (    s  *^ ( view @(AnIndex _) (i+1) colors ) )
  where n :: AST Float
        n = Lit . fromIntegral $ knownValue @n
        i :: AST Word32
        i = floor ( (n-1) * t )
        s :: AST Float
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

maxDepth :: AST Word32
maxDepth = 256

xSamples, ySamples :: AST Word32
xSamples = 4
ySamples = 4

xWidth, yWidth :: AST Float
xWidth = recip . fromIntegral $ xSamples
yWidth = recip . fromIntegral $ ySamples

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do

    ~(Vec4 x y _ _) <- #gl_FragCoord
    ~(Vec2 mx my) <- use @(Name "ubo" :.: Name "mousePos")


    let (#<) = (<) @(Program _i _i _) -- disambiguate to help type inference

    #total     @Word32 #= 0

    #xSampleNo @Word32 #= 0
    while ( #xSampleNo #< pure xSamples ) do

      #ySampleNo @Word32 #= 0
      while ( #ySampleNo #< pure ySamples ) do


        xNo <- #xSampleNo
        yNo <- #ySampleNo

        let
          dx, dy :: AST Float
          dx = ( fromIntegral xNo + 0.5 ) * xWidth - 0.5
          dy = ( fromIntegral yNo + 0.5 ) * xWidth - 0.5

        #pos @(V 2 Float) #= Vec2 ((x+dx-960)/250) ((y+dy-540)/250)
        #continue         #= Lit True
        #depth @Word32    #= 0

        while #continue do
          pos   <- #pos
          depth <- #depth
          if ( pos ^.^ pos > 4 || depth > maxDepth )
            then ( #continue .= Lit False )
            else do
              #pos   .= complexSquare pos ^+^ Vec2 ((mx-960)/600) ((my-540)/600)
              #depth .= depth + 1

        depth <- #depth
        #total %= (+depth)

        #ySampleNo %= (+1)
      #xSampleNo %= (+1)


    total <- #total
    t <- def @"t" @R
        ( log ( fromIntegral total * xWidth * yWidth ) / log ( fromIntegral maxDepth ) :: AST Float )

    let col = gradient t (Lit sunset)

    #out_colour .= col

------------------------------------------------
-- compiling

vertPath, fragPath :: FilePath
vertPath = "shaders/juliaset_vert.spv"
fragPath = "shaders/juliaset_frag.spv"

compileVertexShader :: IO ( Either ShortText ModuleRequirements )
compileVertexShader = compileTo vertPath [] vertex

compileFragmentShader :: IO ( Either ShortText ModuleRequirements )
compileFragmentShader = compileTo fragPath [] fragment

shaderPipeline :: ShaderPipeline
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(Triangle List)
  :>-> (vertex  , vertPath)
  :>-> (fragment, fragPath)
