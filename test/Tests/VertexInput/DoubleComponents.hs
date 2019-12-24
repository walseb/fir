{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.VertexInput.DoubleComponents where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- test placing Doubles in various (valid) components


------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> Double
     , Slot 0 2 ':-> Double
     , Slot 1 0 ':-> V 2 Double
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_x" ':-> Input      '[ Location 0              ] Double
   , "in_y" ':-> Input      '[ Location 0, Component 2 ] Double
   , "in_z" ':-> Input      '[ Location 1              ] Double
   , "in_w" ':-> Input      '[ Location 1, Component 2 ] Double
   , "out"  ':-> Output     '[ Location 0              ] ( V 4 Float )
   , "main" ':-> EntryPoint '[                         ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  x <- get @"in_x"
  y <- get @"in_y"
  z <- get @"in_z"
  w <- get @"in_w"
  put @"out" ( ( convert :: Code Double -> Code Float ) <$$> Vec4 x y z w )

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in"   ':-> Input      '[ Location 0      ] ( V 4 Float )
   , "out"  ':-> Output     '[ Location 0      ] ( V 4 Float )
   , "main" ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
    put @"out" =<< get @"in"

------------------------------------------------
-- pipeline

shaderPipeline :: ShaderPipeline FilePath
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(Triangle List)
  :>-> (vertex  ,   "vertex.spv")
  :>-> (fragment, "fragment.spv")
