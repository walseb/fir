{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.Interface.InvalidComponent where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 4 Float
     , Slot 1 0 ':-> Double
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in1"  ':-> Input      '[ Location 0              ] ( V 4 Float )
   , "in2"  ':-> Input      '[ Location 1              ] Double
   , "out1" ':-> Output     '[ Location 0              ] ( V 4 Float )
   , "out2" ':-> Output     '[ Location 1, Component 1 ] Double
   , "main" ':-> EntryPoint '[                         ] Vertex
   ]

vertex :: ShaderStage "main" VertexShader VertexDefs _
vertex = shader do
  put @"out1" =<< get @"in1"
  put @"out2" =<< get @"in2"

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in1"  ':-> Input      '[ Location 0              ] ( V 4 Float )
   , "in2"  ':-> Input      '[ Location 1, Component 1 ] Double
   , "out"  ':-> Output     '[ Location 0              ] ( V 4 Float )
   , "main" ':-> EntryPoint '[ OriginUpperLeft         ] Fragment
   ]

fragment :: ShaderStage "main" FragmentShader FragmentDefs _
fragment = shader do
    ~(Vec4 x y z _) <- get @"in1"
    w <- get @"in2"
    put @"out" ( Vec4 x y z ( convert w ) )

------------------------------------------------
-- pipeline

shaderPipeline :: ShaderPipeline
shaderPipeline
  = withStructInput @VertexInput @(Triangle List)
  $    StartPipeline
  :>-> (vertex  ,   "vertex.spv")
  :>-> (fragment, "fragment.spv")
