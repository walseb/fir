{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.Interface.ComponentMismatch where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 4 Float
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in"   ':-> Input      '[ Location 0 ] ( V 4 Float )
   , "out"  ':-> Output     '[ Location 0 ] ( V 4 Float )
   , "main" ':-> EntryPoint '[            ] Vertex
   ]

vertex :: ShaderStage "main" VertexShader VertexDefs _
vertex = shader do
  put @"out" =<< get @"in"

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in1"  ':-> Input      '[ Location 0              ] ( V 3 Float )
   , "in2"  ':-> Input      '[ Location 0, Component 3 ] Float
   , "out"  ':-> Output     '[ Location 0              ] ( V 4 Float )
   , "main" ':-> EntryPoint '[ OriginUpperLeft         ] Fragment
   ]

fragment :: ShaderStage "main" FragmentShader FragmentDefs _
fragment = shader do
    ~(Vec3 x y z) <- get @"in1"
    w <- get @"in2"
    put @"out" ( Vec4 x y z w )

------------------------------------------------
-- pipeline

shaderPipeline :: ShaderPipeline
shaderPipeline
  = withStructInput @VertexInput @(Triangle List)
  $    StartPipeline
  :>-> (vertex  ,   "vertex.spv")
  :>-> (fragment, "fragment.spv")
