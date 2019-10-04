{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.VertexInput.InvalidVectorComponent where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 2 Float
     , Slot 0 2 ':-> V 4 Float
     , Slot 1 2 ':-> V 2 Float
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_x"  ':-> Input      '[ Location 0              ] ( V 2 Float )
   , "in_yz" ':-> Input      '[ Location 0, Component 2 ] ( V 4 Float )
   , "in_w"  ':-> Input      '[ Location 1, Component 2 ] ( V 2 Float )
   , "out"   ':-> Output     '[ Location 0              ] ( V 4 Float )
   , "main"  ':-> EntryPoint '[                         ] Vertex
   ]

vertex :: ShaderStage "main" VertexShader VertexDefs _
vertex = shader do
  put @"out" =<< get @"in_yz"

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in"   ':-> Input      '[ Location 0      ] ( V 4 Float )
   , "out"  ':-> Output     '[ Location 0      ] ( V 4 Float )
   , "main" ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

fragment :: ShaderStage "main" FragmentShader FragmentDefs _
fragment = shader do
    put @"out" =<< get @"in"

------------------------------------------------
-- pipeline

shaderPipeline :: ShaderPipeline
shaderPipeline
  = withStructInput @VertexInput @(Triangle List)
  $    StartPipeline
  :>-> (vertex  ,   "vertex.spv")
  :>-> (fragment, "fragment.spv")
