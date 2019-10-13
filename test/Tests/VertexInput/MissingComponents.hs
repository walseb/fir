{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.VertexInput.MissingComponents where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- test vertex shader accessing data
-- not specified in vertex input
-- ( validation should disallow this )

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> Float ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in"   ':-> Input      '[ Location 0 ] ( V 4 Float )
   , "out"  ':-> Output     '[ Location 0 ] ( V 4 Float )
   , "main" ':-> EntryPoint '[            ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  put @"out" =<< get @"in"

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

shaderPipeline :: ShaderPipeline
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(Triangle List)
  :>-> (vertex  ,   "vertex.spv")
  :>-> (fragment, "fragment.spv")
