{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.VertexInput.ComponentOutOfRange where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- test vertex input specifying disallowed component
-- ( greater than 3 )

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 4 Float
     , Slot 0 4 ':-> Float
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in1"  ':-> Input      '[ Location 0              ] ( V 4 Float )
   , "in2"  ':-> Input      '[ Location 0, Component 4 ] Float
   , "out"  ':-> Output     '[ Location 0              ] ( V 4 Float )
   , "main" ':-> EntryPoint '[                         ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  ~(Vec4 x y z _) <- get @"in1"
  w <- get @"in2"
  put @"out" ( Vec4 x y z w )

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
