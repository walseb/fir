{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.VertexInput.V3Double_Double where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- check for valid placement of doubles

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Double
     , Slot 1 2 ':-> Double
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in"   ':-> Input      '[ Location 0 ] ( V 4 Double )
   , "out"  ':-> Output     '[ Location 0 ] ( V 4 Float  )
   , "main" ':-> EntryPoint '[            ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  v <- get @"in"
  put @"out" ( ( convert :: AST Double -> AST Float ) <$$> v )

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
