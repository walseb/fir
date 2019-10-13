{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.VertexInput.CompatibleTypes where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- test implicit Int32 - Word32 conversion
-- between vertex input and vertex shader

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 4 Int32 ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in"   ':-> Input      '[ Location 0 ] ( V 4 Word32 )
   , "out"  ':-> Output     '[ Location 0 ] ( V 4 Float  )
   , "main" ':-> EntryPoint '[            ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  vec <- get @"in"
  put @"out" ( ( convert :: AST Word32 -> AST Float ) <$$> vec )

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
