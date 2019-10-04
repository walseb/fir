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
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 4 Int32
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in"   ':-> Input      '[ Location 0 ] ( V 4 Word32 )
   , "out"  ':-> Output     '[ Location 0 ] ( V 4 Float  )
   , "main" ':-> EntryPoint '[            ] Vertex
   ]

vertex :: ShaderStage "main" VertexShader VertexDefs _
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
