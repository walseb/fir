{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Examples.Offscreen.Shaders where

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float
     , Slot 1 0 ':-> V 3 Float
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position"  ':-> Input  '[ Location 0 ] (V 3 Float)
   , "in_colour"    ':-> Input  '[ Location 1 ] (V 3 Float)
   , "out_colour"   ':-> Output '[ Location 0 ] (V 4 Float)
   , "ubo"          ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp" ':-> M 4 4 Float ] )
   , "main"         ':-> EntryPoint '[] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
    ~(Vec3 r g b) <- get @"in_colour"
    ~(Vec3 x y z) <- get @"in_position"
    mvp <- use @(Name "ubo" :.: Name "mvp")
    pos <- def @"pos" @R ( mvp !*^ Vec4 x y z 1 )
    put @"out_colour"   (Vec4 r g b 1)
    put @"gl_Position"  pos

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in_colour"   ':-> Input  '[ Location 0 ] (V 4 Float)
   , "out_colour"  ':-> Output '[ Location 0 ] (V 4 Float)
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
    col <- get @"in_colour"
    put @"out_colour" col

------------------------------------------------
-- compiling

vertPath, fragPath :: FilePath
vertPath = "shaders/offscreen_vert.spv"
fragPath = "shaders/offscreen_frag.spv"

compileVertexShader :: IO ( Either ShortText ModuleRequirements )
compileVertexShader = compile vertPath [] vertex

compileFragmentShader :: IO ( Either ShortText ModuleRequirements )
compileFragmentShader = compile fragPath [] fragment

shaderPipeline :: ShaderPipeline
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(Triangle List)
  :>-> (vertex  , vertPath)
  :>-> (fragment, fragPath)
