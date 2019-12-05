{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Examples.Offscreen.Shaders where

-- base
import Data.Foldable
  ( sequence_ )

-- filepath
import System.FilePath
  ( (</>) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.Paths
  ( shaderDir )

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
vertPath = shaderDir </> "offscreen_vert.spv"
fragPath = shaderDir </> "offscreen_frag.spv"

compileVertexShader :: IO ( Either ShortText ModuleRequirements )
compileVertexShader = compileTo vertPath [] vertex

compileFragmentShader :: IO ( Either ShortText ModuleRequirements )
compileFragmentShader = compileTo fragPath [] fragment

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileVertexShader
  , compileFragmentShader
  ]

shaderPipeline :: ShaderPipeline FilePath
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(Triangle List)
  :>-> (vertex  , vertPath)
  :>-> (fragment, fragPath)
