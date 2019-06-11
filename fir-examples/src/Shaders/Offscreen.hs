{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedWildCards         #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Shaders.Offscreen where

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

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

vertex :: ShaderStage "main" VertexShader VertexDefs
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

fragment :: ShaderStage "main" FragmentShader FragmentDefs
fragment = shader do
    col <- get @"in_colour"
    put @"out_colour" col

------------------------------------------------
-- compiling

vertPath, fragPath :: FilePath
vertPath = "shaders/offscreen_vert.spv"
fragPath = "shaders/offscreen_frag.spv"

compileVertexShader :: IO ( Either Text () )
compileVertexShader = compile vertPath [] vertex

compileFragmentShader :: IO ( Either Text () )
compileFragmentShader = compile fragPath [] fragment

shaderPipeline :: ShaderPipeline
shaderPipeline
  = withStructInput @VertexInput @(Triangle List)
  $  StartPipeline
  :> (vertex  , vertPath)
  :> (fragment, fragPath)
