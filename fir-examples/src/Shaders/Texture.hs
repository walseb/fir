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

module Shaders.Texture where

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position" ':-> Input  '[ Location 0 ] (V 3 Float)
   , "in_colour"   ':-> Input  '[ Location 1 ] (V 3 Float)
   , "in_uv"       ':-> Input  '[ Location 2 ] (V 2 Float)
   , "out_colour"  ':-> Output '[ Location 0 ] (V 4 Float)
   , "out_uv"      ':-> Output '[ Location 1 ] (V 2 Float)
   , "ubo"         ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp" ':-> M 4 4 Float ] )
   , "main"        ':-> EntryPoint '[] Vertex
   ]

vertex :: Program VertexDefs ()
vertex = Program $ entryPoint @"main" @Vertex do
    ~(Vec3 r g b) <- get @"in_colour"
    ~(Vec3 x y z) <- get @"in_position"
    uv  <- get @"in_uv"
    mvp <- use @(Name "ubo" :.: Name "mvp")
    pos <- def @"pos" @R ( mvp !*^ Vec4 x y z 1 )
    put @"out_colour"  (Vec4 r g b 1)
    put @"out_uv"      uv
    put @"gl_Position" pos

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in_colour"   ':-> Input      '[ Location 0 ] (V 4 Float)
   , "in_uv"       ':-> Input      '[ Location 1 ] (V 2 Float)
   , "logo"        ':-> Texture2D  '[ Binding 1, DescriptorSet 0 ]
                          (RGBA8 UNorm)
   , "out_colour"  ':-> Output     '[ Location 0 ] (V 4 Float)
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

fragment :: Program FragmentDefs ()
fragment = Program do
  entryPoint @"main" @Fragment do
    col <- get @"in_colour"
    uv  <- get @"in_uv"
    -- naughty texture flipping trick
    let r = view @(Index 0) col
        uv' =
          if r < 0.01 || r > 0.99
          then over @(Index 0) (\x -> 1 - x) uv
          else uv
    tex <- use @(ImageTexel "logo") NoOperands uv'
    let alpha = view @(Index 3) tex
        res   = set  @(Index 3) 1 $ alpha *^ tex ^+^ (1-alpha) *^ col
    put @"out_colour" res


------------------------------------------------
-- compiling

vertPath, fragPath :: FilePath
vertPath = "shaders/texture_vert.spv"
fragPath = "shaders/texture_frag.spv"

compileVertexShader :: IO ( Either Text Text )
compileVertexShader = compile vertPath [] vertex

compileFragmentShader :: IO ( Either Text Text )
compileFragmentShader = compile fragPath [] fragment
