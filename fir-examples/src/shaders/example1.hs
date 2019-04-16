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

module Shaders.Example1 where

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position"  ':-> Input  '[ Location 0 ] (V 3 Float)
   , "in_colour"    ':-> Input  '[ Location 1 ] (V 3 Float)
   , "out_colour"   ':-> Output '[ Location 0 ] (V 4 Float)
   , "ubo"          ':-> Uniform '[ Binding 0, DescriptorSet 0 ] --, Block ]
                          ( Struct '[ "mvp" ':-> M 4 4 Float ] )
   , "main"         ':-> EntryPoint '[ ] Vertex
   ]

vertex :: Program VertexDefs ()
vertex = Program $ entryPoint @"main" @Vertex do
    ~(Vec3 r g b) <- get @"in_colour"
    ~(Vec3 x y z) <- get @"in_position"
    mvp <- use @(Name "ubo" :.: Name "mvp")
    put @"out_colour"  (Vec4 r g b 1)
    put @"gl_Position" ( mvp !*^ Vec4 x y z 1 )

------------------------------------------------
-- geometry shader

type GeometryDefs =
  '[ "in_color"  ':-> Input      '[ Location 0 ] ( Array 3 (V 4 Float ) )
   , "out_color" ':-> Output     '[ Location 0 ] ( V 4 Float )
   , "normal"    ':-> Output     '[ Location 1 ] ( V 3 Float )
   , "main"      ':-> EntryPoint
                         '[ Triangles
                          , OutputTriangleStrip, OutputVertices 3
                          ]
                          Geometry
   ]

geometry :: Program GeometryDefs ()
geometry = Program $ entryPoint @"main" @Geometry do
  v0 <- use @(Name "gl_PerVertex" :.: Index 0 :.: Name "gl_Position")
  v1 <- use @(Name "gl_PerVertex" :.: Index 1 :.: Name "gl_Position")
  v2 <- use @(Name "gl_PerVertex" :.: Index 2 :.: Name "gl_Position")
  let
    Vec4 u1x u1y u1z _ = v1 ^-^ v0
    Vec4 u2x u2y u2z _ = v2 ^-^ v0
    normal = normalise ( Vec3 u1x u1y u1z `cross` Vec3 u2x u2y u2z )
  color <- get @"in_color"

  put @"normal" normal

  put @"gl_Position" v0
  put @"out_color" ( view @(Index 0) color )
  emitVertex
  put @"gl_Position" v1
  put @"out_color" ( view @(Index 1) color )
  emitVertex
  put @"gl_Position" v2
  put @"out_color" ( view @(Index 2) color )
  emitVertex
  endPrimitive

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in_colour"   ':-> Input  '[ Location 0 ] (V 4 Float)
   , "normal"      ':-> Input  '[ Location 1 ] (V 3 Float)
   , "out_colour"  ':-> Output '[ Location 0 ] (V 4 Float)
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

fragment :: Program FragmentDefs ()
fragment = Program do
  entryPoint @"main" @Fragment do
    col    <- get @"in_colour"
    normal <- get @"normal"
    let
      light = Vec3 0 (-0.98058) (-0.19612)
      out_col = ( 0.5 + 0.5 * ( normal ^.^ light) ) *^ col
    put @"out_colour" out_col

------------------------------------------------
-- compiling

vertPath, geomPath, fragPath :: FilePath
vertPath = "src/shaders/example1_vert.spv"
geomPath = "src/shaders/example1_geom.spv"
fragPath = "src/shaders/example1_frag.spv"

compileVertexShader :: IO ( Either Text Text )
compileVertexShader = compile vertPath [] vertex

compileGeometryShader :: IO ( Either Text Text )
compileGeometryShader = compile geomPath [] geometry

compileFragmentShader :: IO ( Either Text Text )
compileFragmentShader = compile fragPath [] fragment
