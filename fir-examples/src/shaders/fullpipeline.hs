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

module Shaders.FullPipeline where

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
   , "origin"       ':-> Output '[ Location 1 ] (V 4 Float)
   , "ubo"          ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp"    ':-> M 4 4 Float
                                    , "origin" ':-> V 4 Float
                                    ]
                          )
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
-- tessellation control

type TessellationControlDefs =
  '[ "in_col"     ':-> Input      '[ Location 0 ] (Array 3 (V 4 Float))
   , "out_col"    ':-> Output     '[ Location 0 ] (Array 3 (V 4 Float))
   , "main"       ':-> EntryPoint '[ SpacingEqual, VertexOrderCw, OutputVertices 3 ]
                        TessellationControl
   ]

tessellationControl :: Program TessellationControlDefs ()
tessellationControl = Program $ entryPoint @"main" @TessellationControl do
  
  i <- get @"gl_InvocationID"
  in_pos <- use @(Name "gl_in" :.: AnIndex Word32 :.: Name "gl_Position") i
  in_col <- use @(Name "in_col" :.: AnIndex Word32) i

  assign @(Name "gl_TessLevelInner" :.: Index 0) 16
  assign @(Name "gl_TessLevelInner" :.: Index 1) 16
  assign @(Name "gl_TessLevelOuter" :.: Index 0) 16
  assign @(Name "gl_TessLevelOuter" :.: Index 1) 16
  assign @(Name "gl_TessLevelOuter" :.: Index 2) 16
  assign @(Name "gl_TessLevelOuter" :.: Index 3) 16

  assign @(Name "gl_out" :.: AnIndex Word32 :.: Name "gl_Position") i in_pos
  assign @(Name "out_col" :.: AnIndex Word32) i in_col

------------------------------------------------
-- tessellation evaluation

type TessellationEvaluationDefs =
  '[ "in_cols" ':-> Input      '[ Location 0 ] (Array 3 (V 4 Float))
   , "out_col" ':-> Output     '[ Location 0 ] (V 4 Float)
   , "ubo"          ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp"    ':-> M 4 4 Float
                                    , "origin" ':-> V 4 Float
                                    ]
                          )
   , "main"    ':-> EntryPoint '[ Triangles ] TessellationEvaluation
   ]

tessellationEvaluation :: Program TessellationEvaluationDefs ()
tessellationEvaluation = Program $ entryPoint @"main" @TessellationEvaluation do
  ~(Vec3 u v w) <- get @"gl_TessCoord"
  
  in_cols <- get @"in_cols"
  put @"out_col" (     u *^ (view @(Index 0) in_cols)
                   ^+^ v *^ (view @(Index 1) in_cols)
                   ^+^ w *^ (view @(Index 2) in_cols)
                 )

  pos0 <- use @(Name "gl_in" :.: Index 0 :.: Name "gl_Position")
  pos1 <- use @(Name "gl_in" :.: Index 1 :.: Name "gl_Position")
  pos2 <- use @(Name "gl_in" :.: Index 2 :.: Name "gl_Position")
  orig <- use @(Name "ubo" :.: Name "origin")
  let t = 0.5 - (2*u - 1)**2 * (2*v - 1)**2 * (2*w - 1)**2
  put @"gl_Position"
    ( t *^ orig ^+^ (1-t) *^ ( u *^ pos0 ^+^ v *^ pos1 ^+^ w *^ pos2 ) )


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
  v0 <- use @(Name "gl_in" :.: Index 0 :.: Name "gl_Position")
  v1 <- use @(Name "gl_in" :.: Index 1 :.: Name "gl_Position")
  v2 <- use @(Name "gl_in" :.: Index 2 :.: Name "gl_Position")
  let
    Vec4 u1x u1y u1z _ = v1 ^-^ v0
    Vec4 u2x u2y u2z _ = v2 ^-^ v0
    normal = normalise ( Vec3 u1x u1y u1z `cross` Vec3 u2x u2y u2z )
  color <- get @"in_color"

  put @"normal" normal
  put @"gl_Position" v0
  put @"out_color" ( view @(Index 0) color )
  emitVertex

  put @"normal" normal
  put @"gl_Position" v1
  put @"out_color" ( view @(Index 1) color )
  emitVertex

  put @"normal" normal
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
fragment = Program $ entryPoint @"main" @Fragment do
    col    <- get @"in_colour"
    normal <- get @"normal"
    let
      light = Vec3 0 (-0.98058) (-0.19612)
      out_col = ( 0.5 + 0.5 * ( normal ^.^ light) ) *^ col
    put @"out_colour" (set @(Index 3) 1 out_col)

------------------------------------------------
-- compiling

vertPath, tescPath, tesePath, geomPath, fragPath :: FilePath
vertPath = "shaders/fullpipeline_vert.spv"
tescPath = "shaders/fullpipeline_tesc.spv"
tesePath = "shaders/fullpipeline_tese.spv"
geomPath = "shaders/fullpipeline_geom.spv"
fragPath = "shaders/fullpipeline_frag.spv"

compileVertexShader :: IO ( Either Text Text )
compileVertexShader = compile vertPath [] vertex

compileTessellationControlShader :: IO ( Either Text Text )
compileTessellationControlShader = compile tescPath [] tessellationControl

compileTessellationEvaluationShader :: IO ( Either Text Text )
compileTessellationEvaluationShader = compile tesePath [] tessellationEvaluation

compileGeometryShader :: IO ( Either Text Text )
compileGeometryShader = compile geomPath [] geometry

compileFragmentShader :: IO ( Either Text Text )
compileFragmentShader = compile fragPath [] fragment
