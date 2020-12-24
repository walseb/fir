{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Geometry.Geometry where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type GeometryDefs =
  '[ "in_color"  ':-> Input  '[ Location 0 ] ( Array 3 (V 4 Float ) )
   , "out_color" ':-> Output '[ Location 0 ] ( V 4 Float )
   , "normal"    ':-> Output '[ Location 1 ] ( V 3 Float )
   , "main"      ':-> EntryPoint
                         '[ Triangles
                          , OutputTriangleStrip
                          , OutputVertices 3
                          , Invocations 1
                          ]
                          Geometry
   ]

program :: Module GeometryDefs
program = Module $ entryPoint @"main" @Geometry do
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

  pure ( Lit () )
