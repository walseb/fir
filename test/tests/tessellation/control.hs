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

module Tests.Tessellation.Control where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "in_col"  ':-> Input      '[] (Array 9 (V 4 Float))
             , "out_col" ':-> Output     '[] (Array 9 (V 4 Float))
             , "main"    ':-> EntryPoint '[SpacingEqual, Quads, VertexOrderCcw, OutputVertices 9]
                                TessellationControl
             ]

program :: Program Defs ()
program = Program do

  entryPoint @"main" @TessellationControl do

    i <- get @"gl_InvocationID"
    in_pos <- use @(Name "gl_in" :.: AnIndex Word32 :.: Name "gl_Position") i
    in_col <- use @(Name "in_col" :.: AnIndex Word32) i

    assign @(Name "gl_TessLevelInner" :.: Index 0) 10
    assign @(Name "gl_TessLevelInner" :.: Index 1) 10
    assign @(Name "gl_TessLevelOuter" :.: Index 0) 12
    assign @(Name "gl_TessLevelOuter" :.: Index 1) 12
    assign @(Name "gl_TessLevelOuter" :.: Index 2) 12
    assign @(Name "gl_TessLevelOuter" :.: Index 3) 12

    assign @(Name "gl_out" :.: AnIndex Word32 :.: Name "gl_Position") i in_pos
    assign @(Name "out_col" :.: AnIndex Word32) i in_col
