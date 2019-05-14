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

type TessellationControlDefs =
  '[ "in_col"   ':-> Input  '[ Location 0 ] (Array 3 (V 4 Float))
   , "out_col"  ':-> Output '[ Location 0 ] (Array 3 (V 4 Float))
   , "patchOut" ':-> Output '[ Location 1, Patch ]
                          ( Struct '[ "center" ':-> V 4 Float ] )
   , "main"    ':-> EntryPoint '[ SpacingEqual, VertexOrderCw, OutputVertices 3 ]
                      TessellationControl
   ]

program :: Program TessellationControlDefs ()
program = Program $ entryPoint @"main" @TessellationControl do
  
  i <- get @"gl_InvocationID"
  in_pos <- use @(Name "gl_in" :.: AnIndex Word32 :.: Name "gl_Position") i
  in_col <- use @(Name "in_col" :.: AnIndex Word32) i
  center <-
    ( recip 3 *^ ) $  use @(Name "gl_in" :.: Index 0 :.: Name "gl_Position")
                  ^+^ use @(Name "gl_in" :.: Index 1 :.: Name "gl_Position")
                  ^+^ use @(Name "gl_in" :.: Index 2 :.: Name "gl_Position")

  assign @(Name "gl_TessLevelInner" :.: Index 0) 16
  assign @(Name "gl_TessLevelInner" :.: Index 1) 16
  assign @(Name "gl_TessLevelOuter" :.: Index 0) 16
  assign @(Name "gl_TessLevelOuter" :.: Index 1) 16
  assign @(Name "gl_TessLevelOuter" :.: Index 2) 16
  assign @(Name "gl_TessLevelOuter" :.: Index 3) 16

  assign @(Name "gl_out"   :.: AnIndex Word32 :.: Name "gl_Position") i in_pos
  assign @(Name "out_col"  :.: AnIndex Word32) i in_col
  assign @(Name "patchOut" :.: Name "center") center
