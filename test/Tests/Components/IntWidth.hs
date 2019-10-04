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

module Tests.Components.IntWidth where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  =  '[ "in_col1" ':-> Input  '[ Location 0              ] (V 2 Word32) -- component mismatch
      , "in_col2" ':-> Input  '[ Location 0, Component 2 ] Word64       -- integer types of different widths
      , "out_col" ':-> Output '[ Location 0              ] (V 4 Word32)
      , "main"    ':-> EntryPoint '[ ]            Vertex
      ]

program :: Program Defs ()
program =
  Program $ entryPoint @"main" @Vertex do
    ~(Vec2 x y) <- get @"in_col1"
    z <- get @"in_col2"
    put @"out_col" ( Vec4 x y (convert z) 1 )
