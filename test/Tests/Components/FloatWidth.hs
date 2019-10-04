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

module Tests.Components.FloatWidth where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  =  '[ "in_col1" ':-> Input  '[ Location 0              ] (V 2 Float) -- component mismatch
      , "in_col2" ':-> Input  '[ Location 0, Component 2 ] Double      -- floating-point types of different widths
      , "out_col" ':-> Output '[ Location 0              ] (V 4 Float)
      , "main"    ':-> EntryPoint '[ ]            Vertex
      ]

program :: Program Defs ()
program =
  Program $ entryPoint @"main" @Vertex do
    ~(Vec2 x y) <- get @"in_col1"
    z <- get @"in_col2"
    put @"out_col" ( Vec4 x y (convert z) 1 )
