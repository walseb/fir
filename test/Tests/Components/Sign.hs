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

module Tests.Components.Sign where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  =  '[ "in_col1" ':-> Input  '[ Location 0              ] (V 2 Int32 ) -- valid component assignment
      , "in_col2" ':-> Input  '[ Location 0, Component 2 ] (V 2 Word32) -- (signedness is not important)
      , "out_col" ':-> Output '[ Location 0              ] (V 4 Int32 )
      , "main"    ':-> EntryPoint '[ ] Vertex
      ]

program :: Module Defs ()
program =
  Module $ entryPoint @"main" @Vertex do
    ~(Vec2 x y) <- get @"in_col1"
    ~(Vec2 z w) <- get @"in_col2"
    put @"out_col" ( Vec4 x y (convert z) (convert w) )
