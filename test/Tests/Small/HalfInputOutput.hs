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

module Tests.Small.HalfInputOutput where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  =  '[ "in_col"  ':-> Input      '[ Location 0 ] (V 4 Half)
      , "out_col" ':-> Output     '[ Location 0 ] (V 4 Half)
      , "main"    ':-> EntryPoint '[ ]            Vertex
      ]

program :: Module Defs
program =
  Module $ entryPoint @"main" @Vertex do
    put @"out_col" =<< get @"in_col"
