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

module Tests.Small.HalfArithmetic where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  =  '[ "in_col"  ':-> Input      '[ Location 0      ] (V 4 Float)
      , "out_col" ':-> Output     '[ Location 0      ] (V 4 Float)
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]

program :: Program Defs ()
program =
  Program $ entryPoint @"main" @Fragment do
    def @"h" @R @Half (5 * pi / 4)
    h <- get @"h"
    if abs (cos h) > sqrt 2
    then put @"out_col" =<< get @"in_col"
    else put @"out_col" (Vec4 0 0 0 1)
