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

module Tests.Images.Sample where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Texture2D a b fmt
  = Image (Properties a b TwoD Nothing NonArrayed SingleSampled Sampled (Just fmt))

type Defs = '[ "image"   ':-> Global UniformConstant (Texture2D Float Float (RGBA16 F)) '[ Binding 0 ]
             , "in_pos"  ':-> Global Input  (V 2 Float) '[ Location 0 ]
             , "out_col" ':-> Global Output (V 4 Float) '[ Location 0 ]
             , "main"    ':-> EntryPoint Fragment '[]
             ]

type MyMethod
  = Method
      NoDepthTest
      Affine

program :: Program Defs ()
program =
  Program $ entryPoint @"main" @Fragment do
    pos <- get @"in_pos"
    col <- sample @"image" @(Just MyMethod)
              ( Operands Done )
              pos       
    put @"out_col" col
