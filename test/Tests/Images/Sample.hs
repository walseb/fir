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

type Defs
  =  '[ "image"   ':-> Texture2D  '[ Binding 0, DescriptorSet 0 ] (RGBA8 UNorm)
      , "in_pos"  ':-> Input      '[ Location 0 ] (V 2 Float)
      , "out_col" ':-> Output     '[ Location 0 ] (V 4 Float)
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]

program :: Module Defs
program =
  Module $ entryPoint @"main" @Fragment do
    pos <- get @"in_pos"
    col <- use @(ImageTexel "image") (MinLOD 0.5 NilOps) pos
    put @"out_col" col
