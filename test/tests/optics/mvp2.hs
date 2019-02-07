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

module Tests.Optics.MVP2 where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "ubo"     ':-> Global Uniform
                               ( Struct '[ "mvp" ':-> M 4 4 Float ] )
                              '[ Binding 0, DescriptorSet 0, Block ]
             , "in_pos"  ':-> Global Input  (V 4 Float) '[ Location 0 ]
             , "out_pos" ':-> Global Output (V 4 Float) '[ Location 0 ]
             , "main"    ':-> EntryPoint Vertex '[]
             ]

program :: Program Defs ()
program =
  Program $ entryPoint @"main" @Vertex do
    ubo    <- use @(Name "ubo")
    let mvp = view @(Name "mvp") ubo
    in_pos <- get @"in_pos"
    let out_pos = set @(Index 3) 1 $ mvp !*^ in_pos
    put @"out_pos" out_pos
