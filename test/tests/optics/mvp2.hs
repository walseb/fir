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

type Defs = '[ "ubo"     ':-> Uniform '[ Binding 0, DescriptorSet 0, Block ]
                                ( Struct '[ "mvp" ':-> M 4 4 Float ] )
             , "in_pos"  ':-> Input  '[ Location 0 ] (V 4 Float) 
             , "out_pos" ':-> Output '[ Location 0 ] (V 4 Float) 
             , "main"    ':-> EntryPoint '[] Vertex
             ]

program :: Program Defs ()
program =
  Program $ entryPoint @"main" @Vertex do
    ubo    <- use @(Name "ubo")
    let mvp = view @(Name "mvp") ubo
    in_pos <- get @"in_pos"
    let out_pos = set @(Index 3) 1 $ mvp !*^ in_pos
    put @"out_pos" out_pos
