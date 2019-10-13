{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Optics.MVP1 where

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

program :: Module Defs ()
program =
  Module $ entryPoint @"main" @Vertex do
    mvp    <- use @(Name "ubo" :.: Name "mvp")
    in_pos <- get @"in_pos"
    put @"out_pos" (mvp !*^ in_pos)
