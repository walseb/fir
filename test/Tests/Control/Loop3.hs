{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}


module Tests.Control.Loop3 where

-- fir
import FIR
import FIR.Syntax.Labels

------------------------------------------------
-- program

type Defs = '[ "main" ':-> EntryPoint '[] Vertex ]

program :: Module Defs
program = Module do

  entryPoint @"main" @Vertex do

    #i #= (1 :: Code Int32)
    #v #= (1728 :: Code Float)

    loop do
      i <- #i

      when (i > 32) do
        #v .= 123
        break @1

      when (i * 2 > 16) do
        #v .= 456
        break @1

      #i %= (+1)

    v <- #v

    #gl_Position .= Vec4 v v v 1
