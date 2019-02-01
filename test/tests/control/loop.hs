{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Control.Loop where

-- base
import Prelude ( Float )

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "position" ':-> Global_ Output (V 4 Float) ]

program :: Program Defs ()
program = Program do

  entryPoint @"main" @Fragment do

    let (#<) = (<) @(Procedure _ _i _i) -- disambiguate to help type inference

    #t @Float #= 0
    #s @Float #= 1
    #r @Float #= 1

    while ( #t #< abs ( #s - #t ) ) do
      t <- #t
      s <- #s
      #t %= (+1)
      #s .= (s+2*t)
      #r .= t + s
      while ( #r #< ( #t * ( #s + #t ) ) ) do
        #r %= (+1)

    r <- #r
    s <- #s
    t <- #t

    #position .= Vec4 r s t 1
