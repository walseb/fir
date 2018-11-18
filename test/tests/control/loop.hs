{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
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

type Uniforms = '[ "position" ':-> Var W (V 4 Float) ]

type Functions = '[ ]

program :: Program Uniforms Functions ()
program = do

  entryPoint @"main" @Fragment do

    let (#<) = (<) @(C _ _) -- disambiguate to help type inference

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

    #position .= vec4 0 0 0 1
