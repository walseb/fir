{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Control.Loop where

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "main" ':-> EntryPoint '[] Vertex ]

program :: Module Defs ()
program = Module do

  entryPoint @"main" @Vertex do

    let (#<) = (<) @(Program _i _i _) -- disambiguate to help type inference

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

    #gl_Position .= Vec4 r s t 1
