{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Control.Def where

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "position" ':-> Output     '[Location 0] (V 4 Float)
             , "main"     ':-> EntryPoint '[] Vertex
             ]

program :: Module Defs
program = Module do

  entryPoint @"main" @Vertex do

    let (#<) = (<) @(Program _i _i _) -- disambiguate to help type inference

    #t @Float #= 0
    #s @Float #= 1
    #r @Float #= 1

    while ( #t #< abs ( #s - #t ) ) do
      t <- #t
      s <- #s

      #r .= t + s

    r <- #r
   
    if r < 1
    then #r .= 1
    else #r .= r - 1

    #u @Float #= 0 -- variable definition not in top block after function declaration

    #position .= Vec4 r r r 1
