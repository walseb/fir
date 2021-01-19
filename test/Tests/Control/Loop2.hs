{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Control.Loop2 where

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "main" ':-> EntryPoint '[] Vertex ]

program :: Module Defs
program = Module do

  entryPoint @"main" @Vertex do

    #p #= ( 99 :: Code Float )
    #q #= ( Vec4 3 4 5 6 :: Code ( V 4 Float ) )

    loop do
      p <- #p
      q <- #q
      if p * norm q > 1555
      then break @1
      else do
        #p .= p + 17
        #q .= 3 *^ q
    
    q <- #q

    #gl_Position .= q
