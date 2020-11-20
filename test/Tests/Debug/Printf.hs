{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedWildCards    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Tests.Debug.Printf where

-- fir
import FIR
import FIR.Syntax.DebugPrintf
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "position" ':-> Input  '[Location 0] (V 4 Float)
             , "colour"   ':-> Input  '[Location 1] (V 4 Word32)
             , "out"      ':-> Output '[Location 0] (V 4 Word32)
             , "main"     ':-> EntryPoint '[OriginUpperLeft] Fragment
             ]

program :: Module Defs
program = Module do

  entryPoint @"main" @Fragment do

    pos <- get @"position"
    col <- get @"colour"

    debugPrintf ( "position = " % vec @4 float % " colour = " % vec @4 word32Hex ) pos col

    put @"out" col
