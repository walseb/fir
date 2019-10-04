{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.Module.SameStageDiffName where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- combined vertex and fragment shaders

type Defs =
  '[ "in"    ':-> Input      '[ Location 0 ] ( V 4 Float )
   , "out"   ':-> Output     '[ Location 0 ] ( V 4 Float )
   , "vert1" ':-> EntryPoint '[            ] Vertex
   , "vert2" ':-> EntryPoint '[            ] Vertex
   ]

program :: Program Defs ()
program = Program do
  entryPoint @"vert1" @Vertex do
    put @"out" =<< get @"in"
  entryPoint @"vert2" @Vertex do
    put @"out" =<< get @"in"
