{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.Module.DiffStageDiffName where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- combined vertex and fragment shaders

type Defs =
  '[ "in"   ':-> Input      '[ Location 0      ] ( V 4 Float )
   , "out"  ':-> Output     '[ Location 0      ] ( V 4 Float )
   , "vert" ':-> EntryPoint '[                 ] Vertex
   , "frag" ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

program :: Program Defs ()
program = Program do
  entryPoint @"vert" @Vertex   do
    put @"out" =<< get @"in"
  entryPoint @"frag" @Fragment do
    put @"out" =<< get @"in"
