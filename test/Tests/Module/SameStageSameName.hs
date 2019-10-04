{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.Module.SameStageSameName where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- combined vertex and fragment shaders

type Defs =
  '[ "in"   ':-> Input      '[ Location 0 ] ( V 4 Float )
   , "out"  ':-> Output     '[ Location 0 ] ( V 4 Float )
   , "main" ':-> EntryPoint '[            ] Vertex
   , "main" ':-> EntryPoint '[            ] Vertex
   ]

program :: Program Defs ()
program = Program do
  entryPoint @"main" @Vertex   do
    put @"out" =<< get @"in"
  entryPoint @"main" @Vertex do
    put @"out" =<< get @"in"
