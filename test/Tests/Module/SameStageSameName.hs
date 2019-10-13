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
-- duplicate entrypoints (not allowed)

type Defs =
  '[ "in"   ':-> Input      '[ Location 0 ] ( V 4 Float )
   , "out"  ':-> Output     '[ Location 0 ] ( V 4 Float )
   , "main" ':-> EntryPoint '[            ] Vertex
   , "main" ':-> EntryPoint '[            ] Vertex
   ]

program :: Module Defs ()
program = Module do
  entryPoint @"main" @Vertex do
    put @"out" =<< get @"in"
  entryPoint @"main" @Vertex do
    put @"out" =<< get @"in"
