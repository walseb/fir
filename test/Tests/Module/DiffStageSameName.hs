{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.Module.DiffStageSameName where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- combined vertex and fragment shaders

type Defs =
  '[ "in"   ':-> Input      '[ Location 0      ] ( V 4 Float )
   , "out"  ':-> Output     '[ Location 0      ] ( V 4 Float )
   , "main" ':-> EntryPoint '[                 ] Vertex
   , "main" ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

program :: Module Defs
program = Module do
  entryPoint @"main" @Vertex   do
    put @"out" =<< get @"in"
  entryPoint @"main" @Fragment do
    put @"out" =<< get @"in"
