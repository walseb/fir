{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Geometry.NotGeometry where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type FragmentDefs =
  '[ "in_position" ':-> Input  '[ Location 0 ] (V 4 Float)
   , "in_colour"   ':-> Input  '[ Location 1 ] (V 4 Float)
   , "out_colour"  ':-> Output '[ Location 0 ] (V 4 Float)
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

program :: Module FragmentDefs ()
program = Module do
  entryPoint @"main" @Fragment do
    col <- get @"in_colour"
    put @"out_colour" col
    emitVertex    -- not allowed: not in a geometry shader
