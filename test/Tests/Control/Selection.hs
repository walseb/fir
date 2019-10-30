{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Control.Selection where

-- fir
import FIR

------------------------------------------------
-- program

type Defs = '[ "x"    ':-> Input      '[Location 0] Float
             , "y"    ':-> Input      '[Location 1] Float
             , "out"  ':-> Output     '[Location 0] Float
             , "main" ':-> EntryPoint '[OriginUpperLeft] Fragment
             ]

program :: Module Defs
program = Module $ entryPoint @"main" @Fragment do

  -- test "selection" instruction
  x  <- get @"x"
  x' <- def @"x'" @R @Float ( if x < 0 then 0 else 3*x )

  -- test monadic "if-then-else" instruction
  y  <- get @"y"
  _  <- def @"y'" @RW @Float y
  if y < 0
  then do
    put @"y'" (y+7)
  else do
    put @"y'" 4
  y' <- get @"y'"
  

  put @"out" (x'+y')
