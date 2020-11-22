{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.ProgramState.Shadowing where

-- fir
import FIR

------------------------------------------------
-- program

type Defs = '[ "out"  ':-> Output     '[Location 0] Float
             , "main" ':-> EntryPoint '[OriginUpperLeft] Fragment
             ]

program :: Module Defs
program = Module $ entryPoint @"main" @Fragment do

  t <- def @"t" @R @Float 5.5

  inner

  s  <- def @"s" @R @Float 7.7
  t' <- get @"t"

  put @"out" (t + s + t')


inner :: Program i i (Code Word32)
inner = purely do
  def @"t" @R @Word32 3
  def @"s" @R @Word32 4
