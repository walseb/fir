{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Control.Switch where

-- fir
import FIR

------------------------------------------------
-- program

type Defs = '[ "a"    ':-> Input      '[Location 0] Word32
             , "b"    ':-> Input      '[Location 1] Int32
             , "t"    ':-> Input      '[Location 2] Float
             , "out"  ':-> Output     '[Location 0] Float
             , "main" ':-> EntryPoint '[OriginUpperLeft] Fragment
             ]

program :: Module Defs
program = Module $ entryPoint @"main" @Fragment do

  -- test "switch" instruction
  a <- get @"a"
  t <- get @"t"
  x <- def @"x" @R @Float $
          switch a
            [ 7 :-> t + 1
            , 2 :-> 2
            , 1 :-> 3 - t
            ]
            ( t - 9 ) -- default

  -- test monadic "switchM" instruction
  y <- switchM (get @"b")
          [ 2 :-> pure 11
          , 1 :-> get @"t"
          ]
          ( abs $ get @"t" )

  put @"out" (x + y)
