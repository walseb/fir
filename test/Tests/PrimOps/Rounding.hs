{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedWildCards      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.PrimOps.Rounding where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "x"    ':-> Input  '[Location 0] Float
             , "y"    ':-> Input  '[Location 1] Int32
             , "z"    ':-> Input  '[Location 2] Float
             , "out"  ':-> Output '[Location 0] (V 4 Float)
             , "main" ':-> EntryPoint '[] Vertex
             ]

program :: Module Defs
program = Module $ entryPoint @"main" @Vertex do

  x <- get @"x"
  let x' = x - floor x
  z <- get @"z"
  z1 :: Code Int32 <- round ( get @"z" )
  let z2 :: Code Int32
      z2 = truncate z
  y' <- convert $ get @"y" + pure z1 - pure z2
  let w = ceiling x + z

  put @"out" (Vec4 x' y' z w)
