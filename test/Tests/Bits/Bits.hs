{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Bits.Bits where

-- fir
import FIR

------------------------------------------------
-- program

type Defs = '[ "x"    ':-> Input  '[Location 0] Word32
             , "y"    ':-> Input  '[Location 1] Word32
             , "z"    ':-> Input  '[Location 2] Word32
             , "w"    ':-> Input  '[Location 3] Word32
             , "out"  ':-> Output '[Location 0] Word32
             , "main" ':-> EntryPoint '[] Vertex
             ]

program :: Module Defs ()
program = Module $ entryPoint @"main" @Vertex do

    a <- get @"x" .&. get @"y"
    
    b <- def @"b" @R =<< (get @"z" .|. get @"w")

    put @"out" $ (a `shiftL` (3 :: AST Word32)) `xor` (a `shiftR` b) `xor` (complement b)
