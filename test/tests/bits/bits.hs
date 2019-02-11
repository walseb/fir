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

type Defs = '[ "x"    ':-> Input  '[] Word32
             , "y"    ':-> Input  '[] Word32
             , "z"    ':-> Input  '[] Word32
             , "w"    ':-> Input  '[] Word32
             , "out"  ':-> Output '[] Word32
             , "main" ':-> EntryPoint '[] Vertex
             ]

program :: Program Defs ()
program = Program $ entryPoint @"main" @Vertex do

    a <- get @"x" .&. get @"y"
    
    b <- def @"b" @R (get @"z" .|. get @"w")

    put @"out" $ (a `shiftL` (3 :: AST Word32)) `xor` (a `shiftR` b) `xor` (complement b)
