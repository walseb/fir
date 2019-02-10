{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Bits.ZipBits where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- some random boolean operations

f :: Bool -> Bool -> Bool
f a b = a && not b

g :: Bool -> Bool -> Bool -- g a b = not ( a || b )
g a b = not a && ( not (f a b) `xor` b )

h :: Bool -> Bool -> Bool -- h a b = a
h a b = a || ( g a (f (g b a) (not a)) && a )

k :: Bool -> Bool -> Bool -- k a b = not ( a && b )
k a b = h ( g a (g b (not a && b)) ) (h (b `xor` a) b) || f (not b) (a && b || h b b)

------------------------------------------------
-- program

type Defs = '[ "a"    ':-> Input  '[] Word32
             , "b"    ':-> Input  '[] Word32
             , "out"  ':-> Output '[] (V 4 Word32)
             , "main" ':-> EntryPoint '[] Vertex
             ]

program :: Program Defs ()
program = Program $ entryPoint @"main" @Vertex do

    a <- get @"a"
    b <- get @"b"

    put @"out" $ Vec4 
                  ( zipBits2 f a b ) -- a .&. complement b
                  ( zipBits2 g a b ) -- complement ( a .|. b )
                  ( zipBits2 h a b ) -- a
                  ( zipBits2 k a b ) -- complement ( a .&. b )
