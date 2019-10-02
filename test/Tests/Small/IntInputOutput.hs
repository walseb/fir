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

module Tests.Small.IntInputOutput where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  =  '[ "in_a"  ':-> Input      '[ Location 0 ] (V 4 Int8  )
      , "in_b"  ':-> Input      '[ Location 1 ] (V 4 Word8 )
      , "in_c"  ':-> Input      '[ Location 2 ] (V 4 Int16 )
      , "in_d"  ':-> Input      '[ Location 3 ] (V 4 Word16)
      , "out_a" ':-> Output     '[ Location 0 ] (V 4 Int8  )
      , "out_b" ':-> Output     '[ Location 1 ] (V 4 Word8 )
      , "out_c" ':-> Output     '[ Location 2 ] (V 4 Int16 )
      , "out_d" ':-> Output     '[ Location 3 ] (V 4 Word16)
      , "main"  ':-> EntryPoint '[ ]            Vertex
      ]

program :: Program Defs ()
program =
  Program $ entryPoint @"main" @Vertex do
    put @"out_a" =<< get @"in_a"
    put @"out_b" =<< get @"in_b"
    put @"out_c" =<< get @"in_c"
    put @"out_d" =<< get @"in_d"
