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

module Tests.Images.Gather where

-- fir
import FIR
import Math.Linear

-- vector
import qualified Data.Vector as Array

------------------------------------------------
-- program

type Defs
  =  '[ "image"   ':-> Texture2D  '[ Binding 0, DescriptorSet 0 ] (RGBA8 UNorm)
      , "in_pos"  ':-> Input      '[ Location 0 ] (V 2 Float)
      , "out_col" ':-> Output     '[ Location 0 ] (V 4 Float)
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]

program :: Module Defs ()
program =
  Module $ entryPoint @"main" @Fragment do
    pos   <- get @"in_pos"
    let offsetArray :: Array 4 (V 2 Int32)
        offsetArray = mkArray (Array.fromList [V2 0 0, V2 0 1, V2 1 0, V2 1 1])
    col <- use @(ImageTexel "image") (Gather (ComponentWithOffsets 0 offsetArray) NilOps) pos
    put @"out_col" col
