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

module Tests.Images.ReadWrite where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  =  '[ "image1"  ':-> Image2D '[ Binding 0, DescriptorSet 0 ] (RG32 UI)
      , "image2"  ':-> Image2D '[ Binding 0, DescriptorSet 1 ] (R32 F)
      , "out_col" ':-> Output     '[ Location 0 ] (V 4 Float)
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]

program :: Module Defs
program = Module $ entryPoint @"main" @Fragment do
  ( index2D :: Code ( V 2 Word32 ) ) <-
    let' =<< imageRead @"image1" ( Vec2 ( 0 :: Word32 ) ( 1 :: Word32 ) )
  ( float :: Code Float ) <-
    let' =<< imageRead @"image2" index2D
  imageWrite @"image2" index2D ( 2 * float )
  put @"out_col" ( Vec4 float float float float )
