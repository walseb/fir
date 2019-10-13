{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Tests.Vector.Swizzle where

-- fir
import FIR
import Math.Linear

---------------------------------------------------------------------------------

type Defs = '[ "in_col"  ':-> Input      '[Location 0] (V 4 Float)
             , "out_col" ':-> Output     '[Location 0] (V 3 Float)
             , "main"    ':-> EntryPoint '[OriginUpperLeft] Fragment
             ]

blue1 :: V 4 Float
blue1 = V4 0.2 0.8 1 1

darkGrey :: V 3 Float
darkGrey = view @(Swizzle "rrr") blue1

blue2 :: V 3 Float
blue2 = view @(Swizzle "rgb") blue1


program :: Module Defs ()
program = Module $ entryPoint @"main" @Fragment do

    col1 <- use @(Name "in_col" :.: Swizzle "rgb")
    col2 <- use @(Name "in_col")
    let col3 = view @(Swizzle "bgr") col2
        col = 0.25 *^ ( col1 ^+^ col3 ^+^ Lit darkGrey ^+^ Lit blue2 )

    put @"out_col" col
