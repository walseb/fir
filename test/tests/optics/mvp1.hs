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

module Tests.Optics.MVP1 where

-- base
import Prelude hiding ( Functor(..), (<$>)
                      , Applicative(..), Monad(..)
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      , Eq(..), Ord(..)
                      , (&&)
                      )

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "ubo"     ':-> Global Uniform
                               ( Struct '[ "mvp" ':-> M 4 4 Float ] )
                              '[ Binding 0, DescriptorSet 0, Block ]
             , "in_pos"  ':-> Global Input  (V 4 Float) '[ Location 0 ]
             , "out_pos" ':-> Global Output (V 4 Float) '[ Location 0 ]
             ]

program :: Program Defs ()
program =
  Program $ entryPoint @"main" @Vertex do
    mvp    <- use @(Name "ubo" :.: Name "mvp")
    in_pos <- get @"in_pos"
    put @"out_pos" (mvp !*^ in_pos)
