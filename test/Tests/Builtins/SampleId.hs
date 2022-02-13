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

module Tests.Builtin.SampleId where

-- fir
import FIR

------------------------------------------------
-- program

type Defs = '[ "out"  ':-> Output     '[ Location 0 ] Float
             , "main" ':-> EntryPoint '[ OriginUpperLeft ] Fragment
             ]

program :: Module Defs
program = Module do

  entryPoint @"main" @Fragment do

    sampleNo <- get @"gl_SampleID"

    put @"out" (fromIntegral sampleNo)
