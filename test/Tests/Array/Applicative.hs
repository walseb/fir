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

module Tests.Array.Applicative where

-- fir
import FIR

------------------------------------------------
-- program

type Defs = '[ "arr"  ':-> Input      '[ Location 0 ] (Array 17 Float)
             , "out"  ':-> Output     '[ Location 0 ] Float
             , "main" ':-> EntryPoint '[ OriginUpperLeft ] Fragment
             ]

program :: Module Defs ()
program = Module do

  entryPoint @"main" @Fragment do

    arr <- get @"arr"

    let f :: AST Float -> AST Float -> AST Float
        f x y = x - abs y
        arr' :: AST (Array 17 Float)
        arr' = f <$$> arr <**> arr
        out = view @(Index 0) arr' + view @(Index 6) arr

    put @"out" out
