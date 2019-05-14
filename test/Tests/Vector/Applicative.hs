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

module Tests.Vector.Applicative where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "in_col1" ':-> Input      '[] (V 3 Float)
             , "in_col2" ':-> Input      '[] (V 3 Float)
             , "in_col3" ':-> Input      '[] (V 3 Float)
             , "out_col" ':-> Output     '[] (V 3 Float)
             , "main"    ':-> EntryPoint '[OriginUpperLeft] Fragment
             ]

program :: Program Defs ()
program = Program do

  entryPoint @"main" @Fragment do

    in_col1 <- get @"in_col1"
    in_col2 <- get @"in_col2"
    in_col3 <- get @"in_col3"

    let func :: AST Float -> AST Float -> AST Float -> AST Float
        func x y z = 2 * z - y + abs x
        out_col :: AST (V 3 Float)
        out_col = func <$$> in_col1 <**> in_col2 <**> in_col3

    put @"out_col" out_col
