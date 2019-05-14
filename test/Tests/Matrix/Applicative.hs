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

module Tests.Matrix.Applicative where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "mvp1"    ':-> Input      '[] (M 3 3 Float)
             , "mvp2"    ':-> Input      '[] (M 3 3 Float)
             , "in_pos"  ':-> Input      '[] (V 3 Float)
             , "out_pos" ':-> Output     '[] (V 3 Float)
             , "main"    ':-> EntryPoint '[] Vertex
             ]

program :: Program Defs ()
program = Program do

  entryPoint @"main" @Vertex do

    mvp1   <- get @"mvp1"
    mvp2   <- get @"mvp2"
    in_pos <- get @"in_pos"

    let adjustment :: AST Float -> AST Float -> AST Float
        adjustment x y = 0.5 * abs (x - y)
        adjusted_mvp :: AST (M 3 3 Float)
        adjusted_mvp = adjustment <$$> mvp1 <**> mvp2
    

    put @"out_pos" (adjusted_mvp !*^ in_pos)
