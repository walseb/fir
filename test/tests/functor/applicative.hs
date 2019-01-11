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

module Tests.Functor.Applicative where

-- base
import Prelude hiding ( Functor(..), (<$>)
                      , Applicative(..), Monad(..)
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      , Eq(..), Ord(..)
                      , (&&)
                      )

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "position" ':-> Global Input (V 3 Float) ]

program :: Program Defs ()
program = Program do

  entryPoint @"main" @Vertex do

    pos <- get @"position"

    let func :: AST Float -> AST Float {--> AST Float-} -> AST Float
        func x y {-z-} = (2 * x - abs y) {-/ z-}
        func' :: AST Float -> AST (Float {--> Float-} -> Float)
        func' = fromAST (toAST func)
        position' :: AST (V 3 Float)
        position' = func' <$$> pos <**> pos {-<**> pos-}

    ~(Vec3 x y z) <- def @"pos" @RW ( position' )

    #gl_Position .= vec4 x z y 10