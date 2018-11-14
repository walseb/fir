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

module Tests.Optics.NoStructIndex where

-- base
import Prelude hiding ( Functor(..), (<$>)
                      , Applicative(..), Monad(..)
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      , Eq(..), Ord(..)
                      , (&&)
                      )
import qualified Prelude

-- vector
import qualified Data.Vector as Array

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type Uniforms
  = '[ "vertexData"':-> Var RW ( Struct [ "position" ':-> V 3 Float
                                        , "colour"   ':-> V 4 Float
                                        , "size"     ':-> Float
                                        , "weight"   ':-> Float
                                        ]
                              )
     ]

type Functions = '[]

program :: Program Uniforms Functions ()
program = do

  entryPoint @"main" @Vertex do

    position <- use @( Name "vertexData" :.: Index 5 )

    #gl_Position .= position