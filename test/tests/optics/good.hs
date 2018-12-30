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

module Tests.Optics.Good where

-- base
import Data.Word(Word32)
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

type InOut
  = '[ "modelMatrix"      ':-> Var RW ( M 4 4 Float )
     , "viewMatrix"       ':-> Var R  ( M 4 4 Float )
     , "projectionMatrix" ':-> Var R  ( M 4 4 Float )
     , "vertexData"       ':-> Var RW ( Struct [ "position" ':-> V 3 Float
                                               , "colour"   ':-> V 4 Float
                                               , "size"     ':-> Float
                                               , "weight"   ':-> Float
                                               ]
                                     )
     , "arr1" ':-> Var R ( RuntimeArray (RuntimeArray ( Struct [ "label1" ':-> V 3 Float
                                                               , "label2" ':-> RuntimeArray Float
                                                               ]
                                                      )
                                        )
                         )
     , "arr2" ':-> Var RW ( Array 17 Float )     
     ]

type Functions = '[]

program :: Program InOut Functions ()
program = do

  entryPoint @"main" @Vertex do

    modelMatrix       <- get @"modelMatrix"
    viewMatrix        <- get @"viewMatrix"
    projectionMatrix  <- get @"projectionMatrix"

    vertexDataPosition <- use @( Name "vertexData" :.: Name "position" )
    vertexData <- use @(Name "vertexData")
    --let vertexDataColour = view @(Name "colour") vertexData
    row <- use @( Name "modelMatrix" :.: Row 2 )
    diagonal <- use @(Name "modelMatrix" :.: Diag )
    lensTest <- use @(Name "arr1" :.: Index 3 :.: AnIndex Word32 :.: Index 0 :.: Index 2) 7

    #array @(Array 10 Float) #= (lit $ mkArray (Array.fromList [1,17,23,4,5,90,88,17,22,21]))
    #gl_Position .= vec4 3 7 17 10