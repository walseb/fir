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

module Tests.Optics.NoStructField where

-- vector
import qualified Data.Vector as Array

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs
  = '[ "vertexData" ':-> Input '[]
          ( Struct [ "position" ':-> V 3 Float
                   , "colour"   ':-> V 4 Float
                   , "size"     ':-> Float
                   , "weight"   ':-> Float
                   ]
          )
     , "main" ':-> EntryPoint '[] Vertex
     ]

program :: Program Defs ()
program = Program do

  entryPoint @"main" @Vertex do

    position <- use @( Name "vertexData" :.: Name "inexistent" )

    #gl_Position .= position