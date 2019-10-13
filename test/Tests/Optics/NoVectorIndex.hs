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

module Tests.Optics.NoVectorIndex where

-- vector
import qualified Data.Vector as Array

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs
  = '[ "vertexData" ':-> Input '[Location 0] ( V 4 Float )
     , "main"       ':-> EntryPoint '[] Vertex
     ]

program :: Module Defs ()
program = Module do

  entryPoint @"main" @Vertex do

    position <- use @( Name "vertexData" :.: Index 4 )

    #gl_Position .= position