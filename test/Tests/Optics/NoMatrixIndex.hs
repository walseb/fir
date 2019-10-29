{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Optics.NoMatrixIndex where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  = '[ "vertexData" ':-> Input '[ Location 0 ] ( M 3 4 Float )
     , "main"       ':-> EntryPoint '[] Vertex
     ]

program :: Module Defs ()
program = Module do

  entryPoint @"main" @Vertex do

    c <- use @( Name "vertexData" :.: Entry 1 4 )

    put @"gl_Position" (Vec4 c c c 1)
