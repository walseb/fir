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

module Tests.Optics.NoStructField where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs
  = '[ "vertexData" ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
          ( Struct [ "position" ':-> V 3 Float
                   , "colour"   ':-> V 4 Float
                   , "size"     ':-> Float
                   , "weight"   ':-> Float
                   ]
          )
     , "main" ':-> EntryPoint '[] Vertex
     ]

program :: Module Defs ()
program = Module do

  entryPoint @"main" @Vertex do

    position <- use @( Name "vertexData" :.: Name "inexistent" )

    put @"gl_Position" position