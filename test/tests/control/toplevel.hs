{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Control.TopLevel where

-- base
import Prelude ( Float )

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type InOut = '[ ]

type TopLevel
  = '[ "notallowed" ':-> Var RW Float
     , "alsono"     ':-> Var R (V 3 Float)
     ]

program :: Program InOut TopLevel ()
program = do

  no <- def @"notallowed"   17
  _  <- def @"alsono"     ( vec3 no no 77 )

  entryPoint @"main" @Vertex do

    put @"gl_Position" ( vec4 0 0 0 1 )
