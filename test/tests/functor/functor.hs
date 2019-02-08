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

module Tests.Functor.Functor where

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs
  = '[ "modelMatrix"      ':-> Uniform  '[] ( M 4 4 Float )
     , "viewMatrix"       ':-> Uniform  '[] ( M 4 4 Float )
     , "projectionMatrix" ':-> Uniform  '[] ( M 4 4 Float )
     , "position"         ':-> Input    '[] ( V   4 Float )
     , "f"                ':-> Function '["u" ':-> Var R Float] Float
     , "main"             ':-> EntryPoint '[] Vertex
     ]


program :: Program Defs ()
program = Program do

  (f :: AST Float -> AST Float) <- fundef @"f" do
    u <- use @(Name "u")
    t <- def @"t" @RW @Float 11 -- local variable
    pure (u + t)

  entryPoint @"main" @Vertex do

    modelMatrix       <- get @"modelMatrix"
    viewMatrix        <- get @"viewMatrix"
    projectionMatrix  <- get @"projectionMatrix"
    position          <- get @"position"

    let mvp = projectionMatrix !*! viewMatrix !*! modelMatrix

    ~(Vec4 x y z _) <- def @"pos" @R ( mvp !*^ (fmapAST (f . (*3) . f) position) )

    #gl_Position .= Vec4 x z y 1
