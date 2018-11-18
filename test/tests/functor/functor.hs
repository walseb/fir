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

-- base
import Prelude hiding ( Functor(..), (<$>)
                      , Applicative(..), Monad(..)
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      , Eq(..), Ord(..)
                      , (&&)
                      )
import qualified Prelude

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type Uniforms
  = '[ "modelMatrix"      ':-> Var R ( M 4 4 Float )
     , "viewMatrix"       ':-> Var R ( M 4 4 Float )
     , "projectionMatrix" ':-> Var R ( M 4 4 Float )
     , "position"         ':-> Var R ( V 4 Float )  
     ]

type Functions
  = '[ "f" ':-> Fun '[ "u" ':-> Var R Float] Float
     ]

program :: Program Uniforms Functions ()
program = do

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

    #gl_Position .= vec4 x z y 1
