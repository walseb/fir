{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Test where

-- base
import Prelude hiding ( Monad(..), Applicative(..) -- for ix monad
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      )

-- tree-view
import Data.Tree.View(drawTree)

-- fir
import FIR.AST
import FIR.Builtin
import FIR.Instances
import FIR.Program
import Control.Monad.Indexed
import Data.Type.Bindings
import Math.Linear
import Math.Algebra.Class


------------------------------------------------
-- program


program ::
  Program
    '[ "model"       :-> Var R (M 4 4 Float)
     , "view"        :-> Var R (M 4 4 Float)
     , "projection"  :-> Var R (M 4 4 Float)
     , "position"    :-> Var R (V 3 Float)
     ]
    '[ "add11"       :-> Fun '[ "u" :-> Var R Float]
                              Float
     ]
    ()
program = do
  model            <- get @"model"
  view             <- get @"view"
  projection       <- get @"projection"
  ~(Vec3 px py pz) <- get @"position"

  let mvp        = projection !*! view !*! model
      position'  = vec4 px py pz 1

  add11 <- fundef @"add11" $ do
    u   <- get @"u"
    _11 <- def @"11" @R @Float 11 -- local variable
    pure $ u + _11

  entryPoint @Vertex $ do
    ~(Vec4 x y z _) <- def @"pos" ( mvp !*^ fmapAST add11 position' )
    put @"gl_Position" ( vec4 x y z 1 )

  
test :: IO()
test = drawTree . toTree . toAST $ program
