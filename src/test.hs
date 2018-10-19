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
import FIR.Instances
import Control.Monad.Indexed
import Data.Type.Bindings
import Math.Linear
import Math.Algebra.Class


------------------------------------------------
-- program


type Program i j a
  = Codensity S (AST a := Union (FromList i) (FromList j)) (FromList i)

program ::
  Program
    '[ "model"       :-> Var R (M 4 4 Float)
     , "view"        :-> Var R (M 4 4 Float)
     , "projection"  :-> Var R (M 4 4 Float)
     , "position"    :-> Var R (V 3 Float)
     , "gl_Position" :-> Var W (V 3 Float)
     ]    
    '[ "add11"       :-> Fun '[ "u" :-> Var R Float]
                              Float
     , "main"        :-> Fun '[] ()
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

  fundef @"main" $ do
    ~(Vec4 x y z _) <- def @"pos" ( mvp !*^ fmapAST add11 position' )
    put @"gl_Position" ( vec3 x y z )

  
test :: IO()
test = drawTree . toTree . toAST $ program
