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

-- fir
import FIR
import AST
import Indexed
import Bindings
import Linear
import TypeClasses.Algebra

-- tree-view
import Data.Tree.View(drawTree)

------------------------------------------------
-- rebindable syntax

(>>=) :: MonadIx m => m (a := j) i -> (a -> m q j) -> m q i
c >>= f = extendIx (\(WithIx a) -> f a) c  

(>>) :: MonadIx m => m (a := j) i -> m q j -> m q i
ma >> mb = ma >>= const mb

return, pure :: (MonadIx m, m ~ Codensity S) => a -> m (a := i) i
return = returnIx . WithIx
pure   = returnIx . WithIx

class MonadIx m => MonadIxFail m where
  fail :: String -> m (a := j) i
instance MonadIxFail (Codensity S) where
  fail = error "fail"

------------------------------------------------
-- program


type Program i j a
  = Codensity S (AST a := Union (FromList i) (FromList j)) (FromList i)

type R  = '[ 'Read  ]
type W  = '[ 'Write ]
type RW = '[ 'Read, 'Write ]

program :: 
  Program
    '[ "model"       ':-> 'Var R (M 4 4 Float)
     , "view"        ':-> 'Var R (M 4 4 Float)
     , "projection"  ':-> 'Var R (M 4 4 Float)
     , "position"    ':-> 'Var R (V 3 Float)
     , "gl_Position" ':-> 'Var W (V 3 Float)
     ]    
    '[ "main"        ':-> 'Fun '[] ()
     ]
    ()
program = do
  model         <- get @"model"
  view          <- get @"view"
  projection    <- get @"projection"
  Vec3 px py pz <- get @"position"

  let mvp        = projection !*! view !*! model
      position'  = vec4 px py pz 1

  fundef @"main" $ do
    def @"pos" @R ( mvp !*^ position' )
    Vec4 x y z _ <- get @"pos"
    put @"gl_Position" ( vec3 x y z )

  
test :: IO()
test = drawTree . toTree . toAST $ program
