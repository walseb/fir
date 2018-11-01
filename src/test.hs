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

module Test where

-- base
--import Data.Int(Int32)
--import Data.Word(Word32)
import Prelude hiding ( Functor(..), (<$>)
                      , Applicative(..), Monad(..)
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      , Eq(..), Ord(..)
                      )
import qualified Prelude

-- bytestring
import qualified Data.ByteString.Lazy as ByteString

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
import Math.Logic.Class
import CodeGen.CodeGen
import CodeGen.State

------------------------------------------------
-- program

program ::
  Program
    '[ "model"       :-> Var R (M 4 4 Float)
     , "view"        :-> Var R (M 4 4 Float)
     , "projection"  :-> Var R (M 4 4 Float)
     , "position"    :-> Var R (V 3 Float)
     ]
    '[ "f" :-> Fun '[ "u" :-> Var R Float] Float
     ]
    ()
program = do

  f <- fundef @"f" do
    u <- get @"u"
    t <- def @"t" @RW @Float 11 -- local variable
    pure (u + t)

  entryPoint @"main" @Vertex do

    model            <- get @"model"
    view             <- get @"view"
    projection       <- get @"projection"
    ~(Vec3 px py pz) <- get @"position"

    def @"t" @RW @Float 11
    def @"s" @RW @Float 0.17
  
    if px > 0
    then put @"t" px
    else put @"t" (-px)

    -- while (t < abs s)
    while ( (<) <$> get @"t" <*> (abs <$> get @"s") )
      do
        t <- get @"t"
        s <- get @"s"
        put @"t" (t+1)
        put @"s" (s+2*t)

    t <- get @"t"
  
    let mvp        = projection !*! view !*! model
        position'  = vec4 px py pz 1

    ~(Vec4 x y z _) <- def @"pos" ( mvp !*^ position' )
    put @"gl_Position" ( vec4 x (f :$ y) (z + t) 1 )


cgContext :: CGContext
cgContext = CGContext { userGlobals = programGlobals program }

draw :: IO ()
draw = drawTree . toTree . toAST $ program

write :: String -> IO ()
write path = case runCodeGen cgContext (toAST program) of
    Left  err -> print err
    Right bin -> -- can't use normal do notation with the current rebindable syntax
      ByteString.writeFile path bin
      Prelude.>> putStrLn ( "output written to " ++ path )