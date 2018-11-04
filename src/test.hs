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

module Test where

-- base
--import Data.Int(Int32)
--import Data.Word(Word32)
import Prelude hiding ( Functor(..), (<$>)
                      , Applicative(..), Monad(..)
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      , Eq(..), Ord(..)
                      , (&&)
                      )
import qualified Prelude
import System.FilePath((</>),(<.>))

-- bytestring
import qualified Data.ByteString.Lazy as ByteString

-- tree-view
import Data.Tree.View(drawTree)

-- fir
import FIR.AST
import FIR.Builtin
import FIR.Codensity
import FIR.Instances
import FIR.Labels
import FIR.PrimTy
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

--type T a i = Codensity AST (AST a := i) i

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

  (f :: AST Float -> AST Float) <- fundef @"f" do
    u <- get @"u"
    t <- def @"t" @RW @Float 11 -- local variable
    pure (u + t)

  entryPoint @"main" @Vertex do

    model            <- get @"model"
    view             <- get @"view"
    projection       <- get @"projection"
    ~pos@(Vec3 px py pz) <- get @"position"

    {-
    #t @Float #= 11
    #s @Float #= 0.17

    if px > 0
    then #t .=   px
    else #t .= (-px)

    let (#<) = (<) @(S _ _) -- help type inference along...

    while ( #t #< abs (#s + 3) )
      do
        y <- 11 * #s - 7 * #t
        #t .= y + 1
        #s .= 2 * y

        while ( #t - #s #< 111 )
          do
            t <- #t
            s <- #s
            #t .= t + 1.5
            #s .= s - 2.2

            while ( #t - #s #< 111 )
              do
                t' <- #t
                s' <- #s
                #t .= t' - 2 * t
                #s .= s' + s + t
     -}
    

    #array @(Array 10 Float) #= (lit $ MkArray [1,17,23,4,5,90,88,17,22,21])

    
    let mvp        = fmapAST (*11) $ projection !*! view !*! model
        position'  = vec4 px py pz 1
        --func :: AST Float -> AST Float {--> AST Float-} -> AST Float
        --func x y {-z-} = (2 * x - abs y) {-/ z-}
        --func' :: AST Float -> AST (Float {--> Float-} -> Float)
        --func' = fromAST (toAST func)
        --applicativeTest :: AST (V 3 Float)
        --applicativeTest = func' <$$> pos <**> pos {-<**> pos-}

    ~(Vec4 x y z _) <- def @"pos" ( mvp !*^ (fmapAST ((*3) .f) position') )

    --def @"applicativeTest" @RW applicativeTest
    
    put @"gl_Position" ( vec4 x y z 10 )
    


cgContext :: CGContext
cgContext
  = CGContext
      { userGlobals = programGlobals program
      , debugMode   = True
      }

draw :: IO ()
draw = drawTree . toTree . toAST $ program

write :: String -> IO ()
write filename = case runCodeGen cgContext (toAST program) of
    Left  err -> print err
    Right bin -> -- can't use normal do notation with the current rebindable syntax
      let path = "shaders" </> filename <.> "spv"
      in ByteString.writeFile path bin
         Prelude.>> putStrLn ( "output written to " ++ path )