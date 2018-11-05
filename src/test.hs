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
import FIR.Binding
import FIR.Builtin
import FIR.Instances.AST
import FIR.Instances.Codensity
import FIR.Instances.Optic
import FIR.Labels
import FIR.PrimTy
import FIR.Program
import Control.Monad.Indexed
import Control.Type.Optic
import Data.Type.Map
import Math.Linear
import Math.Algebra.Class
import Math.Logic.Class
import CodeGen.CodeGen
import CodeGen.State

------------------------------------------------
-- program

--type T a i = Codensity AST (AST a := i) i

type Start
  = '[ "model"      ':-> Var R  ( M 4 4 Float )
     , "view"       ':-> Var R  ( M 4 4 Float )
     , "projection" ':-> Var R  ( M 4 4 Float )
     , "vertexData" ':-> Var RW ( Struct [ "position" ':-> V 3 Float
                                         , "colour"   ':-> V 4 Float
                                         , "size"     ':-> Float
                                         , "weight"   ':-> Float
                                         ]
                               )
     , "arr1" ':-> Var R ( RuntimeArray (RuntimeArray ( Struct [ "label1" ':-> V 3 Float
                                                               , "label2" ':-> RuntimeArray Float
                                                               ]
                                                     )
                                        )
                        )
          ]


program ::
  Program
    Start
    '[ "f" ':-> Fun '[ "u" ':-> Var R Float] Float
     ]
    ()
program = do

  (f :: AST Float -> AST Float) <- fundef @"f" do
    u <- get @(Name "u")
    t <- def @"t" @RW @Float 11 -- local variable
    pure (u + t)

  entryPoint @"main" @Vertex do

    model            <- get @(Name "model")
    view             <- get @(Name "view")
    projection       <- get @(Name "projection")

    vertexDataPosition <- get @(Name "vertexData" :.: Name "position")
    lensTest <- get @(Name "arr1" :.: Index 3 :.: AnIndex :.: Index 0 :.: Index 2) 7

    

    {-

    #array @(Array 10 Float) #= (lit $ MkArray [1,17,23,4,5,90,88,17,22,21])

    let mvp        = fmapAST (*11) $ projection !*! view !*! model
        position'  = vec4 px py pz 1
        func :: AST Float -> AST Float {--> AST Float-} -> AST Float
        func x y {-z-} = (2 * x - abs y) {-/ z-}
        func' :: AST Float -> AST (Float {--> Float-} -> Float)
        func' = fromAST (toAST func)
        applicativeTest :: AST (V 3 Float)
        applicativeTest = func' <$$> pos <**> pos {-<**> pos-}

    def @"applicativeTest" @RW applicativeTest

    ~(Vec4 x y z _) <- def @"pos" ( mvp !*^ (fmapAST ((*3) .f) position') )

    -}
    put @(Name "gl_Position") ( vec4 3 7 17 lensTest )
    --put @"gl_Position" ( vec4 3 7 17 10 )
    


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