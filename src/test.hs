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
import Data.Int(Int32)
import Prelude hiding ( Monad(..), Applicative(..) -- for ix monad
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      , Eq(..), Ord(..) -- custom logic for AST
                      )
import qualified Prelude
import Data.Proxy

-- binary
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Builder as ByteString

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

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
import CodeGen.Instruction
import CodeGen.Monad
import CodeGen.State
import qualified SPIRV.PrimTy as SPIRV

------------------------------------------------
-- program

type I = '[ "add11"      :-> Fun '["u" :-> Var R Float] Float
          , "model"      :-> Var R (M 4 4 Float)
          , "position"   :-> Var R (V 3 Float)
          , "projection" :-> Var R (M 4 4 Float)
          , "test"       :-> Var W (V 3 Float)
          , "view"       :-> Var R (M 4 4 Float)
          ]

program ::
  Program
    '[ "model"       :-> Var R (M 4 4 Float)
     , "view"        :-> Var R (M 4 4 Float)
     , "projection"  :-> Var R (M 4 4 Float)
     , "position"    :-> Var R (V 3 Float)
     ]
    '[ "f" :-> Fun '[ "u" :-> Var R Float] Float
     , "t" :-> Var RW Float
     ]
    ()
program = do
  model            <- get @"model"
  view             <- get @"view"
  projection       <- get @"projection"
  ~(Vec3 px py pz) <- get @"position"

  let mvp        = projection !*! view !*! model
      position'  = vec4 px py pz 1

  f <- fundef @"f" do
    u <- get @"u"
    t <- def @"t" @RW @Float 11 -- local variable
    pure (u + t)

  def @"t" 11

  if px > 0
  then put @"t" px
  else put @"t" (-px)

  while ( do t <- get @"t"
             pure (t < 10)
        )
    ( do t <- get @"t"
         put @"t" (t+1)
    )

  t <- get @"t"
  
  

  entryPoint @"main" @Vertex do
    ~(Vec4 x y z _) <- def @"pos" ( mvp !*^ position' )
    put @"gl_Position" ( vec4 x (f :$ y) (z + t) 1 )


cgContext :: CGContext
cgContext = CGContext { userGlobals = programGlobals program }

draw :: IO ()
draw = drawTree . toTree . toAST $ program

gen :: Either Text ((ID, SPIRV.PrimTy), CGState, ByteString)
gen = runCGMonad cgContext . codeGen . toAST $ program

write :: String -> IO ()
write path = case runCodeGen cgContext (toAST program) of
    Left err  -> print err
    Right bin -> -- can't use normal do notation with the current rebindable syntax
      ByteString.writeFile path bin
      Prelude.>> putStrLn ( "output written to " ++ path )

showPutBin :: Binary.Binary a => a -> Strict.ByteString
showPutBin = ByteString.toStrict . ByteString.toLazyByteString . ByteString.lazyByteStringHex . Binary.runPut . Binary.put