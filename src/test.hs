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
                      )
import qualified Prelude
import Data.Proxy

-- binary
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy(ByteString)
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
import CodeGen.CodeGen
import CodeGen.Instruction
import CodeGen.Monad
import CodeGen.State


------------------------------------------------
-- program

{-
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

  add11 <- fundef @"add11" do
    u   <- get @"u"
    _11 <- def @"11" @R 11 -- local variable
    pure $ u + _11

  let _14 :: AST Int32
      _14 = convert ( add11 :$ 3 )

  entryPoint @"main" @Vertex do
    ~(Vec4 x y z _) <- def @"pos" ( mvp !*^ fmapAST add11 position' )
    put @"gl_Position" ( vec4 x y z (convert _14) )
-}

program :: Program '[ "t" :-> Var R Float ] '[] ()
program = do
  t <- get @"t"
  entryPoint @"main" @Vertex do
    vertexId <- get @"gl_VertexId"
    let size = t * convert vertexId
    put @"gl_PointSize" size

cgContext :: CGContext
cgContext = CGContext { userGlobals = programGlobals program }

draw :: IO ()
draw = drawTree . toTree . toAST $ program

gen :: Either Text (ID, CGState, ByteString)
gen = runCGMonad cgContext . codeGen . toAST $ program

write :: IO ()
write = case runCodeGen cgContext (toAST program) of
    Left err  -> print err
    Right bin -> -- can't use normal do notation with the current rebindable syntax
      ByteString.writeFile "program.spv" bin
      Prelude.>> putStrLn "output written to program.spv"

showPutBin :: Binary.Binary a => a -> ByteString.ByteString
showPutBin = ByteString.toLazyByteString . ByteString.lazyByteStringHex . Binary.runPut . Binary.put