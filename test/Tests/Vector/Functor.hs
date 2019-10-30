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

module Tests.Vector.Functor where

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs
  = '[ "ubo"      ':-> Uniform  '[ Binding 0, DescriptorSet 0 ] ( Struct '[ "mvp" ':-> M 4 4 Float ] )
     , "position" ':-> Input    '[ Location 0 ] ( V 4 Float )
     , "f"        ':-> Function '[ "u" ':-> Var R Float ] Float
     , "main"     ':-> EntryPoint '[] Vertex
     ]


program :: Module Defs
program = Module do

  (f :: AST Float -> AST Float) <- fundef @"f" do
    u <- use @(Name "u")
    t <- def @"t" @RW ( 11 :: AST Float ) -- local variable
    pure (u + t)

  entryPoint @"main" @Vertex do

    mvp      <- use @(Name "ubo" :.: Name "mvp")
    position <- get @"position"

    ~(Vec4 x y z _) <- def @"pos" @R ( mvp !*^ (fmapAST (f . (*3) . f) position) )

    #gl_Position .= Vec4 x z y 1
