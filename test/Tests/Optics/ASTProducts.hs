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

module Tests.Optics.ASTProducts where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs =
  '[ "ubo"  ':-> Uniform    '[Binding 0, DescriptorSet 0] ( Struct '[ "mvp" ':-> M 4 4 Float ] )
   , "out"  ':-> Output     '[Location 0] ( M 4 4 Float )
   , "main" ':-> EntryPoint '[] Vertex
   ]

program :: Module Defs
program = Module $ entryPoint @"main" @Vertex do

  row <- use @(Name "ubo" :.: Name "mvp" :.: Row 0)
  col <- use @(Name "ubo" :.: Name "mvp" :.: Col 2)
  dia <- use @(Name "ubo" :.: Name "mvp" :.: Diag)
  ( mid :: AST (M 2 2 Float) )
    <- use @(Name "ubo" :.: Name "mvp" :.: Prod (Entry 1 1 :*: Entry 1 2 :*: Entry 2 1 :*: Entry 2 2 :*: EndProd))
  ( flatMid :: AST (V 4 Float) )
    <- use @(Name "ubo" :.: Name "mvp" :.: Prod (Entry 1 1 :*: Entry 1 2 :*: Entry 2 1 :*: Entry 2 2 :*: EndProd))

  assign @(Name "out") identity
  assign @(Name "out" :.: Diag) dia
  assign @(Name "out" :.: Prod (Entry 1 1 :*: Entry 1 2 :*: Entry 2 1 :*: Entry 2 2 :*: EndProd)) mid
  assign @(Name "out" :.: Col 3) col
  assign @(Name "out" :.: Row 2) row
  assign @(Name "out" :.: Row 1) flatMid
