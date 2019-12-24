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

module Tests.Optics.ASTIndices where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs =
  '[ "nested" ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
        ( Array 4 ( Struct '[ "r" ':-> Float, "ijk" ':-> V 3 Float ] ) )
   , "out1"    ':-> Output '[Location 0] ( V 4 Float )
   , "out2"    ':-> Output '[Location 1] ( M 3 2 Float )
   , "main"   ':-> EntryPoint '[] Vertex
   ]

type TwoFloats
  = Struct
  '[ "1" ':-> Float
   , "2" ':-> Float
   ]

program :: Module Defs
program = Module $ entryPoint @"main" @Vertex do

  r <- use @(Name "nested" :.: AnIndex Word32 :.: Name "r") 3

  twoFloats
    <- use @(   Name "nested"
            :.: Prod @_ @_ @_ @TwoFloats
                 (   ( AnIndex Word32 :.: Name "ijk" :.: AnIndex Word32 )
                 :*: ( AnIndex Word32 :.: Name "ijk" :.: AnIndex Word32 )
                 :*: EndProd
                 )
            )
            ( ConsHList :$ 1 :$ ( ConsHList :$ 2 :$ NilHList ) )
            ( ConsHList :$ 4 :$ ( ConsHList :$ 3 :$ NilHList ) )
  let f = view @(Index 1) twoFloats

  assign @(Name "out1" :.: AnIndex Word32) 0 (r + f)

  let
    mat :: Code (M 3 4 Float)
    mat = Mat34
      0 1  2  3
      4 5  6  7
      8 9 10 11

    mat' :: Code (M 3 2 Float)
    mat' = view @( Prod ( Col 1 :*: Col 3 :*: EndProd ) ) mat

  assign @(Name "out2") mat'

  assign @(Name "out2" :.: Rows) (Vec2 1 3)
