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
   , "out"    ':-> Output '[Location 0 ] ( V 4 Float )
   , "main"   ':-> EntryPoint '[] Vertex
   ]

type TwoFloats
  = Struct
  '[ "1" ':-> Float
   , "2" ':-> Float
   ]

program :: Module Defs ()
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

  assign @(Name "out" :.: AnIndex Word32) 0 r
  assign @(Name "out" :.: Swizzle "yzw" :.: AnIndex) 1 f
