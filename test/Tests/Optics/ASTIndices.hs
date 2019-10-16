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
  '[ "nested" ':-> Uniform'[ Binding 0, DescriptorSet 0 ]
        ( Array 4 ( Array 6 ( Struct '[ "r" ':-> Float, "ijk" ':-> V 3 Float ] ) ) )
   , "out"    ':-> Output '[Location 0 ] ( V 4 Float )
   , "main"   ':-> EntryPoint '[] Vertex
   ]

type TwoVecs
  = Struct
  '[ "1" ':-> V 3 Float
   , "2" ':-> V 3 Float
   ]

program :: Module Defs ()
program = Module $ entryPoint @"main" @Vertex do

  r <- use @(Name "nested" :.: AnIndex Word32 :.: AnIndex Word32 :.: Name "r") 1 4

  twoVecs
    <- use @(   Name "nested"
            :.: Prod @_ @_ @_ @TwoVecs
                 (   ( AnIndex Word32 :.: AnIndex Word32 :.: Name "ijk" )
                 :*: ( AnIndex Word32 :.: AnIndex Word32 :.: Name "ijk" )
                 :*: EndProd
                 )
            )
            ( ConsHList :$ 1 :$ ( ConsHList :$ 2 :$ NilHList ) )
            ( ConsHList :$ 4 :$ ( ConsHList :$ 3 :$ NilHList ) )
  let v = view @(Index 1) twoVecs

  assign @(Name "out" :.: AnIndex Word32) 0 r
  assign @(Name "out" :.: Swizzle "yzw" ) v
