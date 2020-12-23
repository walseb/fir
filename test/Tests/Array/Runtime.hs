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

module Tests.Array.Runtime where

-- fir
import FIR

------------------------------------------------
-- program

type Defs = '[ "storage" ':-> StorageBuffer '[ DescriptorSet 0, Binding 0 ]
                                ( Struct '[ "buffer" ':-> RuntimeArray Float ] )
             , "out"  ':-> Output     '[ Location 0 ] Float
             , "main" ':-> EntryPoint '[ OriginUpperLeft ] Fragment
             ]

program :: Module Defs
program = Module do

  entryPoint @"main" @Fragment do

    -- Not allowed to retrieve a runtime array in this manner.
    buffer <- use @( Name "storage" :.: Name "buffer" )

    let
      res :: Code Float
      res = view @( AnIndex ( Code Word32 ) ) 1728 buffer
    
    put @"out" res
