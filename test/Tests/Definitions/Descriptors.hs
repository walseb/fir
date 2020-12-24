{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedWildCards   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Definitions.Descriptors where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type Defs = '[ "desc1" ':-> Uniform    '[ DescriptorSet 1, Binding 2 ]
                              ( Struct '[ "v" ':-> V 2 Word32 ] )
             , "desc2" ':-> Image2D    '[ DescriptorSet 1, Binding 2 ] ( RGBA8 UNorm )
             , "desc3" ':-> StorageBuffer '[ DescriptorSet 1, Binding 2 ]
                              ( Struct '[ "rt" ':-> RuntimeArray ( V 4 Float ) ] )
             , "main"  ':-> EntryPoint '[] Vertex
             ]

program :: Module Defs
program = Module do

  entryPoint @"main" @Vertex do

    xy <- use @( Name "desc1" :.: Name "v" )
    abcd <- imageRead @"desc2" xy
    assign @( Name "desc3" :.: Name "rt" :.: AnIndex Word32 ) 17 abcd
