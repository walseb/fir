{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Tests.Layout.Nested where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- layout of nested uniform buffer

type Nested =
  Struct
    '[ "a" ':-> V 4 Float
     , "b" ':-> Word32
     , "c" ':-> Struct
         '[ "c1" ':-> V 4 Float
          , "c2" ':-> V 4 Double
          ]
     , "d" ':-> V 2 Int32
     ]

type Defs =
  '[ "uniform" ':-> Uniform    '[ DescriptorSet 0, Binding 0 ] Nested
   , "out"     ':-> Output     '[ Location 0      ] ( V 4 Float )
   , "vert"    ':-> EntryPoint '[ ] Vertex
   ]

program :: Module Defs
program = Module do
  entryPoint @"vert" @Vertex do
    put @"out" =<< use @( Name "uniform" :.: Name "c" :.: Name "c1" )
