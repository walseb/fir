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

module Tests.Optics.Overlapping where

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type MyStruct = Struct '[ "field_0" ':-> Float, "field_1" ':-> Bool ]
type OtherStruct = Struct '[ "x" ':-> Float, "y" ':-> Float ]

program :: Module '[ "main" ':-> EntryPoint '[] Vertex ] ()
program = Module do

  entryPoint @"main" @Vertex do

    def @"struct" @RW @MyStruct
      ( Lit ( 3 :& True :& End ) )

    assign
      @(    Name "struct"
        :.: ( Prod (Name "field_0" :*: Index 0 :*: EndProd)
               :: Optic '[]
                    MyStruct
                    OtherStruct
            )
       )
      ( Lit ( 4 :& 5 :& End ) )
