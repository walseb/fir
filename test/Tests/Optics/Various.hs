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

module Tests.Optics.Various where

-- vector
import qualified Data.Vector as Array

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type Defs
  = '[ "ubo" ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
          ( Struct
            [ "modelMatrix"      ':-> M 4 4 Float
            , "viewMatrix"       ':-> M 4 4 Float
            , "projectionMatrix" ':-> M 4 4 Float
            ]
          )
     , "vertexData" ':->
          Uniform '[ Binding 1, DescriptorSet 0 ]
            ( Struct
              [ "position" ':-> V 3 Float
              , "colour"   ':-> V 4 Float
              , "size"     ':-> Float
              , "weight"   ':-> Float
              ]
            )
     , "arr1" ':->
          Uniform '[ Binding 2, DescriptorSet 0 ]
            ( Array 12
                  ( Struct
                      [ "label1" ':-> V 3 Float
                      , "label2" ':-> Array 3 Float
                      ]
                  )
            )
     , "arr2" ':->
          Uniform '[ Binding 3, DescriptorSet 0 ]
            ( Struct
              [ "x1" ':-> V 4 Float
              , "x2" ':-> Float
              , "rt" ':-> RuntimeArray Float
              ]
            )
     , "main" ':-> EntryPoint '[] Vertex
     , "out_col" ':-> Output '[Location 0] ( V 4 Float )
     ]

program :: Module Defs ()
program = Module do

  entryPoint @"main" @Vertex do

    modelMatrix       <- use @(Name "ubo" :.: Name "modelMatrix"      )
    viewMatrix        <- use @(Name "ubo" :.: Name "viewMatrix"       )
    projectionMatrix  <- use @(Name "ubo" :.: Name "projectionMatrix" )
    let mvp = modelMatrix !*! viewMatrix !*! projectionMatrix

    vertexData <- use @( Name "vertexData" )
    let vertexDataColour = view @(Name "colour") vertexData
    row <- use @( Name "ubo" :.: Name "modelMatrix" :.: Row 2 )
    diagonal <- use @(Name "ubo" :.: Name "modelMatrix" :.: Diag )
    lensTest <- use @(Name "arr1" :.: AnIndex Word32 :.: Index 0 :.: Index 2) 7
    rtTest1 <- use @(Name "arr2" :.: Name "rt" :.: AnIndex Word32) 6
    rtTest2 <- use @(Name "arr2" :.: Name "rt" :.: Index 6)

    #array @(Array 10 Float) #= (Lit $ mkArray (Array.fromList [1,17,23,4,5,90,88,17,22,21]))
    #gl_Position .=  (rtTest1 * rtTest2 + abs lensTest) *^ ( mvp !*^ diagonal )
    #out_col .= vertexDataColour ^+^ row
