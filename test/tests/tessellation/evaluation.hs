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

module Tests.Tessellation.Evaluation where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- program

type TessellationEvaluationDefs =
  '[ "in_cols" ':-> Input      '[ Location 0 ] (Array 3 (V 4 Float))
   , "out_col" ':-> Output     '[ Location 0 ] (V 4 Float)
   , "ubo"          ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp"    ':-> M 4 4 Float
                                    , "origin" ':-> V 4 Float
                                    ]
                          )
   , "main"    ':-> EntryPoint '[ Triangles ] TessellationEvaluation
   ]

program :: Program TessellationEvaluationDefs ()
program = Program $ entryPoint @"main" @TessellationEvaluation do
  ~(Vec3 u v w) <- get @"gl_TessCoord"
  
  in_cols <- get @"in_cols"
  put @"out_col" (     u *^ (view @(Index 0) in_cols)
                   ^+^ v *^ (view @(Index 1) in_cols)
                   ^+^ w *^ (view @(Index 2) in_cols)
                 )

  pos0 <- use @(Name "gl_PerVertex" :.: Index 0 :.: Name "gl_Position")
  pos1 <- use @(Name "gl_PerVertex" :.: Index 1 :.: Name "gl_Position")
  pos2 <- use @(Name "gl_PerVertex" :.: Index 2 :.: Name "gl_Position")
  orig <- use @(Name "ubo" :.: Name "origin")
  let t = 0.5 - (2*u - 1)**2 * (2*v - 1)**2 * (2*w - 1)**2
  put @"gl_Position"
    ( t *^ orig ^+^ (1-t) *^ ( u *^ pos0 ^+^ v *^ pos1 ^+^ w *^ pos2 ) )
