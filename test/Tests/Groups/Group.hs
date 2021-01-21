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

module Tests.Groups.GroupAdd where

-- fir
import FIR
import Math.Linear


------------------------------------------------
-- program

type Defs =
  '[    "img"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( RGBA8 UNorm )
        , "main" ':-> EntryPoint '[ LocalSize 16 8 1 ]
                    Compute
   ]

program :: Module Defs
program = Module $ entryPoint @"main" @Compute do
    
  ~(Vec3 i_x i_y _) <- get @"gl_GlobalInvocationID"
  gl_SubgroupInvocationID <- get @"gl_SubgroupInvocationID"

  someSubgroupOutput <- groupAdd @Subgroup @Reduce (Vec2 i_x i_y)

  imageWrite @"img"
      someSubgroupOutput
      (Vec4 0 0 0 0)

