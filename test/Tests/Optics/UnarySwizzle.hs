{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}

module UnarySwizzle where

import FIR
import Control.Type.Optic (Optic(..), ProductComponents(..))
import Math.Linear

type ComputeDefs =
  '[ "positions" ':-> StorageBuffer [DescriptorSet 1, Binding 0      ]
                      (Struct '["posArray" ':-> RuntimeArray (Struct '[ "pos" ':-> V 2 Float ])])
   , "main"      ':-> EntryPoint   '[LocalSize 256 1 1] Compute
   ]

program :: Module ComputeDefs
program = Module $ entryPoint @"main" @Compute do
  positions <- use @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) 0
  pos' <- let' $ over @(Name "pos" :.: O) (+ 0.01) positions
  assign @(Name "positions" :.: Name "posArray" :.: AnIndex Word32) 0 positions

type O =
    ( Prod_ ( ProductO
                (Index 0 :: Optic '[] (Code (V 2 Float)) (Code Float))
                (EndProd :: ProductComponents '[] (Code (V 2 Float)) '[])
              :: ProductComponents '[] (Code (V 2 Float)) '[Code Float] )
        :: Optic '[] (Code (V 2 Float)) (Code Float) )
