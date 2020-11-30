{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedWildCards         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Tests.Inference.Ising1 where

-- fir
import FIR

------------------------------------------------

data Parity = Even | Odd
data SParity ( parity :: Parity ) where
  SEven :: SParity Even
  SOdd  :: SParity Odd

type family Bd ( parity :: Parity ) = r | r -> parity where
  Bd Even = 2
  Bd Odd  = 3

type Defs ( parity :: Parity ) =
  '[ "out"  ':-> Image2D    '[ DescriptorSet 0, Binding 37 ] ( R32 F )
   , "main" ':-> EntryPoint '[ LocalSize 1 1 1 ] Compute
   ]

shader :: _ => SParity parity -> Module ( Defs parity )
shader sParity = Module $ entryPoint @"main" @Compute $ do
  val <- foo
  pure ( Lit () )

foo :: Program _s _t ( Code Float )
foo = def @"x" @R @Float 3
