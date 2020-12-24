{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedWildCards         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Tests.Inference.Ising2 where

-- fir
import FIR

------------------------------------------------
-- Work sizes.

data Parity = Even | Odd
data SParity ( parity :: Parity ) where
  SEven :: SParity Even
  SOdd  :: SParity Odd

type Defs ( parity :: Parity ) =
  '[ "even" ':-> Input '[ Location 0 ] Float
   , "odd"  ':-> Input '[ Location 1 ] Float
   , "main" ':-> EntryPoint '[] Vertex
   ]

shader :: _ => SParity parity -> Module ( Defs parity )
shader sParity = Module $ entryPoint @"main" @Vertex $ do
  val <- readInputFromParity sParity
  pure ( Lit () )

readInputFromParity :: _ => SParity parity -> Program _s _s ( Code Float )
readInputFromParity sParity = readInput
  where
    readInput
      :: _ => Program _s _s ( Code Float )
    readInput = case sParity of
      SEven -> get @"even"
      SOdd  -> get @"odd"
