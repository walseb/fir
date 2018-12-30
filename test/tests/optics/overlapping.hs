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

module Tests.Optics.Overlapping where

-- base
import Prelude hiding ( Functor(..), (<$>)
                      , Applicative(..), Monad(..)
                      , Num(..), Fractional(..), Integral(..), Floating(..)
                      , Eq(..), Ord(..)
                      , (&&)
                      )
import qualified Prelude

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- program

type InOut     = '[]
type Functions = '[]

program :: Program InOut Functions ()
program = do

  entryPoint @"main" @Vertex do

    def @"struct" @RW @(Struct '[ "n" ':-> Float, "b" ':-> Bool ])
      ( lit ( 3 :& True :& End ) )

    assign @(Name "struct" :.: (Name "n" :*: Index 0)) ( lit ( 4 :& 5 :& End ) )