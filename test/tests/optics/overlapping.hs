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

program :: Program '[] ()
program = Program do

  entryPoint @"main" @Vertex do

    def @"struct" @RW @(Struct '[ "field_0" ':-> Float, "field_1" ':-> Bool ])
      ( Lit ( 3 :& True :& End ) )

    assign @(Name "struct" :.: (Name "field_0" :*: Index 0)) ( Lit ( 4 :& 5 :& End ) )
