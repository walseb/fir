{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Tests.Vector.MixedSwizzle where

-- fir
import FIR
import Math.Linear

---------------------------------------------------------------------------------

type Defs = '[ "in_col"  ':-> Input      '[Location 0] (V 4 Float)
             , "out_col" ':-> Output     '[Location 0] (V 4 Float)
             , "main"    ':-> EntryPoint '[OriginUpperLeft] Fragment
             ]


program :: Module Defs ()
program = Module $ entryPoint @"main" @Fragment do

    col <- use @(Name "in_col" :.: Swizzle "rgxb") -- cannot mix "rgba" and "xyzw"

    put @"out_col" col
