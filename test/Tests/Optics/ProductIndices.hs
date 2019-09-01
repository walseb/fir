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

module Tests.Optics.ProductIndices where

-- prelude
import qualified Prelude
  ( Functor(fmap) )

-- vector
import qualified Data.Vector as Array

-- fir
import FIR

------------------------------------------------
-- testing product runtime indices

arr1 :: Array 12 Float
arr1 = mkArray ( Array.fromList [10..21] )

test1 :: (Float, Float, Float)
test1 = view @( Prod (AnIndex Word32 :*: AnIndex Word32 :*: AnIndex Word32 :*: EndProd) )
          ( 2 :> 7 :> 3 :> HNil )
          arr1

-- > test1
-- (12.0,17.0,13.0)

test2 :: HList '[ Float, Float, Float ]
test2 = view @( Prod (AnIndex Word32 :*: AnIndex Word32 :*: AnIndex Word32 :*: EndProd) )
          ( 2 :> 7 :> 3 :> HNil )
          arr1

-- > test2
-- 12.0 :> 17.0 :> 13.0 :> HNil


arr2 :: Array 2 ( Array 12 Float )
arr2 = mkArray ( Array.fromList [ Prelude.fmap (+10) arr1, arr1 ] )

test3 :: ( Float, Float )
test3 = view @( AnIndex Word32 :.: ( Prod (AnIndex Word32 :*: AnIndex Word32 :*: EndProd) ) )
          0
          ( 11 :> 0 :> HNil )
          arr2

-- > test3
-- (31.0,20.0)

-- doesn't work
--test4 :: ( Float, Float )
--test4 = view @( ( Prod (AnIndex Word32 :*: AnIndex Word32 :*: EndProd) ) :.: AnIndex Word32 )
--          ( 1 :> 0 :> HNil )
--          7
--          arr2
