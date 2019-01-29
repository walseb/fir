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

module Tests.Optics.Products where

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
import Math.Linear

------------------------------------------------
-- testing product getters

-- structs

struct1 :: Struct '[ "x" ':-> Float, "y" ':-> Float, "z" ':-> Float ]
struct1 = 11.1 :& 7 :& 3.0 :& End

struct2 :: Struct '[ "x" ':-> Float, "z" ':-> Float ]
struct2 = view @(Name "x" :*: Name "z") struct1

struct3 :: Struct '[ "x" ':-> Float, "z" ':-> Float, "x" ':-> Float ]
struct3 = view @(Name "x" :*: Name "z" :*: Name "x") struct1

nested1 :: Struct '[ "a" ':-> Float
                   , "b" ':-> Struct '[ "r" ':-> V 3 Float
                                      , "s" ':-> M 4 4 Double
                                      , "t" ':-> Struct '[ "u" ':-> Double
                                                         , "v" ':-> Double
                                                         ]
                                      ]
                   ]
nested1 = 17.3 :& ( Prelude.pure 7.77 :& diag 4.1 :& ( 3 :& 4 :& End ) :& End ) :& End

nested2 :: Struct '[ "a" ':-> Float, "r" ':-> V 3 Float ]
nested2 = view @(Name "a" :*: (Name "b" :.: Index 0)) nested1

nested3 :: Struct '[ "r" ':-> V 3 Float
                   , "s" ':-> M 4 4 Double
                   ]
nested3 = view @(Name "b" :.: (Name "r" :*: Name "s")) nested1

-- doesn't work, expects return type
--     Struct '[ "a" ':-> Float
--             , "r" ':-> V 3 Float
--             , "s" ':-> M 4 4 Double
--             ]
--nested4 :: Struct '[ "a" ':-> Float
--        , "b" ':-> Struct '[ "r" ':-> V 3 Float
--                           , "s" ':-> M 4 4 Double
--                           ]
--        ]
--nested4 = view @(Index 0 :*: (Name "b" :.: (Name "r" :*: Name "s"))) nested1

-- vectors

v1 :: V 3 Double
v1 = 0 :. 11 :. 77 :. Nil

v2 :: V 2 Double
v2 = view @(Index 2 :*: Index 0) v1

-- matrices

m1 :: M 4 4 Float
m1 = identity

m2 :: M 4 2 Float
m2 = view @(Col 2 :*: Col 1) m1

-- doesn't work, expects return type 'V 8 Float'
--m3 :: M 2 4 Float
--m3 = view @(Row 2 :*: Row 3) m1

-- doesn't work, expects return type 'V 4 Float'
--m4 :: M 2 2 Float
--m4 = view @(Entry 1 1 :*: Entry 1 2 :*: Entry 2 1 :*: Entry 2 2) m1

m5 :: V 3 Float
m5 = view @(Entry 1 1 :*: Entry 3 3 :*: Entry 2 0) m1

------------------------------------------------
-- testing product setters

struct5 :: Struct '[ "x" ':-> Float, "y" ':-> Float, "z" ':-> Float ]
struct5 = set @(Name "y" :*: Name "z") (8.8 :& 11.0 :& End) struct1

nested5 :: Struct '[ "a" ':-> Float
                   , "b" ':-> Struct '[ "r" ':-> V 3 Float
                                      , "s" ':-> M 4 4 Double
                                      , "t" ':-> Struct '[ "u" ':-> Double
                                                         , "v" ':-> Double
                                                         ]
                                      ]
                   ]
nested5 = set @(Index 0 :*: (Name "b" :.: Name "s")) (0 :& identity :& End) nested1

m6 :: M 3 3 Double
m6 = M ( V3
          (V3 0 1 2)
          (V3 3 4 5)
          (V3 6 7 8)
       )

m7 :: M 3 3 Double
m7 = set @( ( Entry 0 0 :*: Entry 0 2 :*: Entry 2 0 :*: Entry 2 2 ) :.: Joint ) 9 m6

m8 :: M 3 3 Double
m8 = set @Center 9 m6

