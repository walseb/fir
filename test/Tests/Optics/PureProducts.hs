{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.Optics.PureProducts where

-- base
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
struct2 = view @(Prod (Name "x" :*: Name "z" :*: EndProd)) struct1

struct3 :: Struct '[ "x" ':-> Float, "z" ':-> Float, "x" ':-> Float ]
struct3 = view @(Prod (Name "x" :*: Name "z" :*: Name "x" :*: EndProd)) struct1

nested1
  :: Struct
      '[ "a" ':-> Float
       , "b" ':-> Struct
           '[ "r" ':-> V 3 Float
            , "s" ':-> M 4 4 Double
            , "t" ':-> Struct
                '[ "u" ':-> Double
                 , "v" ':-> Double
                 ]
            ]
       ]
nested1 = 17.3 :& ( Prelude.pure 7.77 :& diag 4.1 :& ( 3 :& 4 :& End ) :& End ) :& End

nested2 :: Struct '[ "a" ':-> Float, "r" ':-> V 3 Float ]
nested2 = view @(Prod ( Name "a" :*: (Name "b" :.: Index 0) :*: EndProd)) nested1

nested3 :: Struct '[ "r" ':-> V 3 Float
                   , "s" ':-> M 4 4 Double
                   ]
nested3 = view @(Name "b" :.: (Prod (Name "r" :*: Name "s" :*: EndProd))) nested1


type RS
  = Struct
    '[ "r" ':-> V 3 Float
     , "s" ':-> M 4 4 Double
     ]

nested4 :: Struct '[ "a" ':-> Float
                   , "b" ':-> Struct '[ "r" ':-> V 3 Float
                                      , "s" ':-> M 4 4 Double
                                      ]
                   ]
nested4 = view @( Prod
                    (   Index 0
                    :*: ( Name "b" :.: (Prod @_ @_ @_ @RS (Name "r" :*: Name "s" :*: EndProd)) )
                    :*: EndProd
                    )
                )
           nested1


-- vectors

v1 :: V 3 Double
v1 = 0 :. 11 :. 77 :. VNil

v2 :: V 2 Double
v2 = view @(Prod (Index 2 :*: Index 0 :*: EndProd)) v1

-- matrices

m1 :: M 4 4 Float
m1 = identity

m2 :: M 4 2 Float
m2 = view @(Prod (Col 2 :*: Col 1 :*: EndProd)) m1

m3 :: M 4 2 Float
m3 = view @(Prod (Col 2 :*: Col 3 :*: EndProd)) m1

m4 :: M 2 2 Float
m4 = view @(Prod (Entry 1 1 :*: Entry 1 2 :*: Entry 2 1 :*: Entry 2 2 :*: EndProd)) m1

m5 :: V 3 Float
m5 = view @(Prod (Entry 1 1 :*: Entry 3 3 :*: Entry 2 0 :*: EndProd)) m1

------------------------------------------------
-- testing product setters

struct4 :: Struct '[ "x" ':-> Float, "y" ':-> Float, "z" ':-> Float ]
struct4 =
  let part :: Struct '[ "y" ':-> Float, "z" ':-> Float ]
      part = 8.8 :& 11.0 :& End
  in set @( Prod ( (Name "y" :*: Name "z" :*: EndProd) ) )
        part
        struct1

struct5 :: Struct '[ "x" ':-> Float, "y" ':-> Float, "z" ':-> Float ]
struct5 =
  let part :: Struct '[ "field1" ':-> Float, "field2" ':-> Float ]
      part = 8.8 :& 11.0 :& End
  in set @( Prod ( (Name "y" :*: Name "x" :*: EndProd) ) )
        part
        struct1

nested5
  :: Struct
      '[ "a" ':-> Float
       , "b" ':-> Struct
           '[ "r" ':-> V 3 Float
            , "s" ':-> M 4 4 Double
            , "t" ':-> Struct
                '[ "u" ':-> Double
                 , "v" ':-> Double
                 ]
            ]
       ]
nested5 =
  let part :: Struct '[ "a" ':-> Float, "s" ':-> M 4 4 Double ]
      part = 0 :& identity :& End
  in set @( Prod ( Index 0 :*: (Name "b" :.: Name "s") :*: EndProd ))
       part
       nested1

m6 :: M 3 3 Float
m6 = M ( V3
          (V3 0 1 2)
          (V3 3 4 5)
          (V3 6 7 8)
       )

m7 :: M 3 3 Float
m7 = set @( Prod @_ @_ @_ @(V 3 Float) ( Entry 0 0 :*: Entry 0 2 :*: Entry 2 2 :*: EndProd ) :.: OfType Float ) 9 m6

m8 :: M 3 3 Float
m8 = set @Center 9 m6

m9 :: M 3 3 Float
m9 = set @( Prod ( Entry 0 0 :*: Entry 2 0 :*: Entry 0 2 :*: Entry 2 2 :*: EndProd ) ) m4 m6
