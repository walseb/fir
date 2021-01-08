{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module FIR.Examples.RayTracing.Scenes.CornellBox2
  ( cornellBox2 )
  where

-- base
import Data.Proxy
  ( Proxy(..) )
import Data.Word
  ( Word32 )

-- text-short
import Data.Text.Short
  ( ShortText )

-- unordered-containers
import Data.HashMap.Strict
  ( HashMap )
import qualified Data.HashMap.Strict as HashMap
  ( fromList, map, toList )

-- fir
import FIR
  ( Array, GradedSemigroup((<!>)), Struct(..) )
import Math.Linear
  ( V, M(..)
  , pattern V2, pattern V3
  , (^+^), (*^), (^-^), (!*^)
  , cross, identity, konst
  )

-- fir-examples
import FIR.Examples.RayTracing.IOR
  ( IORData(..), MaterialInterface(..), materialInterfaceArray
  , au, bk7
  )
import FIR.Examples.RayTracing.Geometry
  ( Geometry(GeometryData) )
import FIR.Examples.RayTracing.Luminaire
  ( LightSamplingMethod(SurfaceArea) )
import FIR.Examples.RayTracing.Scene
  ( Scene(..), InstanceType(..), MissInfo(..)
  , GeometryObject(..), SomeMaterialProperties(..), EmitterObject(..)
  )
import FIR.Examples.RayTracing.Types
  ( GeometryKind(..), LuminaireKind(..), MaterialKind(..), MissKind(..)
  , STriangleQ(..)
  )
import FIR.Examples.RenderState
  ( Observer(..), initialObserver )

--------------------------------------------------------------------------

cornellBox2 :: Scene
cornellBox2 =
  Scene
    { sceneEmitters             = [cornellBoxEmitter]
    , sceneTriangleGeometries   = HashMap.map fst cornellBoxTriangleGeometries
    , sceneProceduralGeometries = HashMap.map fst cornellBoxAABBLambertianGeometries <> HashMap.map fst cornellBoxAABBFresnelGeometries
    , sceneInstances            = [ ( ProceduralInstance, identity <!> konst 0, HashMap.toList ( HashMap.map snd cornellBoxAABBLambertianGeometries ) )
                                  , ( ProceduralInstance, identity <!> konst 0, HashMap.toList ( HashMap.map snd cornellBoxAABBFresnelGeometries ) )
                                  , ( TrianglesInstance , identity <!> konst 0, HashMap.toList ( HashMap.map snd cornellBoxTriangleGeometries ) )
                                  ]
    , sceneObserver             = cornellBoxObserver
    , sceneMissInfo             = cornellMissInfo
    , sceneMovementMultiplier   = 3
    }
cornellBoxEmitter :: EmitterObject
cornellBoxEmitter =
  EmitterObject @Sphere @Blackbody @Lambertian
    SurfaceArea
    SNotTriangle
    [ V3 50 -65.1 81.6 :& 1.5 :& End ]
    Proxy
    1.0
    ( 6.5e3 :& 1 :& End )
    Proxy
    ( V3 1 1 1 :& End )

cornellBoxAABBLambertianGeometries :: HashMap ShortText ( GeometryObject, SomeMaterialProperties )
cornellBoxAABBLambertianGeometries
  = HashMap.map
      ( \ ( sphere, matProps ) ->
        ( GeometryObject         @Sphere     Proxy [ sphere ]
        , SomeMaterialProperties @Lambertian Proxy matProps
        )
      )
  . HashMap.fromList
  $
  [ ( "leftWall"  , ( V3 (-1e5+1)  -40.8         81.6     :& 1e5  :& End, V3 0.9 0.2 0.2 :& End ) )
  , ( "rightWall" , ( V3 (1e5+99)  -40.8         81.6     :& 1e5  :& End, V3 0.4 0.8 0.1 :& End ) )
  , ( "backWall"  , ( V3      50   -40.8        (1e5+190) :& 1e5  :& End, V3 1   1   1   :& End ) )
  , ( "frontWall" , ( V3      50   -40.8        -1e5      :& 1e5  :& End, V3 0.1 0.1 0.1 :& End ) )
  , ( "bottomWall", ( V3      50    1e5          81.6     :& 1e5  :& End, V3 0.7 0.7 0.7 :& End ) )
  , ( "topWall"   , ( V3      50  (-(1e5+81.6))  81.6     :& 1e5  :& End, V3 1   1   1   :& End ) )
  , ( "sphere1"   , ( V3      73   -16.5         78       :& 16.5 :& End, V3 0.2 0.5 0.9 :& End ) )
  , ( "sphere3"   , ( V3      28   -3            118      :& 3    :& End, V3 0.9 0.7 0.1 :& End ) )
  ]

cornellBoxAABBFresnelGeometries :: HashMap ShortText ( GeometryObject, SomeMaterialProperties )
cornellBoxAABBFresnelGeometries
  = HashMap.map
      ( \ ( sphere, matProps ) ->
        ( GeometryObject         @Sphere  Proxy [ sphere ]
        , SomeMaterialProperties @Fresnel Proxy matProps
        )
      )
  . HashMap.fromList
  $
  [ ( "sphere2", ( V3 28 -19 92 :& 19 :& End, V3 1 1 1 :& iors2 :& End ) )
  ]
  where
    iors2 :: Array 82 ( V 4 Float )
    iors2 = materialInterfaceArray
      ( MaterialInterface
        { outsideMaterial = IORFunction ( const ( V2 1 0 ) )
        , insideMaterial  = IORSellmeier bk7
        }
      )

cornellBoxTriangleGeometries :: HashMap ShortText ( ( [ Word32 ], [ GeometryData Triangle ] ), SomeMaterialProperties )
cornellBoxTriangleGeometries
  = HashMap.fromList
  [ ( "triangle" :: ShortText
    , ( ( [ 0, 2, 1, 0, 1, 3, 0, 3, 2, 1, 2, 3 ]
        , [ p0 :& foldr (^+^) (V3 0 0 0) [ n012, n013, n023 ] :& End
          , p1 :& foldr (^+^) (V3 0 0 0) [ n012, n013, n123 ] :& End
          , p2 :& foldr (^+^) (V3 0 0 0) [ n012, n023, n123 ] :& End
          , p3 :& foldr (^+^) (V3 0 0 0) [ n013, n023, n123 ] :& End
          ]
        )
      , SomeMaterialProperties @Fresnel Proxy ( V3 1 1 1 :& iors :& End )
      )
    )
  ]
  where
    p0, p1, p2, p3, n012, n013, n023, n123, c :: V 3 Float
    p0 = c ^+^ k *^ ( m !*^ V3 1     0            0 )
    p1 = c ^+^ k *^ ( m !*^ V3 -0.5  0            (   sqrt 3 / 2 ) )
    p2 = c ^+^ k *^ ( m !*^ V3 -0.5  0            ( - sqrt 3 / 2 ) )
    p3 = c ^+^ k *^ ( m !*^ V3 0     ( - sqrt 2 ) 0 )
    n012 = ( p1 ^-^ p0 ) `cross` ( p2 ^-^ p0 )
    n013 = ( p3 ^-^ p0 ) `cross` ( p1 ^-^ p0 )
    n023 = ( p0 ^-^ p3 ) `cross` ( p2 ^-^ p0 )
    n123 = ( p3 ^-^ p1 ) `cross` ( p2 ^-^ p1 )
    c = V3 60 ( - k * sqrt 2 ) 150
    k :: Float
    k = 40
    φ :: Float
    φ = 0.8 * pi
    m :: M 3 3 Float
    m = M $ V3
          ( V3 (   cos φ )  0   ( sin φ ) )
          ( V3      0      -1       0     )
          ( V3 ( - sin φ )  0   ( cos φ ) )
    iors :: Array 82 ( V 4 Float )
    iors = materialInterfaceArray
      ( MaterialInterface
        { outsideMaterial = IORFunction ( const ( V2 1 0 ) )
        , insideMaterial  = IORMap au
        }
      )

cornellBoxObserver :: Observer
cornellBoxObserver = initialObserver { position = V3 50 -40.8 35 , angles = V2 0 0 }

cornellMissInfo :: MissInfo
cornellMissInfo =
  MissInfo
    ( Proxy :: Proxy Factor )
    2 -- miss shader index
    0
