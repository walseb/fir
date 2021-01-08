{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module FIR.Examples.RayTracing.Scenes.CornellBox
  ( cornellBox )
  where

-- base
import Data.Proxy
  ( Proxy(..) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- unordered-containers
import Data.HashMap.Strict
  ( HashMap )
import qualified Data.HashMap.Strict as HashMap
  ( empty, fromList, map, toList )

-- fir
import FIR
  ( GradedSemigroup((<!>)), Struct(..) )
import Math.Linear
  ( pattern V2, pattern V3
  , identity, konst
  )

-- fir-examples
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

cornellBox :: Scene
cornellBox =
  Scene
    { sceneEmitters             = [cornellBoxEmitter]
    , sceneTriangleGeometries   = HashMap.empty
    , sceneProceduralGeometries = HashMap.map fst cornellBoxAABBGeometries
    , sceneInstances            = [ ( ProceduralInstance, identity <!> konst 0, HashMap.toList ( HashMap.map snd cornellBoxAABBGeometries ) ) ]
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

cornellBoxAABBGeometries :: HashMap ShortText ( GeometryObject, SomeMaterialProperties )
cornellBoxAABBGeometries
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
  , ( "backWall"  , ( V3      50   -40.8        (1e5+170) :& 1e5  :& End, V3 1   1   1   :& End ) )
  , ( "frontWall" , ( V3      50   -40.8        -1e5      :& 1e5  :& End, V3 0.1 0.1 0.1 :& End ) )
  , ( "bottomWall", ( V3      50    1e5          81.6     :& 1e5  :& End, V3 0.7 0.7 0.7 :& End ) )
  , ( "topWall"   , ( V3      50  (-(1e5+81.6))  81.6     :& 1e5  :& End, V3 1   1   1   :& End ) )
  , ( "sphere1"   , ( V3      73   -16.5         78       :& 16.5 :& End, V3 1   1   1   :& End ) )
  , ( "sphere2"   , ( V3      28   -19           92       :& 19   :& End, V3 0.2 0.5 1.0 :& End ) )
  ]

cornellBoxObserver :: Observer
cornellBoxObserver = initialObserver { position = V3 50 -40.8 35 , angles = V2 0 0 }

cornellMissInfo :: MissInfo
cornellMissInfo =
  MissInfo
    ( Proxy :: Proxy Factor )
    2 -- miss shader index
    0
