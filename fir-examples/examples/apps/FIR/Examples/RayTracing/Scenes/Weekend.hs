{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module FIR.Examples.RayTracing.Scenes.Weekend
  ( weekend )
  where

-- base
import Data.Traversable
  ( for )
import Data.Proxy
  ( Proxy(..) )

-- random
import qualified System.Random as Random
  ( StdGen, mkStdGen )
import qualified System.Random.Stateful as Random
  ( StateGenM, runStateGen_, uniformFloat01M )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- transformers
import Control.Monad.Trans.State.Strict
  ( State )

-- unordered-containers
import Data.HashMap.Strict
  ( HashMap )
import qualified Data.HashMap.Strict as HashMap
  ( empty, fromList, map, toList )

-- fir
import FIR
  ( Array, GradedSemigroup((<!>)), Struct(..) )
import Math.Linear
  ( V
  , pattern V2, pattern V3
  , identity, konst
  )

-- fir-examples
import FIR.Examples.RayTracing.IOR
  ( IORData(..), MaterialInterface(..), materialInterfaceArray
  , au, ag, bk7, cu
  )
import FIR.Examples.RayTracing.Scene
  ( Scene(..), InstanceType(..), MissInfo(..)
  , GeometryObject(..), SomeMaterialProperties(..)
  )
import FIR.Examples.RayTracing.Types
  ( GeometryKind(..), MaterialKind(..), MissKind(..)
  )
import FIR.Examples.RenderState
  ( Observer(..), initialObserver )

--------------------------------------------------------------------------

weekend :: Scene
weekend =
  Scene
    { sceneEmitters             = []
    , sceneTriangleGeometries   = HashMap.empty
    , sceneProceduralGeometries = HashMap.map fst weekendGeometries
    , sceneInstances            = [ ( ProceduralInstance, identity <!> konst 0, HashMap.toList ( HashMap.map snd weekendGeometries ) ) ]
    , sceneObserver             = weekendObserver
    , sceneMissInfo             = weekendMissInfo
    }

weekendObserver :: Observer
weekendObserver =
  initialObserver
    { position = V3 4.5 -1.1 -3.5
    , angles   = V2 0.6 0
    }

weekendGeometries :: HashMap ShortText ( GeometryObject, SomeMaterialProperties )
weekendGeometries = Random.runStateGen_ ( Random.mkStdGen 1729 ) \ gen -> do
  randomObjs <- 
    for [ ( i, j ) | ( i :: Int ) <- [-20..20], ( j :: Int ) <- [-20..20] ] \ ( i, j ) -> do
      x_r <- Random.uniformFloat01M gen
      z_r <- Random.uniformFloat01M gen
      let
        pos :: V 3 Float
        pos = V3 ( 1.5 * fromIntegral i + 0.9 * x_r ) -0.2 ( 1.5 * fromIntegral j + 0.9 * z_r )
        obj :: GeometryObject
        obj = GeometryObject @Sphere Proxy [ pos :& 0.2 :& End ]
      mat <- randomMat gen
      pure ( ShortText.pack $ show (i,j), ( obj, mat ) )
  let
    bakedObjs :: [ ( ShortText, ( GeometryObject, SomeMaterialProperties ) ) ]
    bakedObjs =
      [ ( "ground"
        , ( GeometryObject @Sphere Proxy [ V3 0 1e4 0 :& 1e4 :& End ]
          , SomeMaterialProperties @Lambertian Proxy ( V3 0.5 0.5 0.5 :& End )
          )
        )
      , ( "bigGlass"
        , ( GeometryObject @Sphere Proxy [ V3 0 -1 0 :& 1 :& End ]
          , SomeMaterialProperties @Fresnel Proxy ( V3 1 1 1 :& glassIOR :& End )
          )
        )
      , ( "bigLambert"
        , ( GeometryObject @Sphere Proxy [ V3 -4 -1 0 :& 1 :& End ]
          , SomeMaterialProperties @Lambertian Proxy ( V3 0.4 0.2 0.1 :& End )
          )
        )
      , ( "bigMetal"
        , ( GeometryObject @Sphere Proxy [ V3 4 -1 0 :& 1 :& End ]
          , SomeMaterialProperties @Fresnel Proxy ( V3 1 1 1 :& copperIOR :& End )
          )
        )
      ]
  pure $ HashMap.fromList ( bakedObjs <> randomObjs )

randomMat :: Random.StateGenM Random.StdGen -> State Random.StdGen SomeMaterialProperties
randomMat gen = do
  mat_r <- Random.uniformFloat01M gen
  if mat_r <= 0.8
  then do
    r <- Random.uniformFloat01M gen
    g <- Random.uniformFloat01M gen
    b <- Random.uniformFloat01M gen
    pure $ SomeMaterialProperties @Lambertian Proxy ( V3 r g b :& End )
  else do
    let
      ior :: Array 82 ( V 4 Float )
      ior = materialInterfaceArray
        ( MaterialInterface
          { outsideMaterial = IORFunction ( const ( V2 1.0003 0 ) )
          , insideMaterial  =
              if mat_r <= 0.93
              then IORMap ag
              else if mat_r <= 0.95
              then IORMap au
              else IORSellmeier bk7
          }
        )
    pure $ SomeMaterialProperties @Fresnel Proxy ( V3 1 1 1 :& ior :& End ) 

copperIOR, glassIOR :: Array 82 ( V 4 Float )
copperIOR =
  materialInterfaceArray
    ( MaterialInterface
      { outsideMaterial = IORFunction ( const ( V2 1.0003 0 ) )
      , insideMaterial  = IORMap cu
      }
    )
glassIOR =
  materialInterfaceArray
    ( MaterialInterface
      { outsideMaterial = IORFunction ( const ( V2 1.0003 0 ) )
      , insideMaterial  = IORSellmeier bk7
      }
    )

weekendMissInfo :: MissInfo
weekendMissInfo =
  MissInfo
    ( Proxy :: Proxy EnvironmentBlackbody )
    1 -- miss shader index
    ( 6.5e3 :& 1 :& End )
