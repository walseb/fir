{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

module FIR.Examples.RayTracing.Scenes.Furnace
  ( furnace )
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
  ( Array, GradedSemigroup((<!>)), Struct(..) )
import Math.Linear
  ( V, pattern V2, pattern V3
  , identity, konst
  )

-- fir-examples
import FIR.Examples.RayTracing.IOR
  ( IORData(..), MaterialInterface(..), materialInterfaceArray
  , ag, bk7
  )
import FIR.Examples.RayTracing.Scene
  ( Scene(..), InstanceType(..), MissInfo(..)
  , GeometryObject(..), SomeMaterialProperties(..)
  )
import FIR.Examples.RayTracing.Types
  ( GeometryKind(..), MaterialKind(..), MissKind(..) )
import FIR.Examples.RenderState
  ( Observer(..), initialObserver )

--------------------------------------------------------------------------

furnace :: Scene
furnace =
  Scene
    { sceneEmitters             = []
    , sceneTriangleGeometries   = HashMap.empty
    , sceneProceduralGeometries = HashMap.map fst furnaceGeometries
    , sceneInstances            = [ ( ProceduralInstance, identity <!> konst 0, HashMap.toList ( HashMap.map snd furnaceGeometries ) ) ]
    , sceneObserver             = furnaceObserver
    , sceneMissInfo             = furnaceMissInfo
    , sceneMovementMultiplier   = 0.8
    }

furnaceGeometries :: HashMap ShortText ( GeometryObject, SomeMaterialProperties )
furnaceGeometries = HashMap.fromList
  [ ( "furnaceObject"
    , ( GeometryObject         @Sphere  Proxy [ V3 0 0 0 :& 30 :& End ]
      , SomeMaterialProperties @Fresnel Proxy ( V3 1 1 1 :& glassIOR :& End )
      )
    )
  ]

furnaceObserver :: Observer
furnaceObserver = initialObserver { position = V3 0 0 -70, angles = V2 0 0 }

furnaceMissInfo :: MissInfo
furnaceMissInfo =
  MissInfo
    ( Proxy :: Proxy Factor )
    2 -- miss shader index
    0.9

silverIOR, glassIOR :: Array 82 ( V 4 Float )
silverIOR =
  materialInterfaceArray
    ( MaterialInterface
      { outsideMaterial = IORFunction ( const ( V2 1.0003 0 ) )
      , insideMaterial  = IORMap ag
      }
    )
glassIOR =
  materialInterfaceArray
    ( MaterialInterface
      { outsideMaterial = IORFunction ( const ( V2 1.0003 0 ) )
      , insideMaterial  = IORSellmeier bk7
      }
    )
