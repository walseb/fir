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
  ( GradedSemigroup((<!>)), Struct(..) )
import Math.Linear
  ( pattern V2, pattern V3
  , identity, konst
  )

-- fir-examples
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
    }

furnaceGeometries :: HashMap ShortText ( GeometryObject, SomeMaterialProperties )
furnaceGeometries = HashMap.fromList
  [ ( "furnaceObject"
    , ( GeometryObject         @Sphere     Proxy [ V3 0 0 0 :& 30 :& End ]
      , SomeMaterialProperties @Lambertian Proxy ( V3 1 1 1 :& End )
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
