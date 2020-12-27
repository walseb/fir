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

-- unordered-containers
import qualified Data.HashMap.Strict as HashMap
  ( empty )

-- fir
import FIR
  ( Struct(..) )
import Math.Linear
  ( pattern V3, pattern V4 )

-- fir-examples
import FIR.Examples.RayTracing.Camera
  ( CameraCoordinates )
import FIR.Examples.RayTracing.Luminaire
  ( LightSamplingMethod(SurfaceArea) )
import FIR.Examples.RayTracing.Scene
  ( Scene(..), EmitterObject(..) )
import FIR.Examples.RayTracing.Types
  ( GeometryKind(..), LuminaireKind(..), MaterialKind(..)
  , STriangleQ(..)
  )

--------------------------------------------------------------------------

furnace :: Scene
furnace =
  Scene
    { sceneEmitters             = [trivialEmitter]
    , sceneTriangleGeometries   = HashMap.empty
    , sceneProceduralGeometries = HashMap.empty
    , sceneInstances            = []
    , sceneCamera               = furnaceCamera
    }

trivialEmitter :: EmitterObject
trivialEmitter =
  EmitterObject @Sphere @Blackbody @Lambertian
    SurfaceArea
    SNotTriangle
    [ V3 0 0 0 :& 30 :& End ]
    Proxy
    1.0
    ( 6.5e3 :& 0 :& End )
    Proxy
    ( V3 1 1 1 :& End )

furnaceCamera :: CameraCoordinates
furnaceCamera = V4 0 0 -70 0 :& V3 1 0 0 :& V3 0 1 0 :& V4 0 0 1 0 :& End
