{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module FIR.Examples.RayTracing.Scene where

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

-- fir
import Math.Linear
  ( M )

-- fir-examples
import FIR.Examples.RayTracing.Camera
  ( CameraCoordinates )
import FIR.Examples.RayTracing.Geometry
  ( Geometry(GeometryData), HittableGeometry )
import FIR.Examples.RayTracing.Luminaire
  ( Luminaire(LuminaireProperties), LightSamplingMethod )
import FIR.Examples.RayTracing.Material
  ( Material(MaterialProperties) )
import FIR.Examples.RayTracing.Types
  ( GeometryKind(..), IsTriangle, STriangleQ )

--------------------------------------------------------------------------

data InstanceType
  = TrianglesInstance
  | ProceduralInstance

data Scene
  = Scene
    { sceneEmitters             :: [EmitterObject]
    , sceneTriangleGeometries   :: HashMap ShortText ( [ Word32 ], [ GeometryData Triangle ] )
    , sceneProceduralGeometries :: HashMap ShortText GeometryObject
    , sceneInstances            :: [ ( InstanceType, M 3 4 Float, [ ( ShortText, SomeMaterialProperties ) ] ) ]
    , sceneCamera               :: CameraCoordinates
    }

data GeometryObject where
  GeometryObject
    :: ( HittableGeometry geom, IsTriangle geom ~ False )
    => Proxy geom
    -> [ GeometryData geom ]
    -> GeometryObject

data SomeMaterialProperties where
  SomeMaterialProperties
    :: Material mat
    => Proxy mat
    -> MaterialProperties mat
    -> SomeMaterialProperties

data EmitterObject where
  EmitterObject
    :: ( HittableGeometry geom, Luminaire lum, Material mat )
    => LightSamplingMethod
    -> STriangleQ geom
    -> [ GeometryData geom ] -- should be of length 3 for triangle geometry and length 0 otherwise
    -> Proxy lum
    -> Float -- ^ luminaire weight; these need to sum to 1 (TODO: normalise within the 'buildScene' function)
    -> LuminaireProperties lum
    -> Proxy mat
    -> MaterialProperties mat
    -> EmitterObject
