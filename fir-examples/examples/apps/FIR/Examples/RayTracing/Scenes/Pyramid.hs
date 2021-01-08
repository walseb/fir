{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module FIR.Examples.RayTracing.Scenes.Pyramid
  ( pyramid )
  where

-- base
import Data.Proxy
  ( Proxy(..) )
import Data.Word
  ( Word32 )

-- dlist
import Data.DList
  ( DList )
import qualified Data.DList as DList
  ( toList, fromList )

-- unordered-containers
import qualified Data.HashMap.Strict as HashMap
  ( singleton )

-- fir
import FIR
  ( Array, GradedSemigroup((<!>)), Struct(..) )
import Math.Linear
  ( V, pattern V2, pattern V3
  , (^+^), (^-^)
  , cross, identity, konst
  )

-- fir-examples
import FIR.Examples.RayTracing.IOR
  ( IORData(..), MaterialInterface(..), materialInterfaceArray
  , au
  )
import FIR.Examples.RayTracing.Geometry
  ( Geometry(GeometryData) )
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

pyramid :: Scene
pyramid =
  Scene
    { sceneEmitters             = []
    , sceneTriangleGeometries   = HashMap.singleton "pyramid" pyramidGeometry
    , sceneProceduralGeometries = HashMap.singleton "ground"  groundGeometry
    , sceneInstances            = [ ( TrianglesInstance , identity <!> konst 0, [ ( "pyramid", pyramidMaterial ) ] )
                                  , ( ProceduralInstance, identity <!> konst 0, [ ( "ground" , groundMaterial  ) ] )
                                  ]
    , sceneObserver             = pyramidObserver
    , sceneMissInfo             = pyramidMissInfo
    , sceneMovementMultiplier   = 0.05
    }

pyramidObserver :: Observer
pyramidObserver = initialObserver { position = V3 1.2 -1 -1, angles = V2 0.9 -0.5 }

sierpiński :: Word32 -> DList ( GeometryData Triangle ) -> DList ( GeometryData Triangle )
sierpiński 0 tris = tris
sierpiński n tris =
  let
    f0, f1, f2, f3 :: V 3 Float -> V 3 Float
    f0 ( V3 x y z ) = V3 ( 0.5 * ( x + 1   ) ) ( 0.5 *  y             ) ( 0.5 *   z                )
    f1 ( V3 x y z ) = V3 ( 0.5 * ( x - 0.5 ) ) ( 0.5 *  y             ) ( 0.5 * ( z + sqrt 3 / 2 ) )
    f2 ( V3 x y z ) = V3 ( 0.5 * ( x - 0.5 ) ) ( 0.5 *  y             ) ( 0.5 * ( z - sqrt 3 / 2 ) )
    f3 ( V3 x y z ) = V3 ( 0.5 *   x         ) ( 0.5 * ( y - sqrt 2 ) ) ( 0.5 *   z                )
    prevTris, tris0, tris1, tris2, tris3 :: DList ( GeometryData Triangle )
    prevTris = sierpiński (n - 1) tris
    tris0 = fmap ( \ ( p :& v :& End ) -> f0 p :& v :& End ) prevTris
    tris1 = fmap ( \ ( p :& v :& End ) -> f1 p :& v :& End ) prevTris
    tris2 = fmap ( \ ( p :& v :& End ) -> f2 p :& v :& End ) prevTris
    tris3 = fmap ( \ ( p :& v :& End ) -> f3 p :& v :& End ) prevTris
  in
    tris0 <> tris1 <> tris2 <> tris3

pyramidGeometry :: ( [ Word32 ], [ GeometryData Triangle ] )
pyramidGeometry = ( [ i + 4 * j | j <- [ 0 .. 4 ^ n - 1 ], i <- tetr_inds ]
                  , DList.toList $ sierpiński n ( DList.fromList ps )
                  )
  where
    n :: Word32
    n = 6
    tetr_inds :: [ Word32 ]
    tetr_inds = [ 0, 2, 1, 0, 1, 3, 0, 3, 2, 1, 2, 3 ]
    ps :: [ GeometryData Triangle ]
    ps =
      [ p0 :& foldr (^+^) (V3 0 0 0) [ n012, n013, n023 ] :& End
      , p1 :& foldr (^+^) (V3 0 0 0) [ n012, n013, n123 ] :& End
      , p2 :& foldr (^+^) (V3 0 0 0) [ n012, n023, n123 ] :& End
      , p3 :& foldr (^+^) (V3 0 0 0) [ n013, n023, n123 ] :& End
      ]
    p0 = V3 1     0            0
    p1 = V3 -0.5  0            (   sqrt 3 / 2 )
    p2 = V3 -0.5  0            ( - sqrt 3 / 2 )
    p3 = V3 0     ( - sqrt 2 ) 0
    n012 = ( p0 ^-^ p1 ) `cross` ( p2 ^-^ p0 )
    n013 = ( p0 ^-^ p3 ) `cross` ( p1 ^-^ p0 )
    n023 = ( p3 ^-^ p0 ) `cross` ( p2 ^-^ p0 )
    n123 = ( p1 ^-^ p3 ) `cross` ( p2 ^-^ p1 )

pyramidMaterial :: SomeMaterialProperties
pyramidMaterial = SomeMaterialProperties @Fresnel Proxy ( V3 1 1 1 :& iors :& End )
  where
    iors :: Array 82 ( V 4 Float )
    iors = materialInterfaceArray
      ( MaterialInterface
        { outsideMaterial = IORFunction ( const ( V2 1.0003 0 ) )
        , insideMaterial  = IORMap au
        }
      )

groundGeometry :: GeometryObject
groundGeometry = GeometryObject @Sphere Proxy [ V3 0 40 0 :& 40 - 1e-3 :& End ]

groundMaterial :: SomeMaterialProperties
groundMaterial = SomeMaterialProperties @Lambertian Proxy ( V3 0.75 0.8 0.9 :& End )

pyramidMissInfo :: MissInfo
pyramidMissInfo =
  MissInfo
    ( Proxy :: Proxy EnvironmentBlackbody )
    1 -- miss shader index
    ( 6.5e3 :& 1 :& End )
