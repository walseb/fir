{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Examples.RayTracing.Geometry
  ( Geometry(..)
  , HittableGeometry(..)
  , occlusionClosestHitShader, primaryClosestHitShader
  )
  where

-- base
import qualified Prelude
import Data.Kind
  ( Type )
import Data.Type.Bool
  ( If )
import Data.Typeable
  ( Typeable )

-- fir
import FIR
  hiding ( Geometry, Triangle )
import Math.Linear

-- fir-examples
import FIR.Examples.RayTracing.Estimator
  ( estimateRadiance )
import FIR.Examples.RayTracing.Material
  ( MaterialSampleCallableData, MaterialQueryCallableData )
import FIR.Examples.RayTracing.Types
  ( ShaderRecord
  , Bindable(BindingNo), GeometryBindingNo
  , IndexBuffer(TriangleIndexBuffer)
  , GeometryKind(..), IsTriangle, LuminaireID
  , EmitterCallableData, LightSamplingCallableData
  , PrimaryPayload, OcclusionPayload
  )

--------------------------------------------------------------------------
-- Geometry types.

class    ( PrimTy        ( GeometryData geom )
         , HasOpaqueType ( GeometryData geom ) ~ False
         , PrimTy        ( GeometryAttributes geom )
         , Typeable geom
         , Bindable geom
         )
      => Geometry ( geom :: GeometryKind )
      where
  type GeometryAttributes geom :: Type
  type GeometryData       geom :: Type
  geometryKind :: GeometryKind

instance Geometry Triangle where
  type GeometryAttributes Triangle = V 2 Float -- barycentric coordinates (built-in)
  type GeometryData       Triangle =
    Struct
      '[ "vertex" ':-> V 3 Float
       , "normal" ':-> V 3 Float
       ]
  geometryKind = Triangle

instance Geometry Sphere where
  type GeometryAttributes Sphere = ()
  type GeometryData       Sphere =
    Struct
      '[ "center" ':-> V 3 Float
       , "radius" ':-> Float
       ]
  geometryKind = Sphere

--------------------------------------------------------------------------

type IntersectionShader geom =
  Module
    [ "geometries"   ':-> StorageBuffer '[ DescriptorSet 0, Binding ( GeometryBindingNo geom ), NonWritable ]
                          ( Struct '[ "geometryArray" ':-> RuntimeArray ( GeometryData geom ) ] )
    , "shaderRecord" ':-> ShaderRecordBuffer '[] ShaderRecord
    , "main"         ':-> EntryPoint   '[] Intersection
    ]

type family AABBFunction ( geom :: GeometryKind ) where
  AABBFunction Triangle = ()
  AABBFunction geom     = GeometryData geom -> ( V 3 Float, V 3 Float )

type ClosestHitOcclusionDefs ( geom :: GeometryKind ) =
  '[ "payload"         ':-> RayPayloadIn '[] OcclusionPayload
   , "triangleIndices" ':-> StorageBuffer '[ DescriptorSet 0, Binding ( BindingNo TriangleIndexBuffer ), NonWritable ]
                              ( Struct '[ "indices" ':-> RuntimeArray ( Struct '[ "i0" ':-> Word32, "i1" ':-> Word32, "i2" ':-> Word32 ] ) ] )
   , "geometries"      ':-> StorageBuffer '[ DescriptorSet 0, Binding ( GeometryBindingNo geom ), NonWritable ]
                              ( Struct '[ "geometryArray" ':-> RuntimeArray ( GeometryData geom ) ] )
   , "shaderRecord"    ':-> ShaderRecordBuffer '[] ShaderRecord
   , "hitAttribute"    ':-> HitAttribute '[] ( Struct '[ "attributes" ':-> GeometryAttributes geom ] )
   , "main"            ':-> EntryPoint   '[] ClosestHit
   ]

type ClosestHitPrimaryDefs ( geom :: GeometryKind ) =
  '[ "accel"           ':-> UniformConstant '[ DescriptorSet 0, Binding 1 ] AccelerationStructure
   , "luminaires"      ':-> StorageBuffer '[ DescriptorSet 0, Binding 2, NonWritable ]
                              ( Struct '[ "luminaireArray" ':-> RuntimeArray LuminaireID ] )
   , "payload"         ':-> RayPayloadIn  '[] PrimaryPayload
   , "occPayload"      ':-> RayPayload    '[] OcclusionPayload
   , "emitterData"     ':-> CallableData  '[] EmitterCallableData
   , "matSampleData"   ':-> CallableData  '[] MaterialSampleCallableData
   , "matQueryData"    ':-> CallableData  '[] MaterialQueryCallableData
   , "lightSampleData" ':-> CallableData  '[] LightSamplingCallableData
   , "triangleIndices" ':-> StorageBuffer '[ DescriptorSet 0, Binding ( BindingNo TriangleIndexBuffer ), NonWritable ]
                              ( Struct '[ "indices" ':-> RuntimeArray ( Struct '[ "i0" ':-> Word32, "i1" ':-> Word32, "i2" ':-> Word32 ] ) ] )
   , "geometries"      ':-> StorageBuffer '[ DescriptorSet 0, Binding ( GeometryBindingNo geom ), NonWritable ]
                              ( Struct '[ "geometryArray" ':-> RuntimeArray ( GeometryData geom ) ] )
   , "hitAttribute"    ':-> HitAttribute '[] ( Struct '[ "attributes" ':-> GeometryAttributes geom ] )
   , "shaderRecord"    ':-> ShaderRecordBuffer '[] ShaderRecord
   , "main"            ':-> EntryPoint    '[] ClosestHit
   ]

class Geometry geom => HittableGeometry geom where

  aabb :: If (IsTriangle geom) () ( GeometryData geom -> ( V 3 Float, V 3 Float ) )

  -- TODO: could pass HitAttribute data from IntersectionShader to ClosestHit shader,
  -- but that doesn't seem necessary at the moment.
  intersectionShader :: If (IsTriangle geom) () (IntersectionShader geom)

  getNormal
    :: ( Has    "geometries"      s ~ Struct '[ "geometryArray" ':-> RuntimeArray ( GeometryData geom ) ]
       , CanGet "geometries"      s
       , Has    "hitAttribute"    s ~ Struct '[ "attributes" ':-> GeometryAttributes geom ]
       , CanGet "hitAttribute"    s
       , Has    "triangleIndices" s ~ Struct '[ "indices" ':-> RuntimeArray ( Struct '[ "i0" ':-> Word32, "i1" ':-> Word32, "i2" ':-> Word32 ] ) ]
       , CanGet "triangleIndices" s
       )
    => Code ( M 3 4 Float ) -- ^ Object-to-world matrix.
    -> Code ( M 4 3 Float ) -- ^ Transpose of world-to-object matrix.
    -> Code ( V 3 Float )   -- ^ World ray origin.
    -> Code ( V 3 Float )   -- ^ World ray direction.
    -> Code Float           -- ^ HitT.
    -> Code Word32          -- ^ Index into the relevant geometry array.
    -> Program s s ( Code ( V 3 Float ) )

instance HittableGeometry Triangle where

  aabb = ()

  intersectionShader = ()

  getNormal _ worldToObject3x4 _ _ _ geometryIndex = do
    is <- use @( Name "triangleIndices" :.: Name "indices" :.: AnIndex Word32 ) geometryIndex
    i0 <- let' $ view @( Name "i0" ) is
    i1 <- let' $ view @( Name "i1" ) is
    i2 <- let' $ view @( Name "i2" ) is
    n0 <- use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 :.: Name "normal" ) i0
    n1 <- use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 :.: Name "normal" ) i1
    n2 <- use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 :.: Name "normal" ) i2
    ~( Vec2 u v ) <- use @( Name "hitAttribute" :.: Name "attributes" )
    objectNormal <- let' $ ( 1 - u - v ) *^ n0 ^+^ u *^ n1 ^+^ v *^ n2
    -- Use the inverse-transpose of the object-to-world transformation to transform normal vectors.
    let' . normalise $ view @( Swizzle "xyz" ) ( worldToObject3x4 !*^ objectNormal )

instance HittableGeometry Sphere where

  aabb ( center :& radius :& End ) = ( center ^-^ Prelude.pure radius, center ^+^ Prelude.pure radius )

  intersectionShader = Module $ entryPoint @"main" @Intersection do

    worldRayOrigin <- let' =<< get @"gl_WorldRayOrigin"
    worldRayDir0   <- let' =<< get @"gl_WorldRayDirection"

    invNmRayDir <- let' $ invSqrt ( worldRayDir0 ^.^ worldRayDir0 )
    worldRayDir <- let' $ invNmRayDir *^ worldRayDir0

    objectToWorld <- get @"gl_ObjectToWorld"

    primitiveID    <- get @"gl_PrimitiveID"
    geometryOffset <- use @( Name "shaderRecord" :.: Name "geometryOffset" )
    objectSphere <- use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 ) ( geometryOffset + primitiveID )

    ~( Vec3 cx_o cy_o cz_o ) <- let' $ view @( Name "center" ) objectSphere
    worldCentre <- let' $ objectToWorld !*^ Vec4 cx_o cy_o cz_o 1

    r_o <- let' $ view @( Name "radius" ) objectSphere
    r   <- let' $ distance worldCentre ( objectToWorld !*^ Vec4 ( cx_o + r_o ) cy_o cz_o 1 )

    delta <- let' $ worldCentre ^-^ worldRayOrigin
    leg   <- let' $ worldRayDir ^.^ delta
    off   <- let' $ delta ^-^ ( leg *^ worldRayDir )
    r²    <- let' $ r * r
    disc  <- let' $ r² - off ^.^ off

    when ( disc >= 0 ) do

      -- Numerically stable quadratic formula (avoid catastrophic cancellation).
      sqrtDisc <- let' $ sqrt disc
      t1 <- let' $
        if leg >= 0
        then leg + sqrtDisc
        else leg - sqrtDisc
      t2 <- let' $ ( delta ^.^ delta - r² ) / t1

      --put @"hitData" sphere
      _ <- reportIntersection ( invNmRayDir * t1 ) 0
      _ <- reportIntersection ( invNmRayDir * t2 ) 0
      pure ( Lit () )


  getNormal objectToWorld _ worldRayOrigin worldRayDir hitT geometryIndex = do

    worldHitPos <- let' $ worldRayOrigin ^+^ ( hitT *^ worldRayDir )

    ~( Vec3 cx cy cz ) <- use @( Name "geometries" :.: Name "geometryArray" :.: AnIndex Word32 :.: Name "center" ) geometryIndex
    worldSphereCenter  <- let' $ ( objectToWorld !*^ Vec4 cx cy cz 1 )

    let' $ normalise ( worldHitPos ^-^ worldSphereCenter )


--------------------------------------------------------------------------
-- Closest hit shaders:
--  - shadow ray closest-hit shader: just one,
--  - primary ray closest-hit shader: one for each geometry type.

occlusionClosestHitShader :: forall geom. ( HittableGeometry geom, _ ) => Module ( ClosestHitOcclusionDefs geom )
occlusionClosestHitShader = Module $ entryPoint @"main" @ClosestHit do
  primitiveID       <- get @"gl_PrimitiveID"
  instanceID        <- get @"gl_InstanceID"
  hitT              <- get @"gl_RayTMax"
  objectToWorld     <- get @"gl_ObjectToWorld"
  worldToObject3x4  <- transpose <<$>> get @"gl_WorldToObject"
  worldRayOrigin    <- get @"gl_WorldRayOrigin"
  worldRayDirection <- get @"gl_WorldRayDirection"

  shaderRecord   <- get @"shaderRecord"
  geometryOffset <- let' $ view @( Name "geometryOffset" ) shaderRecord

  normal <- getNormal @geom objectToWorld worldToObject3x4 worldRayOrigin worldRayDirection hitT ( geometryOffset + primitiveID )

  put @"payload" ( Struct $ fromIntegral primitiveID :& fromIntegral instanceID :& hitT :& normal :& End )
  pure ( Lit () )

primaryClosestHitShader :: forall geom. ( HittableGeometry geom, _ ) => Module ( ClosestHitPrimaryDefs geom )
primaryClosestHitShader = Module $ entryPoint @"main" @ClosestHit do
  primitiveID       <- get @"gl_PrimitiveID"
  hitT              <- get @"gl_RayTMax"
  objectToWorld     <- get @"gl_ObjectToWorld"
  worldToObject3x4  <- transpose <<$>> get @"gl_WorldToObject"
  worldRayOrigin    <- get @"gl_WorldRayOrigin"
  worldRayDirection <- get @"gl_WorldRayDirection"

  accel          <- get @"accel"
  shaderRecord   <- get @"shaderRecord"
  geometryOffset <- let' $ view @( Name "geometryOffset" ) shaderRecord

  hitPos <- let' $ worldRayOrigin ^+^ hitT *^ worldRayDirection
  normal <- getNormal @geom objectToWorld worldToObject3x4 worldRayOrigin worldRayDirection hitT ( geometryOffset + primitiveID )

  def @"quasiRandomConstants" @R  =<< use @( Name "payload" :.: Name "quasiRandomConstants" )
  def @"quasiRandomState"     @RW =<< use @( Name "payload" :.: Name "quasiRandomState"     )

  estimateRadiance accel shaderRecord hitPos normal
