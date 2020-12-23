{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module FIR.Examples.RayTracing.Shaders where

-- base
import qualified Prelude
import Control.Arrow
  ( first )
import Control.Monad
  ( void )
import Data.Foldable
  ( for_, toList )
import Data.Functor
  ( (<&>) )
import Data.Functor.Classes
  ( Show1(..) )
import Data.Functor.Compose
  ( Compose(..) )
import Data.Functor.Identity
  ( Identity(..) )

-- bifunctors
import qualified Data.Bifunctor.Biff as Bifunctor
  ( Biff(..) )
import qualified Data.Bifunctor.Join as Bifunctor
  ( Join(..) )
import qualified Data.Bitraversable  as Bifunctor
  ( Bitraversable(..) )

-- filepath
import System.FilePath
  ( (</>) )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import FIR
  hiding ( Triangle )

-- fir-examples
import FIR.Examples.Paths
  ( shaderDir )
import FIR.Examples.RayTracing.Colour
  ( xyzToRGB )
import FIR.Examples.RayTracing.Geometry
  ( HittableGeometry(..)
  , occlusionClosestHitShader, primaryClosestHitShader
  )
import FIR.Examples.RayTracing.Luminaire
  ( Emitter(..)
  , LightSamplingMethod(..), SampleableGeometry(..)
  )
import FIR.Examples.RayTracing.Material
  ( Material(..) )
import FIR.Examples.RayTracing.Rays
  ( raygenShader
  , occlusionMissShader, primaryMissShader
  )
import FIR.Examples.RayTracing.Types

{--------------------------------------------------------------------------
-- Descriptor set information

--------------------------------
-- Ray-tracing descriptor sets

Descriptor set 0:
  - Binding 0: UBO
  - Binding 1: Acceleration structure
  - Binding 2: storage buffer of all light sources in the scene,
               each with information about the appropriate callable shaders
               and light source information
  - one binding for each geometry type supported,
    as specified by the GeometryBindingNo type family;
    each of these is a storage buffer holding geometry data
    (e.g. center and radius for sphere geometry)
  - one binding for each luminaire type supported,
    as specified by the LuminaireBindingNo type family;
    each of these is a storage buffer holding luminaire data
    (e.g. temperature for a blackbody radiator)
  - one binding for each material type supported,
    as specified by the MaterialBindingNo type family;
    each of these is a storage buffer holding material data
    (e.g. index of refraction for a Fresnel material)

Descriptor set 1:
  - Binding 0: input image data

Descriptor set 2:
  - Binding 0: output image data
  - Binding 1: output log-luminance data

--------------------------------
-- Tone-mapping descriptor sets

Descriptor set 0:
  - Binding 0: image data

[This corresponds to descriptor set 2 above]

Descriptor set 1:
  - Binding 0: present image
  - Binding 1: mip-mapped log-luminance data

-}--------------------------------------------------------------------------
-- Compute shader for tone-mapping.

type TonemapDefs =
  '[ "data"  ':-> Image2D    '[ DescriptorSet 0, Binding 0, NonWritable ] ( RGBA32 F )
   , "image" ':-> Image2D    '[ DescriptorSet 1, Binding 0, NonReadable ] ( RGBA8 UNorm )
   , "mips"  ':-> Texture2D  '[ DescriptorSet 1, Binding 1 ] ( RGBA32 F )
   , "main"  ':-> EntryPoint '[ LocalSize LocalSizeX LocalSizeY 1 ] Compute
   ]

tonemapComputeShader :: Module TonemapDefs
tonemapComputeShader = Module $ entryPoint @"main" @Compute do
  ~( Vec2 ix iy ) <- use @( Name "gl_GlobalInvocationID" :.: Swizzle "xy" )
  ix_mips <- let' @Float $ fromIntegral ix / width
  iy_mips <- let' @Float $ fromIntegral iy / height

  ~( Vec4 nx ny nz n ) <- imageRead @"data" ( Vec2 ix iy )
  x <- let' $ nx / n
  y <- let' $ ny / n
  z <- let' $ nz / n

  -- Reinhard tone mapping using average log-luminance.
  ~( Vec4 _ _ _ avgLogLuminance ) <-
    use @( ImageTexel "mips" )
      ( LOD ( 0 :: Code Float ) NilOps )
      ( Vec2 ix_mips iy_mips )
  y_rh <- let' $ y * exp ( -avgLogLuminance )
  y'   <- let' $ y_rh / ( 1 + y_rh )
  c    <- let' $ y' / y

  ~( Vec3 r g b ) <- let' $ xyzToRGB ( Vec3 ( c * x ) y' ( c * z ) )
  imageWrite @"image" ( Vec2 ix iy ) ( Vec4 r g b 1 )

--------------------------------------------------------------------------
-- Collect up all shaders.

data Shaders a
  = Shaders
  { tonemapCompute
  -- raygen shaders
  , raygen
  -- then miss shaders
  , primaryMiss
  , occlusionMiss
  -- then callable shaders
  , emitter_Blackbody_Callable
  , sample_Triangle_SurfaceArea_Callable
  , sample_Sphere_SurfaceArea_Callable
  , materialSample_Lambertian_Callable
  , materialQuery_Lambertian_Callable
  -- then hit shader groups
  , primary_Triangle_HitGroup
  , occlusion_Triangle_HitGroup
  , primary_Sphere_HitGroup
  , occlusion_Sphere_HitGroup
    :: !a
  }
  deriving stock ( Prelude.Show, Prelude.Functor, Prelude.Foldable, Prelude.Traversable )
instance Show1 Shaders where
  liftShowsPrec shwP shwL p as = liftShowsPrec shwP shwL p ( toList as )

data AnyProgram where
  AnyProgram :: CompilableProgram prog => prog -> AnyProgram

newtype ShaderGroups a = ShaderGroups { shaderGroups :: Shaders ( Either a ( ShaderGroup a ) ) }
  deriving ( Prelude.Functor, Prelude.Foldable )
    via Compose Shaders ( Bifunctor.Join ( Bifunctor.Biff Either Identity ShaderGroup ) )
instance Prelude.Traversable ShaderGroups where
  traverse f ( ShaderGroups shads ) = ShaderGroups Prelude.<$>
    Prelude.traverse ( Bifunctor.bitraverse f ( Prelude.traverse f ) ) shads

shaders :: ShaderGroups ( FilePath, AnyProgram )
shaders = Prelude.fmap ( first ( shaderDir </> ) ) $ ShaderGroups $ Shaders
  { tonemapCompute                       = Left
                                           ( "rt_tonemapCompute.spv"
                                           , AnyProgram tonemapComputeShader
                                           )
  , raygen                               = Right $ RaygenGroup
                                           ( "rt_raygen.spv"
                                           , AnyProgram raygenShader
                                           )
  , primaryMiss                          = Right $ MissGroup
                                           ( "rt_primaryMiss.spv"
                                           , AnyProgram primaryMissShader
                                           )
  , occlusionMiss                        = Right $ MissGroup
                                           ( "rt_occlusionMiss.spv"
                                           , AnyProgram occlusionMissShader
                                           )
  , emitter_Blackbody_Callable           = Right $ CallableGroup
                                           ( "rt_emitter_Blackbody_Callable.spv"
                                           , AnyProgram ( emitterCallableShader @Blackbody )
                                           )
  , sample_Triangle_SurfaceArea_Callable = Right $ CallableGroup
                                           ( "rt_sample_Triangle_SurfaceArea_Callable.spv"
                                           , AnyProgram ( lightSampleCallableShader @Triangle @SurfaceArea )
                                           )
  , sample_Sphere_SurfaceArea_Callable   = Right $ CallableGroup
                                           ( "rt_sample_Sphere_SurfaceArea_Callable.spv"
                                           , AnyProgram ( lightSampleCallableShader @Sphere @SurfaceArea )
                                           )
  , materialSample_Lambertian_Callable   = Right $ CallableGroup
                                           ( "rt_materialSample_Lambertian_Callable.spv"
                                           , AnyProgram ( sampleMaterialCallableShader @Lambertian )
                                           )
  , materialQuery_Lambertian_Callable    = Right $ CallableGroup
                                           ( "rt_materialQuery_Lambertian_Callable.spv"
                                           , AnyProgram ( queryMaterialCallableShader @Lambertian )
                                           )
  , primary_Triangle_HitGroup            = Right $ HitGroup
                                         { intersection = Nothing
                                         , closestHit   = Just
                                                          ( "rt_primary_Triangle_ClosestHit.spv"
                                                          , AnyProgram ( primaryClosestHitShader @Triangle )
                                                          )
                                         , anyHit       = Nothing
                                         }
  , occlusion_Triangle_HitGroup          = Right $ HitGroup
                                         { intersection = Nothing
                                         , closestHit   = Just
                                                          ( "rt_occlusion_Triangle_ClosestHit.spv"
                                                          , AnyProgram ( occlusionClosestHitShader @Triangle )
                                                          )
                                         , anyHit       = Nothing
                                         }
  , primary_Sphere_HitGroup              = Right $ HitGroup
                                         { intersection = Just
                                                          ( "rt_sphereIntersection_Primary.spv"
                                                          , AnyProgram ( intersectionShader @Sphere )
                                                          )
                                         , closestHit   = Just
                                                          ( "rt_primary_Sphere_ClosestHit.spv"
                                                          , AnyProgram ( primaryClosestHitShader @Sphere )
                                                          )
                                         , anyHit       = Nothing
                                         }
  , occlusion_Sphere_HitGroup            = Right $ HitGroup
                                         { intersection = Just
                                                          ( "rt_sphereIntersection_Occlusion.spv"
                                                          , AnyProgram ( intersectionShader @Sphere )
                                                          )
                                         , closestHit   = Just
                                                          ( "rt_occlusion_Sphere_ClosestHit.spv"
                                                          , AnyProgram ( occlusionClosestHitShader @Sphere )
                                                          )
                                         , anyHit       = Nothing
                                         }
  }

allShaderCompilations :: ShaderGroups ( ShortText, IO ( Either ShortText ModuleRequirements ) )
allShaderCompilations = shaders <&> \ ( path, AnyProgram prog ) -> ( ShortText.pack path, compileTo path [Debug, Assert] prog )

compileAllShaders :: IO ()
compileAllShaders = for_ allShaderCompilations \ ( _, compileIt ) -> void compileIt
