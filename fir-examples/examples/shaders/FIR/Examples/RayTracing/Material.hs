{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module FIR.Examples.RayTracing.Material
  ( Material(..)
  , MaterialSampleCallableData, MaterialSampleCallableDefs
  , MaterialQueryCallableData , MaterialQueryCallableDefs
  )
  where

-- base
import Data.Kind
  ( Type )
import Data.Typeable
  ( Typeable )
import GHC.TypeNats
  ( Nat, KnownNat )

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.RayTracing.Colour
  ( rgbToSpectrum )
import FIR.Examples.RayTracing.QuasiRandom
  ( random01s )
import FIR.Examples.RayTracing.Types
  ( HitType, pattern Diffuse
  , MaterialKind(..)
  )

--------------------------------------------------------------------------

-- | Input & output data relevant to sampling of a material BSDF.
type MaterialSampleCallableData =
  Struct
    '[ "materialInfoIndex"    ':-> Word32
     , "quasiRandomConstants" ':-> V 4 Float
     , "quasiRandomState"     ':-> V 4 Float
     , "surfaceNormal"        ':-> V 3 Float
     , "wavelength"           ':-> Float
     , "inOutRayDir"          ':-> V 3 Float
     , "sampleType"           ':-> HitType
     ]

-- | Input & output data relevant to querying a material BSDF.
type MaterialQueryCallableData =
  Struct
    '[ "materialInfoIndex" ':-> Word32
     , "surfaceNormal"     ':-> V 3 Float
     , "incomingRayDir"    ':-> V 3 Float
     , "outgoingRayDir"    ':-> V 3 Float
     , "wavelengths"       ':-> V 4 Float
     , "bsdf"              ':-> V 4 Float -- BSDF value per wavelength (ratio of incoming vs outgoing radiance, with respect to projected solid angle)
     , "probs"             ':-> V 4 Float -- associated probability distribution per wavelength (Radon–Nikodym derivatives)
     ]

type MaterialSampleCallableDefs ( mat :: MaterialKind ) =
  '[ "callableData" ':-> CallableDataIn '[] MaterialSampleCallableData
   , "matProps"     ':-> StorageBuffer  '[ DescriptorSet 0, Binding ( MaterialBindingNo mat ), NonWritable ]
                            ( Struct '[ "props" ':-> RuntimeArray ( MaterialProperties mat ) ] )
   , "main"         ':-> EntryPoint     '[] Callable
   ]

type MaterialQueryCallableDefs ( mat :: MaterialKind ) =
  '[ "callableData" ':-> CallableDataIn '[] MaterialQueryCallableData
   , "matProps"     ':-> StorageBuffer  '[ DescriptorSet 0, Binding ( MaterialBindingNo mat ), NonWritable ]
                            ( Struct '[ "props" ':-> RuntimeArray ( MaterialProperties mat ) ] )
   , "main"         ':-> EntryPoint     '[] Callable
   ]

-- Class for materials with a BSDF that can be sampled.
--
-- A material BSDF operates with respect to some internal probability measure μ.
-- It can be called in two ways:
--
--  Sampling mode.
--    - given eye direction dir_eye and λ,
--      sample an outgoing direction dir with respect to μ.
--
--  Query mode.
--    - given directions dir_eye, dir, and wavelengths ν, return:
--      - the value of the BSDF ( dir --> dir_eye, ν ),
--      - the value of the Radon–Nikodym derivative p(dir,ν) = dσ⟂/dμ(dir,ν)
class ( KnownNat ( MaterialBindingNo  mat )
      , PrimTy   ( MaterialProperties mat )
      , Typeable mat
      )
    => Material ( mat :: MaterialKind ) where

  type MaterialBindingNo  mat = ( bdNo :: Nat ) | bdNo -> mat
  type MaterialProperties mat :: Type
  materialKind :: MaterialKind

  sampleMaterialCallableShader :: Module ( MaterialSampleCallableDefs mat )
  queryMaterialCallableShader  :: Module ( MaterialQueryCallableDefs  mat )

--------------------------------------------------------------------------

instance Material Lambertian where

  type MaterialBindingNo  Lambertian = 6
  type MaterialProperties Lambertian =
    Struct
      '[ "colour" ':-> V 3 Float ]
  materialKind = Lambertian

  -- | Sample positive hemisphere uniformly with respect to projected solid angle.
  sampleMaterialCallableShader = Module $ entryPoint @"main" @Callable do

    n   <- use @( Name "callableData" :.: Name "surfaceNormal" )
    eye <- use @( Name "callableData" :.: Name "inOutRayDir"   )

    _ <- def @"quasiRandomConstants" @R  =<< use @( Name "callableData" :.: Name "quasiRandomConstants" )
    _ <- def @"quasiRandomState"     @RW =<< use @( Name "callableData" :.: Name "quasiRandomState"     )

    -- Obtain a Gram–Schmidt basis.
    v0 <- let' $ ( eye ^.^ n ) *^ n ^-^ eye
    v  <- let' $ normalise v0
    w0 <- let' $ v `cross` n
    w  <- let' $ normalise w0
    -- Obtain random angles.
    ~( Vec4 sinθ f _ _ ) <- random01s
    cosθ <- let' ( sqrt $ 1 - sinθ * sinθ )
    φ    <- let' ( 2 * pi * f )
    -- Use spherical coordinates to return output direction.
    -- Physicist's convention:
    --   - φ: azimuthal angle,
    --   - θ: polar angle.
    --
    -- Uniform choice of sinθ (as above) leads to clustering at the pole, which is exactly what we want.
    dir <- let' . normalise $ sinθ *^ ( cos φ *^ v ^+^ sin φ *^ w ) ^+^ cosθ *^ n

    assign @( Name "callableData" :.: Name "inOutRayDir"      ) dir
    assign @( Name "callableData" :.: Name "sampleType"       ) ( Lit Diffuse )
    assign @( Name "callableData" :.: Name "quasiRandomState" ) =<< get @"quasiRandomState"

  queryMaterialCallableShader = Module $ entryPoint @"main" @Callable do
    n   <- use @( Name "callableData" :.: Name "surfaceNormal"  )
    out <- use @( Name "callableData" :.: Name "outgoingRayDir" )

    if ( n ^.^ out ) < -1e-7
    then do
      assign @( Name "callableData" :.: Name "bsdf"  ) ( pureAST 0 :: Code ( V 4 Float ) )
      assign @( Name "callableData" :.: Name "probs" ) ( pureAST 0 :: Code ( V 4 Float ) )
    else do
      i   <- use @( Name "callableData" :.: Name "materialInfoIndex" )
      rgb <- use @( Name "matProps" :.: Name "props" :.: AnIndex Word32 :.: Name "colour" ) i
      ~( Vec4 λ1 λ2 λ3 λ4 ) <- use @( Name "callableData" :.: Name "wavelengths" )
      -- TODO: vectorise this?
      f1 <- let' =<< ( rgbToSpectrum rgb λ1 )
      f2 <- let' =<< ( rgbToSpectrum rgb λ2 )
      f3 <- let' =<< ( rgbToSpectrum rgb λ3 )
      f4 <- let' =<< ( rgbToSpectrum rgb λ4 )
      assign @( Name "callableData" :.: Name "bsdf"  ) ( Vec4 f1 f2 f3 f4 )
      assign @( Name "callableData" :.: Name "probs" ) ( pureAST 1.0 :: Code ( V 4 Float ) )
