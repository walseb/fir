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
import Data.Proxy
  ( Proxy(..) )
import Data.Typeable
  ( Typeable )
import GHC.TypeNats
  ( Nat )

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.RayTracing.Colour
  ( rgbToSpectrum )
import FIR.Examples.RayTracing.QuasiRandom
  ( random01s )
import FIR.Examples.RayTracing.Types
  ( HitType, BounceDistribution(..), BounceType(..), BounceSide(..)
  , MaterialKind(..), Bindable, MaterialBindingNo
  , bounce
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
     , "extinction"        ':-> V 4 Float -- extinction coefficients per wavelength
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
class ( PrimTy   ( MaterialProperties mat )
      , Typeable mat
      , Bindable mat
      )
    => Material ( mat :: MaterialKind ) where

  type MaterialProperties mat :: Type
  materialKind :: MaterialKind

  sampleMaterialCallableShader :: Module ( MaterialSampleCallableDefs mat )
  queryMaterialCallableShader  :: Module ( MaterialQueryCallableDefs  mat )

--------------------------------------------------------------------------
-- Lambertian material.

instance Material Lambertian where

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
    ~( Vec4 ξ1 ξ2 _ _ ) <- random01s
    sinθ <- let' $ sqrt ξ1
    cosθ <- let' $ sqrt ( 1 - ξ1 )
    φ    <- let' ( 2 * pi * ξ2 )
    -- Use spherical coordinates to return output direction.
    -- Physicist's convention:
    --   - φ: azimuthal angle,
    --   - θ: polar angle.
    dir <- let' . normalise $ sinθ *^ ( cos φ *^ v ^+^ sin φ *^ w ) ^+^ cosθ *^ n

    assign @( Name "callableData" :.: Name "inOutRayDir"      ) dir
    assign @( Name "callableData" :.: Name "sampleType"       ) ( Lit $ bounce Diffuse Reflect Positive )
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
      assign @( Name "callableData" :.: Name "bsdf"  ) =<< colourToValues rgb =<< use @( Name "callableData" :.: Name "wavelengths" )
      assign @( Name "callableData" :.: Name "probs" ) ( pureAST 1.0 :: Code ( V 4 Float ) )

colourToValues :: Code ( V 3 Float ) -> Code ( V 4 Float ) -> Program s s ( Code ( V 4 Float ) )
colourToValues rgb ( Vec4 λ1 λ2 λ3 λ4 ) = do
  -- TODO: vectorise this?
  f1 <- let' =<< rgbToSpectrum rgb λ1
  f2 <- let' =<< rgbToSpectrum rgb λ2
  f3 <- let' =<< rgbToSpectrum rgb λ3
  f4 <- let' =<< rgbToSpectrum rgb λ4
  let' $ Vec4 f1 f2 f3 f4

--------------------------------------------------------------------------
-- Fresnel material.

instance Material Fresnel where
  type MaterialProperties Fresnel =
    Struct
      '[ "colour"  ':-> V 3 Float
       , "iorData" ':-> Array 82 ( V 4 Float )
       ]

  materialKind = Fresnel

  sampleMaterialCallableShader = Module $ entryPoint @"main" @Callable do
    i            <- use @( Name "callableData" :.: Name "materialInfoIndex" )
    fresnelProps <- use @( Name "matProps" :.: Name "props" :.: AnIndex Word32 ) i
    iors <- let' $ view @( Name "iorData" ) fresnelProps

    _ <- def @"quasiRandomConstants" @R  =<< use @( Name "callableData" :.: Name "quasiRandomConstants" )
    _ <- def @"quasiRandomState"     @RW =<< use @( Name "callableData" :.: Name "quasiRandomState"     )

    normal   <- use @( Name "callableData" :.: Name "surfaceNormal" )
    eye      <- use @( Name "callableData" :.: Name "inOutRayDir"   )
    cosθ_inc <- let' $ normalise eye ^.^ normalise normal

    -- Compute whether the light ray is reflected or refracted
    -- using Fresnel equations.
    λ <- use @( Name "callableData" :.: Name "wavelength" )
    ( ~( Vec4 n_exterior k_exterior n_interior k_interior ) :: Code ( V 4 Float ) ) <- refractionIndices iors λ
    ( ~( Vec4 n_inc k_inc n_out k_out ) :: Code ( V 4 Float ), sign ) <-
      let' $
        if   cosθ_inc <= 0 -- are we going from exterior to interior or interior to exterior?
        then ( Vec4 n_exterior k_exterior n_interior k_interior :: Code ( V 4 Float ),  1 ) -- exterior to interior
        else ( Vec4 n_interior k_interior n_exterior k_exterior :: Code ( V 4 Float ), -1 ) -- interior to exterior
    η <- let' $ n_inc / n_out

    out_refl      <- let' . normalise $ normalise eye `reflect'` normalise normal
    abs_cosθ_inc  <- let' $ abs cosθ_inc
    abs_cosθ_refr <- refractionCosine η abs_cosθ_inc
    out_refr      <- refract η eye abs_cosθ_inc abs_cosθ_refr ( sign *^ normal )

    -- In case of total internal reflection, return the reflected vector.
    -- Otherwise, compute the Fresnel coefficients and probabilistically choose between reflected and refracted.
    reflectance_s <- fresnelReflectance S n_inc k_inc abs_cosθ_inc n_out k_out abs_cosθ_refr
    reflectance_p <- fresnelReflectance P n_inc k_inc abs_cosθ_inc n_out k_out abs_cosθ_refr
    reflectance   <- let' $ 0.5 * ( reflectance_s + reflectance_p ) -- Assume light is equally polarised in s and p directions

    ~( Vec4 p_refl _ _ _ ) <- random01s
    if p_refl <= reflectance
    then do
      assign @( Name "callableData" :.: Name "inOutRayDir" ) out_refl
      assign @( Name "callableData" :.: Name "sampleType"  ) $
        if sign > 0
        then ( Lit $ bounce Specular Reflect Positive )
        else ( Lit $ bounce Specular Reflect Negative )
    else do
      assign @( Name "callableData" :.: Name "inOutRayDir" ) out_refr
      assign @( Name "callableData" :.: Name "sampleType"  )
        if sign > 0
        then ( Lit $ bounce Specular Refract Positive )
        else ( Lit $ bounce Specular Refract Negative )

    assign @( Name "callableData" :.: Name "quasiRandomState" ) =<< get @"quasiRandomState"


  queryMaterialCallableShader = Module $ entryPoint @"main" @Callable do

    eye    <- use @( Name "callableData" :.: Name "incomingRayDir" )
    out    <- use @( Name "callableData" :.: Name "outgoingRayDir" )
    normal <- use @( Name "callableData" :.: Name "surfaceNormal"  )

    i            <- use @( Name "callableData" :.: Name "materialInfoIndex" )
    fresnelProps <- use @( Name "matProps" :.: Name "props" :.: AnIndex Word32 ) i
    iors <- let' $ view @( Name "iorData" ) fresnelProps

    ~( Vec4 λ0 λ1 λ2 λ3 ) <- use @( Name "callableData" :.: Name "wavelengths" )

    cosθ_inc     <- let' $ eye ^.^ normal
    sign         <- let' @( Code Float ) $ if cosθ_inc < 0 then 1 else -1
    abs_cosθ_inc <- let' . abs $ cosθ_inc
    ~( Vec4 n0_exterior k0_exterior n0_interior k0_interior ) <- refractionIndices iors λ0
    ~( Vec4 n1_exterior k1_exterior n1_interior k1_interior ) <- refractionIndices iors λ1
    ~( Vec4 n2_exterior k2_exterior n2_interior k2_interior ) <- refractionIndices iors λ2
    ~( Vec4 n3_exterior k3_exterior n3_interior k3_interior ) <- refractionIndices iors λ3

    ( nks :: Code ( Struct '[ "n_inc" ':-> V 4 Float, "k_inc" ':-> V 4 Float, "n_out" ':-> V 4 Float, "k_out" ':-> V 4 Float ] ) ) <- 
      let' $
        if cosθ_inc <= 0 -- are we going from exterior to interior or interior to exterior?
        then -- exterior to interior
          Struct
            (  Vec4 n0_exterior n1_exterior n2_exterior n3_exterior
            :& Vec4 k0_exterior k1_exterior k2_exterior k3_exterior
            :& Vec4 n0_interior n1_interior n2_interior n3_interior
            :& Vec4 k0_interior k1_interior k2_interior k3_interior
            :& End
            )
        else -- interior to exterior
          Struct
            (  Vec4 n0_interior n1_interior n2_interior n3_interior
            :& Vec4 k0_interior k1_interior k2_interior k3_interior
            :& Vec4 n0_exterior n1_exterior n2_exterior n3_exterior
            :& Vec4 k0_exterior k1_exterior k2_exterior k3_exterior
            :& End
            )
    n_incs <- let' $ view @( Name "n_inc" ) nks
    k_incs <- let' $ view @( Name "k_inc" ) nks
    n_outs <- let' $ view @( Name "n_out" ) nks
    k_outs <- let' $ view @( Name "k_out" ) nks
    ηs@( ~( Vec4 η0 η1 η2 η3 ) ) <- let' $ (/) <$$> n_incs <**> n_outs

    let
      computeReflectance
        :: forall ( i :: Nat ) ( s :: ProgramState )
        .  ( ( i :< 4 ) ~ True, _ )
        => Proxy i -> Program s s ( Code Float, Code Float )
      -- N.B.: the Proxy argument here works around https://gitlab.haskell.org/ghc/ghc/-/issues/20921
      computeReflectance _ = do
        abs_cosθ_refr <- refractionCosine ( view @( Index i ) ηs ) abs_cosθ_inc
        reflectance_s <-
          fresnelReflectance S
            ( view @( Index i ) n_incs ) ( view @( Index i ) k_incs ) abs_cosθ_inc
            ( view @( Index i ) n_outs ) ( view @( Index i ) k_outs ) abs_cosθ_refr
        reflectance_p <-
          fresnelReflectance P
            ( view @( Index i ) n_incs ) ( view @( Index i ) k_incs ) abs_cosθ_inc
            ( view @( Index i ) n_outs ) ( view @( Index i ) k_outs ) abs_cosθ_refr
        let' $ ( abs_cosθ_refr, 0.5 * ( reflectance_s + reflectance_p ) )

    ( abs_cosθ_refr0, reflectance0 ) <- computeReflectance @0 Proxy
    ( abs_cosθ_refr1, reflectance1 ) <- computeReflectance @1 Proxy
    ( abs_cosθ_refr2, reflectance2 ) <- computeReflectance @2 Proxy
    ( abs_cosθ_refr3, reflectance3 ) <- computeReflectance @3 Proxy
    reflectance <- let' $ Vec4 reflectance0 reflectance1 reflectance2 reflectance3

    ( vals, probs ) <-
      if ( eye ^.^ normal ) * ( out ^.^ normal ) <= 0
      then do -- reflection: outgoing vector must be reflection of incoming vector with respect to surface normal
        res <- let' $
          if out `closeTo` ( eye `reflect'` normal )
          then reflectance
          else pureAST 0 :: Code ( V 4 Float )
        pure ( res, res )
      else do -- refraction: outgoing vector must be refraction of incoming vector with respect to surface normal
        assign @( Name "callableData" :.: Name "extinction" ) k_outs
        refr0 <- refract η0 eye abs_cosθ_inc abs_cosθ_refr0 ( sign *^ normal )
        refr1 <- refract η1 eye abs_cosθ_inc abs_cosθ_refr1 ( sign *^ normal )
        refr2 <- refract η2 eye abs_cosθ_inc abs_cosθ_refr2 ( sign *^ normal )
        refr3 <- refract η3 eye abs_cosθ_inc abs_cosθ_refr3 ( sign *^ normal )
        res <- let' $ Vec4
          ( if out `closeTo` refr0 then ( 1 - reflectance0 ) else 0 )
          ( if out `closeTo` refr1 then ( 1 - reflectance1 ) else 0 )
          ( if out `closeTo` refr2 then ( 1 - reflectance2 ) else 0 )
          ( if out `closeTo` refr3 then ( 1 - reflectance3 ) else 0 )
        let'
          ( ( \ η r -> r / ( η * η * η ) ) <$$> ηs <**> res -- solid angle and wavelength compression factor 
          , res                                             -- (see Veach's thesis p.183 "Reciprocity for spectral radiance")
          )

    assign @( Name "callableData" :.: Name "probs" ) probs
    assign @( Name "callableData" :.: Name "bsdf"  ) vals

    pure ( Lit () )


--------------------------------------------------------------------------------

closeTo :: Code ( V 3 Float ) -> Code ( V 3 Float ) -> Code Bool
closeTo v1 v2 = abs ( v1 ^.^ v2 - 1 ) < 0.01

-- | Returns the cosine of the angle of refraction relative to the normal.
refractionCosine
  :: Code Float         -- ^ Relative index of refraction: incoming side IOR / refraction side IOR.
  -> Code Float         -- ^ Cosine of incoming angle relative to the normal.
  -> Program s s ( Code Float )
refractionCosine η cosθ_inc =
  let' . sqrt . max 0 $ 1 - η * η * ( 1 - cosθ_inc * cosθ_inc )

-- | Refract along a surface with given normal vector and relative index of refraction.
refract
  :: Code Float         -- ^ Relative index of refraction: incoming side IOR / refraction side IOR.
  -> Code ( V 3 Float ) -- ^ Incoming vector, pointing into the surface.
  -> Code Float         -- ^ Cosine of incoming angle relative to the normal.
  -> Code Float         -- ^ Cosine of refracted angle relative to normal (as computed by 'refractionCosine').
  -> Code ( V 3 Float ) -- ^ Normal vector of the surface.
  -> Program s s ( Code ( V 3 Float ) )
refract η eye cosθ_inc cosθ_refr normal = do
  let' $ ( normalise $ η *^ eye ^+^ ( η * cosθ_inc - cosθ_refr ) *^ normal )

fresnelReflectance
  :: Polarisation
  -> Code Float -> Code Float -> Code Float
  -> Code Float -> Code Float -> Code Float
  -> Program s s ( Code Float )
fresnelReflectance pol n_inc k_inc cosθ_inc n_out k_out cosθ_out = do
  z_inc    <- let' ( n_inc :+: k_inc )
  z_out    <- let' ( n_out :+: k_out )
  fraction <- let' $ ( scale cosθ_1 z_inc - scale cosθ_2 z_out )
                   / ( scale cosθ_1 z_inc + scale cosθ_2 z_out )
  let' . clamp . magnitude $ fraction * fraction
    where
      clamp :: Code Float -> Code Float
      clamp x = if isNaN x then 1 else max 0 ( min 1 x )
      cosθ_1, cosθ_2 :: Code Float
      ( cosθ_1, cosθ_2 ) = case pol of
        S -> ( cosθ_inc, cosθ_out )
        P -> ( cosθ_out, cosθ_inc )

data Polarisation
  = S
  | P

refractionIndices :: Code ( Array 82 ( V 4 Float ) ) -> Code Float -> Program s s ( Code ( V 4 Float ) )
refractionIndices iors λ = do
  l <- let' @( Code Float  ) $ 0.2 * ( λ - 380 )
  i <- let' @( Code Word32 ) $ floor l
  s <- let' @( Code Float  ) $ l - fromIntegral i
  let' $ ( 1 - s ) *^ view @( AnIndex ( Code Word32 ) )   i       iors
     ^+^       s   *^ view @( AnIndex ( Code Word32 ) ) ( i + 1 ) iors
