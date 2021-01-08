{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module FIR.Examples.RayTracing.Sky
  ( ScatteringInfo
  , inScattering, skyScatter, sunIntensity
  ) where

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.RayTracing.Types
  ( RayleighParams, MieParams, SunParams )

--------------------------------------------------------------------------

type ScatteringInfo = Struct
  '[ "β_rs"  ':-> V 4 Float
   , "β_ms"  ':-> V 4 Float
   , "f_exs" ':-> V 4 Float
   ]

skyScatter
  :: Code RayleighParams
  -> Code MieParams
  -> Code SunParams
  -> Code ( V 3 Float ) -- ^ World ray direction.
  -> Code ( V 4 Float ) -- ^ Wavelengths.
  -> Program s s ( Code ScatteringInfo )
skyScatter rayleighParams mieParams sunParams rayDir λs = do

  sunPos   <- let' $ view @( Name "position" ) sunParams
  sunPos_y <- let' . negate $ view @( Swizzle "y" ) sunPos

  rayleighIntensity <- let' $ view @( Name "intensity" ) rayleighParams
  rayleighCoeff <- let' $ rayleighIntensity - ( min 1 . max 0 ) ( 1 - exp ( sunPos_y / 4.5e5 ) )
  β_rs <- ( rayleighCoeff *^ ) <<$>> β_rayleigh rayleighParams λs
  β_ms <- β_mie mieParams λs

  cosω  <- let' $ max 0 ( negate $ view @( Swizzle "y" ) rayDir )
  ω     <- let' $ acos cosω
  denom <- let' . recip $ cosω + 0.15 * ( 93.885 - ( ω * 180 / pi ) ) ** -1.253

  s_r <- let' $ denom * view @( Name "zenithLength" ) rayleighParams
  s_m <- let' $ denom * view @( Name "zenithLength" )      mieParams
 
  -- Combined extinction factor.
  f_exs <- let' $ ( \ β_r β_m -> exp ( - ( s_r * β_r + s_m * β_m ) ) ) <$$> β_rs <**> β_ms

  pure $ Struct ( β_rs :& β_ms :& f_exs :& End )

inScattering
  :: Code MieParams
  -> Code SunParams
  -> Code ( V 3 Float ) -- ^ World ray direction.
  -> Code ScatteringInfo
  -> Program s s ( Code ( V 4 Float ) )
inScattering mieParams sunParams rayDir scatteringInfo = do

  sunPos <- let' $ view @( Name "position" ) sunParams

  β_rs   <- let' $ view @( Name "β_rs"  ) scatteringInfo
  β_ms   <- let' $ view @( Name "β_ms"  ) scatteringInfo
  f_exs  <- let' $ view @( Name "f_exs" ) scatteringInfo

  -- In-scattering.
  sunDir <- let' $ normalise sunPos
  cosθ   <- let' $ sunDir ^.^ rayDir

  βθ_rs  <- let' $ β_rs ^* rayleighPhase ( 0.5 * ( cosθ + 1 ) )
  βθ_ms  <- let' . ( *^ β_ms ) =<< henyeyGreensteinPhase mieParams cosθ

  sunDir_y <- let' . negate $ view @( Swizzle "y" ) sunDir
  ξ        <- let' . acos $ sunDir_y
  sun_e    <- sunIntensity sunParams ξ

  αs <- let' $ sun_e *^ ( (/) <$$> ( βθ_rs ^+^ βθ_ms ) <**> ( β_rs ^+^ β_ms ) )
  t  <- let' $ ( max 0 ( 1.0 - sunDir_y ) ) ** 5
  let' @( Code ( V 4 Float ) ) $
    ( \ f_ex α -> ( ( 1 - f_ex ) * α ) ** 1.5 * ( 1 + t * ( ( f_ex * α ) ** 0.5 - 1 ) ) ) <$$> f_exs <**> αs

β_rayleigh :: Code RayleighParams -> Code ( V 4 Float ) -> Program s s ( Code ( V 4 Float ) )
β_rayleigh params λs = do

  numMolecules   <- let' $ view @( Name "numMolecules"   ) params
  depolarisation <- let' $ view @( Name "depolarisation" ) params
  ior            <- let' $ view @( Name "ior"            ) params

  ior²_m_1   <- let' $ ior * ior - 1
  λ²s        <- let' $ (*) <$$> λs  <**> λs
  λ⁴s        <- let' $ (*) <$$> λ²s <**> λ²s
  coeff      <- let' $ 8 * pi * pi * pi * ( 6 + 3 * depolarisation )
  let' $
    ( \ λ⁴ -> ior²_m_1 * ior²_m_1 * coeff / ( 3 * numMolecules * λ⁴ * ( 6 - 7 * depolarisation ) ) ) <$$> λ⁴s


β_mie :: Code MieParams -> Code ( V 4 Float ) -> Program s s ( Code ( V 4 Float ) )
β_mie params λs = do
  turbidity <- let' $ view @( Name "turbidity" ) params
  v         <- let' $ view @( Name "v"         ) params
  w         <- let' $ view @( Name "weight"    ) params
  k_array   <- let' $ view @( Name "k_array"   ) params
  ks        <- interpolate k_array λs 
  let' $ ( \ λ k -> k * w * turbidity * 8.68e-19 * pi * ( 2 * pi / λ ) ** ( v - 2 ) ) <$$> λs <**> ks

rayleighPhase :: Code Float -> Code Float
rayleighPhase c = 3 * ( 1 + c * c ) / ( 16 * pi )

henyeyGreensteinPhase :: Code MieParams -> Code Float -> Program s s ( Code Float )
henyeyGreensteinPhase params c = do
  g  <- let' $ view @( Name "directionalG" ) params
  g² <- let' $ g * g
  let' $ ( 1 - g² ) / ( 4 * pi * ( 1 - 2 * g * c + g² ) ** 1.5 )

sunIntensity :: Code SunParams -> Code Float -> Program s s ( Code Float )
sunIntensity sunParams θ = do
  falloff   <- let' $ view @( Name "falloff"   ) sunParams
  intensity <- let' $ view @( Name "intensity" ) sunParams
  let' . ( * intensity ) . max 0 $ 1 - exp ( ( θ - cutoffAngle ) / falloff )
    where
      cutoffAngle :: Code Float
      cutoffAngle = pi / 1.95

interpolate :: Code ( Array 82 Float ) -> Code ( V 4 Float ) -> Program s s ( Code ( V 4 Float ) )
interpolate arr ( Vec4 λ0 λ1 λ2 λ3 ) = do
  r0 <- interp λ0
  r1 <- interp λ1
  r2 <- interp λ2
  r3 <- interp λ3
  pure $ Vec4 r0 r1 r2 r3
  where
    interp :: Code Float -> Program s s ( Code Float )
    interp λ =
      if λ < 380 || λ > 780
      then pure 0
      else do
        l <- let' @( Code Float  ) $ 0.2 * ( λ - 380 )
        i <- let' @( Code Word32 ) $ floor l
        s <- let' @( Code Float  ) $ l - fromIntegral i
        t <- let' @( Code Float  ) $ 1 - s
        let' $ t * view @( AnIndex ( Code Word32 ) )   i       arr
             + s * view @( AnIndex ( Code Word32 ) ) ( i + 1 ) arr
