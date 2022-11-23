{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedWildCards           #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE PartialTypeSignatures    #-}
{-# LANGUAGE RebindableSyntax         #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module FIR.Examples.Klein.Shaders where

-- base
import Prelude ( Char, String )
import Data.Foldable
  ( sequence_ )
import Data.Maybe
  ( fromJust )
import GHC.TypeNats
  ( KnownNat )

-- filepath
import System.FilePath
  ( (</>) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

-- fir-examples
import FIR.Examples.Paths
  ( shaderDir )

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]

-------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   , "main"         ':-> EntryPoint '[            ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
    ~(Vec3 x y z) <- get @"in_position"
    put @"gl_Position" (Vec4 x y z 1)

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "out_colour"  ':-> Output     '[ Location 0      ] (V 4 Float)
   , "ubo"         ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mousePos" ':-> V 2 Float ] )
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

maxDepth :: Code Word32
maxDepth = 512

xSamples, ySamples :: Code Word32
xSamples = 4
ySamples = 4

xWidth, yWidth :: Code Float
xWidth = recip . fromIntegral $ xSamples
yWidth = recip . fromIntegral $ ySamples

aaScale :: Code Float
aaScale = xWidth * yWidth

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do

    ~( Vec4 ix iy _ _ ) <- #gl_FragCoord
    ( rescaleSliceParam -> t@(~(mp_x :+: mp_y)) ) <- use @(Name "ubo" :.: Name "mousePos")
    --( rescaleSliceParam -> t@(~(u :+: v)) ) <- use @(Name "ubo" :.: Name "mousePos")

    let u, v :: Code Float
        u = 2 -- 1.63317 -- 1.658312 --1.6421387686534761242 --1.639957 --1.5363939401393656 --1.64213876 --1.958591 --2 --1.732050808 --1.958591 --2 --1.76110014954012 --1.926434053 --1.64213876  --1.90378
        v = 0 --0.680552  --0.7665884174654594068 --0.863393 --0.6602608265537334 --0.76658841 --0.011278 --0 --1           --0.011278 --0 --0.20905593312013 --0.027381792 --0.76658841  --0.03958
        t :: CodeComplex Float
        t = u :+: v

    #col #= ( Vec4 0 0 0 1 :: Code (V 4 Float) )

    supersamplingLoop \ xNo yNo -> locally do

      let
        dx, dy :: Code Float
        dx = ( fromIntegral xNo + 0.5 ) * xWidth - 0.5
        dy = ( fromIntegral yNo + 0.5 ) * xWidth - 0.5

      #z    #= codeComplex ( mkRescaledComplex ( Vec2 (ix + dx) (iy + dy) ) )
      ( CodeComplex -> z0 ) <- #z
      #lz   #= codeComplex ( z0 + (1 :+: 1) )
      #llz  #= codeComplex ( z0 - (1 :+: 1) )
      #depth #= ( 1 :: Code Word32 )
      #scale #= ( 1 :: Code Float )
      #flag  #= ( 1 :: Code Float )

      #hit #= ( Vec4 0 (fromIntegral maxDepth) 0 1 :: Code ( V 4 Float ) )

      doInversion

--------------------------------------------------------------------------------

      --( CodeComplex -> z0 ) <- #z
      --c0 <- let' $ (2-1.233411582534540593) :+: 0.642138768653476124
      --c1 <- applyWord "BAA"    t c0
      --c2 <- applyWord "BAABAA" t c0
      --c3 <- applyWord "BA"     t c0
      --c4 <- applyWord "BAABA"  t c0
      --if ( magnitude (z0 - c0) < 0.01 || magnitude (z0 - c1) < 0.01 || magnitude (z0 - c2) < 0.01 || magnitude (z0 - c3) < 0.01 || magnitude (z0 - c4) < 0.01 )
      --then do
      --  #col .= Vec4 0 0.8 1 1
      --  pure ()
      --else do

      --p <- let' $ 0 :+: 0
      --if ( magnitude (z0 - p) < 0.01 )
      --then do
      --  #col .= Vec4 1 0 1 1
      --  pure ()
      --else do


--------------------------------------------------------------------------------

      loop do

        depth <- #depth
        flag  <- #flag
        scale <- #scale

        ( CodeComplex -> z   ) <- #z
        ( CodeComplex -> llz ) <- #llz

        z@(~( x :+: y )) <- wrapHorizontal t z

        above <- isAboveSeparation t z
        when above do
          #z .= codeComplex ( ( negate v - x ) :+: ( u - y ) )
          #flag %= (+1)

        -- Apply Maskit transformation
        ( CodeComplex -> ~(x :+: y) ) <- #z
        ( codeComplex -> z' ) <- applyWordWithScale "a" t (x :+: y)
        #z .= z'

        ( CodeComplex -> z@(~(x :+: y)) ) <- #z
        scale <- #scale

        when ( magnitude (z - llz) < 1e-5 ) do
          #hit .= Vec4 (fromIntegral maxDepth) (fromIntegral maxDepth) 0.001 scale
          break @1
        when ( y < 0 ) do
          #hit .= Vec4 flag (fromIntegral depth) (negate y) scale
          break @1
        when ( y > u ) do
          #hit .= Vec4 flag (fromIntegral depth) (y - u) scale
          break @1
        when ( depth >= maxDepth ) do
          #hit .= Vec4 flag (fromIntegral depth) y scale
          break @1

        z  <- #z
        lz <- #lz
        #llz .= lz
        #lz  .= z

        #depth %= (+1)

      hit@(~(Vec4 flag depth y scale)) <- #hit
      ( CodeComplex -> z@(~( xf :+: yf )) ) <- #z
      --let r = magnitude z
      --    t = phase z
      let d = depth / fromIntegral maxDepth
      col <- if magnitude z0 > 1.0001 -- 1.01 --magnitude ( z0 - ( 0.5 :+: 0 ) ) > 0.5
             then pure $ Vec4 0 0 0 0
             else --pure $ Vec4 xf yf (negate yf) 1
               --if   flag < 0.5
               --then pure $ Vec4 0 0 0 0
               --else
                 hsvToRgb (Vec4 ( mp_x + phase ( xf :+: y ) / pi ) 1 ( 0.5 * magnitude ( xf :+: y ) ) 1)
                 --  --hsvToRgb (Vec4 (y `mod` 1) 1 1 1)
                -- pure $ along (200 * y/scale)
                --         (gradient (1 - (min 1 $ depth / fromIntegral maxDepth) ** 0.12) (Lit sunset))
                --         (Vec4 0 0 0 0)

      #col %= ( ^+^ ( aaScale *^ col ) )
      pure ()

    col <- #col
    #out_colour .= col


applyWordWithScale = apply_word ( \ f -> #scale %= (* f ) )
applyWord = apply_word (const $ pure (Lit ()))


apply_word _ [] _ z = pure z
apply_word rescale (w:ws) t z = do
  z' <- apply_char rescale w t z
  apply_word rescale ws t z'


apply_char :: ( Code Float -> Program i i (Code ()) ) -> Char -> CodeComplex Float -> CodeComplex Float -> Program i i ( CodeComplex Float )
apply_char rescale 'a' t@(u :+: v) (x :+: y) = do
  iR <- let' $ 1 / ( x * x + y * y )
  rescale iR
  let' $ (iR * x - v) :+: (u - iR * y)
apply_char rescale 'A' t@(u :+: v) (x :+: y) = do
  z'@(~(x :+: y)) <- let' $ (x + v) :+: (u -y)
  iR <- let' $ 1 / ( x * x + y * y )
  rescale iR
  let' $ (iR * x) :+: (iR * y)
apply_char _ 'b' t z = let' $ z + 2
apply_char _ 'B' t z = let' $ z - 2

wrapHorizontal :: CodeComplex Float -> CodeComplex Float -> Program i i ( CodeComplex Float )
wrapHorizontal ( u :+: v ) ( x :+: y ) = do
  x <- let' $ x + y * abs v / u
  x <- let' $ ( ( x + 1 ) `mod` 2 ) - 1
  x <- let' $ x - y * abs v / u
  let' $ x :+: y

mkRescaledComplex :: Code (V 2 Float) -> CodeComplex Float
mkRescaledComplex (Vec2 x y) =
  ( (x - 960) / 400 ) :+: ( (540 - y) / 400 )

rescaleSliceParam :: Code (V 2 Float) -> CodeComplex Float
rescaleSliceParam (Vec2 x y) =
  ( (1080 - y) / 270 ) :+: ( (x - 960) / 480 )

isAboveSeparation :: CodeComplex Float -> CodeComplex Float -> Program i i ( Code Bool )
isAboveSeparation ( u :+: v ) ( x :+: y ) = do
  f <- let' $ signum ( x + 0.5 * v )
  k <- let' $ signum v * ( 2 * u - 1.95 ) / 4.3
  m <- let' $ 7.2 - 18 * ( 1.95 - u )
  e <- let' $ exp ( - m * abs ( x + 0.5 * v ) )
  let'      $ y >= 0.5 * u + k * f * ( 1 - e )

doInversion :: (Has "z" _s ~ V 2 Float, _) => Program _s _s ()
doInversion = do
  #z %= ( codeComplex . ( flip (-) inv_centre ) . CodeComplex )
  (CodeComplex -> z) <- #z
  d2 <- let' $ radius2 / ( magnitude z * magnitude z )
  #z %= ( codeComplex . (scale d2) . CodeComplex )
  #scale %= (* d2)
  #z %= ( codeComplex . (+ inv_centre) . CodeComplex )
  pure ()
    where
      inv_centre :: CodeComplex Float
      inv_centre = 0 :+: (-1)
      radius2 :: Code Float
      radius2 = 2

supersamplingLoop
  :: ( Code Word32 -> Code Word32 -> Program _s _s () )
  -> Program _s _s ()
supersamplingLoop prog = locally do
  #ssX #= 0
  #ssY #= 0
  while ( ( xSamples > ) <<$>> #ssX ) do
    ssX <- #ssX
    #ssY .= 0
    while ( ( ySamples > ) <<$>> #ssY ) do
      ssY <- #ssY
      embed $ prog ssX ssY
      #ssY %= (+1)
    #ssX %= (+1)
  pure ()

gradient :: forall n. KnownNat n
         => Code Float
         -> Code (Array n (V 4 Float))
         -> Code (V 4 Float)
gradient t colors
  =   ( (1-s) *^ ( view @(AnIndex _)  i    colors ) )
  ^+^ (    s  *^ ( view @(AnIndex _) (i+1) colors ) )
  where n :: Code Float
        n = Lit . fromIntegral $ knownValue @n
        i :: Code Word32
        i = floor ( (n-1) * t )
        s :: Code Float
        s = (n-1) * t - fromIntegral i


sunset :: Array 9 (V 4 Float)
sunset = MkArray . fromJust . Vector.fromList $
       [ V4 0    0    0    0
       , V4 0.28 0.1  0.38 1
       , V4 0.58 0.2  0.38 1
       , V4 0.83 0.3  0.22 1
       , V4 0.98 0.45 0.05 1
       , V4 0.99 0.62 0.2  1
       , V4 1    0.78 0.31 1
       , V4 1    0.91 0.6  1
       , V4 1    1    1    1
       ]

hsvToRgb :: Code (V 4 Float) -> Program _s _s (Code (V 4 Float))
hsvToRgb (Vec4 h s v a) = do
  ~( Vec4 kx ky kz kw ) <- let' $ Vec4 1 (2/3) (1/3) 3
  ~( Vec3 px py pz )    <- let' $ Vec3 ( abs ( 6 * fract (h + kx) - kw ) )
                                       ( abs ( 6 * fract (h + ky) - kw ) )
                                       ( abs ( 6 * fract (h + kz) - kw ) )
  let' ( v *^ along s (Vec4 kx kx kx a)
                      (Vec4 (clamp (px - kx)) (clamp (py - kx)) (clamp (pz - kx)) a))

  where
    fract x = x - floor x
    clamp x = min 1 $ max 0 $ x

------------------------------------------------
-- compiling

vertPath, fragPath :: FilePath
vertPath = shaderDir </> "klein_vert.spv"
fragPath = shaderDir </> "klein_frag.spv"

compileVertexShader :: IO ( Either ShortText ModuleRequirements )
compileVertexShader = compileTo vertPath [SPIRV $ Version 1 0] vertex

compileFragmentShader :: IO ( Either ShortText ModuleRequirements )
compileFragmentShader = compileTo fragPath [SPIRV $ Version 1 0] fragment

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileVertexShader
  , compileFragmentShader
  ]

shaderPipeline :: ShaderPipeline FilePath
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(Triangle List)
  :>-> (vertex  , vertPath)
  :>-> (fragment, fragPath)
