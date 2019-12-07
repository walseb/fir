{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Examples.Logo.Shaders where

-- base
import qualified Prelude
import Prelude
  ( Functor, map )
import Control.Monad
  ( void )
import Data.Maybe
  ( fromJust )
import GHC.TypeLits
  ( KnownNat )

-- filepath
import System.FilePath
  ( (</>) )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import FIR
  hiding ( Triangle )
import Math.Linear

-- fir-examples
import FIR.Examples.Paths
  ( shaderDir )

------------------------------------------------
-- basic ray-tracing setup

data AABB a
  = AABB
  { low  :: a
  , high :: a
  }
  deriving Functor

data Triangle a
  = Triangle
  { v0 :: a
  , v1 :: a
  , v2 :: a
  }
  deriving Functor

data Ray a
  = Ray
  { pos    :: a
  , dir    :: a
  , invDir :: a
  }


minV3 :: AST (V 3 Float) -> AST Float
minV3 (Vec3 x y z) = min x (min y z)

maxV3 :: AST (V 3 Float) -> AST Float
maxV3 (Vec3 x y z) = max x (max y z)


intersectAABB
  :: forall (s :: ProgramState). ( _ )
  => Ray (AST (V 3 Float))
  -> AABB (AST (V 3 Float))
  -> Program s s ( AST Float, AST Float )
intersectAABB
  Ray  { pos, invDir }
  AABB { low, high   }
    = locally do
        t1 :: AST (V 3 Float)
          <- def @"t1" @R
             $ ( \ s p i -> ( s - p ) * i :: AST Float ) <$$> low  <**> pos <**> invDir
        t2 :: AST (V 3 Float)
          <- def @"t2" @R
             $ ( \ s p i -> ( s - p ) * i :: AST Float ) <$$> high <**> pos <**> invDir

        let
          tMin, tMax :: AST Float
          tMin = maxV3 ( min @(AST Float) <$$> t1 <**> t2 )
          tMax = minV3 ( max @(AST Float) <$$> t1 <**> t2 )

        pure ( tMin, tMax )


intersectTriangle
  :: forall (s :: ProgramState). ( _ )
  => Ray (AST (V 3 Float))
  -> Triangle (AST (V 3 Float))
  -> Program s s ( AST Float, AST (V 2 Float) )
intersectTriangle
  Ray { pos, dir }
  Triangle { v0, v1, v2 }
    = locally do
        e1 <- def @"e1" @R $ v1 ^-^ v0
        e2 <- def @"e2" @R $ v2 ^-^ v0

        h  <- def @"h" @R $ dir `cross` e2
        f  <- def @"f" @R $ recip (h `dot` e1)
        s  <- def @"s" @R $ pos ^-^ v0
        q  <- def @"q" @R $ s `cross` e1

        let
          u = f * ( s   `dot` h )
          v = f * ( dir `dot` q )
          t = f * ( e2  `dot` q )

        pure (t, Vec2 u v)


onTriangle :: AST (V 2 Float) -> AST Bool
onTriangle (Vec2 u v) =
  u > 0 && v > 0 && u + v < 1

------------------------------------------------
-- scene

cube :: AABB (AST (V 3 Float))
cube = AABB
  ( Lit (Prelude.pure (-1)) )
  ( Lit (Prelude.pure   1 ) )

w, bary, equi :: Float
w = 0.75
bary = (2*w-1)/3
equi = bary + sqrt ( 1 + 6 * bary * (2 * bary - 1) )

tree, trunk :: Triangle (AST (V 3 Float))
tree = Triangle
         ( Lit $ V3 (-w)   w  (-1) )
         ( Lit $ V3   1    w    w  )
         ( Lit $ V3 (-w) (-1)   w  )
trunk = Triangle
         ( Lit $ V3 0       1     (-equi) )
         ( Lit $ V3 equi    1       0     )
         ( Lit $ V3 (-bary) (2*w-1) bary  )

green, red, bgCol, edgeCol :: AST (V 4 Float)
green   = Lit (V4 0.05 0.5  0.2  1)
red     = Lit (V4 0.65 0.1  0.1  1)
bgCol   = Lit (V4 0.95 0.93 0.88 0)
edgeCol = bgCol -- Lit (V4 0    0    0    1)

sceneTriangles
  :: [ ( Triangle (AST (V 3 Float)), AST (V 4 Float) ) ]
sceneTriangles =
  [ (tree , green)
  , (trunk, red  )
  ]

gradient :: forall n. KnownNat n
         => AST Float
         -> AST (Array n (V 4 Float))
         -> AST (V 4 Float)
gradient t colors
  =   ( (1-s) *^ ( view @(AnIndex _)  i    colors ) )
  ^+^ (    s  *^ ( view @(AnIndex _) (i+1) colors ) )
  where n :: AST Float
        n = Lit . fromIntegral $ knownValue @n
        i :: AST Word32
        i = floor ( (n-1) * t )
        s :: AST Float
        s = (n-1) * t - fromIntegral i


cubeGradientStops :: Array 5 (V 4 Float)
cubeGradientStops =
  MkArray . fromJust . Vector.fromList . map ( ^* (1/255) ) $
    [ V4 255 120  90 255
    , V4 240 180  80 255
    , V4 230 120 160 255
    , V4  90 150 240 255
    , V4 190 240 230 255
    ]

cubeColor :: AST (V 3 Float) -> AST (V 4 Float)
cubeColor pt = gradient (1-t) (Lit cubeGradientStops)
  where
    c = 0.57735026 -- 1 / sqrt 3
    t = 0.5 + 0.5 * ( normalise pt `dot` (Vec3 (-c) (-c) c) )

xSamples, ySamples, samples :: Semiring a => a
xSamples = 4
ySamples = 4
samples = xSamples * ySamples

weight :: AST Float
weight = Lit (1 / samples)

------------------------------------------------
-- compute shader

type ComputeDefs =
  '[ "ubo"  ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                    ( Struct
                       '[ "position" ':-> V 3 Float
                        , "right"    ':-> V 3 Float
                        , "up"       ':-> V 3 Float
                        , "forward"  ':-> V 3 Float
                        ]
                    )
    , "img"  ':-> Image2D '[ DescriptorSet 0, Binding 1 ]
                    ( RGBA8 UNorm )
    -- global size: 120 * 135 * 1
    , "main" ':-> EntryPoint '[ LocalSize 16 8 1 ]
                    Compute
   ]

computeShader :: Module ComputeDefs
computeShader = Module $ entryPoint @"main" @Compute do

    ~(Vec3 i_x i_y _) <- get @"gl_GlobalInvocationID"

    let
      x,y :: AST Float
      x = ( fromIntegral i_x - 960 ) / 960
      y = ( fromIntegral i_y - 540 ) / 960

    --pos   <- use @(Name "ubo" :.: Name "position")
    pos0  <- use @(Name "ubo" :.: Name "position")

    right <- use @(Name "ubo" :.: Name "right"   )
    up    <- use @(Name "ubo" :.: Name "up"      )

    --fwd   <- use @(Name "ubo" :.: Name "forward" )
    dir   <- use @(Name "ubo" :.: Name "forward" )


    _ <- def @"col" @RW bgCol

    invDir <- def @"invDir" @R ( recip @(AST Float) <$$> dir )

    -- anti-aliasing
    _ <- def @"i" @RW @Float 0
    _ <- def @"j" @RW @Float 0
    while (get @"i" < xSamples) do
      while (get @"j" < ySamples) do

        i <- get @"i"
        j <- get @"j"

        let
          dx, dy :: AST Float
          dx = ( ( i + 0.5 ) / xSamples - 0.5 ) / 960
          dy = ( ( j + 0.5 ) / ySamples - 0.5 ) / 960

        --dir    <- def @"dir"    @R $ normalise ( fwd ^+^ (x+dx) *^ right ^-^ (y+dy) *^ up )
        --invDir <- def @"invDir" @R ( recip @(AST Float) <$$> dir )
        pos <- def @"pos" @R ( pos0 ^+^ 5 *^ ( (x+dx) *^ right ^-^ (y+dy) *^ up ) )

        let ray = Ray { pos, dir, invDir }

        (tMin, tMax) <- ray `intersectAABB` cube

        let
          cubeOut :: AST (V 3 Float)
          cubeOut@(Vec3 outx outy outz) = pos ^+^ tMax *^ dir
          (Vec3 inx iny inz ) = pos ^+^ tMin *^ dir

        -- check if ray doesn't hit the cube
        -- (either not at all, or hits, from the outside, near one of the edges)
        if (  tMax < 0
           || tMin > tMax
           || ( tMin > 0 &&
                (  ( abs inx > Lit w && abs iny > Lit w )
                || ( abs inx > Lit w && abs inz > Lit w )
                || ( abs iny > Lit w && abs inz > Lit w )
                )
              )
           )
        then pure (Lit ()) -- no cube hit
        else do
          -- ray interacts with cube, perform ray-tracing inside the cube
          let
            cubeCol :: AST (V 4 Float)
            cubeCol =
              if (  ( abs outx > Lit 0.995 && abs outy > Lit 0.995 )
                 || ( abs outx > Lit 0.995 && abs outz > Lit 0.995 )
                 || ( abs outy > Lit 0.995 && abs outz > Lit 0.995 )
                 )
              then edgeCol
              else cubeColor cubeOut

          -- find first triangle hit inside the cube
          -- if no hit inside the cube, return the appropriate cube background colour

          _ <- def @"hitCol" @RW cubeCol
          _ <- def @"hit_t"  @RW tMax

          ixFor_ sceneTriangles \ (tri, col) -> do
              (t, uv) <- ray `intersectTriangle` tri
              curr_t <- get @"hit_t"
              when (t > 0 && onTriangle uv && t < curr_t) do
                put @"hitCol" col
                put @"hit_t"  t

          hitCol <- get @"hitCol"
          modify @"col" ( ^+^ ( weight *^ (hitCol ^-^ bgCol) ) )


        modify @"j" (+1)
      put    @"j" 0
      modify @"i" (+1)

    imageWrite @"img"
      ( Vec2 i_x i_y )
      =<< get @"col"


------------------------------------------------
-- compiling

compPath :: FilePath
compPath = shaderDir </> "logo_comp.spv"

compileComputeShader :: IO ( Either ShortText ModuleRequirements )
compileComputeShader = compileTo compPath [] computeShader

compileAllShaders :: IO ()
compileAllShaders = void compileComputeShader
