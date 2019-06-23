{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedWildCards         #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Shaders.Compute where

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import FIR
import Math.Linear

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

computeShader :: Program ComputeDefs ()
computeShader = Program $ entryPoint @"main" @Compute do

    ~(Vec3 i_x i_y _) <- get @"gl_GlobalInvocationID"

    let
      x,y :: AST Float
      x = ( fromIntegral i_x - 960 ) / 960
      y = ( fromIntegral i_y - 540 ) / 960

    pos   <- use @(Name "ubo" :.: Name "position")
    right <- use @(Name "ubo" :.: Name "right"   )
    up    <- use @(Name "ubo" :.: Name "up"      )
    fwd   <- use @(Name "ubo" :.: Name "forward" )


    _ <- def @"col" @RW ( Vec4 0 0 0 0 :: AST (V 4 Float) )

    -- 16x AA
    _ <- def @"i" @RW @Float 0
    _ <- def @"j" @RW @Float 0
    while (get @"i" < 4) do
      while (get @"j" < 4) do

        i <- get @"i"
        j <- get @"j"

        let
          dx, dy :: AST Float
          dx = ( ( i + 0.5 ) / 4 - 0.5 ) / 960
          dy = ( ( j + 0.5 ) / 4 - 0.5 ) / 960

        dir    <- def @"dir"    @R $ normalise ( fwd ^+^ (x+dx) *^ right ^-^ (y+dy) *^ up )
        invdir <- def @"invdir" @R ( recip @(AST Float) <$$> dir )

        let
          tMin, tMax :: AST Float
          (tMin, tMax) = cubeIntersections pos invdir

          cubeIntersection :: AST (V 3 Float)
          cubeIntersection@(Vec3 ix iy iz) =
            if tMin < 0
            then pos ^+^ tMax *^ dir
            else pos ^+^ tMin *^ dir

        if (  tMax < 0
           || tMin > tMax
           || ( abs ix > 0.75 && abs iy > 0.75 )
           || ( abs ix > 0.75 && abs iz > 0.75 )
           || ( abs iy > 0.75 && abs iz > 0.75 )
           )
        then pure (Lit ())
        --else modify @"col" ( ^+^ ( 0.0625 *^ green ) )
        else let
           (t, Vec2 u v) = triangleIntersection pos dir
          in if ( t < tMax && u > 0 && u < 1 && v > 0 && v < 1 && u + v < 1 )
            then modify @"col" ( ^+^ ( 0.0625 *^ green ) )
            else modify @"col" ( ^+^ ( 0.0625 *^ cubeColor ( (pos ^+^ tMax *^ dir) ) ) )


        modify @"j" (+1)
      put    @"j" 0
      modify @"i" (+1)

    imageWrite @"img"
      ( Vec2 i_x i_y )
      =<< get @"col"


minV3 :: AST (V 3 Float) -> AST Float
minV3 (Vec3 x y z) = min x (min y z)

maxV3 :: AST (V 3 Float) -> AST Float
maxV3 (Vec3 x y z) = max x (max y z)

cubeIntersections :: AST (V 3 Float) -> AST (V 3 Float) -> ( AST Float, AST Float )
cubeIntersections pos invdir = (tMin, tMax)
  where

    cube0, cube1 :: AST (V 3 Float)
    cube0 = pureAST (-1)
    cube1 = pureAST   1

    t1, t2 :: AST (V 3 Float)
    t1 = ( \ c p i -> ( c - p ) * i :: AST Float ) <$$> cube0 <**> pos <**> invdir
    t2 = ( \ c p i -> ( c - p ) * i :: AST Float ) <$$> cube1 <**> pos <**> invdir

    tMin, tMax :: AST Float
    tMin = maxV3 ( min @(AST Float) <$$> t1 <**> t2 )
    tMax = minV3 ( max @(AST Float) <$$> t1 <**> t2 )


triangleIntersection :: AST (V 3 Float) -> AST (V 3 Float) -> (AST Float, AST (V 2 Float))
triangleIntersection pos dir = ( t, Vec2 u v )
  -- TODO: too much inlining
  where
    p1 = Vec3 (-0.75) 0.75 (-1)
    p2 = Vec3   1     0.75 0.75
    p3 = Vec3 (-0.75) (-1) 0.75
    e1 = p2 ^-^ p1
    e2 = p3 ^-^ p1

    h  = dir `cross` e2
    a  = h `dot` e1
    f  = recip a
    s  = pos ^-^ p1
    q  = s `cross` e1

    u  = f * ( s   `dot` h )
    v  = f * ( dir `dot` q )
    t  = f * ( e2  `dot` q )


anyV3 :: AST (V 3 Bool) -> AST Bool
anyV3 (Vec3 b1 b2 b3) = b1 || b2 || b3

green :: AST (V 4 Float)
green = Vec4 0.05 0.5 0.2 1

cubeColor :: AST (V 3 Float) -> AST (V 4 Float)
cubeColor (Vec3 x y z) =
  if y > 0.95
  then Vec4 0.0 (abs (x+z)/4) 0.2 1.0
  else Vec4 (1-0.5*(y+1)) 0.5 0.2 1.0

------------------------------------------------
-- compiling

compPath :: FilePath
compPath = "shaders/compute_comp.spv"

compileComputeShader :: IO ( Either Text () )
compileComputeShader = compile compPath [] computeShader
