{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module FIR.Examples.Kerr.Noise
  ( hash, hash2, noise, brownianNoise )
  where

-- fir
import FIR
import Math.Linear

------------------------------------------------

hash :: Code Float -> Code Float -> Code Float
hash x y = 0.5 + 0.5 * sin ( (sin x * 5.34 + sin y * 7.13) * 5865.273458 )


hash2 :: Code (V 2 Float) -> Program i i (Code (V 2 Float))
hash2 p = purely do
  q <- def @"q" @R $ Vec2 ( p ^.^ Vec2 127.1 311.7 ) ( p ^.^ Vec2 269.5 183.3 )
  def @"r" @R $ ( ( \ x -> -1 + 2 * ( sin x * 43758.5453123 `mod` 1 ) ) :: Code Float -> Code Float ) <$$> q

noise :: Code (V 2 Float) -> Program i i (Code Float)
noise v@(Vec2 x y) = purely do
  i@(~(Vec2 ix iy))
     <- def @"i" @R $ ( floor :: Code Float -> Code Float ) <$$> ( v ^+^ pureAST ( ( x + y ) * k1 ) )
  a@(~(Vec2 ax ay)) <- def @"a" @R $ v ^-^ i ^+^ pureAST ( (ix + iy) * k2 )
  s <- def @"s" @R $ signum ( ax - ay )
  o@(~(Vec2 ox oy)) <- def @"o" @R $ Vec2 ( 0.5 + 0.5 * s ) ( 0.5 - 0.5 * s )
  j  <- def @"j" @R $ Vec2 (ix + ox) (iy + oy)
  b  <- def @"b"  @R $ a ^-^ o ^+^ pureAST k2
  c  <- def @"c"  @R $ a ^+^ pureAST ( 2 * k2 - 1 )
  h  <- def @"h"  @R @(V 3 Float) $ ( max 0 :: Code Float -> Code Float ) <$$> ( pureAST 0.5 ^-^ Vec3 (a ^.^ a) (b ^.^ b) (c ^.^ c) )
  h² <- def @"h²" @R @(V 3 Float) $ ( ( \ t -> t * t ) :: Code Float -> Code Float ) <$$> h
  hx <- def @"hx" @R =<< hash2 i
  hy <- def @"hy" @R =<< hash2 j
  hz <- def @"hz" @R =<< hash2 ( i ^+^ Vec2 1 1 )
  n  <- def @"n"  @R @(V 3 Float) $ ( ( \ r t -> r * r * t ) :: Code Float -> Code Float -> Code Float )
                        <$$> h² <**> Vec3 ( a ^.^ hx ) ( b ^.^ hy ) ( c ^.^ hz )
  def @"res" @R $ n ^.^ pureAST 70
    where
      k1, k2 :: Code Float
      k1 = Lit $ ( sqrt 3 - 1 ) / 2
      k2 = Lit $ ( 3 - sqrt 3 ) / 6

brownianNoise :: Code (V 2 Float) -> Program i i (Code Float)
brownianNoise n0 = purely do
  _ <- def @"total"     @RW @Float 0
  _ <- def @"amplitude" @RW @Float 0.1
  _ <- def @"n"         @RW n0
  _ <- def @"i" @RW @Word32 0
  while ( get @"i" < 8 ) do
    n  <- get @"n"
    n' <- noise n
    a  <- get @"amplitude"
    modify @"total"     ( + (n' * a) )
    modify @"n"         ( mat !*^ )
    modify @"amplitude" ( * 0.4 )
    modify @"i"         ( + 1   )
  get @"total"
    where
      mat :: Code (M 2 2 Float)
      mat = Mat22 1.6 1.2 (-1.2) 1.6
