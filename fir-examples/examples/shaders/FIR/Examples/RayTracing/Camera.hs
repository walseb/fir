{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Examples.RayTracing.Camera
  ( Camera(..), CameraCoordinates, RayBearing
  , cameraRay
  )
  where

-- fir
import FIR
import Math.Linear

--------------------------------------------------------------------------

data Camera
  = Pinhole
  | Orthographic

type CameraCoordinates =
  Struct
    '[ "position" ':-> V 4 Float -- includes frame number as 4th coordinate
     , "right"    ':-> V 3 Float
     , "up"       ':-> V 3 Float
     , "forward"  ':-> V 4 Float -- 4th component is an additional offset
     ]

type RayBearing =
  Struct
    '[ "worldRayOrigin"    ':-> V 3 Float
     , "worldRayDirection" ':-> V 3 Float
     ]

cameraRay :: Camera -> Code CameraCoordinates -> Code Float -> Code Float -> Program s s ( Code RayBearing )
cameraRay cam camCoords x y = do
  position <- let' $ view @( Name "position" :.: Swizzle "xyz" ) camCoords
  right    <- let' $ view @( Name "right"   )                    camCoords
  up       <- let' $ view @( Name "up"      )                    camCoords
  forward  <- let' $ view @( Name "forward" :.: Swizzle "xyz")   camCoords
  offset   <- let' $ view @( Name "forward" :.: Index 3 )        camCoords

  case cam of
    Pinhole -> do
      rayDir  <- let' $ normalise ( forward ^+^ x *^ right ^+^ y *^ up )
      rayOrig <- let' $ position ^+^ offset *^ rayDir
      let' $ Struct ( rayOrig :& rayDir :& End )

    Orthographic -> do
      rayOrig <- let' $ position ^+^ x *^ right ^+^ y *^ up
      let' $ Struct ( rayOrig :& forward :& End )
