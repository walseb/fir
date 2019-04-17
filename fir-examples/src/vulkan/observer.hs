{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

module Vulkan.Observer
  ( Input(..), nullInput
  , Action(..)
  , Quit(Quit)
  , Observer(..), initialObserver
  , onSDLInput
  , interpretInput
  , move
  , modelViewProjection
  ) where

-- base
import Prelude
  hiding ( Num(..), Fractional(..), Floating(..) )
import Data.Coerce
  ( coerce )
import Data.Maybe
  ( fromMaybe )
import Data.Monoid
  ( Any(..), Sum(..) )
import Foreign.C
import GHC.TypeNats
  ( KnownNat )

-- sdl2
import qualified SDL

-- fir
import FIR
  hiding
    ( Input, Eq(..), Ord(..), Any, pure, view )
import Math.Linear
  ( V, M
  , pattern V2, pattern V3
  , norm, normalise
  , (^+^), (*^), (-^)
  , (!*!)
  , transpose
  , perspective, lookAt
  )
import Math.Quaternion
  ( Quaternion
  , rotate, axisAngle
  )

----------------------------------------------------------------------------

newtype Quit = MkQuit Any
  deriving (Eq, Show, Semigroup, Monoid)

pattern Quit :: Quit
pattern Quit = MkQuit (Any True)

data Input = Input
  { keysDown    :: [SDL.Scancode]
  , keysPressed :: [SDL.Scancode]
  , mousePos    :: V 2 Foreign.C.CFloat
  , mouseRel    :: V 2 Foreign.C.CFloat
  , quitEvent   :: Quit
  } deriving Show

data Action = Action
  { movement       :: V 3 (Sum Foreign.C.CFloat)
  , look           :: V 2 (Sum Foreign.C.CFloat)
  , shouldQuit     :: Quit
  , locate         :: Bool
  , takeScreenshot :: Bool
  } deriving Show

data Observer = Observer
  { position :: V 3 (Foreign.C.CFloat)
  , angles   :: V 2 (Foreign.C.CFloat)
  } deriving Show


nullInput :: Input
nullInput
  = Input
    { keysDown    = []
    , keysPressed = []
    , mousePos    = V2 0 0
    , mouseRel    = V2 0 0
    , quitEvent   = coerce False
    }

initialObserver :: Observer
initialObserver
  = Observer
      { position = V3 0 0 (-6)
      , angles   = pure 0
      }

p, n :: Foreign.C.CFloat
p =   1
n = (-1)

strafeDir :: SDL.Scancode -> V 3 Foreign.C.CFloat
strafeDir SDL.ScancodeW      = V3 0 0 p
strafeDir SDL.ScancodeS      = V3 0 0 n
strafeDir SDL.ScancodeA      = V3 n 0 0
strafeDir SDL.ScancodeD      = V3 p 0 0
strafeDir SDL.ScancodeLCtrl  = V3 0 p 0
strafeDir SDL.ScancodeLShift = V3 0 n 0
strafeDir SDL.ScancodeUp     = V3 0 0 p
strafeDir SDL.ScancodeDown   = V3 0 0 n
strafeDir SDL.ScancodeLeft   = V3 n 0 0
strafeDir SDL.ScancodeRight  = V3 p 0 0
strafeDir SDL.ScancodeRCtrl  = V3 0 p 0
strafeDir SDL.ScancodeRShift = V3 0 n 0
strafeDir _                  = V3 0 0 0

normaliseStrafing
  :: (KnownNat n, Floating a, DivisionRing a, Ord a)
  => V n a -> V n a
normaliseStrafing v
  | norm v < 0.01 = pure 0
  | otherwise     = normalise v


strafe :: [SDL.Scancode] -> V 3 (Sum Foreign.C.CFloat)
strafe = coerce
       . (0.05 *^)
       . normaliseStrafing @3 @Foreign.C.CFloat
       . coerce
       . foldMap (fmap Sum . strafeDir)


onSDLInput :: Input -> SDL.EventPayload -> Input
onSDLInput input SDL.QuitEvent
  = input { quitEvent = Quit }
onSDLInput input (SDL.WindowClosedEvent _)
  = input { quitEvent = Quit }
onSDLInput input (SDL.KeyboardEvent ev)
  = let keyCode = SDL.keysymScancode (SDL.keyboardEventKeysym ev)
    in case SDL.keyboardEventKeyMotion ev of
         SDL.Pressed  -> input { keysDown    = keyCode : filter (/= keyCode) (keysDown    input)
                               , keysPressed = keyCode : filter (/= keyCode) (keysPressed input)
                               }
         SDL.Released -> input { keysDown = filter (/= keyCode) (keysDown input) }
onSDLInput input (SDL.MouseMotionEvent ev)
  = input { mousePos = fmap Prelude.fromIntegral (V2 px py)
          , mouseRel = fmap ((* 0.003) . Prelude.fromIntegral) (V2 rx ry)
          }
    where
      SDL.P (SDL.V2 px py) = SDL.mouseMotionEventPos       ev
      SDL.V2        rx ry  = SDL.mouseMotionEventRelMotion ev
onSDLInput input _ = input


interpretInput :: Input -> Action
interpretInput Input { .. } =
  let movement       = strafe keysDown
      escape         = foldMap
                          ( \case { SDL.ScancodeEscape -> Quit; _ -> mempty } )
                          keysPressed
      shouldQuit     = quitEvent <> escape
      look           = fmap Sum . (-^) $ mouseRel
      locate         = SDL.ScancodeSpace `elem` keysDown
      takeScreenshot = SDL.ScancodeF12   `elem` keysPressed
  in Action { .. }

move :: Observer -> Action -> (Observer, Quaternion Foreign.C.CFloat)
move  Observer { position = oldPosition, angles = oldAngles }
      Action   { .. }
  = let angles@(V2 x y) = oldAngles ^+^ fmap getSum look
        orientation = axisAngle (V3 0 (-1) 0) x * axisAngle (V3 1 0 0) y
        position = oldPosition ^+^ rotate orientation (fmap getSum movement)
    in (Observer { .. }, orientation)

modelViewProjection :: Observer -> Maybe (Quaternion (Foreign.C.CFloat)) -> M 4 4 Foreign.C.CFloat
modelViewProjection Observer { angles = V2 x y, position } mbOrientation
  = let orientation
          = fromMaybe
              ( axisAngle (V3 0 (-1) 0) x * axisAngle (V3 1 0 0) y )
              mbOrientation
        forward = rotate orientation ( V3 0   0  1 ) -- Vulkan coordinate system
        up      = rotate orientation ( V3 0 (-1) 0 )
        view    = lookAt ( position ^+^ forward ) position up
        projection = perspective ( pi / 2 ) ( 16 / 9 ) 0.1 100000
                   
    in transpose ( projection !*! view )
