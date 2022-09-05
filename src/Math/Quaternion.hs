{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|
Module: Math.Quaternion

Bare-bones implementation of quaternions,
together with a few simple operations
such as the conjugation action of quaternions on 3-d vectors.

-}

module Math.Quaternion where

-- base
import Prelude
  ( Eq(..), Show(..), Functor(..), Applicative(..)
  , Foldable(..), Traversable(..) )
import Control.Applicative
  ( liftA2 )

-- fir
import FIR.Prelude
import Math.Algebra.Class
  ( AdditiveMonoid(..), CancellativeAdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), Ring, DivisionRing(..)
  , Signed(..), Floating(..)
  )
import Math.Linear
  ( V(..), M(M)
  , pattern V3
  , pattern V4
  , Semimodule(..), LinearModule(..), Inner(..)
  , norm, normalise
  )

----------------------------------------------------------------------------

newtype Quaternion a = Quaternion { quaternion :: V 4 a }
 deriving stock   ( Eq, Show, Foldable, Traversable )
 deriving newtype ( Functor, Applicative )

instance Semiring a => Semimodule () (Quaternion a) where
  type Scalar (Quaternion a) = a
  type OfDim (Quaternion a) () '() = Quaternion a
  type ValidDim (Quaternion a) () unit = ( unit ~ '() )
  (^+^) = coerce ( (^+^) :: V 4 a -> V 4 a -> V 4 a )
  (*^)  = coerce ( (*^) :: a -> V 4 a -> V 4 a )

instance Ring a => LinearModule () (Quaternion a) where
  (^-^) = coerce ( (^-^) :: V 4 a -> V 4 a -> V 4 a )
  (-^)  = coerce ( ( -^) :: V 4 a -> V 4 a )

instance (Semiring a, Floating a) => Inner () (Quaternion a) where
  dot = coerce ( dot :: V 4 a -> V 4 a -> a )
  normalise = coerce ( normalise :: V 4 a -> V 4 a )

instance AdditiveMonoid a => AdditiveMonoid (Quaternion a) where
  (+) = liftA2 (+)
  zero = Quaternion (pure zero)
  fromInteger = Quaternion . ( :. pure zero ) . fromInteger

instance Ring a => Semiring (Quaternion a) where
  Quaternion { quaternion = V4 xr xi xj xk } * Quaternion { quaternion = V4 yr yi yj yk }
    = Quaternion $
        V4 (xr*yr - xi*yi - xj*yj - xk*yk)
           (xr*yi + xi*yr + xj*yk - xk*yj)
           (xr*yj + xj*yr + xk*yi - xi*yk)
           (xr*yk + xk*yr + xi*yj - xj*yi)

instance CancellativeAdditiveMonoid a => CancellativeAdditiveMonoid (Quaternion a) where
  (-) = liftA2 (-)

instance AdditiveGroup a => AdditiveGroup (Quaternion a) where
  negate = fmap negate

instance Floating a => Signed (Quaternion a) where
  abs    q = norm q *^ Quaternion (V4 1 0 0 0)
  signum q = recip (norm q) *^ q

instance ( DivisionRing a, Floating a ) => DivisionRing (Quaternion a) where
  recip a = recip (norm a) *^ conjugate a
  fromRational = Quaternion . ( :. pure zero ) . fromRational

conjugate :: Ring a => Quaternion a -> Quaternion a
conjugate (Quaternion (a :. v)) = Quaternion (a :. (-^) @_ @_ @3 v)

-- 'rotate' expects the quaternion to be of unit norm
rotate :: Ring a => Quaternion a -> V 3 a -> V 3 a
rotate q v = ijk where
  Quaternion (_ :. ijk) = q * Quaternion (0 :. v) * conjugate q

axisAngle :: ( Semiring a, Floating a ) => V 3 a -> a -> Quaternion a
axisAngle axis theta = Quaternion ( cos half :. sin half *^ normalise axis )
  where half = 0.5 * theta

fromQuaternion :: Ring a => Quaternion a -> M 3 3 a
fromQuaternion (Quaternion (V4 w x y z))
 = M $ V3 (V3 (1-2*(y2+z2)) (2*(xy-zw)) (2*(xz+yw)))
          (V3 (2*(xy+zw)) (1-2*(x2+z2)) (2*(yz-xw)))
          (V3 (2*(xz-yw)) (2*(yz+xw)) (1-2*(x2+y2)))
  where x2 = x*x
        y2 = y*y
        z2 = z*z
        xy = x*y
        xz = x*z
        xw = x*w
        yz = y*z
        yw = y*w
        zw = z*w
