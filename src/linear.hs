{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Linear where

-- base
import Prelude hiding( Functor(..), (<$>), Foldable(..), Traversable(..), Applicative(..)
                     , Eq(..), and, or, any, all, ifThenElse
                     )
import qualified Prelude
import Control.Applicative(liftA2)
import Data.Coerce(coerce)
import Data.Kind(Type)
import Data.Type.Equality((:~:)(..))
import Data.Proxy(Proxy(..))
import Foreign.Ptr(Ptr, castPtr)
import Foreign.Storable(Storable(alignment, sizeOf, peek, poke, peekElemOff, pokeElemOff))
import GHC.Base(Int#, Int(I#), (+#))
import GHC.TypeLits(Nat, KnownNat, natVal, type (+), type (-))
import GHC.TypeLits.Compare
import GHC.TypeNats(type (<=?))
import Unsafe.Coerce(unsafeCoerce)

-- fir
import TypeClasses.Equality(Eq(Logic,(==)), HasBool(bool), and, ifThenElse)
import TypeClasses.Category( Functor(FunctorApp,fmap), (<$>)
                           , Applicative(pure,(<*>))
                           , Foldable(foldMap)
                           , Traversable(traverse)
                           , sum
                           )
import qualified SPIRV

infixr 3 :.

data V :: Nat -> Type -> Type where
  Nil :: V 0 a
  (:.) :: KnownNat n => a -> V n a -> V (n+1) a

deriving instance KnownNat n => Prelude.Functor     (V n)
deriving instance KnownNat n => Prelude.Foldable    (V n)
deriving instance KnownNat n => Prelude.Traversable (V n)
deriving instance (KnownNat n, Show a) => Show (V n a)

instance (KnownNat n, Semigroup a) => Semigroup (V n a) where
  (<>) = liftA2 (<>)

instance (KnownNat n, Monoid a) => Monoid (V n a) where
  mempty = repeatV mempty

repeatV :: forall a n. KnownNat n => a -> V n a
repeatV = unfold id

instance (KnownNat n, Storable a) => Storable (V n a) where
  sizeOf :: V n a -> Int
  sizeOf _ = n * sizeOf (undefined :: a)
    where n = fromInteger $ natVal (Proxy @n)

  alignment _ = alignment (undefined :: a)
  
  poke ptr u = go u 0#
    where go :: KnownNat m => V m a -> Int# -> IO()
          go Nil     _  = pure ()
          go (a :. v) n# = do
              pokeElemOff (castPtr ptr) (I# n#) a
              go v (n# +# 1#)
  
  peek :: Ptr (V n a) -> IO (V n a)
  peek ptr = traverse (peekElemOff (castPtr ptr)) $ ixVec
      where ixVec :: V n Int
            ixVec = unfold pred (fromInteger $ natVal (Proxy @n))

-- TODO: indexing is slightly off, I'm doing {0, ..., n} instead of {1, ..., n}
buildV :: forall n a v. KnownNat n
       => ( forall i. (KnownNat i, (i <=? n) ~ 'True) => Proxy i -> v -> a)
       -> v
       -> V n a
buildV f v = go @n f v Nil where
  go :: forall j. (KnownNat j, (j <=? n) ~ 'True)
     => ( forall i. (KnownNat i, (i <=? n) ~ 'True) => Proxy i -> v -> a)
     -> v
     -> V (n-j) a
     -> V n a
  go f v w = case (Proxy @1) %<=? (Proxy @j) of
                  LE Refl   -> case deduceOK @j @n Refl  of
                                    Refl -> go @(j-1) f v (f (Proxy @j) v :. w)
                  NLE nle _ -> case deduceZero nle of
                                    Refl -> w

deduceZero :: KnownNat n => (1 <=? n) :~: 'False -> (n :~: 0)
deduceZero = unsafeCoerce

deduceOK :: (KnownNat j, KnownNat n) => (j <=? n) :~: 'True -> ((j-1) <=? n) :~: 'True
deduceOK = unsafeCoerce

dfoldrV :: forall n a b. KnownNat n 
        => (forall k. (KnownNat k, (k+1 <=? n) ~ 'True) => a -> b k -> b (k+1))
        -> b 0 -> V n a -> b n
dfoldrV f d = go
  where go :: (KnownNat m, (m <=? n) ~ 'True) => V m a -> b m
        go Nil    = d
        go (a:.as) = f a (go as)

unfold :: forall n a. KnownNat n => (a -> a) -> a -> V n a
unfold f a = case (Proxy @1) %<=? (Proxy @n) of
                   LE Refl   -> let b = f a in b :. (unfold f b :: V (n-1) a)
                   NLE nle _ -> case deduceZero nle of
                                     Refl -> Nil

infixl 6 <++>

(<++>) :: (KnownNat n, KnownNat m) => V n a -> V m a -> V (n+m) a
(<++>) Nil      v = v
(<++>) (a:.Nil) v = a :. v
(<++>) (a:.as)  v = a :. (as <++> v)

instance KnownNat n => Prelude.Applicative (V n) where
  pure                    = repeatV
  Nil       <*>  _        = Nil
  (f :. fs) <*> (a :. as) = f a :. fs <*> as

{-# COMPLETE V0 #-}
pattern V0 :: V 0 a
pattern V0 = Nil

{-# COMPLETE V1 #-}
pattern V1 :: a -> V 1 a
pattern V1 x = x :. Nil

{-# COMPLETE V2 #-}
pattern V2 :: a -> a -> V 2 a
pattern V2 x y = x :. y :. Nil

{-# COMPLETE V3 #-}
pattern V3 :: a -> a -> a -> V 3 a
pattern V3 x y z = x :. y :. z :. Nil

{-# COMPLETE V4 #-}
pattern V4 :: a -> a -> a -> a -> V 4 a
pattern V4 x y z w = x :. y :. z :. w :. Nil


------------------------------------------------------------------
-- type classes for vector operations

infixl 6 ^+^, ^-^
infixl 7 ^.^, ^×^
infix  8 ^*, *^

class Num (Scalar v) => Module v where
  type Scalar v :: Type
  type OfDim v (n :: Nat) = res | res -> v n
  (^+^), (^-^) :: KnownNat n => OfDim v n -> OfDim v n -> OfDim v n
  (*^) :: KnownNat n => Scalar v -> OfDim v n -> OfDim v n
  (^*) :: KnownNat n => OfDim v n -> Scalar v -> OfDim v n
  (*^) = flip (^*) -- SPIRV defines VectorTimesScalar not ScalarTimesVector...
  (^-^) x y = x ^+^ ((-1) *^ y)

class Module v => Inner v where
  (^.^), dot :: KnownNat n => OfDim v n -> OfDim v n -> Scalar v
  dot = (^.^)

class Inner v => Cross v where
  (^×^), cross :: OfDim v 3 -> OfDim v 3 -> OfDim v 3
  (^×^) = cross


-- newtype WrapEq (v :: Nat -> Type) n = WrapEq { wrapEq :: v n }
-- 
-- instance {-# INCOHERENT #-} (KnownNat n, HasBool b (v n)) => HasBool b (WrapEq v n) where
--   bool b (WrapEq x) (WrapEq y) = WrapEq ( bool b x y )
-- 
-- instance HasEquality v n => Eq (WrapEq v n) where
--   type Logic (WrapEq v n) = Logic (Scalar v)
--   (WrapEq u) == (WrapEq v) = components u `match` components v
--     where match :: V n (Scalar v) -> V n (Scalar v) -> Logic (Scalar v)
--           match = (and .) . liftA2 (==)

class    (Module v, KnownNat n, Eq (Scalar v), HasBool (Logic (Scalar v)) (OfDim v n)) => HasEquality v n
instance (Module v, KnownNat n, Eq (Scalar v), HasBool (Logic (Scalar v)) (OfDim v n)) => HasEquality v n


------------------------------------------------------------------
-- instances for plain vectors

instance Num a => Module (V 0 a) where
  type Scalar (V 0 a) = a
  type OfDim (V 0 a) n = V n a
  (^+^) = liftA2 (+)
  (^-^) = liftA2 (-)
  v ^* k = fmap (*k) v

instance Num a => Inner (V 0 a) where
  (^.^) = (sum .) . liftA2 (*)

instance Num a => Cross (V 0 a) where
  cross (V3 x y z) (V3 x' y' z') = V3 a b c
    where a = y * z' - z * y'
          b = z * x' - x * z'
          c = x * y' - y * x'

------------------------------------------------------------------
-- derived operations

angle :: (Floating (Scalar v), KnownNat n, Inner v, HasEquality v n) => OfDim v n -> OfDim v n -> Scalar v
angle x y = asin $ dot (normalise x) (normalise y)

norm :: (Floating (Scalar v), KnownNat n, Inner v) => OfDim v n -> Scalar v
norm v = sqrt $ squaredNorm v

squaredNorm :: (KnownNat n, Inner v) => OfDim v n -> Scalar v
squaredNorm v = dot v v

sqdist :: (KnownNat n, Inner v) => OfDim v n -> OfDim v n -> Scalar v
sqdist x y = squaredNorm (x ^-^ y)

distance :: (Floating (Scalar v), KnownNat n, Inner v) => OfDim v n -> OfDim v n -> Scalar v
distance x y = norm (x ^-^ y)

along :: (KnownNat n, Module v) => Scalar v -> OfDim v n -> OfDim v n -> OfDim v n
along t x y = (1-t) *^ x ^+^ t *^ y

normalise :: (Floating (Scalar v), KnownNat n, Inner v, HasEquality v n) => OfDim v n -> OfDim v n
normalise v = 
  let nm = norm v in
  if nm == 0
     then 0 *^ v
     else (1 / nm ) *^ v

reflect :: (Floating (Scalar v), KnownNat n, Inner v, HasEquality v n) => OfDim v n -> OfDim v n -> OfDim v n
reflect v n = reflect' v (normalise n)

reflect' :: (KnownNat n, Inner v) => OfDim v n -> OfDim v n -> OfDim v n
reflect' v n = v ^-^ (2 * dot v n) *^ n -- assumes n is normalised

-- |Projects the first argument onto the second.
proj :: (Fractional (Scalar v), KnownNat n, Inner v, HasEquality v n) => OfDim v n -> OfDim v n -> OfDim v n
proj x y = projC x y *^ y

projC :: (Fractional (Scalar v), KnownNat n, Inner v, HasEquality v n) => OfDim v n -> OfDim v n -> Scalar v
projC x y = 
  let sqNm = dot y y
  in if sqNm == 0
     then 0
     else dot x y / sqNm

isOrthogonal :: (KnownNat n, Inner v, Eq (Scalar v)) => OfDim v n -> OfDim v n -> Logic (Scalar v)
isOrthogonal v w = dot v w == 0

-- |Gram-Schmidt algorithm.
gramSchmidt :: (Floating (Scalar v), KnownNat n, Inner v, HasEquality v n) => [OfDim v n] -> [OfDim v n]
gramSchmidt []     = []
gramSchmidt (x:xs) = x' : gramSchmidt (map (\v -> v ^-^ proj v x') xs)
  where x' = normalise x

rotateAroundAxis :: (Cross v, Floating (Scalar v)) => OfDim v 3 -> Scalar v -> OfDim v 3 -> OfDim v 3
rotateAroundAxis n theta v = cos theta *^ v ^+^ sin theta *^ (n ^×^ v)

------------------------------------------------------------------
-- specific implementation of matrices using nested vectors

type M n m a = V n (V m a)

identityMat :: forall n a. (KnownNat n, Num a) => M n n a
identityMat = 
  case (Proxy :: Proxy 1) %<=? (Proxy :: Proxy n) of
       LE Refl   -> (1 :. repeatV 0) :. fmap (0:.) (identityMat :: M (n-1) (n-1) a)
       NLE nle _ -> case deduceZero nle of
                         Refl -> Nil

newtype WrappedMatrix m n a k = WrappedMatrix { wrappedMatrix :: M m (n+k) a }

transposeMat :: (KnownNat m, KnownNat n) => M m n a -> M n m a
transposeMat rows = repeatV Nil `addCols` rows

addCol' :: (KnownNat m, KnownNat n, KnownNat k) => WrappedMatrix m n a k -> V m a -> WrappedMatrix m n a (k+1)
addCol' WrappedMatrix { wrappedMatrix = mat } col = WrappedMatrix { wrappedMatrix = liftA2 (:.) col mat }

addCol :: forall a m n. (KnownNat m, KnownNat n) => M m n a -> V m a -> M m (n+1) a
addCol mat v = wrappedMatrix $ addCol' (WrappedMatrix { wrappedMatrix = mat } :: WrappedMatrix m n a 0) v

addCols :: (KnownNat m, KnownNat n, KnownNat l) => M m n a -> V l (V m a) -> M m (n+l) a
addCols mat cols = wrappedMatrix $ dfoldrV (flip addCol') (WrappedMatrix mat) cols

rowMatrix :: KnownNat n => V n a -> M 1 n a
rowMatrix = (:. Nil)

columnMatrix :: KnownNat n => V n a -> M n 1 a
columnMatrix = fmap (:. Nil)

blockSum :: forall m1 m2 n1 n2 a. (KnownNat m1, KnownNat m2, KnownNat n1, KnownNat n2, Num a) 
         => M m1 n1 a -> M m2 n2 a -> M (m1+m2) (n1+n2) a
blockSum mat1 mat2 = fmap (<++> repeatV 0) mat1 <++> fmap (repeatV 0 <++>) mat2

fromColumns :: forall l m a. (KnownNat m, KnownNat l) => V l (V m a) -> M m l a
fromColumns = addCols ( repeatV Nil :: M m 0 a)


{-
applyM44 :: Num a => M 4 4 a -> V 3 a -> V 3 a
applyM44 mat (V3 x y z) =
  let V4 x' y' z' _ = mat !*^ (V4 x y z 0)
  in V3 x' y' z'
-}


------------------------------------------------------------------
-- type classes for matrix operations

infixl 6 !+!, !-!
infixl 7 !*!
infix  8 !*^, ^*!
infix  9 *!, !*

class Module (Vector m) => Matrix m where
  type Vector m
  type OfDims m (i :: Nat) (j :: Nat) = res | res -> m i j

  identity    :: KnownNat i                           => OfDims m i i
  diag        :: KnownNat i                           => Scalar (Vector m) -> OfDims m i i
  transpose   :: (KnownNat i, KnownNat j)             => OfDims m i j -> OfDims m j i
  inverse     :: (KnownNat i, KnownNat j)             => OfDims m i j -> OfDims m j i
  determinant :: (KnownNat i, KnownNat j)             => OfDims m i j -> Scalar (Vector m)
  (!+!)       :: (KnownNat i, KnownNat j)             => OfDims m i j -> OfDims m i j -> OfDims m i j
  (!-!)       :: (KnownNat i, KnownNat j)             => OfDims m i j -> OfDims m i j -> OfDims m i j
  konst       :: (KnownNat i, KnownNat j)             => Scalar (Vector m) -> OfDims m i j
  (*!)        :: (KnownNat i, KnownNat j)             => Scalar (Vector m) -> OfDims m i j -> OfDims m i j
  (!*)        :: (KnownNat i, KnownNat j)             => OfDims m i j -> Scalar (Vector m) -> OfDims m i j
  (!*^)       :: (KnownNat i, KnownNat j)             => OfDims m i j -> OfDim (Vector m) j -> OfDim (Vector m) i
  (^*!)       :: (KnownNat i, KnownNat j)             => OfDim (Vector m) i -> OfDims m i j -> OfDim (Vector m) j
  (!*!)       :: (KnownNat i, KnownNat j, KnownNat k) => OfDims m i j -> OfDims m j k -> OfDims m i k

  identity = diag 1
  (*!) = flip (!*) -- SPIRV defines MatrixTimesScalar

instance Num a => Matrix (M 0 0 a) where
  type Vector (M 0 0 a) = V 0 a
  type OfDims (M 0 0 a) i j = M i j a
  identity = identityMat
  diag a = a *! identity
  transpose rows = repeatV Nil `addCols` rows
  m !*! n = fmap (\row -> fmap (sum . liftA2 (*) row) (transposeMat n)) m
  m !+! n = liftA2 ( liftA2 (+) ) m n
  m !-! n = liftA2 ( liftA2 (-) ) m n
  konst = repeatV . repeatV
  m !* a = liftA2 ( liftA2 (*) ) (konst a) m
  m !*^ v = fmap (\row -> sum $ liftA2 (*) row v) m
  v ^*! m = fmap (\row -> sum $ liftA2 (*) row v) (transposeMat m)
  determinant = undefined "todo"
  inverse = undefined "todo"