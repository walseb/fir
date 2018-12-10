{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Math.Linear where

-- base
import Prelude hiding
  ( Eq(..), (&&), (||)
  , Ord(..)
  , Num(..), sum
  , Fractional(..), Floating(..)
  , Ordering(..)
  )
import qualified Prelude
import Control.Applicative(liftA2)
import Control.Arrow(second)
import Data.Foldable(traverse_)
import Data.Kind(Type)
import Data.Type.Equality((:~:)(Refl))
import Data.Proxy(Proxy(Proxy))
import Foreign.Ptr(Ptr, castPtr)
import Foreign.Storable(Storable(alignment, sizeOf, peek, poke, peekElemOff, pokeElemOff))
import GHC.Base(Int#, Int(I#), (+#))
import GHC.TypeLits.Compare((:<=?)(LE,NLE), (%<=?))
import GHC.TypeNats
  ( Nat, KnownNat, natVal
  , type (+), type (-)
  , CmpNat, type (<=), type (<=?)
  )
import Numeric.Natural(Natural)
import Unsafe.Coerce(unsafeCoerce)

-- binary
import Data.Binary(Binary(put,get))

-- distributive
import Data.Distributive(Distributive(..))

-- lens
import Control.Lens.Iso(Iso', iso)

-- fir
import Control.Arrow.Strength(strong)
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GradedPresentedSemigroup(..)
  , GradedFreeSemigroup(..)
  )
import Math.Logic.Class
  ( Boolean(..), Eq(Logic,(==))
  , Choose(choose), ifThenElse, Triple
  , (#.)
  , Ord(..)
  )
import Math.Algebra.Class(AdditiveGroup(..), Semiring(..), Ring(..), DivisionRing(..), Floating(..))

infixr 3 :.

data V :: Nat -> Type -> Type where
  Nil  :: V 0 a
  (:.) :: a -> V n a -> V (1+n) a

deriving instance Functor     (V n)
deriving instance Foldable    (V n)
deriving instance Traversable (V n)
deriving instance Show a => Show (V n a)

deriving instance Prelude.Eq  a => Prelude.Eq  (V n a)
deriving instance Prelude.Ord a => Prelude.Ord (V n a)

instance (KnownNat n, Binary a) => Binary (V n a) where
  put = traverse_ put
  get = case Proxy @1 %<=? Proxy @n of
            LE Refl   -> liftA2 (:.) 
                           get 
                           $ get @(V (n-1) a)
            NLE nle _ ->
              case deduceZero nle of
                   Refl -> pure Nil

instance KnownNat n => Distributive (V n) where
  distribute :: Functor f => f (V n a) -> V n (f a)
  distribute
    = case Proxy @1 %<=? Proxy @n of
        LE Refl   -> uncurry (:.) 
                   . second distribute
                   . strong
                   . fmap headTailV
        NLE nle _ ->
          case deduceZero nle of
               Refl -> const Nil
    
instance KnownNat n => Applicative (V n) where
  pure                    = unfold id
  _         <*>  Nil      = Nil
  (f :. fs) <*> (a :. as) = f a :. fs <*> as
  _         <*> _         = error "unreachable"


instance (KnownNat n, Choose b '(x,y,z))
        => Choose b '(V n x, V n y, V n z) where
  choose b = liftA2 ( choose b )

instance (KnownNat n, Eq a) => Eq (V n a) where
  type Logic (V n a) = Logic a
  (==) = (foldr (&&) true .) . liftA2 (==)

instance (KnownNat n, Ord a) => Ord (V n a) where
  type Ordering (V n a) = Ordering a
  compare = error "todo"

  Nil <= Nil = true
  (a :. as) <= (b :. bs) = a < b || (a <= b && as <= bs)
  _ <= _ = error "unreachable"

  Nil < Nil = false
  (a :. as) <  (b :. bs) = a < b || (a <= b && as <  bs)
  _ < _ = error "unreachable"

  min = liftA2 min
  max = liftA2 max

instance (KnownNat n, Semigroup a) => Semigroup (V n a) where
  (<>) = liftA2 (<>)

instance (KnownNat n, Monoid a) => Monoid (V n a) where
  mempty = pure mempty

headV :: (KnownNat n, 1 <= n) => V n a -> a
headV (a :. _) = a

tailV :: (KnownNat n, 1 <= n) => V n a -> V (n-1) a
tailV (_ :. as) = as

headTailV :: (KnownNat n, 1 <= n) => V n a -> (a, V (n-1) a)
headTailV (a :. as) = ( a, as )

instance (KnownNat n, Storable a) => Storable (V n a) where
  sizeOf :: V n a -> Int
  sizeOf _ = n * sizeOf (undefined :: a)
    where n = fromIntegral $ natVal (Proxy @n)

  alignment _ = alignment (undefined :: a)
  
  poke ptr u = go u 0#
    where go :: KnownNat m => V m a -> Int# -> IO()
          go Nil     _  = pure ()
          go (a :. v) n# = do
              pokeElemOff (castPtr ptr) (I# n#) a
              go v (n# +# 1#)
  
  peek :: Ptr (V n a) -> IO (V n a)
  peek ptr = traverse (peekElemOff (castPtr ptr)) ixVec
      where ixVec :: V n Int
            ixVec = unfold pred (fromIntegral $ natVal (Proxy @n))

infixl 9 ^!

-- unsafe indexing
(^!) :: V n a -> Int -> a
(^!) (a :. _)  0 = a
(^!) Nil       _ = error "empty vector"
(^!) (_ :. as) n = as ^! (n-1)

-- safe indexing
at :: forall i n a. (KnownNat i, KnownNat n, CmpNat i n ~ Prelude.LT)
   => V n a -> a
at v = v ^! (fromIntegral (dim @i))

-----------------------------------------------------------
-- todo: temporary workaround
newtype Sum a = Sum { getSum :: a }
instance AdditiveGroup a => Semigroup (Sum a) where
  (Sum a) <> (Sum b) = Sum (a + b)
instance AdditiveGroup a => Monoid (Sum a) where
  mempty = Sum zero

sum :: (AdditiveGroup a, Traversable t) => t a -> a
sum = getSum #. foldMap Sum
-----------------------------------------------------------



-- TODO: indexing is slightly off, I'm doing {0, ..., n} instead of {1, ..., n}
buildV :: forall n a v. KnownNat n
       => ( forall i. (KnownNat i, i <= n) => Proxy i -> v -> a)
       -> v
       -> V n a
buildV f u = go @n f u Nil where
  go :: forall j. (KnownNat j, j <= n)
     => ( forall i. (KnownNat i, i <= n) => Proxy i -> v -> a)
     -> v
     -> V (n-j) a
     -> V n a
  go g v w =
    case Proxy @1 %<=? Proxy @j of
         LE Refl   -> 
          case deduceOK @j @n Refl  of
               Refl -> go @(j-1) g v (g (Proxy @j) v :. w)
         NLE nle _ -> 
          case deduceZero nle of
               Refl -> w

deduceZero :: KnownNat n => (1 <=? n) :~: 'False -> (n :~: 0)
deduceZero = unsafeCoerce

deduceOK :: (KnownNat j, KnownNat n) => (j <=? n) :~: 'True -> ((j-1) <=? n) :~: 'True
deduceOK = unsafeCoerce

dfoldrV :: forall n a b. KnownNat n 
        => (forall k. (KnownNat k, k+1 <= n) => a -> b k -> b (k+1))
        -> b 0 -> V n a -> b n
dfoldrV f d = go
  where go :: (KnownNat m, m <= n) => V m a -> b m
        go Nil    = d
        go (a:.as) = f a (go as)

unfold :: forall n a. KnownNat n => (a -> a) -> a -> V n a
unfold f a 
    = case Proxy @1 %<=? Proxy @n of
           LE Refl   -> let b = f a in b :. (unfold f b :: V (n-1) a)
           NLE nle _ -> 
            case deduceZero nle of
                 Refl -> Nil

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
-- graded semigroup for vectors

instance GradedSemigroup (V 0 a) Nat where
  type Apply Nat (V 0 a) i = V i a
  type i :<!>: j = i + j
  (<!>) :: V i a -> V j a -> V (i+j) a
  (<!>) Nil      v = v
  (<!>) (a:.as)  v = a :. (as <!> v)

instance GradedPresentedSemigroup (V 0 a) Nat () where
  type Element    (V 0 a) ()  _  = a
  type Degree Nat (V 0 a) () '() = 1
  homogeneous :: Iso' (V (Degree Nat (V 0 a) () unit) a) a
  homogeneous 
    = iso
        ( unsafeCoerce (headV :: V 1 a -> a) )
        ( unsafeCoerce ( (:. Nil) :: a -> V 1 a ) )

instance GradedFreeSemigroup (V 0 a) Nat () where
  type ValidDegree (V 0 a) n = KnownNat n
  (>!<) :: forall i j. (KnownNat i, KnownNat j) => V (i+j) a -> ( V i a, V j a )
  (>!<) Nil = unsafeCoerce ( Nil, Nil )
  (>!<) (a :. as)
    = case Proxy @1 %<=? Proxy @i of
         LE Refl   ->
            let u :: V (i-1) a
                v :: V j a
                (u, v) = (>!<) (as :: V ((i+j)-1) a)
            in (a :. u, v)
         NLE _ _ -> unsafeCoerce ( Nil, a :. as )

------------------------------------------------------------------

dim :: forall n. KnownNat n => Natural
dim = natVal ( Proxy @n )

------------------------------------------------------------------
-- type classes for vector operations

infixl 6 ^+^, ^-^
infixl 7 ^.^, ^×^
infix  8 ^*, *^

class Semiring (Scalar v) => Semimodule v where
  type Scalar v :: Type
  type OfDim v (n :: Nat) = r | r -> v n
  (^+^) :: KnownNat n => OfDim v n -> OfDim v n -> OfDim v n
  (*^)  :: KnownNat n => Scalar v  -> OfDim v n -> OfDim v n
  (^*)  :: KnownNat n => OfDim v n -> Scalar v  -> OfDim v n
  (*^) = flip (^*) -- SPIRV defines VectorTimesScalar not ScalarTimesVector...

class (Ring (Scalar v), Semimodule v) => Module v where
  (^-^) :: KnownNat n => OfDim v n -> OfDim v n -> OfDim v n
  (^-^) x y = x ^+^ ((-1) *^ y)

class Semimodule v => Inner v where
  (^.^), dot :: KnownNat n => OfDim v n -> OfDim v n -> Scalar v
  dot = (^.^)

class Module v => Cross v where
  (^×^), cross :: OfDim v 3 -> OfDim v 3 -> OfDim v 3
  (^×^) = cross

class    ( Semimodule v, KnownNat n
         , Eq (Scalar v)
         , Choose (Logic (Scalar v)) (Triple (Scalar v))
         , Choose (Logic (Scalar v)) (Triple (OfDim v n))
         ) => HasEquality v n
instance ( Semimodule v, KnownNat n
         , Eq (Scalar v)
         , Choose (Logic (Scalar v)) (Triple (Scalar v))
         , Choose (Logic (Scalar v)) (Triple (OfDim v n))
         ) => HasEquality v n

------------------------------------------------------------------
-- instances for plain vectors

instance Semiring a => Semimodule (V 0 a) where
  type Scalar (V 0 a) = a
  type OfDim (V 0 a) n = V n a
  (^+^) = liftA2 (+)
  v ^* k = fmap (*k) v

instance Ring a => Module (V 0 a) where
  (^-^) = liftA2 (-)  

instance Semiring a => Inner (V 0 a) where
  (^.^) = (sum .) . liftA2 (*)

instance Ring a => Cross (V 0 a) where
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

sqdist :: (KnownNat n, Module v, Inner v) => OfDim v n -> OfDim v n -> Scalar v
sqdist x y = squaredNorm (x ^-^ y)

distance :: (Floating (Scalar v), KnownNat n, Module v, Inner v) => OfDim v n -> OfDim v n -> Scalar v
distance x y = norm (x ^-^ y)

along :: (KnownNat n, Module v) => Scalar v -> OfDim v n -> OfDim v n -> OfDim v n
along t x y = (1-t) *^ x ^+^ t *^ y

normalise :: (Floating (Scalar v), KnownNat n, Inner v, HasEquality v n) => OfDim v n -> OfDim v n
normalise v =
  let nm = norm v in
  if nm == 0
     then 0 *^ v
     else (1 / nm ) *^ v

reflect :: (Floating (Scalar v), KnownNat n, Module v, Inner v, HasEquality v n) => OfDim v n -> OfDim v n -> OfDim v n
reflect v n = reflect' v (normalise n)

reflect' :: (KnownNat n, Module v, Inner v) => OfDim v n -> OfDim v n -> OfDim v n
reflect' v n = v ^-^ (2 * dot v n) *^ n -- assumes n is normalised

-- |Projects the first argument onto the second.
proj :: (DivisionRing (Scalar v), KnownNat n, Inner v, HasEquality v n) => OfDim v n -> OfDim v n -> OfDim v n
proj x y = projC x y *^ y

projC :: forall n v. (DivisionRing (Scalar v), KnownNat n, Inner v, HasEquality v n) => OfDim v n -> OfDim v n -> Scalar v
projC x y =
  let sqNm = dot y y
  in if sqNm == 0
     then 0 :: Scalar v
     else dot x y / sqNm

isOrthogonal :: (KnownNat n, Module v, Inner v, Eq (Scalar v)) => OfDim v n -> OfDim v n -> Logic (Scalar v)
isOrthogonal v w = dot v w == 0

-- |Gram-Schmidt algorithm.
gramSchmidt :: (Floating (Scalar v), KnownNat n, Module v, Inner v, HasEquality v n) => [OfDim v n] -> [OfDim v n]
gramSchmidt []     = []
gramSchmidt (x:xs) = x' : gramSchmidt (map (\v -> v ^-^ proj v x') xs)
  where x' = normalise x

rotateAroundAxis :: (Cross v, Floating (Scalar v)) => OfDim v 3 -> Scalar v -> OfDim v 3 -> OfDim v 3
rotateAroundAxis n theta v = cos theta *^ v ^+^ sin theta *^ (n ^×^ v)

------------------------------------------------------------------
-- specific implementation of matrices using nested vectors

identityMat :: forall n a. (KnownNat n, Ring a) => V n (V n a)
identityMat =
  case (Proxy :: Proxy 1) %<=? (Proxy :: Proxy n) of
       LE Refl   -> (1 :. pure 0) :. fmap (0:.) (identityMat :: V (n-1) (V (n-1) a) )
       NLE nle _ -> case deduceZero nle of
                         Refl -> Nil

newtype WrappedMatrix m n a k = WrappedMatrix { wrappedMatrix :: V m (V (n+k) a) }

addCol' :: (KnownNat m, KnownNat n, KnownNat k) => WrappedMatrix m n a k -> V m a -> WrappedMatrix m n a (k+1)
addCol' WrappedMatrix { wrappedMatrix = mat } col = WrappedMatrix { wrappedMatrix = liftA2 (:.) col mat }

addCol :: forall a m n. (KnownNat m, KnownNat n) => V m (V n a) -> V m a -> V m (V (n+1) a)
addCol mat v = wrappedMatrix $ addCol' (WrappedMatrix { wrappedMatrix = mat } :: WrappedMatrix m n a 0) v

addCols :: (KnownNat m, KnownNat n, KnownNat l) => V m (V n a) -> V l (V m a) -> V m (V (n+l) a)
addCols mat cols = wrappedMatrix $ dfoldrV (flip addCol') (WrappedMatrix mat) cols

rowMatrix :: KnownNat n => V n a -> V 1 (V n a)
rowMatrix = (:. Nil)

columnMatrix :: KnownNat n => V n a -> V n (V 1 a)
columnMatrix = fmap (:. Nil)

blockSum :: forall m1 m2 n1 n2 a. (KnownNat m1, KnownNat m2, KnownNat n1, KnownNat n2, Semiring a) 
         => V m1 (V n1 a)-> V m2 (V n2 a) -> V (m1+m2) (V (n1+n2) a)
blockSum mat1 mat2 = fmap (<!> pure zero) mat1 <!> fmap (pure zero <!>) mat2

fromColumns :: forall l m a. (KnownNat m, KnownNat l) => V l (V m a) -> V m (V l a)
fromColumns = addCols ( pure Nil :: V m (V 0 a))


------------------------------------------------------------------
-- matrix newtype

newtype M m n a = M { unM :: V m (V n a) }
deriving instance Prelude.Eq a => Prelude.Eq (M m n a)
deriving instance (KnownNat m, KnownNat n, Prelude.Ord a) => Prelude.Ord (M m n a)
deriving instance (KnownNat m, KnownNat n, Eq   a) => Eq   (M m n a)
deriving instance (KnownNat m, KnownNat n, Ord  a) => Ord  (M m n a)
deriving instance (KnownNat m, KnownNat n, Show a) => Show (M m n a)
deriving instance Functor (M m n)
deriving instance (KnownNat m, KnownNat n) => Foldable    (M m n)
deriving instance (KnownNat m, KnownNat n) => Traversable (M m n)
deriving instance (KnownNat m, KnownNat n, Binary a) => Binary (M m n a)

deriving via '(V m (V n x), V m (V n y), V m (V n z))
  instance (KnownNat m, KnownNat n, Choose b '(x,y,z)) => Choose b '(M m n x, M m n y, M m n z)

------------------------------------------------------------------
-- graded semigroup for matrices

instance KnownNat m => GradedSemigroup (M m 0 a) Nat where
  type Apply Nat (M m 0 a) i = M m i a
  type i :<!>: j = i + j
  (<!>) :: M m i a -> M m j a -> M m (i+j) a
  (M m1) <!> (M m2) = M (liftA2 (<!>) m1 m2)

instance KnownNat m => GradedPresentedSemigroup (M m 0 a) Nat () where
  type Element    (M m 0 a) ()  _  = V m a
  type Degree Nat (M m 0 a) () '() = 1
  homogeneous :: Iso' (M m (Degree Nat (M m 0 a) () unit) a) (V m a)
  homogeneous
    = iso
        ( unsafeCoerce ( (\(M m) -> (headV (distribute m) )) :: M m 1 a -> V m a ) )
        ( unsafeCoerce ( M . columnMatrix :: V m a -> M m 1 a ) )

instance KnownNat m => GradedFreeSemigroup (M m 0 a) Nat () where
  type ValidDegree (M m 0 a) i = KnownNat i
  (>!<) :: forall i j. (KnownNat i, KnownNat j) => M m (i+j) a -> ( M m i a, M m j a )
  (>!<) (M m)
    = let u :: V i (V m a)
          v :: V j (V m a)
          (u, v) = (>!<) (distribute m)
      in (M (distribute u), M (distribute v))

------------------------------------------------------------------
-- type classes for matrix operations

infixl 6 !+!, !-!
infixl 7 !*!
infix  8 !*^, ^*!
infix  9 *!, !*

class Module (Vector m) => Matrix m where
  type Vector m
  type OfDims m (i :: Nat) (j :: Nat) = r | r -> m i j

  identity    :: KnownNat i                           => OfDims m i i
  diag        :: KnownNat i                           => Scalar (Vector m) -> OfDims m i i
  inverse     :: KnownNat i                           => OfDims m i i -> OfDims m i i
  determinant :: KnownNat i                           => OfDims m i i -> Scalar (Vector m)
  transpose   :: (KnownNat i, KnownNat j)             => OfDims m i j -> OfDims m j i
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

instance Ring a => Matrix (M 0 0 a) where
  type Vector (M 0 0 a) = V 0 a
  type OfDims (M 0 0 a) i j = M i j a
  identity = M identityMat
  diag a = a *! identity
  transpose (M m) = M ( distribute m )
  (M m1) !+! (M m2) = M $ liftA2 ( liftA2 (+) ) m1 m2
  (M m1) !-! (M m2) = M $ liftA2 ( liftA2 (-) ) m1 m2
  konst = M . pure . pure
  (M m) !* a = M $ liftA2 ( liftA2 (*) ) ka m
    where M ka = konst a
  (M m) !*^ v = fmap (\row -> sum $ liftA2 (*) row v) m
  v ^*! m = fmap (\row -> sum $ liftA2 (*) row v) mt
    where M mt = transpose m
  (M m) !*! n = M $ fmap (\row -> fmap (sum . liftA2 (*) row) nt) m
    where M nt = transpose n
  determinant = error "todo"
  inverse     = error "todo"