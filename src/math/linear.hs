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
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

{-|
Module: Math.Linear
Description: @---@ Vectors & matrices, as used by this library. __Import required to use vectors & matrices.__

Vectors and matrices, indexed by dimension.

Vectors are represented as linked lists, whereas matrices are represented as a vector of their rows.
-}

module Math.Linear
  (
  -- * Vector and matrix types
  V(..), M(..)

  -- * Typeclasses
  -- ** Vectors
  , Semimodule(..), Module(..), Inner(..), Cross(..)
  -- ** Matrices
  , Matrix(..)

  -- * General vector operations
  , angle, norm, squaredNorm
  , quadrance, distance
  , along
  , normalise
  , reflect, reflect'
  , proj, projC
  , isOrthogonal, gramSchmidt
  , rotateAroundAxis

  -- * Operations involving the vector data type
  , headV, tailV, headTailV
  , (^!), at
  , sum
  , buildV
  , dfoldrV
  , unfold
  , pattern V0, pattern V1, pattern V2, pattern V3, pattern V4

  -- * Operations involving vectors of vectors
  , addCol, addCols
  , rowMatrix, columnMatrix, fromColumns
  , blockSum

  -- * Operations involving the matrix newtype
  , perspective
  )
where

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

-- fir
import Control.Arrow.Strength(strong)
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GeneratedGradedSemigroup(..)
  , FreeGradedSemigroup(..)
  )
import Math.Logic.Class
  ( Boolean(..), Eq(Logic,(==))
  , Choose(choose), ifThenElse, Triple
  , (#.)
  , Ord(..)
  )
import Math.Algebra.Class
  ( AdditiveGroup(..), Semiring(..), Ring(..)
  , DivisionRing(..), Floating(..)
  )

-------------------------------------------------------------------------------

infixr 3 :.

-- | Vector data type.
--
-- @V m a@ denotes a vector of @m@ inhabitants of @a@.
-- Represented as a linked list.
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

-- | Unsafe indexing.
(^!) :: V n a -> Int -> a
(^!) (a :. _)  0 = a
(^!) Nil       _ = error "empty vector"
(^!) (_ :. as) n = as ^! (n-1)

-- | Safe indexing.
--
-- Use with /TypeApplications/,
-- e.g. to access index @3@ of vector @v@: @at \@3 v@.
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
-- some basic facts about inequalities, bypassing the type-checker
-- a proper solution would be to use a type-checking plugin for this

deduceZero :: KnownNat n => (1 <=? n) :~: 'False -> (n :~: 0)
deduceZero _ = unsafeCoerce Refl

lemma1 :: forall j n. (KnownNat j, KnownNat n)
       => ((j+1) <=? n) :~: 'True -> (CmpNat j n :~: Prelude.LT)
lemma1 _ = unsafeCoerce Refl

lemma2 :: forall j n. (KnownNat j, KnownNat n, j <= n, 1 <= j)
       => CmpNat (n-j) n :~: 'Prelude.LT
lemma2 = unsafeCoerce Refl

lemma3 :: forall j n. (KnownNat j, KnownNat n)
       => (j     <=? n) :~: 'True
       -> ((j+1) <=? n) :~: 'False
       -> (j :~: n)
lemma3 _ _ = unsafeCoerce Refl

-----------------------------------------------------------

-- | Build a vector using an indexing function.
buildV :: forall n a v. KnownNat n
       => ( forall i. (KnownNat i, CmpNat i n ~ Prelude.LT) => Proxy i -> v -> a) -- ^ Indexing function.
       -> v -- ^ Object to index into.
       -> V n a
buildV f v = go @0 Nil where
  go :: forall j. (KnownNat j, j <= n)
     => V j a
     -> V n a
  go w =
    case Proxy @(j+1) %<=? Proxy @n of
        LE Refl
          -> case (lemma1 @j @n Refl, lemma2 @(j+1) @n) of
                (Refl, Refl)
                  -> go @(j+1) (f (Proxy @(n-(j+1))) v :. w)
        NLE Refl Refl
          -> case lemma3 @j @n Refl Refl of
                Refl -> w

-- | Dependent fold of a vector.
-- Folds a vector with a function whose type depends on the index.
dfoldrV :: forall n a b. KnownNat n 
        => (forall k. (KnownNat k, k+1 <= n) => a -> b k -> b (k+1)) -- ^ Function used to fold.
        -> b 0   -- ^ Starting value.
        -> V n a -- ^ Vector to fold.
        -> b n
dfoldrV f d = go
  where go :: (KnownNat m, m <= n) => V m a -> b m
        go Nil     = d
        go (a:.as) = f a (go as)

-- | Unfold: create a vector of specified length,
-- by repeatedly applying a function to a starting value.
unfold :: forall n a. KnownNat n
       => (a -> a) -- ^ Function to iterate.
       -> a        -- ^ Starting value.
       -> V n a
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

instance GeneratedGradedSemigroup (V 0 a) Nat () where
  type GenType    (V 0 a) ()  _  = a
  type GenDeg Nat (V 0 a) () '() = 1
  generator :: a -> V (GenDeg Nat (V 0 a) () unit) a
  generator a = unsafeCoerce (a :. Nil)

instance FreeGradedSemigroup (V 0 a) Nat () where
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
  generated :: (V (GenDeg Nat (V 0 a) () unit) a) -> a
  generated = unsafeCoerce (headV :: V 1 a -> a)

------------------------------------------------------------------

dim :: forall n. KnownNat n => Natural
dim = natVal ( Proxy @n )

------------------------------------------------------------------
-- type classes for vector operations

infixl 6 ^+^, ^-^
infixl 7 ^.^, `dot`, ^×^, `cross`
infix  8 ^*, *^

-- | A semimodule is to a semiring as a module is to a ring.
--
-- That is, a semimodule M over a semiring R consists of
--
--    * a commutative monoid M,
--    * an action of R on M.
--
-- The associated type family 'OfDim' is used to allow class methods
-- which involve elements in different dimensions.
-- This is crucial for the class methods for matrices (see the 'Matrix' type class)
class Semiring (Scalar v) => Semimodule v where
  type Scalar v :: Type
  type OfDim v (n :: Nat) = r | r -> v n
  (^+^) :: KnownNat n => OfDim v n -> OfDim v n -> OfDim v n
  (*^)  :: KnownNat n => Scalar v  -> OfDim v n -> OfDim v n
  (^*)  :: KnownNat n => OfDim v n -> Scalar v  -> OfDim v n
  (*^) = flip (^*) -- SPIRV defines VectorTimesScalar not ScalarTimesVector...

-- | A module M over a ring R consists of:
--
--     * an abelian group M,
--     * an action of R on M.
--
-- That is, we simply add additive inverses to the previous definition.
class (Ring (Scalar v), Semimodule v) => Module v where
  (^-^) :: KnownNat n => OfDim v n -> OfDim v n -> OfDim v n
  (^-^) x y = x ^+^ ((-1) *^ y)

-- | Semimodule with an inner product.
--
-- This usually consists of a non-degenerate symmetric bilinear form on the underlying semimodule.
--
-- Other axioms might be more relevant depending on the underlying ring.
-- For instance, one instead insists on sesquilinearity over the field of complex numbers.
class Semimodule v => Inner v where
  (^.^), dot :: KnownNat n => OfDim v n -> OfDim v n -> Scalar v
  dot = (^.^)

-- | Module with a cross product.
--
-- This usually consists of a non-degenerate skew-symmetric bilinear multiplication satisfying the Jacobi identity.
-- Over the real numbers, this implies the dimension is 3.
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

-- | Angle between two vectors, computed using the inner product structure.
angle :: (Floating (Scalar v), KnownNat n, Inner v, HasEquality v n)
      => OfDim v n -> OfDim v n -> Scalar v
angle x y = acos $ dot (normalise x) (normalise y)

-- | Norm of a vector, computed using the inner product.
norm :: (Floating (Scalar v), KnownNat n, Inner v)
     => OfDim v n -> Scalar v
norm v = sqrt $ squaredNorm v

-- | Squared norm of a vector, computed using the inner product.
squaredNorm :: (KnownNat n, Inner v)
            => OfDim v n -> Scalar v
squaredNorm v = dot v v

-- | Quadrance between two points.
quadrance :: (KnownNat n, Module v, Inner v)
       => OfDim v n -> OfDim v n -> Scalar v
quadrance x y = squaredNorm (x ^-^ y)

-- | Distance between two points.
distance :: (Floating (Scalar v), KnownNat n, Module v, Inner v)
         => OfDim v n -> OfDim v n -> Scalar v
distance x y = norm (x ^-^ y)

-- | Linear interpolation between two points.
along :: (KnownNat n, Module v)
      => Scalar v  -- ^ Interpolation factor. 0 = start, 1 = end.
      -> OfDim v n -- ^ Start.
      -> OfDim v n -- ^ End.
      -> OfDim v n
along t x y = (1-t) *^ x ^+^ t *^ y

-- | Normalises a vector to have unit norm.
normalise :: (Floating (Scalar v), KnownNat n, Inner v, HasEquality v n)
          => OfDim v n -> OfDim v n
normalise v =
  let nm = norm v in
  if nm == 0
     then 0 *^ v
     else (1 / nm ) *^ v

-- | Reflects a vector along a hyperplane, specified by a normal vector.
-- /Computes the normalisation of the normal vector in the process./
reflect :: (Floating (Scalar v), KnownNat n, Module v, Inner v, HasEquality v n)
        => OfDim v n -- ^ Vector to be reflected.
        -> OfDim v n -- ^ A normal vector of the reflecting hyperplane.
        -> OfDim v n
reflect v n = reflect' v (normalise n)

-- | Same as 'reflect': reflects a vector along a hyperplane, specified by a normal vector.
-- However, /__this function assumes the given normal vector is already normalised__/.
reflect' :: (KnownNat n, Module v, Inner v)
         => OfDim v n -- ^ Vector to be reflected.
         -> OfDim v n -- ^ /__Normalised__/ normal vector of the reflecting hyperplane.
         -> OfDim v n
reflect' v n = v ^-^ (2 * dot v n) *^ n

-- | Projects the first argument onto the second.
proj :: (DivisionRing (Scalar v), KnownNat n, Inner v, HasEquality v n)
     => OfDim v n -> OfDim v n -> OfDim v n
proj x y = projC x y *^ y

-- | Projection constant: how far along the projection of the first vector lands along the second vector.
projC :: forall n v. (DivisionRing (Scalar v), KnownNat n, Inner v, HasEquality v n)
      => OfDim v n -> OfDim v n -> Scalar v
projC x y =
  let sqNm = dot y y
  in if sqNm == 0
     then 0 :: Scalar v
     else dot x y / sqNm

-- | Orthogonality test.
--
-- /__Uses a precise equality test, so use at your own risk with floating point numbers.__/
isOrthogonal :: (KnownNat n, Module v, Inner v, Eq (Scalar v))
             => OfDim v n -> OfDim v n -> Logic (Scalar v)
isOrthogonal v w = dot v w == 0

-- | Gram-Schmidt algorithm.
gramSchmidt :: (Floating (Scalar v), KnownNat n, Module v, Inner v, HasEquality v n)
            => [OfDim v n] -> [OfDim v n]
gramSchmidt []     = []
gramSchmidt (x:xs) = x' : gramSchmidt (map (\v -> v ^-^ proj v x') xs)
  where x' = normalise x

-- | Rotate a vector around an axis (in dimension 3), using the cross product.
rotateAroundAxis :: (Cross v, Floating (Scalar v))
                 => OfDim v 3 -- ^ Axis of rotation.
                 -> Scalar v  -- ^ Angle of rotation, according to the right hand rule.
                 -> OfDim v 3 -- ^ Vector to be rotated.
                 -> OfDim v 3
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

-- | Matrix newtype.
--
-- @M m n a@ denotes a matrix with @m@ rows and @n@ columns.
--
-- This is represented as a vector of rows. That is, the representation is row major.
-- Note that this contrasts with the OpenGL/Vulkan default,
-- as both these frameworks represent matrices in column major order.
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
deriving instance (KnownNat n, KnownNat m, Storable a) => Storable (M m n a)

deriving via '(V m (V n x), V m (V n y), V m (V n z))
  instance (KnownNat m, KnownNat n, Choose b '(x,y,z)) => Choose b '(M m n x, M m n y, M m n z)

------------------------------------------------------------------
-- graded semigroup for matrices

instance KnownNat m => GradedSemigroup (M m 0 a) Nat where
  type Apply Nat (M m 0 a) i = M m i a
  type i :<!>: j = i + j
  (<!>) :: M m i a -> M m j a -> M m (i+j) a
  (M m1) <!> (M m2) = M (liftA2 (<!>) m1 m2)

instance KnownNat m => GeneratedGradedSemigroup (M m 0 a) Nat () where
  type GenType    (M m 0 a) ()  _  = V m a
  type GenDeg Nat (M m 0 a) () '() = 1
  generator :: V m a -> M m (GenDeg Nat (M m 0 a) () unit) a
  generator = ( unsafeCoerce ( M . columnMatrix :: V m a -> M m 1 a ) )

instance KnownNat m => FreeGradedSemigroup (M m 0 a) Nat () where
  type ValidDegree (M m 0 a) i = KnownNat i
  (>!<) :: forall i j. (KnownNat i, KnownNat j) => M m (i+j) a -> ( M m i a, M m j a )
  (>!<) (M m)
    = let u :: V i (V m a)
          v :: V j (V m a)
          (u, v) = (>!<) (distribute m)
      in (M (distribute u), M (distribute v))
  generated :: M m (GenDeg Nat (M m 0 a) () unit) a -> V m a
  generated = ( unsafeCoerce ( ( \(M m) -> headV (distribute m) ) :: M m 1 a -> V m a ) )

------------------------------------------------------------------
-- type classes for matrix operations

infixl 6 !+!, !-!
infixl 7 !*!
infix  8 !*^, ^*!
infix  9 *!, !*

-- | Typeclass for matrix operations.
--
-- The 'OfDims' associated type family allows the type class methods to involve various dimension indices.
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
  (*!) = flip (!*) -- SPIR-V defines MatrixTimesScalar

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
  determinant = error "determinant: not implemented"
  inverse     = error "inverse matrix: not implemented"

------------------------------------------------------------------
-- utility 4x4 matrices for camera viewpoints in 3D

-- | Affine 3D transformation matrix for a perspective view.
-- (Taken from Edward Kmett's __Linear__ library.)
perspective
  :: Floating a
  => a -- ^ FOV.
  -> a -- ^ Aspect ratio.
  -> a -- ^ Near plane.
  -> a -- ^ Far plane.
  -> M 4 4 a
perspective fovy aspect near far
  = M $ V4 (V4 x 0 0    0)
           (V4 0 y 0    0)
           (V4 0 0 z    w)
           (V4 0 0 (-1) 0)
  where tanHalfFovy = tan $ fovy / 2
        x = 1 / (aspect * tanHalfFovy)
        y = 1 / tanHalfFovy
        fpn = far + near
        fmn = far - near
        oon = 0.5/near
        oof = 0.5/far
        z = -fpn/fmn
        w = 1/(oof-oon)
