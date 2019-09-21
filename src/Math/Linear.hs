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
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
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
  , VectorOf, Matrix(..)

  -- * General vector operations
  , angle, norm, squaredNorm
  , quadrance, distance
  , along
  , reflect, reflect'
  , proj, projC
  , isOrthogonal, gramSchmidt
  , rotateAroundAxis

  -- * Operations involving the vector data type
  , headV, tailV, headTailV
  , (^!), at
  , sum
  , buildV, mkVec
  , dfoldrV
  , unfold
  , pattern V0, pattern V1, pattern V2, pattern V3, pattern V4

  -- * Operations involving vectors of vectors
  , addCol, addCols
  , rowMatrix, columnMatrix, fromColumns
  , blockSum

  -- * Operations involving the matrix newtype
  , perspective, lookAt
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
import Control.Applicative
  ( liftA2 )
import Control.Arrow
  ( second )
import Data.Foldable
  ( traverse_ )
import Data.Functor.Compose
  ( Compose(..) )
import Data.Kind
  ( Type, Constraint )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Proxy
  ( Proxy(Proxy) )
import Foreign.Ptr
  ( Ptr, castPtr )
import Foreign.Storable
  ( Storable(alignment, sizeOf, peek, poke, peekElemOff, pokeElemOff) )
import GHC.Base
  ( Int#, Int(I#), (+#) )
import GHC.TypeLits.Compare
  ( (:<=?)(LE,NLE), (%<=?) )
import GHC.TypeNats
  ( Nat, KnownNat, natVal
  , type (+), type (-), type (*)
  , CmpNat, type (<=), type (<=?)
  )
import Numeric.Natural
  ( Natural )
import Unsafe.Coerce
  ( unsafeCoerce )

-- binary
import Data.Binary
  ( Binary(put,get) )

-- distributive
import Data.Distributive
  ( Distributive(..) )

-- fir
import Control.Arrow.Strength
  ( strong )
import {-# SOURCE #-} Data.Product
  ( IsProduct(..)
  , HList(HNil, (:>))
  )
import Data.Type.List
  ( Replicate
  , Length, KnownLength
  )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(..)
  , GeneratedGradedSemigroup(..)
  , FreeGradedSemigroup(..)
  )
import Math.Logic.Class
  ( Boolean(..), Eq(Logic,(==))
  , Choose(choose)
  , (#.)
  , Ord(..)
  )
import Math.Algebra.Class
  ( AdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), Ring
  , DivisionRing(..), Floating(..)
  )

-------------------------------------------------------------------------------

infixr 3 :.

-- | Vector data type.
--
-- @V m a@ denotes a vector of @m@ inhabitants of @a@.
-- Represented as a linked list.
data V :: Nat -> Type -> Type where
  VNil :: V 0 a
  (:.) :: a -> V n a -> V (1+n) a

deriving instance Functor     (V n)
deriving instance Foldable    (V n)
deriving instance Traversable (V n)

instance (KnownNat n, Show a) => Show (V n a) where
  show :: V n a -> String
  show v = "V" ++ n ++ foldMap ( \a -> " " ++ show a ) v
    where n :: String
          n = show (natVal (Proxy @n))

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
                   Refl -> pure VNil

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
               Refl -> const VNil
    
instance KnownNat n => Applicative (V n) where
  pure                    = unfold id
  _         <*>  VNil     = VNil
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

  VNil <= VNil = true
  (a :. as) <= (b :. bs) = a < b || (a <= b && as <= bs)
  _ <= _ = error "unreachable"

  VNil < VNil = false
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

splitAtV
  :: forall (s :: Nat) (n :: Nat) (a :: Type)
  .  ( KnownNat s, KnownNat n, s <= n )
  => V n a
  -> ( V s a, V (n-s) a )
splitAtV v = case Proxy @1 %<=? Proxy @s of
  LE Refl -> case lte_transitive @1 @s @n of
    Refl -> case headTailV v of
      ( h, t ) -> case splitAtV @(s-1) t of
        ( hs, d ) -> ( h :. hs, d )
  NLE nle _ -> case deduceZero nle of
    Refl -> ( VNil, v )

instance (KnownNat n, Storable a) => Storable (V n a) where
  sizeOf :: V n a -> Int
  sizeOf _ = n * sizeOf (undefined :: a)
    where n = fromIntegral $ natVal (Proxy @n)

  alignment _ = alignment (undefined :: a)
  
  poke ptr u = go u 0#
    where go :: KnownNat m => V m a -> Int# -> IO()
          go VNil     _  = pure ()
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
(^!) VNil      _ = error "empty vector"
(^!) (_ :. as) n = as ^! (n-1)

-- | Safe indexing.
--
-- Use with /TypeApplications/,
-- e.g. to access index @3@ of vector @v@: @at \@3 v@.
at :: forall i n a. (KnownNat i, KnownNat n, CmpNat i n ~ Prelude.LT)
   => V n a -> a
at v = v ^! fromIntegral (dim @i)

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

lemma4 :: forall j n. ( j <= n, 1 <= (n-j) )
       => ( CmpNat j n :~: Prelude.LT )
lemma4 = unsafeCoerce Refl

lemma5 :: forall j n. ( j <= n, 1 <= (n-j) )
       => ( (j+1) <=? n ) :~: 'True
lemma5 = unsafeCoerce Refl

lte_transitive
  :: forall i j k
  .  ( KnownNat i, KnownNat j, KnownNat k, i <= j, j <= k )
  => ( i <=? k ) :~: 'True
lte_transitive = unsafeCoerce Refl

-----------------------------------------------------------

-- | Build a vector by indexing into another object.
buildV :: forall n a v. KnownNat n
       => ( forall i. (KnownNat i, CmpNat i n ~ Prelude.LT) => Proxy i -> v -> a) -- ^ Indexing function.
       -> v -- ^ Object to index into.
       -> V n a
buildV f v = go @0 VNil where
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

-- | Build a vector using a generating function.
mkVec :: forall n a. KnownNat n
      => ( forall i. (KnownNat i, CmpNat i n ~ Prelude.LT) => Proxy i -> a)
      -> V n a
mkVec f = go @0
  where
    go :: forall j. (KnownNat j, j <= n) => V (n-j) a
    go = case Proxy @1 %<=? Proxy @(n-j) of
      NLE nle _ ->
        case deduceZero nle of
          Refl -> VNil
      LE Refl -> case ( lemma4 @j @n, lemma5 @j @n ) of
        (Refl, Refl) -> f ( Proxy @j ) :. go @(j+1)

-- | Dependent fold of a vector.
-- Folds a vector with a function whose type depends on the index.
dfoldrV :: forall n a b. KnownNat n 
        => (forall k. (KnownNat k, k+1 <= n) => a -> b k -> b (k+1)) -- ^ Function used to fold.
        -> b 0   -- ^ Starting value.
        -> V n a -- ^ Vector to fold.
        -> b n
dfoldrV f d = go
  where go :: (KnownNat m, m <= n) => V m a -> b m
        go VNil    = d
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
            Refl -> VNil


{-# COMPLETE V0 #-}
pattern V0 :: V 0 a
pattern V0 = VNil

{-# COMPLETE V1 #-}
pattern V1 :: a -> V 1 a
pattern V1 x = x :. VNil

{-# COMPLETE V2 #-}
pattern V2 :: a -> a -> V 2 a
pattern V2 x y = x :. y :. VNil

{-# COMPLETE V3 #-}
pattern V3 :: a -> a -> a -> V 3 a
pattern V3 x y z = x :. y :. z :. VNil

{-# COMPLETE V4 #-}
pattern V4 :: a -> a -> a -> a -> V 4 a
pattern V4 x y z w = x :. y :. z :. w :. VNil

------------------------------------------------------------------
-- products for vectors

instance (KnownLength rs, Replicated n a rs)
      => IsProduct (V n a) rs where
  fromHList = fromHListVec replication
  toHList   = toHListVec   replication

data Replication n a rs where
  NilReplication  :: Replication 0 a '[]
  ConsReplication :: Replication n a rs -> Replication (1+n) a (a ': rs)

class ( rs ~ Replicate n a, KnownLength rs, n ~ Length rs )
    => Replicated n a rs | rs -> n where
  replication :: Replication n a rs
instance Replicated 0 a '[] where
  replication = NilReplication
instance (m ~ (1+n), rs ~ Replicate (m-1) a, (a ': rs) ~ Replicate m a, n ~ Length rs, Replicated n a rs)
       => Replicated m a (a ': rs) where
  replication = ConsReplication ( replication @n @a @rs )

fromHListVec :: Replication n a rs -> HList rs -> V n a
fromHListVec NilReplication          _        = VNil
fromHListVec (ConsReplication repl) (x :> xs) = x :. fromHListVec repl xs

toHListVec :: Replication n a rs -> V n a -> HList rs
toHListVec NilReplication         _         = HNil
toHListVec (ConsReplication repl) (x :. xs) = x :> toHListVec repl xs
toHListVec (ConsReplication _   ) VNil      = error "impossible"

instance GradedSemigroup (V 0 a) Nat where
  type Grade Nat (V 0 a) i = V i a
  type i :<!>: j = i + j
  (<!>) :: V i a -> V j a -> V (i+j) a
  (<!>) VNil     v = v
  (<!>) (a:.as)  v = a :. (as <!> v)

instance GeneratedGradedSemigroup (V 0 a) Nat () where
  type GenType    (V 0 a) ()  _  = a
  type GenDeg Nat (V 0 a) () '() = 1
  generator :: a -> V (GenDeg Nat (V 0 a) () unit) a
  generator a = unsafeCoerce (a :. VNil)
  --              ^^^
  -- GHC cannot deduce unit ~ '()
  -- see [GHC trac #7259](https://gitlab.haskell.org/ghc/ghc/issues/7259)

instance FreeGradedSemigroup (V 0 a) Nat () where
  type ValidDegree (V 0 a) n = KnownNat n
  (>!<) :: forall i j. (KnownNat i, KnownNat j) => V (i+j) a -> ( V i a, V j a )
  (>!<) VNil = unsafeCoerce ( VNil, VNil )
  (>!<) (a :. as)
    = case Proxy @1 %<=? Proxy @i of
         LE Refl   ->
            let u :: V (i-1) a
                v :: V j a
                (u, v) = (>!<) (as :: V ((i+j)-1) a)
            in (a :. u, v)
         NLE _ _ -> unsafeCoerce ( VNil, a :. as )
  generated :: V (GenDeg Nat (V 0 a) () unit) a -> a
  generated = unsafeCoerce (headV :: V 1 a -> a)
  --           ^^^^^   ditto

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
class Semiring (Scalar v) => Semimodule d v | v -> d where
  type Scalar v :: Type
  type OfDim v d (n :: d) = (r :: Type) | r -> v d n
  type ValidDim v d (n :: d) :: Constraint
  {-# MINIMAL (^+^), ( (*^) | (^*) ) #-}
  (^+^) :: ValidDim v d n => OfDim v d n -> OfDim v d n -> OfDim v d n
  (*^)  :: ValidDim v d n => Scalar v    -> OfDim v d n -> OfDim v d n
  (^*)  :: ValidDim v d n => OfDim v d n -> Scalar v    -> OfDim v d n
  (*^) = flip (^*)
  (^*) = flip (*^)

-- | A module M over a ring R consists of:
--
--     * an abelian group M,
--     * an action of R on M.
--
-- That is, we simply add additive inverses to the previous definition.
class (Ring (Scalar v), Semimodule d v) => Module d v | v -> d where
  {-# MINIMAL (^-^) | (-^) #-}
  (^-^) :: ValidDim v d n => OfDim v d n -> OfDim v d n -> OfDim v d n
  (^-^) x y = x ^+^ ((-^) y)
  (-^) :: ValidDim v d n => OfDim v d n -> OfDim v d n
  (-^) x = (-1) *^ x

-- | Semimodule with an inner product.
--
-- This usually consists of a non-degenerate symmetric bilinear form on the underlying semimodule.
--
-- Other axioms might be more relevant depending on the underlying ring.
-- For instance, one instead insists on sesquilinearity over the field of complex numbers.
class Semimodule d v => Inner d v where
  {-# MINIMAL ( (^.^) | dot ), normalise #-}
  (^.^), dot :: ValidDim v d n => OfDim v d n -> OfDim v d n -> Scalar v
  dot = (^.^)
  (^.^) = dot
  normalise :: ValidDim v d n => OfDim v d n -> OfDim v d n

-- | Module with a cross product.
--
-- This usually consists of a non-degenerate skew-symmetric bilinear multiplication satisfying the Jacobi identity.
-- Over the real numbers, this implies the dimension is 3.
class Module d v => Cross d v where
  type CrossDim v d (n :: d) :: Constraint
  {-# MINIMAL (^×^) | cross #-}
  (^×^), cross :: (ValidDim v d n, CrossDim v d n) => OfDim v d n -> OfDim v d n -> OfDim v d n
  (^×^) = cross
  cross = (^×^)

------------------------------------------------------------------
-- instances for plain vectors

instance Semiring a => Semimodule Nat (V 0 a) where
  type Scalar (V 0 a) = a
  type OfDim (V 0 a) Nat n = V n a
  type ValidDim (V 0 a) Nat n = KnownNat n
  (^+^) = liftA2 (+)
  v ^* k = fmap (*k) v

instance Ring a => Module Nat (V 0 a) where
  (^-^) = liftA2 (-)  

instance (Semiring a, Floating a) => Inner Nat (V 0 a) where
  (^.^) = (sum .) . liftA2 (*)
  normalise v = invSqrt (dot v v) *^ v

instance Ring a => Cross Nat (V 0 a) where
  type CrossDim (V 0 a) Nat n = ( n ~ 3 )
  cross (V3 x y z) (V3 x' y' z') = V3 a b c
    where a = y * z' - z * y'
          b = z * x' - x * z'
          c = x * y' - y * x'

------------------------------------------------------------------
-- derived operations

-- | Angle between two vectors, computed using the inner product structure.
angle :: (Floating (Scalar v), ValidDim v d n, Inner d v)
      => OfDim v d n -> OfDim v d n -> Scalar v
angle x y = acos $ dot (normalise x) (normalise y)

-- | Norm of a vector, computed using the inner product.
norm :: (Floating (Scalar v), ValidDim v d n, Inner d v)
     => OfDim v d n -> Scalar v
norm v = sqrt $ squaredNorm v

-- | Squared norm of a vector, computed using the inner product.
squaredNorm :: (ValidDim v d n, Inner d v)
            => OfDim v d n -> Scalar v
squaredNorm v = dot v v

-- | Quadrance between two points.
quadrance :: (ValidDim v d n, Module d v, Inner d v)
       => OfDim v d n -> OfDim v d n -> Scalar v
quadrance x y = squaredNorm (x ^-^ y)

-- | Distance between two points.
distance :: (Floating (Scalar v), ValidDim v d n, Module d v, Inner d v)
         => OfDim v d n -> OfDim v d n -> Scalar v
distance x y = norm (x ^-^ y)

-- | Linear interpolation between two points.
along :: (ValidDim v d n, Module d v)
      => Scalar v    -- ^ Interpolation coefficient. 0 = start, 1 = end.
      -> OfDim v d n -- ^ Start.
      -> OfDim v d n -- ^ End.
      -> OfDim v d n
along t x y = (1-t) *^ x ^+^ t *^ y

-- | Reflects a vector along a hyperplane, specified by a normal vector.
-- /Computes the normalisation of the normal vector in the process./
reflect :: (Floating (Scalar v), ValidDim v d n, Module d v, Inner d v)
        => OfDim v d n -- ^ Vector to be reflected.
        -> OfDim v d n -- ^ A normal vector of the reflecting hyperplane.
        -> OfDim v d n
reflect v n = reflect' v (normalise n)

-- | Same as 'reflect': reflects a vector along a hyperplane, specified by a normal vector.
-- However, /__this function assumes the given normal vector is already normalised__/.
reflect' :: (ValidDim v d n, Module d v, Inner d v)
         => OfDim v d n -- ^ Vector to be reflected.
         -> OfDim v d n -- ^ /__Normalised__/ normal vector of the reflecting hyperplane.
         -> OfDim v d n
reflect' v n = v ^-^ (2 * dot v n) *^ n

-- | Projects the first argument onto the second.
proj :: (DivisionRing (Scalar v), ValidDim v d n, Inner d v)
     => OfDim v d n -> OfDim v d n -> OfDim v d n
proj x y = projC x y *^ y

-- | Projection constant: how far along the projection of the first vector lands along the second vector.
projC :: forall v d (n :: d). (DivisionRing (Scalar v), ValidDim v d n, Inner d v)
      => OfDim v d n -> OfDim v d n -> Scalar v
projC x y = dot x y / dot y y

-- | Orthogonality test.
--
-- /__Uses a precise equality test, so use at your own risk with floating point numbers.__/
isOrthogonal :: (ValidDim v d n, Module d v, Inner d v, Eq (Scalar v))
             => OfDim v d n -> OfDim v d n -> Logic (Scalar v)
isOrthogonal v w = dot v w == 0

-- | Gram-Schmidt algorithm.
gramSchmidt :: (Floating (Scalar v), ValidDim v d n, Module d v, Inner d v)
            => [OfDim v d n] -> [OfDim v d n]
gramSchmidt []     = []
gramSchmidt (x:xs) = x' : gramSchmidt (map (\v -> v ^-^ proj v x') xs)
  where x' = normalise x

-- | Rotate a vector around an axis (in dimension 3), using the cross product.
rotateAroundAxis :: (Cross d v, Floating (Scalar v), ValidDim v d n, CrossDim v d n)
                 => OfDim v d n -- ^ Axis of rotation.
                 -> Scalar v    -- ^ Angle of rotation, according to the right hand rule.
                 -> OfDim v d n -- ^ Vector to be rotated.
                 -> OfDim v d n
rotateAroundAxis n theta v = cos theta *^ v ^+^ sin theta *^ (n ^×^ v)

------------------------------------------------------------------
-- specific implementation of matrices using nested vectors

identityMat :: forall n a. (KnownNat n, Ring a) => V n (V n a)
identityMat =
  case (Proxy :: Proxy 1) %<=? (Proxy :: Proxy n) of
       LE Refl   -> (1 :. pure 0) :. fmap (0:.) (identityMat :: V (n-1) (V (n-1) a) )
       NLE nle _ -> case deduceZero nle of
                         Refl -> VNil

newtype WrappedMatrix m n a k = WrappedMatrix { wrappedMatrix :: V m (V (n+k) a) }

addCol' :: (KnownNat m, KnownNat n, KnownNat k) => WrappedMatrix m n a k -> V m a -> WrappedMatrix m n a (k+1)
addCol' WrappedMatrix { wrappedMatrix = mat } col = WrappedMatrix { wrappedMatrix = liftA2 (:.) col mat }

addCol :: forall a m n. (KnownNat m, KnownNat n) => V m (V n a) -> V m a -> V m (V (n+1) a)
addCol mat v = wrappedMatrix $ addCol' (WrappedMatrix { wrappedMatrix = mat } :: WrappedMatrix m n a 0) v

addCols :: (KnownNat m, KnownNat n, KnownNat l) => V m (V n a) -> V l (V m a) -> V m (V (n+l) a)
addCols mat cols = wrappedMatrix $ dfoldrV (flip addCol') (WrappedMatrix mat) cols

rowMatrix :: KnownNat n => V n a -> V 1 (V n a)
rowMatrix = (:. VNil)

columnMatrix :: KnownNat n => V n a -> V n (V 1 a)
columnMatrix = fmap (:. VNil)

blockSum :: forall m1 m2 n1 n2 a. (KnownNat m1, KnownNat m2, KnownNat n1, KnownNat n2, Semiring a) 
         => V m1 (V n1 a)-> V m2 (V n2 a) -> V (m1+m2) (V (n1+n2) a)
blockSum mat1 mat2 = fmap (<!> pure zero) mat1 <!> fmap (pure zero <!>) mat2

fromColumns :: forall l m a. (KnownNat m, KnownNat l) => V l (V m a) -> V m (V l a)
fromColumns = addCols ( pure VNil :: V m (V 0 a))

joinV :: V m (V n a) -> V (m * n) a
joinV VNil          = VNil
joinV (row :. rows) = row <!> joinV rows

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
deriving via ( V m `Compose` V n )
         instance (KnownNat m, KnownNat n) => Applicative (M m n)
deriving instance (KnownNat m, KnownNat n) => Foldable    (M m n)
deriving instance (KnownNat m, KnownNat n) => Traversable (M m n)
deriving instance (KnownNat m, KnownNat n, Binary a) => Binary (M m n a)
deriving instance (KnownNat n, KnownNat m, Storable a) => Storable (M m n a)

deriving via '(V m (V n x), V m (V n y), V m (V n z))
  instance (KnownNat m, KnownNat n, Choose b '(x,y,z)) => Choose b '(M m n x, M m n y, M m n z)

------------------------------------------------------------------
-- products for matrices


instance {-# OVERLAPPING #-}  IsProduct (M 0 0 a) '[] where
  fromHList _ = M VNil
  toHList   _ = HNil
instance {-# OVERLAPPABLE #-} KnownNat m => IsProduct (M m 0 a) '[] where
  fromHList _ = M ( pure VNil )
  toHList   _ = HNil
instance {-# OVERLAPPABLE #-} IsProduct (M 0 n a) '[] where
  fromHList _ = M VNil
  toHList   _ = HNil
instance ( KnownNat m
         , KnownNat n, 1 <= n
         , Replicated (n-1) (V m a) rs
         , KnownLength rs
         , (Length rs + 1) ~ n
         )
      => IsProduct (M m n a) (V m a ': rs)
      where
  fromHList (col :> cols) = M $ addCol ( distribute (fromHListVec (replication @(n-1) @(V m a)) cols )) col
  toHList (M rows) = case distribute rows of
    ( col :. cols ) -> col :> toHListVec (replication @(n-1) @(V m a)) cols

instance ( KnownNat n, 1 <= n
         , KnownNat m, 1 <= m
         , Replicated (m*n-1) a rs
         , Length rs ~ (n*m-1)
         , (m*n) ~ (n*m)
         , Replicate (n*m) a ~ (a ': rs) -- type checker unfortunately can't deduce this
         )
      => IsProduct (M m n a) (a ': rs)
      where
  toHList   = toHList . joinV . distribute . unM
  fromHList = M . distribute . splitRows . fromHList

splitRows
  :: forall (n :: Nat) (m :: Nat) (a :: Type)
  .  ( KnownNat n, KnownNat m )
  => V (m*n) a -> V n (V m a)
splitRows v = case Proxy @1 %<=? Proxy @n of
  NLE nle Refl ->
    case deduceZero nle of
      Refl -> VNil
  LE Refl ->
    case splitAtV @m v of
      ( row, next ) ->
        row :. splitRows @(n-1) next

instance KnownNat m => GradedSemigroup (M m 0 a) Nat where
  type Grade Nat (M m 0 a) i = M m i a
  type i :<!>: j = i + j
  (<!>) :: M m i a -> M m j a -> M m (i+j) a
  (M cs) <!> (M ds) = M ( liftA2 (<!>) cs ds )

instance KnownNat m => GeneratedGradedSemigroup (M m 0 a) Nat () where
  type GenType    (M m 0 a) ()  _  = V m a
  type GenDeg Nat (M m 0 a) () '() = 1
  generator :: V m a -> M m (GenDeg Nat (M m 0 a) () unit) a
  generator = unsafeCoerce ( M . columnMatrix :: V m a -> M m 1 a )
  --          ^^^^
  -- same comment as for the instance for vectors, GHC cannot deduce @unit ~ '()@

instance KnownNat m => FreeGradedSemigroup (M m 0 a) Nat () where
  type ValidDegree (M m 0 a) i = KnownNat i
  (>!<) :: forall i j. (KnownNat i, KnownNat j)
        => M m (i+j) a -> ( M m i a, M m j a )
  (>!<) (M m) = case go m of { (m1, m2) -> ( M m1, M m2 ) }
    where go :: V k (V (i+j) a) -> (V k (V i a), V k (V j a))
          go VNil = ( VNil, VNil )
          go (c :. cs) = case ( (>!<) c, go cs ) of
              ( ( ci, cj ), (cis, cjs) ) -> (ci :. cis, cj :. cjs)
  generated :: M m (GenDeg Nat (M m 0 a) () unit) a -> V m a
  -- same comment again...
  generated = unsafeCoerce go
    where go :: M m 1 a -> V m a
          go (M v) = headV (distribute v)

------------------------------------------------------------------
-- type classes for matrix operations

infixl 6 !+!, !-!
infixl 7 !*!
infix  8 !*^, ^*!
infix  9 *!, !*

type family VectorOf m :: Type

-- | Typeclass for matrix operations.
--
-- The 'OfDims' associated type family allows the type class methods to involve various dimension indices.
class Module d (VectorOf m) => Matrix d m | m -> d where
  type OfDims m d (ij :: (d, d)) = r | r -> m ij
  identity    :: ValidDim (VectorOf m) d i => OfDims m d '(i,i)
  diag        :: ValidDim (VectorOf m) d i => Scalar (VectorOf m) -> OfDims m d '(i,i)
  inverse     :: ValidDim (VectorOf m) d i => OfDims m d '(i,i) -> OfDims m d '(i,i)
  determinant :: ValidDim (VectorOf m) d i => OfDims m d '(i,i) -> Scalar (VectorOf m)
  transpose   :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 )
              => OfDims m d '(i,j) -> OfDims m d '(j,i)

  (!+!)       :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 )
              => OfDims m d '(i,j) -> OfDims m d '(i,j) -> OfDims m d '(i,j)
  (!-!)       :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 )
              => OfDims m d '(i,j) -> OfDims m d '(i,j) -> OfDims m d '(i,j)
  konst       :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 )
              => Scalar (VectorOf m) -> OfDims m d '(i,j)
  (*!)        :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 )
              => Scalar (VectorOf m) -> OfDims m d '(i,j) -> OfDims m d '(i,j)
  (!*)        :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 )
              => OfDims m d '(i,j) -> Scalar (VectorOf m) -> OfDims m d '(i,j)
  (!*^)       :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 )
              => OfDims m d '(i,j) -> OfDim (VectorOf m) d j -> OfDim (VectorOf m) d i
  (^*!)       :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 )
              => OfDim (VectorOf m) d i -> OfDims m d '(i,j) -> OfDim (VectorOf m) d j
  (!*!)       :: ( ValidDim (VectorOf m) d i
                 , ValidDim (VectorOf m) d j
                 , ValidDim (VectorOf m) d k
                 )
              => OfDims m d '(i,j) -> OfDims m d '(j,k) -> OfDims m d '(i,k)
  identity = diag 1
  (*!) = flip (!*) -- SPIR-V defines MatrixTimesScalar

type instance VectorOf (M 0 0 a) = V 0 a

instance Ring a => Matrix Nat (M 0 0 a) where
  type OfDims (M 0 0 a) Nat '(i,j) = M i j a
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
-- (Adapted from Edward Kmett's __Linear__ library for the Vulkan coordinate system.)
perspective
  :: Floating a
  => a -- ^ FOV.
  -> a -- ^ Aspect ratio.
  -> a -- ^ Near plane.
  -> a -- ^ Far plane.
  -> M 4 4 a
perspective fovy aspect near far
  = M $ V4 (V4 x 0 0 0)
           (V4 0 y 0 0)
           (V4 0 0 z w)
           (V4 0 0 1 0)
  where tanHalfFovy = tan $ fovy / 2
        x = 1 / (aspect * tanHalfFovy)
        y = 1 / tanHalfFovy
        z = far / (far - near)
        w = far * near / (near - far)

-- | Affine 3D transformation matrix for looking at focus point from a given view point.
-- (Adapted from Edward Kmett's __Linear__ library for the Vulkan coordinate system.)
lookAt
  :: Floating a
  => V 3 a -- ^ Focus point.
  -> V 3 a -- ^ View point.
  -> V 3 a -- ^ Up vector.
  -> M 4 4 a
lookAt focus viewPoint up
  = M $ V4 ( xa <!> V1 (-xd) )
           ( ya <!> V1 (-yd) )
           ( za <!> V1 (-zd) )
           ( V4 0 0 0 1 )
  where za = normalise (focus ^-^ viewPoint)
        xa = normalise (za `cross` up)
        ya = za `cross` xa
        xd = xa ^.^ focus
        yd = ya ^.^ focus
        zd = za ^.^ focus
