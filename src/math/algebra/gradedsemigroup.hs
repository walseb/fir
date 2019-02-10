{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

{-|
Module: Math.Algebra.GradedSemigroup

A /graded/ object is one which splits up into parts of different indices.

For instance, the ring of polynomials \( R[x_1, \ldots, x_n] \) is graded
by degree, in that \( R \) can be decomposed:

\[ R = \bigoplus_{d \in \mathbb{N}} R_d, \]

where \( R_d \) consists of homogeneous polynomials of degree \( d \).

What's important is how the ring structure of \( R[x_1, \ldots, x_n] \) interacts with the grading.
We have:

\[ (+) \ \colon R_{d\phantom{_1}} \times R_{d\phantom{_2}} \to R_{d\phantom{_1+d_2}} \]

\[ (\times) \ \colon R_{d_1} \times R_{d_2} \to R_{d_1 + d_2}. \]

In this module, we are interested in the /second/ of these two behaviours.
Note then that two operations are in play:

  * the monoid \( \mathbb{N} \) of degrees under addition,
  * the graded multiplication \[ (\times) \ \colon R_{d_1} \times R_{d_2} \to R_{d_1 + d_2}. \]

One can alternatively think of a graded semigroup as a semigroup in a (monoidal) category of graded objects.

A motivating example is that of length-indexed vectors. In that case, we have:

  * type-level addition of natural numbers \( (+) \ \colon \mathbb{N} \times \mathbb{N} \to \mathbb{N}, \)
  * value-level concatenation of vectors \( \textrm{V}\ n\ a \times \textrm{V}\ m\ a \to \textrm{V}\ (n+m)\ a. \)

Another important example is that of structs (i.e. ordered records). In this case we have:

  * a type-level monoid operation on lists of fields,
      @(:++:) :: [ Symbol :-> Type ] -> [ Symbol :-> Type ] -> [ Symbol :-> Type ]@,
  * a value-level concatenation of structs, of type @Struct as -> Struct bs -> Struct (as :++: bs)@.

In this module, we follow the above two examples and insist that the operation on indices is at the type-level,
whereas the graded multiplication sits at the value-level.
-}

module Math.Algebra.GradedSemigroup where

-- base
import Data.Kind(Type, Constraint)


infixl 6 <!>
infixl 6 :<!>:

-- * Graded semigroups

-- $graded
-- A __graded semigroup__ consists of:
--
--   * Types \( G_i \) for \( i : I \), where \( I \) is the index type (also called the degree type),
--   * a semigroup structure \( (+) \ \colon I \times I \to I \) on the index type \( I \),
--   * an associative operation \( (\cdot) \ \colon G_i \times G_j \to G_{i + j} \).

-- | Semigroup with graded structure.
class GradedSemigroup g k | g -> k where
  -- | For extra flexibility, instead of specifying a type-level function \( G \colon I \to \textrm{Type} \)
  -- to define the grading (which is often too restrictive), we use an associated /injective/ type family.
  type Grade k g (i :: k) = (r :: Type) | r -> k g i
  -- | Type-level semigroup operation on indices.
  type (i :: k) :<!>: (j :: k) :: k
  -- | Value-level graded semigroup operation.
  (<!>) :: Grade k g i -> Grade k g j -> Grade k g (i :<!>: j)

-- ** Presentation of a graded semigroup

-- $presentation
--
-- A /presentation/ of a graded semigroup consists of __generators__ and /relations/,
-- such that any element of the semigroup can be obtained from the generators
-- using the graded semigroup operation.
-- The generators are required to be homogeneous.

-- | Specifying generators of a graded semigroup.
class GradedSemigroup g k => GeneratedGradedSemigroup g k d | g -> k d where
  -- | /Parametrised/ type of homogeneous generators.
  --
  -- The extra parameter @l@ accomodates the fact that this type might vary.
  type GenType  g d (l :: d) :: Type
  -- | Degree of homogeneous generators.
  type GenDeg k g d (l :: d) = (r :: k) | r -> k d l
  -- | Inject generators into the semigroup.
  generator :: GenType g d l -> Grade k g (GenDeg k g d l)

-- | Utility type synonym for the degree of a homogeneous generator,
-- to avoid manually specifying the kind of the labelling parameter.
type GenDegAt k g (l :: d) = (GenDeg k g d (l :: d) :: k)

-- $generated
-- We illustrate this type class with some examples:
--
--   * Length-indexed vectors.
--   Every length-indexed vector can be (uniquely) expressed as a concatenation
--   of its components, which are length 1 vectors.
--   To define a type class instance for length-indexed vectors with components of type @a@,
--   we then specify that the element type is always @a@,
--   and the degree of a homogeneous generator is always 1.
--
--   * Heterogeneous lists.
--   In this case, an index is specified by a list of types,
--   elements can be of any type (making use of the type parameter @l@ now),
--   and the degree of a generator is the singleton list of the corresponding type.

-- ** Freenees of graded semigroup operation

-- $bijective
--
-- A __degree-bijective__ graded semigroup is one in which semigroup multiplication
-- is bijective in individual degrees.
--
-- Specifically, we require that for any indices \( i, j \in I \),
-- and for any \( g \in G_{i+j} \), there are unique
-- \( g_i \in G_i \), \( g_j \in G_j \) such that
-- \( g = g_i \cdot g_j \).
--
-- Moreover, we require that all values of the semigroup whose degrees
-- are degrees of generators are in fact themselves generators.

-- | Graded semigroup with degree-bijective operation.
class GeneratedGradedSemigroup g k d => FreeGradedSemigroup g k d | g -> k d where
  -- | Additional constraint on indices, necessary to make the index /relevant/
  -- (e.g. for length indexed vectors, we need a 'GHC.TypeNats.KnownNat' constraint to be able to
  -- meaningfully use the length).
  type ValidDegree g (i :: k) :: Constraint
  -- | /Splitting/ operation: given \( g \in G_{i+j} \),
  -- returns the unique pair \( g_i \in G_i \), \( g_j \in G_j \)
  -- such that \( g = g_i \cdot g_j \).
  (>!<) :: (ValidDegree g i, ValidDegree g j)
        => Grade k g (i :<!>: j) -> ( Grade k g i, Grade k g j )
  -- | In appropriate degrees, all elements of the semigroup
  -- should be images of generators.
  generated :: Grade k g (GenDeg k g d l) -> GenType g d l

-- $freeness
-- The above type class can be understood as a freeness property of the graded semigroup operation,
-- relative to the grading.
--
-- It says that, given a multiplication, we can recover the factors if we are given their indices.
--
-- For instance, with length-indexed vectors, knowing that a vector is a concatenation
-- is not enough information to determine the vectors which were concatenated.
-- The missing information is precisely the length of these vectors,
-- which is encoded in the graded structure.
--
-- Another, rather trivial, example of this arises when the grading of the semigroup is given by itself.
-- By definition then, knowing the grading is equivalent to knowing the value,
-- allowing us to recover the factors of a multiplication from their degrees.
