{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
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
import Data.Kind
  ( Type )


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
