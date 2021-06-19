{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Syntax.Complex

Syntax for manipulating complex numbers.    

@CodeComplex a@ is a newtype around @Code (V 2 a)@, but with a @ComplexFloat@ instance allowing for complex arithmetic.    

Complex numbers can be constructed using using @:+:@, e.g. @0 :+: 1@ represents the imaginary unit @i@.

Note that @Floating@ operations such as trigonometric functions are not provided, as these are not supported by @SPIR-V@.
-}


module FIR.Syntax.Complex
  ( CodeComplex(..), pattern (:+:), complexLog )
  where

-- base
import Prelude
  ( Bool, ($) )
import Data.Coerce
  ( coerce )
import Data.Complex
  ( Complex(..) )

-- fir
import Math.Algebra.Class
  ( AdditiveMonoid(..), CancellativeAdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), DivisionRing(..)
  , Floating(..), RealFloat(..), ComplexFloat(..)
  )
import Math.Logic.Class
  ( Eq(Logic) )
import Math.Linear
  ( V, (^+^), (^-^), (-^), (*^) )
import FIR.AST
  ( Code, Syntactic(..), SyntacticVal, InternalType )
import FIR.AST.Type
  ( AugType(Val) )
import FIR.Prim.Types
  ( PrimTy, ScalarTy )
import FIR.Syntax.AST
  ( )
import FIR.Syntax.Synonyms
  ( pattern Vec2 )

----------------------------------------------------------------------------

-- | Newtype around @Code (V 2 a)@, but with typeclass instances
-- for complex arithmetic.
newtype CodeComplex a = CodeComplex { codeComplex :: Code ( V 2 a ) }

instance (ScalarTy a) => Syntactic (CodeComplex a) where
  type Internal (CodeComplex a) = Val ( V 2 a )
  toAST ( CodeComplex vec ) = vec
  fromAST vec = CodeComplex vec

instance (SyntacticVal a, ScalarTy (InternalType a)) => Syntactic (Complex a) where
  type Internal (Complex a) = Val ( V 2 (InternalType a) )
  toAST (x :+ y) = Vec2 ( toAST x ) ( toAST y )
  fromAST (Vec2 x y) = fromAST x :+ fromAST y

{-# COMPLETE (:+:) #-}
pattern (:+:) :: PrimTy a => Code a -> Code a -> CodeComplex a
pattern a :+: b = CodeComplex ( Vec2 a b )

instance ( Semiring a, ScalarTy a ) => AdditiveMonoid (CodeComplex a) where
  (+)           = coerce ( (^+^) :: Code ( V 2 a ) -> Code ( V 2 a ) -> Code ( V 2 a ) )
  zero          = zero :+: zero
  fromInteger a = fromInteger a :+: zero

instance ( Semiring a, CancellativeAdditiveMonoid a, ScalarTy a ) => Semiring (CodeComplex a) where
  ( a1 :+: b1 ) * ( a2 :+: b2 ) = ( a1 * a2 - b1 * b2 ) :+: ( a1 * b2 + b1 * a2 )

instance ( Semiring a, AdditiveGroup a, ScalarTy a ) => CancellativeAdditiveMonoid (CodeComplex a) where
  (-) = coerce ( (^-^) :: Code ( V 2 a ) -> Code ( V 2 a ) -> Code ( V 2 a ) )

instance ( Semiring a, AdditiveGroup a, ScalarTy a ) => AdditiveGroup (CodeComplex a) where
  negate = coerce ( (-^) :: Code ( V 2 a ) -> Code ( V 2 a ) )

instance ( RealFloat a, ScalarTy a, Logic a ~ Bool ) => DivisionRing (CodeComplex a) where
  z1 / ( z2@( a2 :+: b2 ) ) = scale ( recip $ sqMag_z2 ) ( z1 * conjugate z2 )
    where
      sqMag_z2 :: Code a
      sqMag_z2 = a2 * a2 + b2 * b2
  fromRational r = fromRational r :+: zero

instance ( b ~ Code a, ScalarTy a, RealFloat a, Logic a ~ Bool ) => ComplexFloat (CodeComplex a) b where
  conjugate ( a :+: b ) = a :+: negate b
  magnitude ( a :+: b ) = sqrt ( a * a + b * b )
  phase     ( a :+: b ) = atan2 b a
  mkCartesian a b       = a :+: b
  mkPolar r theta       = scale r $ cis theta
  cis theta             = cos theta :+: sin theta
  scale r               = coerce ( ( r *^ ) :: Code ( V 2 a ) -> Code ( V 2 a ) )
  realPart  ( a :+: _ ) = a
  imagPart  ( _ :+: b ) = b

complexLog :: ( ScalarTy a, RealFloat a, Logic a ~ Bool ) => CodeComplex a -> CodeComplex a
complexLog z = log ( magnitude z ) :+: phase z
