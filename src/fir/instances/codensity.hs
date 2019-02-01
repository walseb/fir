{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Instances.Codensity

This module, together with "FIR.Instances.AST",
provides most of the user-facing syntax for constructing
and manipulating values in the EDSL.

This is done through type class overloading, here in the form of
orphan instances for types of the form @Codensity AST (AST a := j) i@
(representing stateful values in the EDSL).

-}

module FIR.Instances.Codensity
  ( -- Monadic control operations
    when, unless, locally, while
    -- Stateful operations
  , def, fundef, entryPoint
  , use, assign, modifying
  , get, put
    -- Functor functionality
  , CodensityASTFunctor(fmapCodAST), (<$$$>)
  , CodensityASTApplicative(pureCodAST, (<***>))
    -- + orphan instances
  )
  where

-- base
import Prelude hiding
  ( Eq(..), (&&), (||), not
  , Ord(..)
  , Num(..), Floating(..)
  , Integral(..)
  , Fractional(..), fromRational
  , Floating(..), RealFloat(..)
  , Functor(..)
  , Applicative(..)
  )
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import Data.Word(Word16)
import qualified GHC.Stack
import GHC.TypeLits(Symbol, KnownSymbol)
import GHC.TypeNats(KnownNat)

-- fir
import Control.Monad.Indexed
  ( (:=)(AtKey), Codensity(Codensity)
  , ixFmap, ixPure, ixLiftA2
  )
import qualified Control.Monad.Indexed as Indexed
import Control.Type.Optic(Optic, Name, Gettable, Settable, Part, Whole, Indices)
import Data.Type.List(KnownLength(sLength), Postpend)
import Data.Type.Map(Insert, Union)
import FIR.AST(AST(..), Syntactic(Internal,toAST,fromAST))
import FIR.Binding(BindingsMap, BindingType, Var, Fun, KnownPermissions)
import FIR.Builtin(StageBuiltins)
import FIR.Instances.AST()
import FIR.Instances.Bindings(ValidDef, ValidFunDef, ValidEntryPoint)
import FIR.Instances.Optics(User, Assigner, KnownOptic, opticSing)
import FIR.Prim.Singletons(PrimTy, ScalarTy, KnownVars)
import Math.Algebra.Class
  ( AdditiveGroup(..)
  , Semiring(..), Ring(..)
  , DivisionRing(..)
  , Signed(..), Archimedean(..)
  , Floating(..), RealFloat(..)
  , Convert(..)
  )
import Math.Linear
  ( Semimodule(..), Module(..)
  , Inner(..), Cross(..)
  , Matrix(..)
  , V, M
  )
import Math.Logic.Class
  ( Eq(..), Boolean(..)
  , Choose(..), ifThenElse
  , Ord(..)
  )
import SPIRV.Stage(KnownStage)

--------------------------------------------------------------------------
-- * Monadic control operations

when :: forall i. AST Bool -> Codensity AST (AST () := i) i -> Codensity AST (AST () := i) i
when b action
  = if b
    then action
    else ixPure (Lit Proxy ()) :: Codensity AST (AST () := i) i

unless :: forall i. AST Bool -> Codensity AST (AST () := i) i -> Codensity AST (AST () := i) i
unless b action
  = if b
    then ixPure (Lit Proxy ()) :: Codensity AST (AST () := i) i
    else action

locally :: Codensity AST (AST a := j) i -> Codensity AST (AST a := i) i
locally = fromAST Locally

while :: ( GHC.Stack.HasCallStack
         , i' ~ i, i'' ~ i
         , l ~ (AST () := j)
         , b ~ (AST Bool := i)
         , r ~ (AST () := i)
         )
      => Codensity AST b (i :: BindingsMap)
      -> Codensity AST l i'
      -> Codensity AST r i''
while = fromAST While

--------------------------------------------------------------------------
-- * Syntactic type class
--
-- $syntactic
-- Instance for 'Syntactic'.

instance Syntactic a => Syntactic (Codensity AST (a := j) i) where
  type Internal (Codensity AST (a := j) i) = (Internal a := j) i

  toAST :: Codensity AST (a := j) i -> AST ( (Internal a := j) i )
  toAST (Codensity k) = k ( \(AtKey a) -> Return :$ toAST a )

  fromAST :: AST ( (Internal a := j) i) -> Codensity AST (a := j) i
  fromAST a = Codensity ( \k -> fromAST Bind a (k . AtKey) )

--------------------------------------------------------------------------
-- * Stateful operations (with indexed monadic state)

-- | Define a new variable.
--
-- Type-level arguments:
--
-- *@k@: name to use for definition,
-- *@ps@: 'FIR.Binding.Permission's (readable, writable, ...),
-- *@a@: type of definition,
-- *@i@: state at start of definition (usually inferred).
def :: forall k ps a i.
       ( GHC.Stack.HasCallStack
       , KnownSymbol k
       , KnownPermissions ps
       , PrimTy a
       , ValidDef k i ~ 'True
       )
    => AST a -- ^ Initial value.
    -> Codensity AST (AST a := Insert k (Var ps a) i) i

fundef' :: forall k as b l i.
          ( GHC.Stack.HasCallStack
          , KnownSymbol k
          , KnownVars as
          , PrimTy b
          , ValidFunDef k as i l ~ 'True
          )
       => Codensity AST (AST b := l) (Union i as)
       -> Codensity AST (AST (BindingType (Fun as b)) := Insert k (Fun as b) i) i

-- | Define a new entry-point (or shader stage).
--
-- Builtin variables for the relevant shader stage are made available in the entry-point body.
--
-- Type-level arguments:
--
-- *@k@: name of entry-point,
-- *@s@: entry-point 'SPIRV.Stage.Stage',
-- *@l@: state at end of entry-point body (usually inferred),
-- *@i@: state at start of entry-point body (usually inferred).
entryPoint :: forall k s l i.
             ( GHC.Stack.HasCallStack
             , KnownSymbol k
             , KnownStage s
             , ValidEntryPoint s i l ~ 'True
             )
           => Codensity AST (AST () := l) (Union i (StageBuiltins s)) -- ^ Entry-point body.
           -> Codensity AST (AST () := i) i

-- | /Use/ an optic, returning a monadic value read from the (indexed) state.
--
-- Like @use@ from the lens library, except the optic needs to be passed with a type application.
use :: forall optic.
             ( GHC.Stack.HasCallStack
             , KnownOptic optic
             , Gettable optic
             , Syntactic (CodUser optic)
             , Internal (CodUser optic) ~ User optic
             )
           => CodUser optic

-- | Assign a new value with an optic.
--
-- Like @assign@ from the lens library, except the optic needs to be passed with a type application.
assign :: forall optic.
             ( GHC.Stack.HasCallStack
             , KnownOptic optic
             , Settable optic
             , Syntactic (CodAssigner optic)
             , Internal (CodAssigner optic) ~ Assigner optic
             )
           => CodAssigner optic

def        = fromAST ( Def    @k @ps @a    @i Proxy Proxy       ) . toAST
fundef'    = fromAST ( FunDef @k @as @b @l @i Proxy Proxy Proxy ) . toAST
entryPoint = fromAST ( Entry  @k     @s @l @i Proxy Proxy       ) . toAST
use        = fromAST ( Use    @optic          sLength opticSing )
assign     = fromAST ( Assign @optic          sLength opticSing )

-- | Get the value of a variable.
-- Like @get@ for state monads, except a binding name needs to be specified with a type application.
get :: forall (k :: Symbol) a (i :: BindingsMap).
       ( KnownSymbol k, Gettable (Name k :: Optic '[] i a) )
    => Codensity AST (AST a := i) i
get = use @(Name k :: Optic '[] i a)

-- | Set the value of a variable.
-- Like @put@ for state monads, except a binding name needs to be specified with a type application.
put :: forall (k :: Symbol) a (i :: BindingsMap).
       ( KnownSymbol k, Settable (Name k :: Optic '[] i a) )
    => AST a -> Codensity AST (AST () := i) i
put = assign @(Name k :: Optic '[] i a)

-- | Define a new function.
--
-- Type-level arguments:
-- 
-- * @k@: function name,
-- * @as@: list of argument types,
-- * @b@: return type,
-- * @l@: state at end of function body (usually inferred),
-- * @i@: state at start of function body (usually inferred),
-- * @r@: function type itself, result of 'fundef' (usually inferred).
fundef :: forall k as b l i r.
           ( GHC.Stack.HasCallStack
           , Syntactic r
           , Internal r ~ BindingType (Fun as b)
           , KnownSymbol k
           , KnownVars as
           , PrimTy b
           , ValidFunDef k as i l ~ 'True
           )
        => Codensity AST (AST b := l) (Union i as) -- ^ Function body code.
        -> Codensity AST ( r := Insert k (Fun as b) i) i
fundef = fromAST . toAST . fundef' @k @as @b @l @i

--------------------------------------------------------------------------
-- type synonyms for use/assign

type family ListVariadicCod
              ( is :: [Type]      )
              ( s  :: BindingsMap )
              ( a  :: Type        )
            = ( r  :: Type        )
            | r -> is s a where
  ListVariadicCod '[]        s a = Codensity AST (AST a := s) s
  ListVariadicCod ( i ': is) s a = AST i -> ListVariadicCod is s a


-- recall (defined in FIR.Instances.Optics):
-- type User     (g :: Optic is s a) = ListVariadicIx is            s a
-- type Assigner (g :: Optic is s a) = ListVariadicIx (Append is a) s ()

type CodUser     (optic :: Optic is s a) = ListVariadicCod  is               s a
type CodAssigner (optic :: Optic is s a) = ListVariadicCod (is `Postpend` a) s ()

--------------------------------------------------------------------------
-- modifying

type family VariadicCodModifier
              ( is :: [Type] )
              ( s  :: BindingsMap )
              ( a  :: Type )
            = ( r  :: Type )
            | r -> is s a
            where
  VariadicCodModifier '[]       s a = (AST a -> AST a) -> Codensity AST (AST () := s) s
  VariadicCodModifier (i ': is) s a = AST i -> VariadicCodModifier is s a

type CodModifier (optic :: Optic is s a) = VariadicCodModifier is s a

-- | Modify a value with an optic.
--
-- Like @modifying@ from the lens library, except the optic needs to be passed with a type application.
modifying
    :: forall optic.
       ( GHC.Stack.HasCallStack
       , KnownOptic optic
       , Settable optic
       , Gettable optic
       , Syntactic (CodUser optic)
       , Internal (CodUser optic) ~ User optic
       , Syntactic (CodAssigner optic)
       , Internal (CodAssigner optic) ~ Assigner optic
       , Modifier (Indices optic) (Whole optic) (Part optic)
       )
    => CodModifier optic
modifying
  = modifier @(Indices optic) @(Whole optic) @(Part optic)
      ( use    @optic )
      ( assign @optic )

class Modifier is s a where
  modifier :: ListVariadicCod      is               s a
           -> ListVariadicCod     (is `Postpend` a) s ()
           -> VariadicCodModifier  is               s a

instance Modifier '[] s a where
  modifier used assigned f
    = (ixFmap f used) Indexed.>>= assigned

instance Modifier is s a => Modifier (i ': is) s a where
  modifier used assigned i
    = modifier @is @s @a (used i) (assigned i)

--------------------------------------------------------------------------
-- * Instances for codensity representation

-- ** Logical operations
--
-- $logical
-- Instances for:
--
-- 'Boolean', 'Choose',
--
-- 'Eq', 'Ord' (note: not the "Prelude" type classes).
instance Boolean (Codensity AST (AST Bool := i) i) where
  true  = ixPure true
  false = ixPure false
  (&&)  = ixLiftA2 (&&)
  (||)  = ixLiftA2 (||)
  not   = ixFmap not

instance ( PrimTy a
         , i' ~ i, i'' ~ i
         , a' ~ a
         , r ~ (AST a := i)
         ) =>
  Choose  ( AST Bool )
         '( Codensity AST (AST a  := j) i
          , Codensity AST (AST a' := k) i'
          , Codensity AST r             i''
          ) where
  choose = fromAST IfM

instance ( PrimTy a, Eq a, Logic a ~ Bool
         , x ~ (AST a := i)
         )
  => Eq (Codensity AST x i) where
  type Logic (Codensity AST x i) = Codensity AST (AST Bool := i) i
  (==) = ixLiftA2 (==)
  (/=) = ixLiftA2 (/=)

instance ( PrimTy a, Ord a, Logic a ~ Bool
         , x ~ (AST a := i)
         )
  => Ord (Codensity AST x i) where
  type Ordering (Codensity AST x i) = Codensity AST (AST Word16 := i) i
  compare = ixLiftA2 compare
  (<=) = ixLiftA2 (<=)
  (>=) = ixLiftA2 (>=)
  (<)  = ixLiftA2 (<)
  (>)  = ixLiftA2 (>)
  min  = ixLiftA2 min
  max  = ixLiftA2 max

-- * Numeric operations
-- 
-- $numeric
-- Instances for:
--
-- 'AdditiveGroup', 'Semiring', 'Ring', 'Signed',
--
-- 'DivisionRing', 'Archimedean' (Archimedean ordered group),
--
-- 'Floating', 'RealFloat' (note: not the "Prelude" type classes).
instance (ScalarTy a, AdditiveGroup a, j ~ i) => AdditiveGroup (Codensity AST (AST a := j) i) where
  (+)    = ixLiftA2 (+)
  zero   = ixPure zero
  fromInteger = ixPure . fromInteger
instance (ScalarTy a, Semiring a, j ~ i) => Semiring (Codensity AST (AST a := j) i) where
  (*)    = ixLiftA2 (*)
instance (ScalarTy a, Ring a, j ~ i) => Ring (Codensity AST (AST a := j) i) where
  (-)    = ixLiftA2 (-)
  negate = ixFmap negate  
instance (ScalarTy a, Signed a, j ~ i) => Signed (Codensity AST (AST a := j) i) where
  abs    = ixFmap abs
  signum = ixFmap signum
instance (ScalarTy a, DivisionRing a, j ~ i) => DivisionRing (Codensity AST (AST a := j) i) where
  (/)    = ixLiftA2 (/)
  fromRational = ixPure . fromRational
instance ( ScalarTy a
         , Archimedean a
         , Logic a ~ Bool
         , j ~ i
         ) => Archimedean (Codensity AST (AST a := j) i) where
  mod    = ixLiftA2 mod
  rem    = ixLiftA2 rem

instance (ScalarTy a, Floating a, j ~ i) => Floating (Codensity AST (AST a := j) i) where
  pi      = ixPure pi
  exp     = ixFmap exp
  log     = ixFmap log
  sqrt    = ixFmap sqrt
  invSqrt = ixFmap invSqrt
  sin     = ixFmap sin
  cos     = ixFmap cos
  tan     = ixFmap tan
  asin    = ixFmap asin
  acos    = ixFmap acos
  atan    = ixFmap atan
  sinh    = ixFmap sinh
  cosh    = ixFmap cosh
  tanh    = ixFmap tanh
  asinh   = ixFmap asinh
  acosh   = ixFmap acosh
  atanh   = ixFmap atanh
  (**)    = ixLiftA2 (**)

instance (ScalarTy a, RealFloat a, j ~ i) => RealFloat (Codensity AST (AST a := j) i ) where
  atan2 = ixLiftA2 atan2

-- * Numeric conversions
--
-- $conversions
-- Instance for 'Convert'.
instance ( ScalarTy a, ScalarTy b, Convert '(a,b)
         , j ~ i, k ~ i, l ~ i
         )
         => Convert '( Codensity AST (AST a := j) i
                     , Codensity AST (AST b := l) k
                     ) where
  convert = ixFmap convert


-- * Vectors
--
-- $vectors
-- Instances for:
--
-- 'Semimodule', 'Module', 'Inner', 'Cross'.
instance (ScalarTy a, Semiring a, j ~ i) => Semimodule (Codensity AST (AST (V 0 a) := j) i) where
  type Scalar (Codensity AST (AST (V 0 a) := j) i)   = Codensity AST (AST      a  := j) i
  type OfDim  (Codensity AST (AST (V 0 a) := j) i) n = Codensity AST (AST (V n a) := j) i

  (^+^) = ixLiftA2 (^+^)
  (^*)  = ixLiftA2 (^*)

instance (ScalarTy a, Ring a, j ~ i) => Module (Codensity AST (AST (V 0 a) := j) i) where
  (^-^) = ixLiftA2 (^-^)

instance (ScalarTy a, Semiring a, j ~ i) => Inner (Codensity AST (AST (V 0 a) := j) i) where
  (^.^) = ixLiftA2 (^.^)

instance (ScalarTy a, Floating a, j ~ i) => Cross (Codensity AST (AST (V 0 a) := j) i) where
  cross = ixLiftA2 cross

-- * Matrices
--
-- $matrices
-- Instance for 'Matrix'.

instance (ScalarTy a, Ring a, j ~ i) => Matrix (Codensity AST (AST (M 0 0 a) := j) i) where
  type Vector (Codensity AST (AST (M 0 0 a) := j) i)     = Codensity AST (AST (V 0   a) := j) i
  type OfDims (Codensity AST (AST (M 0 0 a) := j) i) m n = Codensity AST (AST (M m n a) := j) i

  diag  = ixFmap diag
  konst = ixFmap konst

  transpose   = ixFmap transpose
  inverse     = ixFmap inverse
  determinant = ixFmap determinant

  (!+!) = ixLiftA2 (!+!)
  (!-!) = ixLiftA2 (!-!)
  (!*!) = ixLiftA2 (!*!)
  (^*!) = ixLiftA2 (^*!)
  (!*^) = ixLiftA2 (!*^)
  (!*)  = ixLiftA2 (!*)


-- * Functor functionality

infixl 4 <$$$>
infixl 4 <***>

class CodensityASTFunctor f where
  fmapCodAST :: AST (a -> b) -> Codensity AST (f a := j) i -> Codensity AST (f b := j) i

class CodensityASTApplicative f where
  pureCodAST :: AST a -> AST (f a)
  (<***>)  :: AST ( f (a -> b) ) -> AST ( f a ) -> AST ( f b )

(<$$$>) :: CodensityASTFunctor f => AST (a -> b) -> Codensity AST (f a := j) i -> Codensity AST (f b := j) i
(<$$$>) = fmapCodAST

instance KnownNat n => CodensityASTFunctor (V n) where
  fmapCodAST = error "fmapCodAST: todo"

instance (KnownNat m, KnownNat n) => CodensityASTFunctor (M m n) where
  fmapCodAST = error "fmapCodAST: todo"

instance KnownNat n => CodensityASTApplicative (V n) where
  pureCodAST = error "pureCodAST: todo"
  (<***>)  = error "pureCodAST: todo"