{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module FIR.Instances.Codensity where

-- base
import Prelude hiding( Eq(..), (&&), (||), not
                     , Ord(..)
                     , Num(..), Floating(..)
                     , Integral(..)
                     , Fractional(..), fromRational
                     , Functor(..)
                     , Applicative(..)
                     )
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import Data.Word(Word16)
import qualified GHC.Stack
import GHC.TypeLits(KnownSymbol)
import GHC.TypeNats(KnownNat)

-- fir
import Control.Monad.Indexed( (:=)(AtKey), Codensity(Codensity)
                            , ixFmap, ixPure, ixLiftA2
                            )
import Control.Type.Optic( Optic, KnownOptic, opticSing
                         , Gettable(Get), Settable(Set)
                         , RequiredIndices
                         )
import Data.Type.Map(Insert, Union, type (:++:))
import FIR.AST(AST(..), Syntactic(Internal,toAST,fromAST))
import FIR.Binding(BindingsMap, BindingType, Var, Fun)
import FIR.Builtin(StageBuiltins, KnownStage)
import FIR.Instances.AST()
import FIR.Instances.Bindings(ValidDef, ValidFunDef, ValidEntryPoint)
import FIR.Instances.Optics(Getter, Setter)
import FIR.PrimTy(PrimTy, ScalarTy, KnownVars)
import Math.Algebra.Class ( AdditiveGroup(..)
                          , Semiring(..), Ring(..)
                          , DivisionRing(..)
                          , Signed(..), Archimedean(..)
                          , Convert(..)
                          )
import Math.Linear( Semimodule(..), Module(..)
                  , Inner(..)
                  , Matrix(..)
                  , V, M
                  )
import Math.Logic.Class ( Eq(..), Boolean(..)
                        , Choose(..)
                        , Ord(..)
                        )

--------------------------------------------------------------------------

instance Syntactic a => Syntactic (Codensity AST (a := j) i) where
  type Internal (Codensity AST (a := j) i) = (Internal a := j) i

  toAST :: Codensity AST (a := j) i -> AST ( (Internal a := j) i )
  toAST (Codensity k) = k ( \(AtKey a) -> Return :$ toAST a )

  fromAST :: AST ( (Internal a := j) i) -> Codensity AST (a := j) i
  fromAST a = Codensity ( \k -> fromAST Bind a (k . AtKey) )

--------------------------------------------------------------------------
-- specialise stateful functions to work with the Codensity indexed monad

def :: forall k ps a i.
       ( GHC.Stack.HasCallStack
       , KnownSymbol k
       , PrimTy a
       , ValidDef k i ~ 'True
       )
    => AST a
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

entryPoint :: forall k s l i.
             ( GHC.Stack.HasCallStack
             , KnownSymbol k
             , KnownStage s
             , ValidEntryPoint s i l ~ 'True
             )
           => Codensity AST (AST () := l) (Union i (StageBuiltins s))
           -> Codensity AST (AST () := i) i

-- would want to make i and a invisible
get :: forall i a (optic :: Optic i a).
            ( GHC.Stack.HasCallStack
            , KnownOptic optic
            , Gettable i a optic
            , Syntactic (CodGetter i optic)
            , Internal (CodGetter i optic) ~ Getter i optic
            )
          => CodGetter i optic

-- would want to make i and a invisible
put :: forall i a (optic :: Optic i a).
            ( GHC.Stack.HasCallStack
            , KnownOptic optic
            , Settable i a optic
            , Syntactic (CodSetter i optic)
            , Internal (CodSetter i optic) ~ Setter i optic
            )
          => CodSetter i optic


def        = fromAST ( Def    @k @ps @a    @i Proxy Proxy       ) . toAST
fundef'    = fromAST ( FunDef @k @as @b @l @i Proxy Proxy Proxy ) . toAST
entryPoint = fromAST ( Entry  @k     @s @l @i Proxy Proxy       ) . toAST
get        = fromAST ( Get @i @a @optic       opticSing         )
put        = fromAST ( Put @i @a @optic       opticSing         )


fundef :: forall k as b l i r.
           ( GHC.Stack.HasCallStack
           , Syntactic r
           , Internal r ~ BindingType (Fun as b)
           , KnownSymbol k
           , KnownVars as
           , PrimTy b
           , ValidFunDef k as i l ~ 'True
           )
        => Codensity AST (AST b := l) (Union i as)
        -> Codensity AST ( r := Insert k (Fun as b) i) i
fundef = fromAST . toAST . fundef' @k @as @b @l @i

--------------------------------------------------------------------------
-- utility types for get/put

type CodGetter i (o :: Optic i a) = CodVariadicList (RequiredIndices o              ) (Get o) i
type CodSetter i (o :: Optic i a) = CodVariadicList (RequiredIndices o :++: '[Set o]) ()      i

type family CodVariadicList
              ( as :: [Type]      )
              ( b  :: Type        )
              ( i  :: BindingsMap )
            = ( r  :: Type        )
            | r -> as i b  where
  CodVariadicList '[]       b i = Codensity AST (AST b := i) i
  CodVariadicList (a ': as) b i = AST a -> CodVariadicList as b i

--------------------------------------------------------------------------
-- instances for codensity representation

-- logical operations
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

while :: ( GHC.Stack.HasCallStack
         , PrimTy a
         , i' ~ i, i'' ~ i
         , b ~ (AST Bool := i)
         , r ~ (AST a := i)
         )
      => Codensity AST b               (i :: BindingsMap)
      -> Codensity AST ( AST a := j  ) i'
      -> Codensity AST r               i''
while = fromAST While

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

-- numeric operations
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


-- numeric conversions
instance ( ScalarTy a, ScalarTy b, Convert '(a,b)
         , j ~ i, k ~ i, l ~ i
         )
         => Convert '( Codensity AST (AST a := j) i
                     , Codensity AST (AST b := l) k
                     ) where
  convert = ixFmap convert


-- vectors
instance (ScalarTy a, Semiring a, j ~ i) => Semimodule (Codensity AST (AST (V 0 a) := j) i) where
  type Scalar (Codensity AST (AST (V 0 a) := j) i)   = Codensity AST (AST      a  := j) i
  type OfDim  (Codensity AST (AST (V 0 a) := j) i) n = Codensity AST (AST (V n a) := j) i

  (^+^) = ixLiftA2 (^+^)
  (^*)  = ixLiftA2 (^*)

instance (ScalarTy a, Ring a, j ~ i) => Module (Codensity AST (AST (V 0 a) := j) i) where
  (^-^) = ixLiftA2 (^-^)

instance (ScalarTy a, Semiring a, j ~ i) => Inner (Codensity AST (AST (V 0 a) := j) i) where
  (^.^) = ixLiftA2 (^.^)


-- matrices

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


infixl 4 <$$$>
infixl 4 <***>

-- functor functionality
class CodensityASTFunctor f where
  fmapCodAST :: AST (a -> b) -> Codensity AST (f a := j) i -> Codensity AST (f b := j) i

class CodensityASTApplicative f where
  pureCodAST :: AST a -> AST (f a)
  (<***>)  :: AST ( f (a -> b) ) -> AST ( f a ) -> AST ( f b )

(<$$$>) :: CodensityASTFunctor f => AST (a -> b) -> Codensity AST (f a := j) i -> Codensity AST (f b := j) i
(<$$$>) = fmapCodAST


instance KnownNat n => CodensityASTFunctor (V n) where
  fmapCodAST = error "todo"

instance (KnownNat m, KnownNat n) => CodensityASTFunctor (M m n) where
  fmapCodAST = error "todo"

instance KnownNat n => CodensityASTApplicative (V n) where
  pureCodAST = error "todo"
  (<***>)  = error "todo"