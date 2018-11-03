{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module FIR.Codensity where

-- base
import Prelude hiding( Eq(..), (&&), (||), not
                     , Ord(..)
                     , Num(..), Floating(..)
                     , Integral(..)
                     , Fractional(..), fromRational
                     , Functor(..)
                     , Applicative(..)
                     )
import Data.Proxy(Proxy(Proxy))                     
import Data.Word(Word16)                     
import qualified GHC.Stack
import GHC.TypeLits(KnownSymbol)
import GHC.TypeNats(KnownNat)

-- fir
import Control.Monad.Indexed( (:=)(AtKey), Codensity(Codensity)
                            , ixFmap, ixPure, ixLiftA2
                            )
import Data.Type.Bindings(Insert, Union, BindingType, Var, Fun, BindingsMap)
import FIR.Binding ( ValidDef, ValidFunDef, ValidEntryPoint
                   , Put, Get
                   )
import FIR.AST(AST(..))
import FIR.Builtin(StageBuiltins, KnownStage)
import FIR.Instances(Syntactic(Internal,toAST,fromAST)) -- also importing orphan instances
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

get :: forall k i. (GHC.Stack.HasCallStack, KnownSymbol k)
    => Codensity AST (AST (Get k i) := i) i

put :: forall k i. (GHC.Stack.HasCallStack, KnownSymbol k, PrimTy (Put k i))
    => AST (Put k i) -> Codensity AST (AST () := i) i

def        a = Codensity ( \h -> Bind :$ (Def    @k @ps @a    @i Proxy Proxy       :$ a      ) :$ (Lam $ h . AtKey) )
fundef'    f = Codensity ( \h -> Bind :$ (FunDef @k @as @b @l @i Proxy Proxy Proxy :$ toAST f) :$ (Lam $ h . AtKey) )
entryPoint f = Codensity ( \h -> Bind :$ (Entry  @k     @s @l @i Proxy Proxy       :$ toAST f) :$ (Lam $ h . AtKey) )
get          = Codensity ( \h -> Bind :$  Get    @k           @i Proxy                         :$ (Lam $ h . AtKey) )
put        a = Codensity ( \h -> Bind :$ (Put    @k           @i Proxy             :$ a      ) :$ (Lam $ h . AtKey) )

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
instance (ScalarTy a, Semiring a, j ~ i) => Semiring (Codensity AST (AST a := j) i) where
  (*)    = ixLiftA2 (*)
instance (ScalarTy a, Ring a, j ~ i) => Ring (Codensity AST (AST a := j) i) where
  (-)    = ixLiftA2 (-)
  negate = ixFmap negate
  fromInteger = ixPure . fromInteger
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