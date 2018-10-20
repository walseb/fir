{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


module FIR.AST where

-- base  
import Data.Kind(Type)
import Data.Proxy(Proxy)
import GHC.TypeLits(KnownSymbol, symbolVal)
import GHC.TypeNats (Nat, KnownNat, natVal, type (-), type (<=?))             

-- containers
import Data.Tree(Tree(Node))

-- mtl
import Control.Monad.State.Lazy(State, put, get, evalState)

-- fir
import FIR.PrimTy(PrimTy)
import Control.Monad.Indexed( FunctorIx(..), MonadIx(..), (:=)(..) )
import Data.Type.Bindings 
  ( BindingType, Var, Fun
  , CanDef, CanFunDef, Get, Put
  , Insert, Union
  )
import Math.Linear(V, M)
import qualified SPIRV.PrimOp as SPIRV

------------------------------------------------------------

data S a i

type family Variadic (n :: Nat) a b = r where
  Variadic n a b = Variadic' n a b (1 <=? n)

type family Variadic' (n :: Nat) a b geq1 where
  Variadic' _ _ b 'False = b
  Variadic' n a b 'True  = a -> Variadic (n-1) a b

------------------------------------------------------------
-- main AST data type

data AST :: Type -> Type where
  Lam :: {-PrimTy a =>-} (AST a -> AST b) -> AST (a -> b)
  (:$) :: AST (a -> b) -> AST a -> AST b

  Lit :: PrimTy a => a -> AST a
  PrimOp :: SPIRV.PrimOp -> a -> AST a

  If :: PrimTy a => AST (Bool -> a -> a -> a)

  -- (indexed) monadic operations
  Pure :: AST (a i -> m a i)
  Bind :: AST (m (a := j) i -> (a -> m b j) -> m b i) -- angelic bind

  Ix :: AST (a -> (a := i) i) -- hack, would be solved if we could have
                              -- Pure :: AST (a -> m (a := i) i)

  Def :: forall k perms a i. (KnownSymbol k, CanDef k i ~ 'True, PrimTy a)
      => Proxy k
      -> Proxy perms
      -> AST (    a
               -> S ( a := Insert k (Var perms a) i) i
             )
  FunDef :: forall k as b l i. (KnownSymbol k, CanFunDef k as i l ~ 'True, PrimTy b)
         => Proxy k
         -> Proxy as
         -> Proxy l
         -> AST (    S (b := l) (Union i as)
                  -> S (BindingType (Fun as b) := Insert k (Fun as b) i) i
                )

  Get    :: forall k a i. (KnownSymbol k, Get k i ~ a)
         => Proxy k
         -> AST ( S (a := i) i)
  Put    :: forall k a i. (KnownSymbol k, Put k i ~ a, PrimTy a)
         => Proxy k
         -> AST ( a -> S (():= i) i )

  -- vectors (and matrices)
  MkVector :: KnownNat n => Proxy n -> AST ( Variadic n a ( V n a ) )
  VectorAt :: (KnownNat n, KnownNat i) => Proxy i -> AST ( V n a -> a )
  FmapVector :: KnownNat n => Proxy n -> AST ( (a -> b) -> (V n a -> V n b) )

  Mat   :: (KnownNat m, KnownNat n) => AST ( V m (V n a) -> M m n a )
  UnMat :: (KnownNat m, KnownNat n) => AST ( M m n a -> V m (V n a) )

  -- for printing lambdas
  NamedVar :: String -> AST a

------------------------------------------------ 
-- codensity indexed monad, specialised with AST return type

-- demonic codensity
newtype Codensity m a i
  = Codensity
    { runCodensity :: forall (b :: k -> Type)
    . (forall (j :: k). a j -> AST (m b j) ) -> AST (m b i)
    }

instance FunctorIx (Codensity m) where
  fmapIx :: ( forall ix.               p ix ->               q ix )
         -> ( forall ix. (Codensity m) p ix -> (Codensity m) q ix )
  fmapIx f (Codensity m) = Codensity ( \k -> m ( k . f ) )

instance MonadIx (Codensity m) where
  returnIx :: p i -> Codensity m p i
  returnIx a = Codensity ( \k -> k a )

  extendIx :: ( forall ix.               p ix -> (Codensity m) q ix )
           -> ( forall ix. (Codensity m) p ix -> (Codensity m) q ix )
  extendIx f (Codensity ma) = Codensity ( \k -> ma ( \a -> runCodensity (f a) k ) )

------------------------------------------------
-- convert to explicit AST

toTreeArgs :: AST a -> [Tree String] -> State Int (Tree String)
toTreeArgs (f :$ a) as = do
  at <- toTreeArgs a []
  toTreeArgs f (at:as)
toTreeArgs (Lam f) as = do
  v <- get
  put (v+1)
  let var = NamedVar ('v' : show v)
  body <- toTreeArgs (f var) []
  return $ case as of
    [] -> Node ("Lam v" ++ show v) [body]
    _  -> Node  ":$"               (body : as)
toTreeArgs (PrimOp op _ ) as 
  = return (Node ("PrimOp " ++ opName ) as)
    where opName = show ( SPIRV.op op )
toTreeArgs If                  as = return (Node "If"    as)
toTreeArgs Pure                as = return (Node "Pure"  as)
toTreeArgs Bind                as = return (Node "Bind"  as)
toTreeArgs Ix                  as = return (Node "Ix"    as)
toTreeArgs Mat                 as = return (Node "Mat"   as)
toTreeArgs UnMat               as = return (Node "UnMat" as)
toTreeArgs (NamedVar   v     ) as = return (Node v       as)
toTreeArgs (Lit        a     ) as = return (Node ("Lit "     ++ show a          ) as)
toTreeArgs (Get        px    ) as = return (Node ("Get @"    ++ symbolVal    px ) as)
toTreeArgs (Put        px    ) as = return (Node ("Put @"    ++ symbolVal    px ) as)
toTreeArgs (Def        px _  ) as = return (Node ("Def @"    ++ symbolVal    px ) as)
toTreeArgs (FunDef     px _ _) as = return (Node ("FunDef @" ++ symbolVal    px ) as)
toTreeArgs (MkVector   px    ) as = return (Node ("Vec"      ++ show (natVal px)) as)
toTreeArgs (VectorAt   px    ) as = return (Node ("At "      ++ show (natVal px)) as)
toTreeArgs (FmapVector px    ) as = return (Node ("Fmap V"   ++ show (natVal px)) as)

toTree :: AST a -> Tree String
toTree a = evalState (toTreeArgs a []) 0

instance Show (AST a) where
  show = show . toTree

