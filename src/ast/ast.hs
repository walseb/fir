{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--{-# LANGUAGE NoStarIsType #-} -- for GHC 8.6.1

module AST.AST where

-- base  
import Data.Kind(Type)
import Data.Proxy(Proxy)
import GHC.TypeLits( Nat, KnownNat, natVal, type (-)
                   , KnownSymbol, symbolVal)
import GHC.TypeNats(type (<=?))

-- containers
import Data.Tree(Tree(Node))

-- mtl
import Control.Monad.State.Lazy(State, put, get, evalState)

-- fir
import Control.Monad.Indexed( FunctorIx(..), MonadIx(..), (:=)(..) )
import Data.Type.Bindings 
  ( Binding(Var, Fun), BindingType
  , CanDef, CanFunDef, Get, Put
  , Insert, Union
  )
import Math.Linear(V, M)
import qualified SPIRV.PrimOps as SPIRV
import qualified SPIRV.Types   as SPIRV
import SPIRV.Types ( Signedness(Unsigned, Signed)
                   , Width(W16, W32)
                   )


------------------------------------------------------------
-- primitive types, which can be internalised in the AST

class Show ty => PrimTy ty     where 
instance         PrimTy ()     where
instance         PrimTy Bool   where
instance         PrimTy Word   where
instance         PrimTy Int    where
instance         PrimTy Float  where
instance         PrimTy Double where


class PrimTy a => PrimScalarTy a where
  scalar :: SPIRV.PrimTy

instance PrimScalarTy Word where
  scalar = SPIRV.Integer Unsigned W32
instance PrimScalarTy Int where
  scalar = SPIRV.Integer Signed   W32
instance PrimScalarTy Float where
  scalar = SPIRV.Floating         W16
instance PrimScalarTy Double where
  scalar = SPIRV.Floating         W32

instance ( PrimScalarTy a
         , KnownNat n
         ) => PrimTy (V n a) where

instance ( PrimScalarTy a
         , KnownNat m
         , KnownNat n
         ) => PrimTy (M m n a) where

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
  Pure :: AST ((a := j) i -> m (a := j) i)
  Bind :: AST (m (a := j) i -> (a -> m b j) -> m b i) -- angelic bind

  Ix :: AST (a -> (a := i) i) -- hack, would be solved if we could have
                              -- Pure :: AST (a -> m (a := j) i)

  Def :: forall k perms ty i. (KnownSymbol k, CanDef k i ~ 'True, PrimTy ty)
      => Proxy k
      -> Proxy perms
      -> AST (    ty
               -> S ( ty := Insert k ('Var perms ty) i) i
             )
  FunDef :: forall k j ty l i. (KnownSymbol k, CanFunDef k i j l ~ 'True, PrimTy ty)
         => Proxy k
         -> Proxy j
         -> Proxy l
         -> AST (    S (ty := l) (Union i j)
                  -> S (BindingType ('Fun j ty) := Insert k ('Fun j ty) i) i
                )

  Get    :: forall k ty i. (KnownSymbol k, Get k i ~ ty)
         => Proxy k
         -> AST ( S (ty := i) i)
  Put    :: forall k ty i. (KnownSymbol k, Put k i ~ ty, PrimTy ty)
         => Proxy k
         -> AST ( ty -> S (():= i) i )

  -- vectors (and matrices)
  MkVector :: KnownNat n => Proxy n -> AST ( Variadic n a ( V n a ) )
  VectorAt :: (KnownNat n, KnownNat i) => Proxy i -> AST ( V n a -> a )

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
toTreeArgs If                as = return (Node "If"   as)
toTreeArgs Pure              as = return (Node "Pure" as)
toTreeArgs Bind              as = return (Node "Bind" as)
toTreeArgs Ix                as = return (Node "Ix"   as)
toTreeArgs (NamedVar v     ) as = return (Node v      as)
toTreeArgs (Lit      a     ) as = return (Node ("Lit "     ++ show a          ) as)
toTreeArgs (Get      px    ) as = return (Node ("Get @"    ++ symbolVal    px ) as)
toTreeArgs (Put      px    ) as = return (Node ("Put @"    ++ symbolVal    px ) as)
toTreeArgs (Def      px _  ) as = return (Node ("Def @"    ++ symbolVal    px ) as)
toTreeArgs (FunDef   px _ _) as = return (Node ("FunDef @" ++ symbolVal    px ) as)
toTreeArgs (MkVector px    ) as = return (Node ("Vec"      ++ show (natVal px)) as)
toTreeArgs (VectorAt px    ) as = return (Node ("At "      ++ show (natVal px)) as)

toTree :: AST a -> Tree String
toTree a = evalState (toTreeArgs a []) 0

instance Show (AST a) where
  show = show . toTree

