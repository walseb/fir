{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
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

-- tree-view
import Data.Tree.View(showTree)

-- fir
import CodeGen.Instruction(ID(ID))
import Control.Monad.Indexed( FunctorIx(..), MonadIx(..), (:=)(..)
                            , Id )
import Data.Type.Bindings 
  ( BindingType, Var, Fun
  , Insert, Union
  )
import FIR.Binding ( ValidDef, ValidFunDef, ValidEntryPoint
                   , Get, Put
                   )
import FIR.Builtin(KnownStage(stageVal), StageBuiltins)
import FIR.PrimTy(PrimTy, KnownVars)
import Math.Linear(V, M)
import qualified SPIRV.PrimOp as SPIRV

------------------------------------------------------------

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
  -- (specialised to the identity indexed monad)
  Pure :: AST (p i -> Id p i)
  Bind :: AST (Id (a := j) i -> (a -> Id q j) -> Id q i) -- angelic bind

  MkAtKey :: AST (a -> (a := i) i) -- hack, would be solved if we could have
                                   -- Pure :: AST (a -> m (a := i) i)
  RunAtKey :: AST ( (a := j) i -> a )
  RunId :: AST (Id p i -> p i)

  Def :: forall k ps a i. (KnownSymbol k, ValidDef k i ~ 'True, PrimTy a)
      => Proxy k
      -> Proxy ps
      -> AST (    a
               -> Id ( a := Insert k (Var ps a) i) i
             )
  FunDef :: forall k as b l i. (KnownSymbol k, KnownVars as, PrimTy b, ValidFunDef k as i l ~ 'True)
         => Proxy k
         -> Proxy as -- function arguments
         -> Proxy b
         -> AST (    Id (b := l) (Union i as)
                  -> Id (BindingType (Fun as b) := Insert k (Fun as b) i) i
                )
  -- entry point: a function definition, with no arguments and Unit return type
  -- it is given access to addtional builtins
  -- this function definition is not added to the index of items in scope
  -- ( it is not allowed to be called )
  -- TODO: add a special "entry point" binding instead of nothing
  Entry :: forall k s l i. (KnownSymbol k, KnownStage s, ValidEntryPoint s i l ~ 'True)
         => Proxy k
         -> Proxy s
         -> AST (    Id (() := l) (Union i (StageBuiltins s))
                  -> Id (() := i) i
                )
  Get :: forall k i. KnownSymbol k
      => Proxy k
      -> AST ( Id (Get k i := i) i)
  Put :: forall k i. (KnownSymbol k, PrimTy (Put k i))
      => Proxy k
      -> AST ( Put k i -> Id (():= i) i )

  -- vectors (and matrices)
  MkVector :: (KnownNat n, PrimTy a)
           => Proxy n
           -> Proxy a
           -> AST ( Variadic n a ( V n a ) )
  VectorAt :: (KnownNat n, KnownNat i, PrimTy a)
           => Proxy a
           -> Proxy i
           -> AST ( V n a -> a )
  FmapVector :: KnownNat n => Proxy n -> AST ( (a -> b) -> (V n a -> V n b) )

  Mat   :: (KnownNat m, KnownNat n) => AST ( V m (V n a) -> M m n a )
  UnMat :: (KnownNat m, KnownNat n) => AST ( M m n a -> V m (V n a) )

  MkID :: ID -> AST a

  -- for printing lambdas
  NamedVar :: String -> AST a

------------------------------------------------ 
-- codensity indexed monad, specialised with AST return type

-- demonic codensity
newtype Codensity m p i
  = Codensity
    { runCodensity :: forall (q :: k -> Type)
    . (forall (j :: k). p j -> AST (m q j) ) -> AST (m q i)
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
-- display AST for viewing

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
toTreeArgs If       as = return (Node "If"       as)
toTreeArgs Pure     as = return (Node "Pure"     as)
toTreeArgs Bind     as = return (Node "Bind"     as)
toTreeArgs MkAtKey  as = return (Node "MkAtKey"  as)
toTreeArgs RunAtKey as = return (Node "RunAtKey" as)
toTreeArgs RunId    as = return (Node "RunId"    as)
toTreeArgs Mat      as = return (Node "Mat"      as)
toTreeArgs UnMat    as = return (Node "UnMat"    as)
toTreeArgs (NamedVar   v  ) as = return (Node v as)
toTreeArgs (MkID       v  ) as = return (Node (show v) as)
toTreeArgs (Lit        a  ) as = return (Node ("Lit "     ++ show a ) as)
toTreeArgs (MkVector   n _) as = return (Node ("Vec"      ++ show (natVal n)) as)
toTreeArgs (VectorAt   _ i) as = return (Node ("At "      ++ show (natVal i)) as)
toTreeArgs (FmapVector n  ) as = return (Node ("Fmap V"   ++ show (natVal n)) as)
toTreeArgs (Get    k      ) as = return (Node ("Get @"    ++ symbolVal k ) as)
toTreeArgs (Put    k      ) as = return (Node ("Put @"    ++ symbolVal k ) as)
toTreeArgs (Def    k _    ) as = return (Node ("Def @"    ++ symbolVal k ) as)
toTreeArgs (FunDef k _ _  ) as = return (Node ("FunDef @" ++ symbolVal k ) as)
toTreeArgs (Entry  _ s    ) as = return (Node ("Entry @"  ++ show (stageVal s)) as)

toTree :: AST a -> Tree String
toTree a = evalState (toTreeArgs a []) 0

instance Show (AST a) where
  show = showTree . toTree

