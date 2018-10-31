{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
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
import Control.Monad.State.Lazy(evalState)

-- tree-view
import Data.Tree.View(showTree)

-- fir
import CodeGen.Instruction(ID(ID))
import CodeGen.Monad(MonadFresh(fresh), runFreshSuccT)
import Control.Monad.Indexed((:=))
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

  If :: AST (    Bool
              -> ( a := j ) i -- variables declared in a branch
              -> ( a := k ) i -- remain local to that branch
              -> ( a := i ) i
            )
  While :: AST (    ( Bool := i ) i
                 -> ( a    := j ) i -- ditto
                 -> ( a    := i ) i
               )

  -- angelic indexed monadic bind (for the identity indexed monad)
  Bind :: AST ( (a := j) i -> (a -> q j) -> q i )

  MkAtKey :: AST (a -> (a := i) i)
  RunAtKey :: AST ( (a := j) i -> a ) -- unsafe hack

  Def :: forall k ps a i. (KnownSymbol k, ValidDef k i ~ 'True, PrimTy a)
      => Proxy k
      -> Proxy ps
      -> AST (    a
               -> (a := Insert k (Var ps a) i) i
             )
  FunDef :: forall k as b l i. (KnownSymbol k, KnownVars as, PrimTy b, ValidFunDef k as i l ~ 'True)
         => Proxy k
         -> Proxy as -- function arguments
         -> Proxy b
         -> AST (    (b := l) (Union i as)
                  -> (BindingType (Fun as b) := Insert k (Fun as b) i) i
                )
  -- entry point: a function definition, with no arguments and Unit return type
  -- it is given access to addtional builtins
  -- this function definition is not added to the index of items in scope
  -- ( it is not allowed to be called )
  -- TODO: add a special "entry point" binding instead of nothing
  Entry :: forall k s l i. (KnownSymbol k, KnownStage s, ValidEntryPoint s i l ~ 'True)
         => Proxy k
         -> Proxy s
         -> AST (    (() := l) (Union i (StageBuiltins s))
                  -> (() := i) i
                )
  Get :: forall k i. KnownSymbol k
      => Proxy k
      -> AST ( (Get k i := i) i)
  Put :: forall k i. (KnownSymbol k, PrimTy (Put k i))
      => Proxy k
      -> AST ( Put k i -> (():= i) i )

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

------------------------------------------------
-- display AST for viewing

toTreeArgs :: forall m a. MonadFresh ID m => AST a -> [Tree String] -> m (Tree String)
toTreeArgs (f :$ a) as = do
  at <- toTreeArgs a []
  toTreeArgs f (at:as)
toTreeArgs (Lam f) as = do
  v <- fresh
  let var = MkID v
  body <- toTreeArgs (f var) []
  return $ case as of
    [] -> Node ("Lam " ++ show v) [body]
    _  -> Node  ":$"              (body : as)
toTreeArgs (PrimOp op _ ) as 
  = return (Node ("PrimOp " ++ opName ) as)
    where opName = show ( SPIRV.op op )
toTreeArgs If       as = return (Node "If"       as)
toTreeArgs While    as = return (Node "While"    as)
toTreeArgs Bind     as = return (Node "Bind"     as)
toTreeArgs MkAtKey  as = return (Node "MkAtKey"  as)
toTreeArgs RunAtKey as = return (Node "RunAtKey" as)
toTreeArgs Mat      as = return (Node "Mat"      as)
toTreeArgs UnMat    as = return (Node "UnMat"    as)
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
toTree = (`evalState` (ID 1)) . runFreshSuccT . ( `toTreeArgs` [] )

instance Show (AST a) where
  show = showTree . toTree

