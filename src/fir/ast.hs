{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module FIR.AST where

-- base
import Data.Kind(Type)
import Data.Proxy(Proxy)
import qualified GHC.Stack
import GHC.TypeLits(KnownSymbol, symbolVal)
import GHC.TypeNats(KnownNat, natVal)

-- containers
import Data.Tree(Tree(Node))

-- mtl
import Control.Monad.State.Lazy(evalState)

-- tree-view
import Data.Tree.View(showTree)

-- fir
import CodeGen.Instruction(ID(ID))
import CodeGen.Monad(MonadFresh(fresh), runFreshSuccT)
import Control.Type.Optic(Gettable, Settable)
import Control.Monad.Indexed((:=))
import Data.Function.Variadic(NatVariadic)
import Data.Type.Map(Insert, Union)
import FIR.Binding(BindingType, Var, Fun, KnownPermissions)
import FIR.Builtin(KnownStage(stageVal), StageBuiltins)
import FIR.Instances.Bindings(ValidDef, ValidFunDef, ValidEntryPoint)
import FIR.Instances.Optics(User, Assigner, KnownOptic, SOptic, showSOptic)
import FIR.Prim.Singletons(PrimTy, primTyVal, SPrimFunc, primFuncName, KnownVars)
import Math.Linear(V, M)
import qualified SPIRV.PrimOp as SPIRV
import qualified SPIRV.PrimTy as SPIRV

------------------------------------------------------------
-- main AST data type

data AST :: Type -> Type where
  Lam :: {-PrimTy a =>-} (AST a -> AST b) -> AST (a -> b)
  (:$) :: AST (a -> b) -> AST a -> AST b

  Lit :: PrimTy a => Proxy a -> a -> AST a
  PrimOp :: SPIRV.PrimOp -> a -> AST a

  -- indexed monadic operations (for the AST itself)
  Return :: AST (a -> (a := i) i)
  Bind :: AST ( (a := j) i -> (a -> q j) -> q i ) -- angelic bind

  Def :: forall k ps a i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , KnownPermissions ps
        , PrimTy a
        , ValidDef k i ~ 'True
        )
      => Proxy k
      -> Proxy ps
      -> AST (    a
               -> (a := Insert k (Var ps a) i) i
             )
  FunDef :: forall k as b l i.
            ( GHC.Stack.HasCallStack
            , KnownSymbol k
            , KnownVars as
            , PrimTy b
            , ValidFunDef k as i l ~ 'True
            )
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
  Entry :: forall k s l i.
           ( GHC.Stack.HasCallStack
           , KnownSymbol k
           , KnownStage s
           , ValidEntryPoint s i l ~ 'True
           )
         => Proxy k
         -> Proxy s
         -> AST (    (() := l) (Union i (StageBuiltins s))
                  -> (() := i) i
                )

  Use :: forall optic.
        ( GHC.Stack.HasCallStack, KnownOptic optic, Gettable optic )
      => SOptic optic -> AST ( User optic )
  Assign :: forall optic.
        ( GHC.Stack.HasCallStack, KnownOptic optic, Settable optic )
      => SOptic optic -> AST ( Assigner optic )

  -- control flow
  If    :: ( GHC.Stack.HasCallStack
           , PrimTy a
           )
        => AST ( Bool -> a -> a -> a )
  IfM   :: GHC.Stack.HasCallStack
        => AST ( Bool -> (a := j) i -> (a := k) i -> (a := i) i )
  While :: GHC.Stack.HasCallStack
        => AST ( ( Bool := i ) i -> (a := j) i -> (a := i) i )

  Locally :: AST ( (a := j) i -> (a := i) i )

  -- functor, applicative
  -- passing a singleton representing the functor
  Fmap :: PrimTy a
       => SPrimFunc f -> AST ( (a -> b) -> f a -> f b )
  Pure :: SPrimFunc f -> AST ( a -> f a )
  Ap   :: PrimTy a
       => SPrimFunc f
       -> Proxy a
       -> AST ( f (a -> b) -> f a -> f b )

  -- vectors (and matrices)
  MkVector :: (KnownNat n, PrimTy a)
           => Proxy n
           -> Proxy a
           -> AST ( NatVariadic n a ( V n a ) )
  VectorAt :: (KnownNat n, KnownNat i, PrimTy a)
           => Proxy a
           -> Proxy i
           -> AST ( V n a -> a )

  Mat   :: (KnownNat m, KnownNat n) => AST ( V m (V n a) -> M m n a )
  UnMat :: (KnownNat m, KnownNat n) => AST ( M m n a -> V m (V n a) )

  -- pairs (for internal use at the moment)
  Pair :: AST ( a -> b -> (a,b) )
  Fst  :: AST ( (a,b) -> a )
  Snd  :: AST ( (a,b) -> b )

  -- used to bypass the type-system by injecting IDs at any type
  MkID :: (ID, SPIRV.PrimTy) -> AST a

------------------------------------------------
-- syntactic type class (Axelsson, Svenningsson)

class Syntactic a where
  type Internal a
  toAST :: a -> AST (Internal a)
  fromAST :: AST (Internal a) -> a

------------------------------------------------
-- display AST for viewing

toTreeArgs :: forall m a. MonadFresh ID m => AST a -> [Tree String] -> m (Tree String)
toTreeArgs (f :$ a) as = do
  at <- toTreeArgs a []
  toTreeArgs f (at:as)
toTreeArgs (Lam f) as = do
  v <- fresh
  let var = MkID (v,undefined)
  body <- toTreeArgs (f var) []
  return $ case as of
    [] -> Node ("Lam " ++ show v) [body]
    _  -> Node  ":$"              (body : as)
toTreeArgs (PrimOp op _ ) as 
  = return (Node ("PrimOp " ++ opName ) as)
    where opName = show ( SPIRV.op op )
toTreeArgs If       as = return (Node "If"       as)
toTreeArgs IfM      as = return (Node "IfM"      as)
toTreeArgs While    as = return (Node "While"    as)
toTreeArgs Locally  as = return (Node "Locally"  as)
toTreeArgs Return   as = return (Node "Return"   as)
toTreeArgs Bind     as = return (Node "Bind"     as)
toTreeArgs Mat      as = return (Node "Mat"      as)
toTreeArgs UnMat    as = return (Node "UnMat"    as)
toTreeArgs Pair     as = return (Node "Pair"     as)
toTreeArgs Fst      as = return (Node "Fst"      as)
toTreeArgs Snd      as = return (Node "Snd"      as)
toTreeArgs (MkID     (v,_)) as = return (Node (show v) as)
toTreeArgs (MkVector   n _) as = return (Node ("Vec"       ++ show (natVal n)) as)
toTreeArgs (VectorAt   _ i) as = return (Node ("At "       ++ show (natVal i)) as)
toTreeArgs (Use    o      ) as = return (Node ("Use @("    ++ showSOptic o ++ ")") as)
toTreeArgs (Assign o      ) as = return (Node ("Assign @(" ++ showSOptic o ++ ")") as)
toTreeArgs (Def    k _    ) as = return (Node ("Def @"     ++ symbolVal k ) as)
toTreeArgs (FunDef k _ _  ) as = return (Node ("FunDef @"  ++ symbolVal k ) as)
toTreeArgs (Entry  _ s    ) as = return (Node ("Entry @"   ++ show (stageVal s)) as)
toTreeArgs (Lit        t a) as = return (Node ("Lit @("    ++ show (primTyVal t) ++ ") " ++ show a ) as)
toTreeArgs (Fmap   f      ) as = return (Node ("Fmap @("   ++ primFuncName f ++ ") ") as)
toTreeArgs (Pure   f      ) as = return (Node ("Pure @("   ++ primFuncName f ++ ") ") as)
toTreeArgs (Ap     f _    ) as = return (Node ("Ap @("     ++ primFuncName f ++ ") ") as)

toTree :: AST a -> Tree String
toTree = (`evalState` (ID 1)) . runFreshSuccT . ( `toTreeArgs` [] )

instance Show (AST a) where
  show = showTree . toTree

