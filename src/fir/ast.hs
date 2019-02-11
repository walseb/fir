{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module: FIR.AST

Representation of programs using abstract syntax trees.

The user interface to the AST is through type class overloading,
see "FIR.Instances.AST" and "FIR.Instances.Codensity".

This AST uses a higher-order abstract syntax representation (HOAS),
as seen in the paper /Combining Deep and Shallow Embeddings of Domain-Specific Languages/
by Josef Svenningsson and Emil Axelsson.
-}

module FIR.AST
  ( -- * Main AST data type
    AST(..)
    -- * Syntactic type class
  , Syntactic(Internal, toAST, fromAST)
    -- * Displaying ASTs graphically
  , toTree
  )
  where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy )
import qualified GHC.Stack
import GHC.TypeLits
  ( KnownSymbol, symbolVal )
import GHC.TypeNats
  ( KnownNat, natVal )

-- containers
import Data.Tree
 ( Tree(Node) )

-- mtl
import Control.Monad.State.Lazy
  ( evalState )

-- tree-view
import Data.Tree.View
  ( showTree )

-- fir
import CodeGen.Instruction
  ( ID(ID) )
import CodeGen.Monad
  ( MonadFresh(fresh), runFreshSuccT )
import Control.Type.Optic
  ( Gettable, Settable, Indices )
import Control.Monad.Indexed
  ( (:=) )
import Data.Function.Variadic
  ( NatVariadic )
import Data.Type.Known
  ( Known, knownValue )
import Data.Type.List
  ( SLength )
import Data.Type.Map
  ( Insert, Union )
import FIR.Binding
  ( Binding(EntryPoint), FunctionType, Var, Fun, Permission )
import FIR.Builtin
  ( StageBuiltins )
import FIR.Instances.Bindings
  ( ValidDef, ValidFunDef, ValidEntryPoint )
import FIR.Instances.Optics
  ( User, Assigner, Viewer, Setter, KnownOptic, SOptic, showSOptic )
import FIR.Prim.Image
  ( ImageOperands )
import FIR.Prim.Singletons
  ( PrimTy, primTyVal, SPrimFunc, primFuncName, KnownVars )
import Math.Linear
  ( V, M )
import qualified SPIRV.PrimOp as SPIRV
import qualified SPIRV.PrimTy as SPIRV
import qualified SPIRV.Stage  as SPIRV

------------------------------------------------------------
-- main AST data type

-- | AST representation of the EDSL.
data AST :: Type -> Type where

  -- | Lambda abstraction
  Lam :: {-PrimTy a =>-} (AST a -> AST b) -> AST (a -> b)

  -- | Function application
  (:$) :: AST (a -> b) -> AST a -> AST b

  -- | Haskell-level constants can be embedded into the AST.
  Lit :: PrimTy a => a -> AST a
  -- | @SPIR-V@ primitive operations
  PrimOp :: SPIRV.PrimOp -> a -> AST a

  -- = Indexed monadic operations (for the AST itself)
  -- | Indexed /return/
  Return :: AST (a -> (a := i) i)
  -- | Indexed /angelic bind/
  Bind :: AST ( (a := j) i -> (a -> q j) -> q i )

  -- | Defining a new constant/variable.
  Def :: forall k ps a i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , Known [Permission] ps
        , PrimTy a
        , ValidDef k i
        )
      => Proxy k  -- ^ Variable name.
      -> Proxy ps -- ^ Permissions (read,write,...).
      -> AST (    (a := i) i
               -> (a := Insert k (Var ps a) i) i
             )
  -- | Defining a new function.
  FunDef :: forall k as b l i.
            ( GHC.Stack.HasCallStack
            , KnownSymbol k
            , KnownVars as
            , PrimTy b
            , ValidFunDef k as i l
            )
         => Proxy k  -- ^ Funtion name.
         -> Proxy as -- ^ Function argument types.
         -> Proxy b  -- ^ Function return type.
         -> AST (    (b := l) (Union i as)
                  -> (FunctionType as b := Insert k (Fun as b) i) i
                )
  -- | Defining a new entry point.
  --
  -- An entry point is like a function definition with no arguments and Unit return type.
  -- Code within an entry point is given access to additional builtins.
  Entry :: forall k s l i.
           ( GHC.Stack.HasCallStack
           , KnownSymbol k
           , Known SPIRV.Stage s
           , ValidEntryPoint k s i l
           )
         => Proxy k -- ^ Entry point name.
         -> Proxy s -- ^ Entry point stage.
         -> AST (    (() := l) (Union i (StageBuiltins s))
                  -> (() := Insert k (EntryPoint s) i) i
                )
  -- | /Use/ an optic, returning a monadic value read from the (indexed) state.
  --
  -- Like @use@ from the lens library.
  Use :: forall optic.
         ( GHC.Stack.HasCallStack, KnownOptic optic, Gettable optic )
       => SLength (Indices optic) -- ^ Singleton for the number of run-time indices.
       -> SOptic optic            -- ^ Singleton for the optic.
       -> AST ( User optic )
  -- | /Assign/ a new value with an optic.
  --
  -- Like @assign@ from the lens library.
  Assign :: forall optic.
           ( GHC.Stack.HasCallStack, KnownOptic optic, Settable optic )
         => SLength (Indices optic) -- ^ Singleton for the number of run-time indices.
         -> SOptic optic            -- ^ Singleton for the optic.
         -> AST ( Assigner optic )
  -- | /View/: access a value using an optic.
  --
  -- Like @view@ from the lens library.
  View :: forall optic.
          ( GHC.Stack.HasCallStack, KnownOptic optic, Gettable optic )
       => SLength (Indices optic) -- ^ Singleton for the number of run-time indices.
       -> SOptic optic            -- ^ Singleton for the optic.
       -> AST ( Viewer optic )
  -- | /Set/: set a value using an optic.
  --
  -- Like @set@ from the lens library.
  Set :: forall optic.
         ( GHC.Stack.HasCallStack, KnownOptic optic, Settable optic )
      => SLength (Indices optic) -- ^ Singleton for the number of run-time indices.
      -> SOptic optic            -- ^ Singleton for the optic.
      -> AST ( Setter optic )

  If    :: ( GHC.Stack.HasCallStack
           , PrimTy a
           )
        => AST ( Bool -> a -> a -> a )
  IfM   :: GHC.Stack.HasCallStack
        => AST ( Bool -> (a := j) i -> (a := k) i -> (a := i) i )
  While :: GHC.Stack.HasCallStack
        => AST ( ( Bool := i ) i -> (() := j) i -> (() := i) i )

  -- Encapsulate local state.
  Locally :: AST ( (a := j) i -> (a := i) i )


  -- (passing a singleton representing the functor)
  Fmap :: PrimTy a
       => SPrimFunc f -> AST ( (a -> b) -> f a -> f b )
  Pure :: SPrimFunc f -> AST ( a -> f a )
  Ap   :: PrimTy a
       => SPrimFunc f
       -> Proxy a
       -> AST ( f (a -> b) -> f a -> f b )

  MkVector :: (KnownNat n, PrimTy a)
           => Proxy n
           -> Proxy a
           -> AST ( NatVariadic n a ( V n a ) )

  -- Newtype wrapping/unwrapping.
  Mat   :: (KnownNat m, KnownNat n) => AST ( V m (V n a) -> M m n a )
  UnMat :: (KnownNat m, KnownNat n) => AST ( M m n a -> V m (V n a) )
  Ops   :: ImageOperands props ops -> AST ( ImageOperands props ops )

  -- | Internal pair data type.
  --
  -- Only used for providing multiple run-time indices to product optics.
  -- See [FIR issue #13](https://gitlab.com/sheaf/fir/issues/13).
  Pair :: AST ( a -> b -> (a,b) )
  Fst  :: AST ( (a,b) -> a )
  Snd  :: AST ( (a,b) -> b )

  -- | As @SPIR-V@ is based around identifiers,
  -- this function can be used to create values of any type using their IDs.
  MkID :: (ID, SPIRV.PrimTy) -> AST a

------------------------------------------------

-- | Syntactic type class (Axelsson, Svenningsson).
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
toTreeArgs (Ops _ ) as = return (Node "ImageOperands" as)
toTreeArgs Pair     as = return (Node "Pair"     as)
toTreeArgs Fst      as = return (Node "Fst"      as)
toTreeArgs Snd      as = return (Node "Snd"      as)
toTreeArgs (MkID     (v,_)) as = return (Node (show v) as)
toTreeArgs (MkVector   n _) as = return (Node ("Vec"       ++ show (natVal n)) as)
toTreeArgs (Use    _ o    ) as = return (Node ("Use @("    ++ showSOptic o ++ ")") as)
toTreeArgs (Assign _ o    ) as = return (Node ("Assign @(" ++ showSOptic o ++ ")") as)
toTreeArgs (View   _ o    ) as = return (Node ("View @("   ++ showSOptic o ++ ")") as)
toTreeArgs (Set    _ o    ) as = return (Node ("Set @("    ++ showSOptic o ++ ")") as)
toTreeArgs (Def    k _    ) as = return (Node ("Def @"     ++ symbolVal k ) as)
toTreeArgs (FunDef k _ _  ) as = return (Node ("FunDef @"  ++ symbolVal k ) as)
toTreeArgs (Fmap   f      ) as = return (Node ("Fmap @("   ++ primFuncName f ++ ") ") as)
toTreeArgs (Pure   f      ) as = return (Node ("Pure @("   ++ primFuncName f ++ ") ") as)
toTreeArgs (Ap     f _    ) as = return (Node ("Ap @("     ++ primFuncName f ++ ") ") as)
toTreeArgs (Entry  _ (_ :: Proxy stage) ) as
  = return (Node ("Entry @" ++ show (knownValue @stage)) as)
toTreeArgs (Lit (a :: ty)) as
  = return (Node ("Lit @(" ++ show (primTyVal @ty) ++ ") " ++ show a ) as)

toTree :: AST a -> Tree String
toTree = (`evalState` (ID 1)) . runFreshSuccT . ( `toTreeArgs` [] )

instance Show (AST a) where
  show = showTree . toTree
