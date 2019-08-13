{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
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
    -- * Primitive operations
  , primOp
  , HasUndefined(undefined)
    -- * Displaying ASTs graphically
  , toTree
  )
  where

-- base
import Prelude
  hiding ( undefined )
import qualified Prelude
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import qualified GHC.Stack
import GHC.TypeLits
  ( Symbol, KnownSymbol, symbolVal )
import GHC.TypeNats
  ( Nat, KnownNat, natVal )

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
import Data.Product
  ( HList, IsProduct )
import Data.Type.Known
  ( Known, knownValue )
import Data.Type.List
  ( SLength )
import FIR.ASTState
  ( TLInterface
  , ASTState(..)
  )
import FIR.Binding
  ( BindingsMap
  , FunctionType, Var, Permissions
  )
import FIR.Instances.Bindings
  ( ValidDef, ValidFunDef, FunctionTypes
  , AddBinding, AddFunBinding
  , FunctionDefinitionStartState, FunctionDefinitionEndState
  , ValidEntryPoint, SetInterface
  , GetExecutionInfo
  , EntryPointStartState, EntryPointEndState
  , Embeddable
  )
import FIR.Instances.Optics
  ( User, Assigner, Viewer, Setter, KnownOptic, SOptic, showSOptic )
import FIR.Prim.Image
  ( ImageOperands )
import FIR.Prim.Op
  ( PrimOp(PrimOpType, PrimOpConstraint, opName) )
import FIR.Prim.Singletons
  ( PrimTy, primTy, KnownVars
  , PrimFunc, primFuncName
  , KnownArity
  )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(Grade, (:<!>:)) )
import Math.Linear
  ( V, M )
import qualified SPIRV.PrimOp as SPIRV
import qualified SPIRV.PrimTy as SPIRV
import qualified SPIRV.Stage  as SPIRV
  ( ExecutionModel, ExecutionInfo )

------------------------------------------------------------
-- main AST data type

infixl 9 :$

-- | AST representation of the EDSL.
data AST :: Type -> Type where

  -- | Lambda abstraction
  Lam :: (AST a -> AST b) -> AST (a -> b)

  -- | Function application
  (:$) :: AST (a -> b) -> AST a -> AST b

  -- | Haskell-level constants can be embedded into the AST.
  Lit :: ( PrimTy a, KnownArity a ) => a -> AST a
  -- | @SPIR-V@ primitive operations
  PrimOp :: ( PrimOp op a, PrimOpConstraint op a )
         => Proxy a -> Proxy op -> AST (PrimOpType op a)

  -- Indexed monadic operations (for the AST itself)
  -- | Indexed /return/
  Return :: AST (a -> (a := i) i)
  -- | Indexed /angelic bind/
  Bind :: AST ( (a := j) i -> (a -> q j) -> q i )

  -- | Defining a new constant/variable.
  Def :: forall k ps a i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , Known Permissions ps
        , PrimTy a
        , ValidDef k i
        )
      => Proxy k  -- ^ Variable name.
      -> Proxy ps -- ^ Permissions (read,write,...).
      -> AST ( a -> (a := AddBinding k (Var ps a) i) i )

  -- | Defining a new function.
  --
  -- Meaning of type variables:
  -- * name: function name,
  -- * as: named function arguments (usually inferred),
  -- * b: function return type (usually inferred),
  -- * j_bds: bindings state at end of function definition (usually inferred),
  -- * i: monadic state at function definition site (usually inferred).
  FunDef :: forall name as b j_bds i.
            ( GHC.Stack.HasCallStack
            , KnownSymbol name
            , KnownVars as
            , PrimTy b
            , ValidFunDef name as i j_bds
            , '(as,b) ~ FunctionTypes name i
            )
         => Proxy name -- ^ Funtion name.
         -> Proxy as   -- ^ Function argument types.
         -> Proxy b    -- ^ Function return type.
         -> AST ( ( b := FunctionDefinitionEndState name as j_bds i )
                    ( FunctionDefinitionStartState name as i )
                -> ( FunctionType as b := AddFunBinding name as b i ) i
                )

  -- | Defining a new entry point.
  --
  -- An entry point is like a function definition with no arguments and Unit return type.
  -- Code within an entry point is given access to additional builtins.
  --
  -- Meaning of type variables:
  -- * name: entry point name,
  -- * stage: entry point stage,
  -- * stageInfo: entry point stage info (usually inferred),
  -- * j_bds: bindings state at end of entry point definition (usually inferred),
  -- * j_iface: entry point interface (usually inferred),
  -- * i: monadic state at entry point definition site (usually inferred),
  Entry :: forall
             ( name      :: Symbol )
             ( stage     :: SPIRV.ExecutionModel          )
             ( stageInfo :: SPIRV.ExecutionInfo Nat stage )
             ( j_bds     :: BindingsMap )
             ( j_iface   :: TLInterface )
             ( i         :: ASTState    )
           .
           ( GHC.Stack.HasCallStack
           , KnownSymbol name
           , Known SPIRV.ExecutionModel stage
           , Known (SPIRV.ExecutionInfo Nat stage) stageInfo
           , ValidEntryPoint name stageInfo i j_bds
           , stageInfo ~ GetExecutionInfo name stage i
           )
         => Proxy name      -- ^ Entry point name.
         -> Proxy stageInfo -- ^ Entry point stage info.
         -> AST (  ( () := EntryPointEndState name stageInfo j_bds j_iface i )
                   ( EntryPointStartState name stageInfo i )
                -> ( () := SetInterface name stageInfo j_iface i ) i
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

  -- | Encapsulate local state.
  Locally :: AST ( (a := j) i -> (a := i) i )
  -- | Embed a computation into one with larger starte.
  Embed :: Embeddable i j => AST ( (a := i) i -> (a := j) j )

  -- functor/applicative operations
  Fmap :: forall f a b. ( PrimFunc f, PrimTy a, KnownArity b )
       => AST ( (a -> b) -> f a -> f b )
  Pure :: forall f a. ( PrimFunc f, KnownArity a )
       => AST ( a -> f a )
  Ap   :: forall f a b. ( PrimFunc f, PrimTy a, KnownArity b )
       => AST ( f (a -> b) -> f a -> f b )

  MkVector :: (KnownNat n, PrimTy a)
           => Proxy n
           -> Proxy a
           -> AST ( NatVariadic n a ( V n a ) )
  GradedMappend :: ( GradedSemigroup g k, a ~ Grade k g i, b ~ Grade k g j )
    => AST ( a -> b -> Grade k g (i :<!>: j) )

  -- Newtype wrapping/unwrapping.
  Mat   :: (KnownNat m, KnownNat n) => AST ( V m (V n a) -> M m n a )
  UnMat :: (KnownNat m, KnownNat n) => AST ( M m n a -> V m (V n a) )
  Ops   :: ImageOperands props ops -> AST ( ImageOperands props ops )
  Coerce :: forall a b. AST (a -> b)

  -- MkStruct
  -- MkArray

  -- | Internal HList data type.
  --
  -- Only used for providing multiple run-time indices to product optics.
  -- See [FIR issue #13](https://gitlab.com/sheaf/fir/issues/13).
  NilHList  :: AST ( HList '[] )
  ConsHList :: AST ( a -> HList as -> HList (a ': as) )
  HeadHList :: AST ( HList (a ': as) -> a )
  TailHList :: AST ( HList (a ': as) -> HList as )
  ProductToHList :: IsProduct p as => AST (p -> HList as)

  -- | Undefined.
  Undefined :: PrimTy a => AST a

  -- | As @SPIR-V@ is based around identifiers,
  -- this function can be used to create values of any type using their IDs.
  MkID :: (ID, SPIRV.PrimTy) -> AST a

------------------------------------------------

-- | Syntactic type class (Axelsson, Svenningsson).
class Syntactic a where
  type Internal a
  toAST :: a -> AST (Internal a)
  fromAST :: AST (Internal a) -> a

instance Syntactic (AST a) where
  type Internal (AST a) = a
  toAST   = id
  fromAST = id

instance (Syntactic a, Syntactic b) => Syntactic (a -> b) where
  type Internal (a -> b) = Internal a -> Internal b
  toAST        f    = Lam ( toAST . f . fromAST )
  fromAST (Lam f) a = fromAST ( f  $ toAST a )
  fromAST      f  a = fromAST ( f :$ toAST a )

------------------------------------------------

-- | Utility function for defining primops.
primOp :: forall a op r. ( PrimOp op a, PrimOpConstraint op a
                         , Syntactic r, Internal r ~ PrimOpType op a
                         )
       => r 
primOp = fromAST $ PrimOp @op @a Proxy Proxy

-- | Types at which we can define "undefined."
class HasUndefined a where
  undefined :: a

instance PrimTy a => HasUndefined (AST a) where
  undefined = Undefined

instance {-# OVERLAPPABLE #-} HasUndefined a where
  undefined = Prelude.undefined

------------------------------------------------
-- display AST for viewing

toTreeArgs :: forall m ast. MonadFresh ID m => AST ast -> [Tree String] -> m (Tree String)
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
toTreeArgs (PrimOp (_ :: Proxy a) (_ :: Proxy op) ) as 
  = return (Node ("PrimOp " ++ show ( SPIRV.op ( opName @_ @_ @op @a ) ) ) as)
toTreeArgs If        as = return (Node "If"            as)
toTreeArgs IfM       as = return (Node "IfM"           as)
toTreeArgs While     as = return (Node "While"         as)
toTreeArgs Locally   as = return (Node "Locally"       as)
toTreeArgs Embed     as = return (Node "Embed"         as)
toTreeArgs Return    as = return (Node "Return"        as)
toTreeArgs Bind      as = return (Node "Bind"          as)
toTreeArgs Mat       as = return (Node "Mat"           as)
toTreeArgs UnMat     as = return (Node "UnMat"         as)
toTreeArgs (Ops _ )  as = return (Node "ImageOperands" as)
toTreeArgs Coerce    as = return (Node "Coerce"        as)
toTreeArgs Undefined as = return (Node "Undefined"     as)
toTreeArgs NilHList  as = return (Node "NilHListAST"  as)
toTreeArgs ConsHList as = return (Node "ConsHListAST" as)
toTreeArgs HeadHList as = return (Node "HeadAST"      as)
toTreeArgs TailHList as = return (Node "TailAST"      as)
toTreeArgs ProductToHList   as = return (Node "ProductToHList"   as)
toTreeArgs (MkID     (v,_)) as = return (Node (show v) as)
toTreeArgs GradedMappend    as = return (Node "GradedMappend" as)
toTreeArgs (MkVector   n _) as = return (Node ("Vec"       ++ show (natVal n)) as)
toTreeArgs (Use    _ o    ) as = return (Node ("Use @("    ++ showSOptic o ++ ")") as)
toTreeArgs (Assign _ o    ) as = return (Node ("Assign @(" ++ showSOptic o ++ ")") as)
toTreeArgs (View   _ o    ) as = return (Node ("View @("   ++ showSOptic o ++ ")") as)
toTreeArgs (Set    _ o    ) as = return (Node ("Set @("    ++ showSOptic o ++ ")") as)
toTreeArgs (Def    k _    ) as = return (Node ("Def @"     ++ symbolVal k ) as)
toTreeArgs (FunDef k _ _  ) as = return (Node ("FunDef @"  ++ symbolVal k ) as)
toTreeArgs (Entry  _ (_ :: Proxy (stageInfo :: SPIRV.ExecutionInfo Nat stage) )) as
  = return (Node ("Entry @(" ++ show (knownValue @stage) ++ ")") as)
toTreeArgs (Lit (a :: ty)) as
  = return (Node ("Lit @(" ++ show (primTy @ty) ++ ") " ++ show a ) as)
toTreeArgs fm@Fmap as
  = case fm of
      ( _ :: AST ( (x -> y) -> f x -> f y ) )
        -> return (Node ("Fmap @(" ++ primFuncName @f ++ ") ") as)
toTreeArgs pur@Pure as
  = case pur of
      ( _ :: AST ( x -> f x ) )
        -> return (Node ("Pure @(" ++ primFuncName @f ++ ") ") as)
toTreeArgs app@Ap as
  = case app of
      ( _ :: AST ( f (x -> y) -> f x -> f y ) )
        -> return (Node ("Ap @("   ++ primFuncName @f ++ ") ") as)

toTree :: AST ast -> Tree String
toTree = (`evalState` (ID 1)) . runFreshSuccT . ( `toTreeArgs` [] )

instance Show (AST ast) where
  show = showTree . toTree
