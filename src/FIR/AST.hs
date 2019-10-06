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
import Data.Word
  ( Word32 )
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
  ( HList )
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
import FIR.Instances.Optics
  ( User, Assigner, Viewer, Setter, KnownOptic, SOptic, showSOptic )
import FIR.Prim.Image
  ( ImageOperands, OperandName(..)
  , ImageComponent
  )
import qualified FIR.Prim.Image as Image
import FIR.Prim.Op
  ( PrimOp(PrimOpType, opName) )
import FIR.Prim.Singletons
  ( PrimTy, primTy, KnownVars
  , PrimFunc, primFuncName
  , KnownArity
  )
import FIR.Validation.Bindings
  ( ValidDef, ValidFunDef, FunctionTypes
  , AddBinding, AddFunBinding
  , FunctionDefinitionStartState, FunctionDefinitionEndState
  , ValidEntryPoint, SetInterface
  , GetExecutionInfo
  , EntryPointStartState, EntryPointEndState
  , Embeddable
  )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(Grade, (:<!>:)) )
import Math.Linear
  ( V, M )
import qualified SPIRV.Image  as SPIRV
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
  PrimOp :: PrimOp op a => Proxy a -> Proxy op -> AST (PrimOpType op a)

  -- | Create an object of the given type from its ID.
  -- (These are the internal @SPIR-V@ identifiers, in SSA form.)
  MkID :: (ID, SPIRV.PrimTy) -> AST a

  -- Indexed monadic operations (for the AST itself)
  -- | Indexed /return/
  Return :: AST (a -> (a := i) i)
  -- | Indexed /angelic bind/
  Bind :: AST ( (a := j) i -> (a -> q j) -> q i )

  -- | Defining a new constant/variable.
  Def :: forall
          ( k  :: Symbol      )
          ( ps :: Permissions )
          ( a  :: Type        )
          ( i  :: ASTState    )
      . ( GHC.Stack.HasCallStack
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
  FunDef :: forall
              ( name  :: Symbol      )
              ( as    :: BindingsMap )
              ( b     :: Type        )
              ( j_bds :: BindingsMap )
              ( i     :: ASTState    )
         .  ( GHC.Stack.HasCallStack
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

  -- | If-then-else statement.
  If    :: ( GHC.Stack.HasCallStack
           , PrimTy a
           )
        => AST ( Bool -> a -> a -> a )
  -- | Monadic if-then-else.
  IfM   :: GHC.Stack.HasCallStack
        => AST ( Bool -> (a := j) i -> (a := k) i -> (a := i) i )
  -- | While loop.
  While :: GHC.Stack.HasCallStack
        => AST ( ( Bool := i ) i -> (() := j) i -> (() := i) i )

  -- | Encapsulate local state.
  Locally :: AST ( (a := j) i -> (a := i) i )
  -- | Embed a computation into one with larger starte.
  Embed :: Embeddable i j => AST ( (a := i) i -> (a := j) j )

  -- | fmap for functors within the AST.
  Fmap :: forall f a b. ( PrimFunc f, PrimTy a, KnownArity b )
       => AST ( (a -> b) -> f a -> f b )
  -- | pure/return for applicative functors within the AST.
  Pure :: forall f a. ( PrimFunc f, KnownArity a )
       => AST ( a -> f a )
  -- | ap/(<*>) for applicative functors within the AST.
  Ap   :: forall f a b. ( PrimFunc f, PrimTy a, KnownArity b )
       => AST ( f (a -> b) -> f a -> f b )

  -- | Create a vector from its components.
  MkVector :: (KnownNat n, PrimTy a)
           => Proxy n
           -> Proxy a
           -> AST ( NatVariadic n a ( V n a ) )
  -- | Graded mappend to concatenate objects (e.g. two vectors).
  GradedMappend :: ( GradedSemigroup g k, a ~ Grade k g i, b ~ Grade k g j )
    => AST ( a -> b -> Grade k g (i :<!>: j) )

  -- | Newtype wrapping for matrices.
  Mat   :: (KnownNat m, KnownNat n) => AST ( V m (V n a) -> M m n a )
  -- | Newtype unwrapping for matrices.
  UnMat :: (KnownNat m, KnownNat n) => AST ( M m n a -> V m (V n a) )
  -- | Coercions (unsafe).
  Coerce :: forall a b. AST (a -> b)

  -- | Undefined.
  Undefined :: PrimTy a => AST a

  -- Image operands
  -- | End of list of image operands.
  NilOps :: AST (ImageOperands props '[])
  -- | Use projective coordinates.
  --
  -- Must be provided after all other operands,
  -- except possibly 'Dref'.
  Proj :: Image.CanAddProj ops
       => AST ( ImageOperands props ops )
       -> AST ( ImageOperands props (ProjectiveCoords ': ops) )
  -- | Provide a depth-comparison reference value.
  --
  -- Must be provided after all other operands,
  -- except possibly 'Proj'.
  Dref :: Image.CanAddDref ops
       => AST ( ImageComponent props ) -- ^ Reference value used to perform the depth comparison.
       -> AST ( ImageOperands props ops )
       -> AST ( ImageOperands props (DepthComparison ': ops) )
  -- | Add a bias to the implicit level of detail.
  Bias :: ( Image.BasicDim "Bias" props
          , Image.NoMS "Bias" props
          , Image.NoDuplicate (BaseOperand ('SPIRV.LODOperand SPIRV.Bias)) ops
          , Image.NoLODOps "Bias" '[SPIRV.LOD, SPIRV.Grad] ops
          )
       => AST (  ImageComponent props ) -- ^ Bias.
       -> AST ( ImageOperands props ops )
       -> AST ( ImageOperands props (BaseOperand ('SPIRV.LODOperand SPIRV.Bias) ': ops) )
  -- | Provide an explicit level of detail.
  LOD  :: ( Image.BasicDim "LOD" props
          , Image.NoMS "LOD" props
          , Image.NoDuplicate (BaseOperand ('SPIRV.LODOperand SPIRV.LOD)) ops
          , Image.NoLODOps "LOD" '[SPIRV.Bias, SPIRV.Grad, SPIRV.MinLOD] ops
          )
       => AST ( ImageComponent props ) -- ^ LOD.
       -> AST ( ImageOperands props ops )
       -> AST ( ImageOperands props (BaseOperand ('SPIRV.LODOperand SPIRV.LOD ) ': ops) )
  -- | Specify the minimum level of detail to use
  -- when sampling the image.
  MinLOD :: ( Image.BasicDim "MinLOD" props
            , Image.NoMS "MinLOD" props
            , Image.NoDuplicate (BaseOperand ('SPIRV.LODOperand SPIRV.MinLOD)) ops
            , Image.NoLODOps "MinLOD" '[SPIRV.LOD, SPIRV.Bias] ops
            )
         => AST ( ImageComponent props ) -- ^ Minimum LOD.
         -> AST ( ImageOperands props ops )
         -> AST ( ImageOperands props (BaseOperand ('SPIRV.LODOperand SPIRV.MinLOD ) ': ops) )
  -- | Provide explicit derivatives.
  Grad :: ( vec ~ Image.GradCoordinates props ops
          , Image.NoMS "Grad" props
          , Image.NoDuplicate (BaseOperand ('SPIRV.LODOperand SPIRV.Grad)) ops
          , Image.NoLODOps "Grad" '[ SPIRV.Bias, SPIRV.LOD ] ops
          )
       => ( AST vec, AST vec ) -- ^ Gradient: ( df\/dx, df\/dy ).
       -> AST ( ImageOperands props ops )
       -> AST ( ImageOperands props (BaseOperand ('SPIRV.LODOperand SPIRV.Grad) ': ops) )
  -- | Add a constant offset to the coordinates.
  ConstOffsetBy
    :: ( PrimTy vec
       , vec ~ Image.OffsetCoordinates props ops
       , Image.NoDuplicate (BaseOperand SPIRV.ConstOffset) ops
       , Image.NotCubeDim "ConstOffsetBy" props
       )
    => vec -- Offset (a Haskell constant).
    -> AST ( ImageOperands props ops )
    -> AST ( ImageOperands props (BaseOperand SPIRV.ConstOffset ': ops) )
  -- | Add an offset to the coordinates.
  OffsetBy
    :: ( PrimTy vec
       , vec ~ Image.OffsetCoordinates props ops
       , Image.NoDuplicate (BaseOperand SPIRV.Offset) ops
       , Image.NotCubeDim "OffsetBy" props
       )
    => AST vec -- Offset.
    -> AST (ImageOperands props ops)
    -> AST (ImageOperands props (BaseOperand SPIRV.Offset ': ops))
  Gather
    :: ( Image.NotCubeDim "Gather" props
       , Image.NoDuplicate (BaseOperand SPIRV.ConstOffsets) ops
       , Image.NoLODOps "Gather" '[ SPIRV.LOD, SPIRV.Grad, SPIRV.Bias, SPIRV.MinLOD ] ops
       , Image.UsesAffineCoords ops
       )
    => Image.GatherInfo (Image.WhichGather ops)
    -> AST (ImageOperands props ops)
    -> AST (ImageOperands props (BaseOperand SPIRV.ConstOffsets ': ops))
  -- | Specify which sample number to use in a multi-sampled image.
  SampleNo
    :: ( Image.CanMultiSample props
       , Image.NoDuplicate (BaseOperand SPIRV.Sample) ops
       )
    => AST Word32  -- ^ Sample number.
    -> AST (ImageOperands props ops)
    -> AST (ImageOperands props (BaseOperand SPIRV.Sample ': ops))

  -- MkStruct
  -- MkArray

  -- | Internal HList data type.
  --
  -- Only used for providing multiple run-time indices to product optics.
  -- See [FIR issue #13](https://gitlab.com/sheaf/fir/issues/13).
  NilHList  :: AST ( HList '[] )
  ConsHList :: PrimTy a => AST ( a -> HList as -> HList (a ': as) )

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
primOp :: forall a op r
       .  ( PrimOp op a
          , Syntactic r
          , Internal r ~ PrimOpType op a
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
toTreeArgs Coerce    as = return (Node "Coerce"        as)
toTreeArgs Undefined as = return (Node "Undefined"     as)
toTreeArgs NilHList  as = return (Node "NilHListAST"  as)
toTreeArgs ConsHList as = return (Node "ConsHListAST" as)
toTreeArgs NilOps    as = return (Node "NilOps" as)
toTreeArgs (Proj ops) as = do
  a <- toTreeArgs ops []
  pure $ Node "Proj" (a:as)
toTreeArgs (Dref ref ops) as = do
  r <- toTreeArgs ref []
  a <- toTreeArgs ops []
  pure $ Node "Dref" (r:a:as)
toTreeArgs (Bias bias ops) as = do
  b <- toTreeArgs bias []
  a <- toTreeArgs ops  []
  pure $ Node "Bias=" (b:a:as)
toTreeArgs (LOD lod ops) as = do
  l <- toTreeArgs lod []
  a <- toTreeArgs ops []
  pure $ Node "LOD" (l:a:as)
toTreeArgs (MinLOD lod ops) as = do
  l <- toTreeArgs lod []
  a <- toTreeArgs ops []
  pure $ Node "MinLOD" (l:a:as)
toTreeArgs (Grad (dfdx, dfdy) ops) as = do
  x <- toTreeArgs dfdx []
  y <- toTreeArgs dfdy []
  a <- toTreeArgs ops  []
  pure $ Node "Grad" (x:y:a:as)
toTreeArgs (ConstOffsetBy vec ops) as = do
  a <- toTreeArgs ops []
  pure $ Node "ConstOffsetBy" (pure (show vec):a:as)
toTreeArgs (OffsetBy vec ops) as = do
  v <- toTreeArgs vec []
  a <- toTreeArgs ops []
  pure $ Node "OffsetBy" (v:a:as)
toTreeArgs (Gather (Image.ComponentWithOffsets comp offs) ops) as = do
  c <- toTreeArgs comp []
  a <- toTreeArgs ops  []
  pure $ Node "GatherComponentWithOffsets" (c:pure (show offs):a:as)
toTreeArgs (Gather (Image.DepthWithOffsets offs) ops) as = do
  a <- toTreeArgs ops  []
  pure $ Node "GatherDepthWithOffsets" (pure (show offs):a:as)
toTreeArgs (SampleNo no ops) as = do
  n <- toTreeArgs no  []
  a <- toTreeArgs ops []
  pure $ Node "SampleNo" (n:a:as)
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
