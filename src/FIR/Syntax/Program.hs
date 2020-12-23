{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RebindableSyntax        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-|
Module: FIR.Syntax.Program

This module, together with "FIR.Syntax.AST",
provides most of the user-facing syntax for constructing
and manipulating values in the EDSL.

This is done through type class overloading, here in the form of
orphan instances for types of the form @Program (i :: ProgramState) (j :: ProgramState) (a :: Type)@,
representing stateful values in the EDSL.    
See also "FIR.ProgramState".

Recall that 'Program' is a wrapper for the internal representation
using the indexed codensity transformation, see "FIR.Module".

See also the validation modules:

  * "FIR.Validation.Bindings" for stateful operations such as 'get'/'put',
  and definitions (of variables, functions, entrypoints).
  * "FIR.Validation.Images" for image read/write operations such as
  'imageRead'/'imageWrite'.

-}

module FIR.Syntax.Program
  ( -- * Monadic control operations
    while, switchM, locally, embed, purely

    -- * Stateful operations (with indexed monadic state)
    -- ** Defining new objects
    -- *** Constants / variables
  , let', def
    -- *** Functions
  , fundef
    -- *** Entry points
  , entryPoint, shader

    -- ** Optics
    -- *** General functions: use, assign, modifying
  , use, assign, modifying
    -- *** Special cases for manipulating variables
  , get, put, modify
    -- *** Special cases for manipulating images
  , imageRead, imageWrite

  -- * Geometry shader primitive instructions
  , emitVertex, endPrimitive

  -- * Memory synchronisation primitive operations
  , controlBarrier, memoryBarrier

    -- * Instances

    -- ** Syntactic type class
    -- $syntactic

    -- ** Undefined type class
    -- $undef

    -- ** Logical operations
    -- $logical

    -- ** Numeric operations
    -- $numeric

    -- ** Numeric conversions
    -- $conversions

    -- ** Vectors
    -- $vectors

    -- ** Matrices
    -- $matrices

    -- + orphan instances
  )
  where

-- base
import Prelude hiding
  ( Eq(..), (&&), (||), not
  , Ord(..)
  , Num(..), Floating(..), RealFrac(..)
  , Integral(..)
  , Fractional(..), fromRational
  , Floating(..), RealFloat(..)
  , Functor(..), Monad(..)
  , Applicative(..)
  , undefined
  )
import qualified Prelude
import Data.Kind
  ( Type )
import Data.Maybe
  ( fromMaybe )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Word
  ( Word32 )
import qualified GHC.Stack
  ( HasCallStack )
import GHC.TypeLits
  ( Symbol, KnownSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, KnownNat )

-- fir
import Control.Monad.Indexed
  ( (:=)(AtKey), Codensity(Codensity)
  , MonadIxFail(fail)
  , ixFmap, ixPure, ixLiftA2
  )
import qualified Control.Monad.Indexed as Indexed
import Control.Type.Optic
  ( Optic, Name, Gettable, Settable
  , Part, Whole, Indices
  )
import Data.Constraint.All
  ( All )
import Data.Type.Known
  ( Known )
import Data.Type.List
  ( KnownLength(sLength), Postpend )
import Data.Type.Map
  ( (:->)((:->)), Map )
import FIR.AST
  ( AST, Code
  , Syntactic(Internal,toAST,fromAST)
  , SyntacticVal, InternalType
  , primOp
  , HasUndefined(undefined)
  , AugUser, AugAssigner
  , FunctionHandle
  , pattern Lam, pattern (:$), pattern Lit
  , pattern Locally, pattern Embed
  , pattern While, pattern SwitchM
  , pattern Return, pattern Bind
  , pattern Let, pattern Def, pattern FunDef, pattern FunCall, pattern DefEntryPoint
  , pattern Use, pattern Assign
  , pattern NilOps
  )
import FIR.AST.Type
  ( AugType(Val), Eff, FunArgs, Nullary )
import FIR.Binding
  ( BindingsMap, Var, Permissions
  , FunctionAugType
  )
import FIR.Definition
  ( Definition, KnownDefinitions
  , StartState
  , StartBindings, EndBindings
  )
import FIR.Module
  ( Program
  , ShaderModule(ShaderModule)
  )
import FIR.Prim.Image
  ( ImageProperties, ImageOperands
  )
import FIR.Prim.Singletons
  ( PrimTy, ScalarTy, IntegralTy
  , KnownVars
  )
import FIR.Prim.RayTracing
  ( RayQueryState )
import FIR.ProgramState
  ( FunctionInfo
  , FunctionContext(..)
  , ProgramState(ProgramState)
  , TLInterface
  , ExecutionContext
  , EntryPointInfo
  )
import FIR.Syntax.AST
  ( )
import FIR.Syntax.Images
  ( ImageTexel )
import FIR.Syntax.Optics
  ( KnownOptic, opticSing
  , StatefulOptic
  )
import FIR.Validation.Bindings
  ( ValidDef, AddBinding, Has
  , ValidFunDef, AddFunBinding, FunctionTypes
  , FunctionDefinitionStartState, FunctionDefinitionEndState
  , ValidEntryPoint
  , EntryPointStartState, EntryPointEndState
  , SetInterface, GetExecutionInfo
  , Embeddable
  )
import FIR.Validation.Definitions
  ( ValidDefinitions )
import FIR.Validation.Images
  ( LookupImageProperties, ImageTexelType
  , ValidImageRead, ValidImageWrite
  )
import Math.Algebra.Class
  ( AdditiveMonoid(..), CancellativeAdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), Ring
  , DivisionRing(..)
  , Signed(..), Archimedean(..)
  , Floating(..), RealFloat(..)
  , Convert(..), Rounding(..)
  )
import Math.Linear
  ( Semimodule(..), LinearModule(..)
  , Inner(..), Cross(..)
  , Matrix(..), VectorOf
  , V, M
  )
import Math.Logic.Bits
  ( Bits(..), BitShift(..), BitCast(..) )
import Math.Logic.Class
  ( Eq(..), Boolean(..), Ord(..) )
import qualified SPIRV.PrimOp          as SPIRV
  ( GeomPrimOp(..), SyncPrimOp(..) )
import qualified SPIRV.Stage           as SPIRV
import qualified SPIRV.Synchronisation as SPIRV
  ( SynchronisationScope, MemorySemantics
  , synchronisationScope, memorySemanticsBitmask
  )

--------------------------------------------------------------------------
-- * Monadic control operations

locally :: forall i j r. (SyntacticVal r) => Program i j r -> Program i i r
locally p = fromAST ( Locally :$ toAST p )

embed :: forall i j r. (Embeddable i j, SyntacticVal r)
      => Program i i r -> Program j j r
embed p = fromAST ( Embed :$ toAST p )

purely
  :: ( SyntacticVal r
     , i ~ 'ProgramState '[] 'TopLevel '[] '[] '[] '[] bkend
     , Embeddable i k
     )
  => Program i j r -> Program k k r
purely = embed . locally

while :: ( GHC.Stack.HasCallStack )
      => Program i i (Code Bool)
      -> Program i j (Code ())
      -> Program i i (Code ())
while c p = fromAST ( While :$ toAST c :$ toAST p )

switchM :: ( Syntactic scrut, Internal scrut ~ Val vscrut
           , IntegralTy vscrut
           , Syntactic val, Internal val ~ Val vval
           , PrimTy vval
           )
        => Program i i scrut -- ^ Scrutinee.
        -> [ InternalType scrut :-> Program i i val ] -- ^ Cases.
        -> Program i i val   -- ^ Default (fallthrough) case.
        -> Program i i val
switchM scrut cases val = fromAST $ SwitchM (toAST scrut) (toAST val) (map ( \ (x :-> y) -> (x, toAST y) ) cases)

--------------------------------------------------------------------------
-- Syntactic type class

-- $syntactic
-- Instance for the 'Syntactic' type class.
-- Recall that @Program i j a@ is a type synonym for @Codensity AST (a := j) i@.

instance SyntacticVal a => Syntactic (Program i j a) where
  type Internal (Codensity AST (a := j) i) = Eff i j (InternalType a)

  toAST :: Codensity AST (a := j) i -> AST ( Eff i j (InternalType a) )
  toAST (Codensity k) = k ( \(AtKey a) -> Return :$ toAST a )

  fromAST :: AST ( Eff i j (InternalType a) ) -> Codensity AST (a := j) i
  fromAST a = Codensity ( \k -> Bind :$ toAST a :$ toAST (k . AtKey) )

--------------------------------------------------------------------------
-- Undefined

-- $undef
-- Instance for the 'Undefined' type class.

-- Enforcing "i ~ j" allows the user to use 'undefined' in indexed-monadic code
-- without wreaking havoc on type inference by causing the computation to result
-- in an arbitrary state.
-- This does mean that the user cannot use 'undefined' to stand-in for an
-- arbitrary state-changing computation.
instance ( PrimTy a, j ~ i ) => HasUndefined (Program i j (Code a)) where
  undefined = ixPure undefined

instance {-# OVERLAPPABLE #-} ( j ~ i ) => HasUndefined (Program i j a) where
  undefined = Prelude.undefined

--------------------------------------------------------------------------
-- Stateful operations (with indexed monadic state)

-- Defining functions and variables.

-- | let binding.
let' :: forall ( a :: Type ) ( i :: ProgramState )
     . ( GHC.Stack.HasCallStack, PrimTy a )
     => Code a
     -> Program i i (Code a)
let' = fromAST Let

-- | Define a new variable.
--
-- Type-level arguments:
--
-- *@k@: name to use for definition,
-- *@ps@: 'FIR.Binding.Permission's (readable, writable, ...),
-- *@a@: type of definition,
-- *@i@: state at start of definition (usually inferred).
def :: forall
         ( k  :: Symbol       )
         ( ps :: Permissions  )
         ( a  :: Type         )
         ( i  :: ProgramState )
    .  ( GHC.Stack.HasCallStack
       , KnownSymbol k
       , Known Permissions ps
       , PrimTy a
       , ValidDef k i a
       )
    => Code a -- ^ Initial value.
    -> Program i (AddBinding k (Var ps a) i) (Code a)
def = fromAST ( Def (Proxy @k) (Proxy @ps) )

-- | Define a new function.
--
-- Type-level arguments:
--
-- * @name@: function name,
-- * @as@: list of argument types,
-- * @b@: return type,
-- * @j_bds@: bindings state at end of function body (usually inferred),
-- * @i@: monadic state at start of function body (usually inferred),
-- * @r@: function type itself, result of 'fundef' (usually inferred).
fundef :: forall
            ( name  :: Symbol       )
            ( as    :: BindingsMap  )
            ( b     :: Type         )
            ( j_bds :: BindingsMap  )
            ( i     :: ProgramState )
            ( r     :: Type         )
        .  ( GHC.Stack.HasCallStack
           , Syntactic r
           , Internal r ~ FunctionAugType as b, All Nullary (FunArgs (FunctionAugType as b))
           , KnownSymbol name
           , KnownVars as
           , PrimTy b
           , ValidFunDef name as i j_bds
           , '(as, b) ~ FunctionTypes name i
           )
        => Program
              ( FunctionDefinitionStartState name as i )
              ( FunctionDefinitionEndState name as j_bds i )
              ( Code b ) -- ^ Function body code.
        -> Program i (AddFunBinding name as b i) r
fundef f = ixFmap ( \g -> fromAST ( FunCall (Proxy @name) ( Proxy @as ) ( Proxy @b ) :$ g ) ) cod
  where
    fun :: AST (Eff i (AddFunBinding name as b i) (FunctionHandle name as b)) 
    fun = FunDef (Proxy @name) (Proxy @as) (Proxy @b) :$ toAST f
    cod :: Program i (AddFunBinding name as b i) (AST (Val (FunctionHandle name as b)))
    cod = Codensity ( \k -> Bind :$ fun :$ Lam (k . AtKey) )

-- | Define a new entry point.
--
-- The appropriate built-in variables are made available in the entry point body.
--
-- Type-level arguments:
--
-- *@name@: name of entry point,
-- *@stage@: entry point 'SPIRV.Stage.ExecutionModel',
-- *@stageInfo@: entry point 'SPIRV.Stage.ExecutionInfo' (usually inferred),
-- *@j_bds@: bindings state at end of entry point body (usually inferred),
-- *@j_iface@: interface of entry point (usually inferred),
-- *@i@: state at start of entry point body (usually inferred).
entryPoint :: forall
               ( name      :: Symbol )
               ( stage     :: SPIRV.ExecutionModel          )
               ( stageInfo :: SPIRV.ExecutionInfo Nat stage )
               ( j_bds     :: BindingsMap  )
               ( j_iface   :: TLInterface  )
               ( i         :: ProgramState )
           .
             ( GHC.Stack.HasCallStack
             , KnownSymbol name
             , Known SPIRV.ExecutionModel stage
             , Known (SPIRV.ExecutionInfo Nat stage) stageInfo
             , ValidEntryPoint name stageInfo i j_bds
             , stageInfo ~ GetExecutionInfo name stage i
             )
           => Program
                ( EntryPointStartState name stageInfo               i )
                ( EntryPointEndState   name stageInfo j_bds j_iface i )
                ( Code () )
           -> Program
                i
                ( SetInterface name stageInfo j_iface i )
                ( Code () )
entryPoint ep = fromAST ( DefEntryPoint (Proxy @name) (Proxy @stageInfo) :$ toAST ep )

-- | Define a new shader stage.
--
-- This function provides a convenience wrapper around 'entryPoint',
-- suitable for creating a program containing a single shader and
-- no top-level functions.
--
-- Type-level arguments:
--
-- *@name@: name of entry point,
-- *@shader@: which shader stage to use,
-- *@defs@: top-level inputs/outputs of shader stage.
--
-- Other type-level arguments should be inferred.
shader :: forall
            ( name      :: Symbol                        )
            ( shader    :: SPIRV.Shader                  )
            ( defs      :: [Symbol :-> Definition]       )
            ( stage     :: SPIRV.ExecutionModel          )
            ( stageInfo :: SPIRV.ExecutionInfo Nat stage )
            ( j_bds     :: BindingsMap                   )
            ( j_iface   :: TLInterface                   )
            ( funs      :: [Symbol :-> FunctionInfo   ]  )
            ( eps       :: [Symbol :-> EntryPointInfo ]  )
            ( g_iface   :: TLInterface                   )
            ( rayQs     :: Map Symbol RayQueryState      )
            ( bkend     :: SPIRV.Backend                 )
            ( i         :: ProgramState                  )
       . ( GHC.Stack.HasCallStack
         , KnownDefinitions defs
         , ValidDefinitions defs
         , GetExecutionInfo name stage i ~ stageInfo
         , funs ~ '[ ]
         , i ~ 'ProgramState (StartBindings defs) 'TopLevel funs eps g_iface rayQs bkend
         , i ~ StartState defs
         , KnownSymbol name
         , stage ~ 'SPIRV.Stage ('SPIRV.ShaderStage shader)
         , Known SPIRV.Shader shader
         , Known (SPIRV.ExecutionInfo Nat stage) stageInfo
         , EndBindings defs ~ StartBindings defs
         , ValidEntryPoint name stageInfo ('ProgramState (EndBindings defs) 'TopLevel funs eps g_iface rayQs bkend) j_bds
         )
       => Program
            ( EntryPointStartState name stageInfo               i )
            ( EntryPointEndState   name stageInfo j_bds j_iface i )
            ( Code () )
       -> ShaderModule name shader defs (SetInterface name stageInfo j_iface i)
shader
  = ShaderModule
  . entryPoint @name @stage

-- Optics.

-- | /Use/ an optic, returning a monadic value read from the (indexed) state.
--
-- Like @use@ from the lens library, except the optic needs to be passed with a type application.
use :: forall optic.
             ( GHC.Stack.HasCallStack
             , KnownOptic optic, StatefulOptic optic
             , Gettable optic
             , Syntactic (ProgUser optic)
             , Internal (ProgUser optic) ~ AugUser optic
             )
           => ProgUser optic
use = fromAST ( Use sLength ( opticSing @optic ) )

-- | Assign a new value with an optic.
--
-- Like @assign@ from the lens library, except the optic needs to be passed with a type application.
assign :: forall optic.
             ( GHC.Stack.HasCallStack
             , KnownOptic optic, StatefulOptic optic
             , Settable optic
             , Syntactic (ProgAssigner optic)
             , Internal (ProgAssigner optic) ~ AugAssigner optic
             )
           => ProgAssigner optic
assign = fromAST ( Assign sLength ( opticSing @optic ) )

-- *** Get, put, modify.

-- | Get the value of a variable.
-- Like @get@ for state monads, except a binding name needs to be specified with a type application.
--
-- Synonym for @use \@(Name k)@.
get :: forall (k :: Symbol) (a :: Type) (i :: ProgramState).
       ( KnownSymbol k
       , Gettable (Name k :: Optic '[] i a)
       , a ~ Has k i
       , PrimTy a
       )
    => Program i i (Code a)
get = use @(Name k :: Optic '[] i a)

-- | Set the value of a variable.
-- Like @put@ for state monads, except a binding name needs to be specified with a type application.
--
-- Synonym for @assign \@(Name k)@.
put :: forall (k :: Symbol) (a :: Type) (i :: ProgramState).
       ( KnownSymbol k
       , Settable (Name k :: Optic '[] i a)
       , a ~ Has k i
       , PrimTy a
       )
    => Code a -> Program i i (Code ())
put = assign @(Name k :: Optic '[] i a)

-- | Modify the value of a variable.
-- | Like @modify@ for state monads, except a binding name needs to be specified with a type application.
--
-- Synonym for @modifying \@(Name k)@.
modify :: forall (k :: Symbol) (a :: Type) (i :: ProgramState).
          ( KnownSymbol k
          , Gettable (Name k :: Optic '[] i a)
          , Settable (Name k :: Optic '[] i a)
          , a ~ Has k i
          , PrimTy a
          )
       => (Code a -> Code a) -> Program i i (Code ())
modify = modifying @(Name k :: Optic '[] i a)

--------------------------------------------------------------------------
-- Image operations (synonyms for 'use'/'assign').

-- | Read from an image (with or without a sampler).
--
-- Synonym for @use \@(ImageTexel \"imgName\")@,
-- but with no additional image operands provided.
--
-- Type-level arguments:
--
-- * @imgName@: image name,
-- * @props@: image properties (usually inferred),
-- * @i@: monadic state (usually inferred).
imageRead :: forall
              ( imgName  :: Symbol          )
              ( props    :: ImageProperties )
              ( imgCds   :: Type            )
              ( imgTexel :: Type            )
              ( i        :: ProgramState    )
            .
            ( KnownSymbol imgName
            , PrimTy imgCds
            , PrimTy imgTexel
            , imgTexel ~ ( ImageTexelType props '[] )
            , Gettable
                ( ImageTexel imgName
                  :: Optic
                      '[ ImageOperands props '[], imgCds ]
                       i
                       ( ImageTexelType props '[] ) -- can't use "imgTexel", GHC trac #15710
                )
            , props ~ LookupImageProperties imgName i
            , Known ImageProperties props
            , ValidImageRead props '[] imgCds
            )
          => Code imgCds
          -> Program i i (Code (ImageTexelType props '[]))
imageRead = use
          @( ImageTexel imgName
              :: Optic
                   '[ ImageOperands props '[], imgCds ]
                    i
                    imgTexel
           )
          NilOps

-- | Write directly to an image (without a sampler).
--
-- Synonym for @assign \@(ImageTexel \"imgName\")@,
-- but with no additional image operands provided.
--
-- Type-level arguments:
--
-- * @imgName@: image name,
-- * @props@: image properties (usually inferred),
-- * @i@: monadic state (usually inferred).
imageWrite :: forall
                ( imgName  :: Symbol          )
                ( props    :: ImageProperties )
                ( imgCds   :: Type            )
                ( imgTexel :: Type            )
                ( i        :: ProgramState    )
             .
             ( KnownSymbol imgName
             , PrimTy imgCds
             , PrimTy imgTexel
             , imgTexel ~ ( ImageTexelType props '[] )
             , Gettable
                ( ImageTexel imgName
                  :: Optic
                      '[ ImageOperands props '[], imgCds ]
                       i
                       ( ImageTexelType props '[] ) -- can't use "imgTexel", GHC trac #15710
                )
             , props ~ LookupImageProperties imgName i
             , Known ImageProperties props
             , ValidImageWrite props '[] imgCds
             )
           => Code imgCds
           -> Code imgTexel
           -> Program i i ( Code () )
imageWrite = assign
           @( ImageTexel imgName
                :: Optic
                    '[ ImageOperands props '[], imgCds ]
                     i
                     imgTexel
            )
           NilOps

--------------------------------------------------------------------------
-- geometry shader primitive instructions

-- | Geometry shader: write the current output values at the current vertex index.
emitVertex :: forall (i :: ProgramState)
           . ( ExecutionContext i ~ Just SPIRV.Geometry )
           => Program i i ( Code () )
emitVertex = primOp @i @SPIRV.EmitGeometryVertex

-- | Geometry shader: end the current primitive and pass to the next one.
endPrimitive :: forall (i :: ProgramState)
             .  ( ExecutionContext i ~ Just SPIRV.Geometry )
             => Program i i ( Code () )
endPrimitive = primOp @i @SPIRV.EndGeometryPrimitive

--------------------------------------------------------------------------
-- memory synchronisation primitive instructions

-- | Control barrier: wait for other invocations to reach the current point of execution.
--
-- Can also act as a memory barrier if a memory synchronisation scope is specified.
controlBarrier :: forall (i :: ProgramState)
               .  SPIRV.SynchronisationScope
               -> Maybe ( SPIRV.SynchronisationScope, SPIRV.MemorySemantics )
               -> Program i i ( Code () )
controlBarrier controlScope memScope
  = primOp @i @SPIRV.ControlSync
      ( Lit $ controlScopeWord32 :: Code Word32 )
      ( Lit $ fromMaybe controlScopeWord32 $ SPIRV.synchronisationScope   . fst <$> memScope :: Code Word32 )
      --                ^^^^^^^^^^^^^^^^^^
      -- can get validation errors if this is set to 0 (which is "CrossDevice" scope)
      -- even though this field is supposed to be ignored if the memory semantics argument is 0
      ( Lit $ fromMaybe 0                  $ SPIRV.memorySemanticsBitmask . snd <$> memScope :: Code Word32 )
    where
      controlScopeWord32 :: Word32
      controlScopeWord32 = SPIRV.synchronisationScope controlScope 

-- | Memory barrier: ensure ordering on memory accesses.
memoryBarrier :: forall (i :: ProgramState)
               .  SPIRV.SynchronisationScope
               -> SPIRV.MemorySemantics
               -> Program i i ( Code () )
memoryBarrier memScope memSem
  = primOp @i @SPIRV.MemorySync
      ( Lit $ SPIRV.synchronisationScope   memScope :: Code Word32 )
      ( Lit $ SPIRV.memorySemanticsBitmask memSem   :: Code Word32 )

--------------------------------------------------------------------------
-- type synonyms for use/assign

type family ProgListVariadic
              ( is :: [Type] )
              ( s  :: ProgramState )
              ( a  :: Type  )
            = ( r  :: Type  )
            | r -> is s a where
  ProgListVariadic '[]         s a = Program s s (Code a)
  ProgListVariadic ( i ': is ) s a = Code i -> ProgListVariadic is s a


-- recall (defined in FIR.Syntax.Optics):
-- type User     (g :: Optic is s a) = ListVariadicIx is                s a
-- type Assigner (g :: Optic is s a) = ListVariadicIx (is `Postpend` s) s ()

type ProgUser     (optic :: Optic is s a) = ProgListVariadic  is               s a
type ProgAssigner (optic :: Optic is s a) = ProgListVariadic (is `Postpend` a) s ()

--------------------------------------------------------------------------
-- modifying

type family ProgVariadicModifier
              ( is :: [Type] )
              ( s  :: ProgramState )
              ( a  :: Type )
            = ( r  :: Type )
            | r -> is s a
            where
  ProgVariadicModifier '[]       s a = (Code a -> Code a) -> Program s s (Code ())
  ProgVariadicModifier (i ': is) s a = Code i -> ProgVariadicModifier is s a

type ProgModifier (optic :: Optic is s a) = ProgVariadicModifier is s a

-- | Modify a value with an optic.
--
-- Like @modifying@ from the lens library, except the optic needs to be passed with a type application.
modifying
    :: forall optic.
       ( GHC.Stack.HasCallStack
       , KnownOptic optic
       , StatefulOptic optic
       , Settable optic
       , Gettable optic
       , Syntactic (ProgUser optic)
       , Internal (ProgUser optic) ~ AugUser optic
       , Syntactic (ProgAssigner optic)
       , Internal (ProgAssigner optic) ~ AugAssigner optic
       , Modifier (Indices optic) (Whole optic) (Part optic)
       )
    => ProgModifier optic
modifying
  = modifier @(Indices optic) @(Whole optic) @(Part optic)
      ( use    @optic )
      ( assign @optic )

class Modifier is s a where
  modifier :: ProgListVariadic      is               s a
           -> ProgListVariadic     (is `Postpend` a) s ()
           -> ProgVariadicModifier  is               s a

instance Modifier '[] s a where
  modifier used assigned f
    = ixFmap f used Indexed.>>= assigned

instance Modifier is s a => Modifier (i ': is) s a where
  modifier used assigned i
    = modifier @is @s @a (used i) (assigned i)

--------------------------------------------------------------------------
-- Instances for codensity representation

-- Logical operations

-- $logical
-- Instances for:
--
-- 'Boolean',
--
-- 'Eq', 'Ord' (note: not the "Prelude" type classes).
instance Boolean b => Boolean (Program i i b) where
  true  = ixPure   true
  false = ixPure   false
  (&&)  = ixLiftA2 (&&)
  (||)  = ixLiftA2 (||)
  not   = ixFmap   not

instance ( PrimTy a, Eq a, Logic a ~ Bool
         , i ~ j
         , r ~ Code a
         )
  => Eq (Program i j r) where
  type Logic (Program i j r) = Program i i (Code Bool)
  (==) = ixLiftA2 (==)
  (/=) = ixLiftA2 (/=)

instance ( ScalarTy a, Ord a, Logic a ~ Bool
         , i ~ j
         , r ~ Code a
         )
  => Ord (Program i j r) where
  (<=) = ixLiftA2 (<=)
  (>=) = ixLiftA2 (>=)
  (<)  = ixLiftA2 (<)
  (>)  = ixLiftA2 (>)
  min  = ixLiftA2 min
  max  = ixLiftA2 max

-- * Bitwise operations
--
-- $bitwise
-- Instances for:
--
-- 'Bits', 'BitShift' (note: not 'Data.Bits.Bits').

instance ( ScalarTy a
         , r ~ Code a
         , j ~ i
         , Bits a
         ) => Bits (Program i j r) where
  (.&.) = ixLiftA2 (.&.)
  (.|.) = ixLiftA2 (.|.)
  xor   = ixLiftA2 xor
  complement = ixFmap complement
  zeroBits   = ixPure zeroBits

instance ( ScalarTy a, ScalarTy s
         , BitShift '(a,s)
         , x ~ Code a
         , t ~ Code s
         , j ~ i, i' ~ i, j' ~ i
         )
  => BitShift '(Program i j x, Program i' j' t) where
  shiftL = ixLiftA2 shiftL
  shiftR = ixLiftA2 shiftR

instance ( ScalarTy a, ScalarTy s
         , BitShift '(a,s)
         , x ~ Code a
         , i ~ j
         )
  => BitShift '(Program i j x, Code s) where
  shiftL a s = shiftL @'(Program i j x, Program i i (Code s)) a (ixPure s)
  shiftR a s = shiftR @'(Program i j x, Program i i (Code s)) a (ixPure s)

instance (ScalarTy a, ScalarTy b, BitCast a b, i1 ~ j1, i2 ~ j2, i1 ~ i2, x ~ Code a, y ~ Code b)
  => BitCast (Program i1 j1 x) (Program i2 j2 y) where
  bitcast = ixFmap bitcast

-- Numeric operations

-- $numeric
-- Instances for:
--
-- 'AdditiveMonoid', 'AdditiveGroup', 'Signed',
--
-- 'Semiring', 'Ring',
--
-- 'DivisionRing', 'Archimedean' (Archimedean ordered group),
--
-- 'Floating', 'RealFloat' (note: not the "Prelude" type classes).
instance (ScalarTy a, AdditiveMonoid a, j ~ i) => AdditiveMonoid (Program i j (Code a)) where
  (+)    = ixLiftA2 (+)
  zero   = ixPure zero
  fromInteger = ixPure . fromInteger
instance (ScalarTy a, Semiring a, j ~ i) => Semiring (Program i j (Code a)) where
  (*)    = ixLiftA2 (*)
instance (ScalarTy a, CancellativeAdditiveMonoid a, j ~ i) => CancellativeAdditiveMonoid (Program i j (Code a)) where
  (-)    = ixLiftA2 (-)
instance (ScalarTy a, AdditiveGroup a, j ~ i) => AdditiveGroup (Program i j (Code a)) where
  negate = ixFmap negate
instance (ScalarTy a, Signed a, j ~ i) => Signed (Program i j (Code a)) where
  abs    = ixFmap abs
  signum = ixFmap signum
instance (ScalarTy a, DivisionRing a, j ~ i) => DivisionRing (Program i j (Code a)) where
  (/)    = ixLiftA2 (/)
  fromRational = ixPure . fromRational
instance ( ScalarTy a
         , Archimedean a
         , Archimedean (Code a)
         , Logic a ~ Bool
         , j ~ i
         ) => Archimedean (Program i j (Code a)) where
  mod    = ixLiftA2 mod
  rem    = ixLiftA2 rem
  div    = ixLiftA2 div

instance (ScalarTy a, Floating a, j ~ i) => Floating (Program i j (Code a)) where
  pi      = ixPure pi
  exp     = ixFmap exp
  log     = ixFmap log
  sqrt    = ixFmap sqrt
  invSqrt = ixFmap invSqrt
  sin     = ixFmap sin
  cos     = ixFmap cos
  tan     = ixFmap tan
  asin    = ixFmap asin
  acos    = ixFmap acos
  atan    = ixFmap atan
  sinh    = ixFmap sinh
  cosh    = ixFmap cosh
  tanh    = ixFmap tanh
  asinh   = ixFmap asinh
  acosh   = ixFmap acosh
  atanh   = ixFmap atanh
  (**)    = ixLiftA2 (**)

instance (ScalarTy a, RealFloat a, j ~ i) => RealFloat (Program i j (Code a)) where
  atan2 = ixLiftA2 atan2

-- Numeric conversions

-- $conversions
-- Instance for 'Convert', 'Rounding'.
instance ( ScalarTy a, ScalarTy b
         , Convert '(Code a, Code b)
         , j ~ i, k ~ i, l ~ i
         )
         => Convert '( Program i j (Code a)
                     , Program k l (Code b)
                     ) where
  convert = ixFmap ( convert @'(Code a, Code b) )

instance ( ScalarTy a, ScalarTy b, Rounding '(Code a, Code b)
         , j ~ i, k ~ i, l ~ i
         )
         => Rounding '( Program i j (Code a)
                      , Program k l (Code b)
                      ) where
  truncate = ixFmap truncate
  round    = ixFmap round
  floor    = ixFmap floor
  ceiling  = ixFmap ceiling

-- Vectors

-- $vectors
-- Instances for:
--
-- 'Semimodule', 'LinearModule', 'Inner', 'Cross'.
instance (ScalarTy a, Semiring a, j ~ i) => Semimodule Nat (Program i j (Code (V 0 a))) where
  type Scalar   (Program i j (Code (V 0 a)))       = Program i j (Code a)
  type OfDim    (Program i j (Code (V 0 a))) Nat n = Program i j (Code (V n a))
  type ValidDim (Program i j (Code (V 0 a))) Nat n = KnownNat n

  (^+^) = ixLiftA2 (^+^)
  (^*)  = ixLiftA2 (^*)

instance (ScalarTy a, Ring a, j ~ i) => LinearModule Nat (Program i j (Code (V 0 a))) where
  (^-^) = ixLiftA2 (^-^)

instance (ScalarTy a, Floating a, j ~ i) => Inner Nat (Program i j (Code (V 0 a))) where
  (^.^) = ixLiftA2 (^.^)
  normalise = ixFmap normalise

instance (ScalarTy a, Floating a, j ~ i) => Cross Nat (Program i j (Code (V 0 a))) where
  type CrossDim (Program i j (Code (V 0 a))) Nat n = ( n ~ 3 )
  cross = ixLiftA2 cross

-- Matrices

-- $matrices
-- Instance for 'Matrix'.

type instance VectorOf (Program i j (Code (M 0 0 a))) = Program i j (Code (V 0 a))

instance (ScalarTy a, Floating a, j ~ i) => Matrix Nat (Program i j (Code (M 0 0 a))) where
  type OfDims (Program i j (Code (M 0 0 a))) Nat '(m,n) = Program i j (Code (M m n a))

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


instance TypeError (     Text "Failable pattern detected in 'do' block, but only unfailable patterns are supported."
                    :$$: Text ""
                    :$$: Text "As inference of pattern failability is sometimes patchy,"
                    :$$: Text "consider using an irrefutable pattern instead, such as:"
                    :$$: Text ""
                    :$$: Text "    ~(Vec4 x y z w) <- get @\"position\""
                    :$$: Text ""
                    :$$: Text "instead of"
                    :$$: Text ""
                    :$$: Text "    Vec4 x y z w <- get @\"position\""
                    :$$: Text ""
                    ) => MonadIxFail (Codensity AST) where
  fail = error "'fail': irrefutable pattern failed to match."
