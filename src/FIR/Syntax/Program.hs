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
orphan instances for types of the form @Program i j a@,
representing stateful values in the EDSL.

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
    while, locally, embed, purely

    -- * Stateful operations (with indexed monadic state)
    -- ** Defining new objects
    -- *** Constants / variables
  , def
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
import qualified GHC.Stack
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
import Data.Type.Known
  ( Known )
import Data.Type.List
  ( KnownLength(sLength), Postpend )
import Data.Type.Map
  ( (:->) )
import FIR.AST
  ( AST(..)
  , Syntactic(Internal,toAST,fromAST)
  , primOp
  , HasUndefined(undefined)
  )
import FIR.Binding
  ( BindingsMap
  , FunctionType, Var
  , Permissions
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
  , ImageData, ImageCoordinates
  )
import FIR.Prim.Singletons
  ( PrimTy, ScalarTy, KnownVars )
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
  ( User, Assigner, KnownOptic, opticSing
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
  ( LookupImageProperties
  , ValidImageRead, ValidImageWrite
  )
import Math.Algebra.Class
  ( AdditiveMonoid(..), AdditiveGroup(..)
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
  ( Bits(..), BitShift(..) )
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

locally :: forall i j r. Syntactic r => Program i j r -> Program i i r
locally = fromAST Locally

embed :: forall i j r. (Embeddable i j, Syntactic r)
      => Program i i r -> Program j j r
embed = fromAST Embed

purely
  :: ( Syntactic r
     , i ~ 'ProgramState '[] 'TopLevel '[] '[] bkend
     , Embeddable i k
     )
  => Program i j r -> Program k k r
purely = embed . locally

while :: ( GHC.Stack.HasCallStack )
      => Program i i (AST Bool)
      -> Program i j (AST ())
      -> Program i i (AST ())
while = fromAST While

--------------------------------------------------------------------------
-- Syntactic type class

-- $syntactic
-- Instance for the 'Syntactic' type class.

instance Syntactic a => Syntactic (Codensity AST (a := j) i) where
  type Internal (Codensity AST (a := j) i) = (Internal a := j) i

  toAST :: Codensity AST (a := j) i -> AST ( (Internal a := j) i )
  toAST (Codensity k) = k ( \(AtKey a) -> Return :$ toAST a )

  fromAST :: AST ( (Internal a := j) i) -> Codensity AST (a := j) i
  fromAST a = Codensity ( \k -> fromAST Bind a (k . AtKey) )

--------------------------------------------------------------------------
-- Undefined

-- $undef
-- Instance for the 'Undefined' type class.

-- Enforcing "i ~ j" allows the user to use 'undefined' in indexed-monadic code
-- without wreaking havoc on type inference by causing the computation to result
-- in an arbitrary state.
-- This does mean that the user cannot use 'undefined' to stand-in for an
-- arbitrary state-changing computation.
instance ( PrimTy a, j ~ i ) => HasUndefined (Program i j (AST a)) where
  undefined = ixPure undefined

instance {-# OVERLAPPABLE #-} ( j ~ i ) => HasUndefined (Program i j a) where
  undefined = Prelude.undefined

--------------------------------------------------------------------------
-- Stateful operations (with indexed monadic state)

-- Defining functions and variables.

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
       , ValidDef k i
       )
    => AST a -- ^ Initial value.
    -> Program i (AddBinding k (Var ps a) i) (AST a)
def = fromAST ( Def @k @ps @a @i Proxy Proxy )

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
       .   ( GHC.Stack.HasCallStack
           , Syntactic r
           , Internal r ~ FunctionType as b
           , KnownSymbol name
           , KnownVars as
           , PrimTy b
           , ValidFunDef name as i j_bds
           , '(as, b) ~ FunctionTypes name i
           )
        => Program
              ( FunctionDefinitionStartState name as i )
              ( FunctionDefinitionEndState name as j_bds i )
              ( AST b ) -- ^ Function body code.
        -> Program i (AddFunBinding name as b i) r
fundef = fromAST ( FunDef @name @as @b @j_bds @i Proxy Proxy Proxy ) . toAST


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
                ( EntryPointStartState name stageInfo i )
                ( EntryPointEndState name stageInfo j_bds j_iface i )
                ( AST () )
           -> Program
                i
                ( SetInterface name stageInfo j_iface i )
                ( AST () )
entryPoint = fromAST
                ( Entry @name @stage @stageInfo @j_bds @j_iface @i Proxy Proxy )
           . toAST

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
            ( bkend     :: SPIRV.Backend                 )
            ( i         :: ProgramState                  )
       . ( GHC.Stack.HasCallStack
         , KnownDefinitions defs
         , ValidDefinitions defs
         , GetExecutionInfo name stage i ~ stageInfo
         , funs ~ '[ ]
         , i ~ 'ProgramState (StartBindings defs) 'TopLevel funs eps bkend
         , i ~ StartState defs
         , KnownSymbol name
         , stage ~ 'SPIRV.Stage ('SPIRV.ShaderStage shader)
         , Known SPIRV.Shader shader
         , Known (SPIRV.ExecutionInfo Nat stage) stageInfo
         , EndBindings defs ~ StartBindings defs
         , ValidEntryPoint name stageInfo ('ProgramState (EndBindings defs) 'TopLevel funs eps bkend) j_bds
         )
       => Program
            ( EntryPointStartState name stageInfo i )
            ( EntryPointEndState name stageInfo j_bds j_iface i )
            ( AST () )
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
             , Internal (ProgUser optic) ~ User optic
             )
           => ProgUser optic
use = fromAST ( Use @optic sLength opticSing )

-- | Assign a new value with an optic.
--
-- Like @assign@ from the lens library, except the optic needs to be passed with a type application.
assign :: forall optic.
             ( GHC.Stack.HasCallStack
             , KnownOptic optic, StatefulOptic optic
             , Settable optic
             , Syntactic (ProgAssigner optic)
             , Internal (ProgAssigner optic) ~ Assigner optic
             )
           => ProgAssigner optic
assign = fromAST ( Assign @optic sLength opticSing )

-- *** Get, put, modify.

-- | Get the value of a variable.
-- Like @get@ for state monads, except a binding name needs to be specified with a type application.
--
-- Synonym for @use \@(Name k)@.
get :: forall (k :: Symbol) (a :: Type) (i :: ProgramState).
       ( KnownSymbol k
       , Gettable (Name k :: Optic '[] i a)
       , a ~ Has k i
       )
    => Program i i (AST a)
get = use @(Name k :: Optic '[] i a)

-- | Set the value of a variable.
-- Like @put@ for state monads, except a binding name needs to be specified with a type application.
--
-- Synonym for @assign \@(Name k)@.
put :: forall (k :: Symbol) (a :: Type) (i :: ProgramState).
       ( KnownSymbol k
       , Settable (Name k :: Optic '[] i a)
       , a ~ Has k i
       )
    => AST a -> Program i i (AST ())
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
          )
       => (AST a -> AST a) -> Program i i (AST ())
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
              ( imgName :: Symbol          )
              ( props   :: ImageProperties )
              ( i       :: ProgramState    )
            .
            ( KnownSymbol imgName
            , Gettable
                ( ImageTexel imgName
                  :: Optic
                      '[ ImageOperands props '[], ImageCoordinates props '[] ]
                       i
                       ( ImageData props '[] )
                )
            , props ~ LookupImageProperties imgName i
            , Known ImageProperties props
            , ValidImageRead props '[]
            )
          => AST (ImageCoordinates props '[])
          -> Program i i (AST (ImageData props '[]))
imageRead = use
          @( ImageTexel imgName
              :: Optic
                   '[ ImageOperands props '[], ImageCoordinates props '[] ]
                    i
                    ( ImageData props '[] )
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
                ( imgName :: Symbol          )
                ( props   :: ImageProperties )
                ( i       :: ProgramState    )
             .
             ( KnownSymbol imgName
             , Gettable
                ( ImageTexel imgName
                  :: Optic
                      '[ ImageOperands props '[], ImageCoordinates props '[] ]
                       i
                       ( ImageData props '[] )
                )
             , props ~ LookupImageProperties imgName i
             , Known ImageProperties props
             , ValidImageWrite props '[]
             )
           => AST (ImageCoordinates props '[])
           -> AST (ImageData props '[])
           -> Program i i ( AST () )
imageWrite = assign
           @( ImageTexel imgName
                :: Optic
                    '[ ImageOperands props '[], ImageCoordinates props '[] ]
                     i
                     ( ImageData props '[] )
            )
           NilOps

--------------------------------------------------------------------------
-- geometry shader primitive instructions

-- | Geometry shader: write the current output values at the current vertex index.
emitVertex :: forall (i :: ProgramState)
           . ( ExecutionContext i ~ Just SPIRV.Geometry )
           => Program i i ( AST () )
emitVertex = primOp @i @SPIRV.EmitGeometryVertex

-- | Geometry shader: end the current primitive and pass to the next one.
endPrimitive :: forall (i :: ProgramState)
             .  ( ExecutionContext i ~ Just SPIRV.Geometry )
             => Program i i ( AST () )
endPrimitive = primOp @i @SPIRV.EndGeometryPrimitive

--------------------------------------------------------------------------
-- memory synchronisation primitive instructions

-- | Control barrier: wait for other invocations to reach the current point of execution.
--
-- Can also act as a memory barrier if a memory synchronisation scope is specified.
controlBarrier :: forall (i :: ProgramState)
               .  SPIRV.SynchronisationScope
               -> Maybe ( SPIRV.SynchronisationScope, SPIRV.MemorySemantics )
               -> Program i i ( AST () )
controlBarrier controlScope memScope
  = primOp @i @SPIRV.ControlSync
      ( Lit $ SPIRV.synchronisationScope controlScope )
      ( Lit $ fromMaybe 0 $ SPIRV.synchronisationScope   . fst <$> memScope )
      ( Lit $ fromMaybe 0 $ SPIRV.memorySemanticsBitmask . snd <$> memScope )

-- | Memory barrier: ensure ordering on memory accesses.
memoryBarrier :: forall (i :: ProgramState)
               .  SPIRV.SynchronisationScope
               -> SPIRV.MemorySemantics
               -> Program i i ( AST () )
memoryBarrier memScope memSem
  = primOp @i @SPIRV.MemorySync
      ( Lit $ SPIRV.synchronisationScope   memScope )
      ( Lit $ SPIRV.memorySemanticsBitmask memSem   )

--------------------------------------------------------------------------
-- type synonyms for use/assign

type family ProgListVariadic
              ( is :: [Type] )
              ( s  :: ProgramState )
              ( a  :: Type  )
            = ( r  :: Type  )
            | r -> is s a where
  ProgListVariadic '[]         s a = Program s s (AST a)
  ProgListVariadic ( i ': is ) s a = AST i -> ProgListVariadic is s a


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
  ProgVariadicModifier '[]       s a = (AST a -> AST a) -> Program s s (AST ())
  ProgVariadicModifier (i ': is) s a = AST i -> ProgVariadicModifier is s a

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
       , Internal (ProgUser optic) ~ User optic
       , Syntactic (ProgAssigner optic)
       , Internal (ProgAssigner optic) ~ Assigner optic
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
         , r ~ AST a
         )
  => Eq (Program i j r) where
  type Logic (Program i j r) = Program i i (AST Bool)
  (==) = ixLiftA2 (==)
  (/=) = ixLiftA2 (/=)

instance ( ScalarTy a, Ord a, Logic a ~ Bool
         , i ~ j
         , r ~ AST a
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
         , r ~ AST a
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
         , x ~ AST a
         , t ~ AST s
         , j ~ i, i' ~ i, j' ~ i
         )
  => BitShift '(Program i j x, Program i' j' t) where
  shiftL = ixLiftA2 shiftL
  shiftR = ixLiftA2 shiftR

instance ( ScalarTy a, ScalarTy s
         , BitShift '(a,s)
         , x ~ AST a
         , i ~ j
         )
  => BitShift '(Program i j x, AST s) where
  shiftL a s = shiftL @'(Program i j x, Program i i (AST s)) a (ixPure s)
  shiftR a s = shiftR @'(Program i j x, Program i i (AST s)) a (ixPure s)


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
instance (ScalarTy a, AdditiveMonoid a, j ~ i) => AdditiveMonoid (Program i j (AST a)) where
  (+)    = ixLiftA2 (+)
  zero   = ixPure zero
  fromInteger = ixPure . fromInteger
instance (ScalarTy a, Semiring a, j ~ i) => Semiring (Program i j (AST a)) where
  (*)    = ixLiftA2 (*)
instance (ScalarTy a, AdditiveGroup a, j ~ i) => AdditiveGroup (Program i j (AST a)) where
  (-)    = ixLiftA2 (-)
  negate = ixFmap negate
instance (ScalarTy a, Signed a, j ~ i) => Signed (Program i j (AST a)) where
  abs    = ixFmap abs
  signum = ixFmap signum
instance (ScalarTy a, DivisionRing a, j ~ i) => DivisionRing (Program i j (AST a)) where
  (/)    = ixLiftA2 (/)
  fromRational = ixPure . fromRational
instance ( ScalarTy a
         , Archimedean a
         , Archimedean (AST a)
         , Logic a ~ Bool
         , j ~ i
         ) => Archimedean (Program i j (AST a)) where
  mod    = ixLiftA2 mod
  rem    = ixLiftA2 rem
  div    = ixLiftA2 div

instance (ScalarTy a, Floating a, j ~ i) => Floating (Program i j (AST a)) where
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

instance (ScalarTy a, RealFloat a, j ~ i) => RealFloat (Program i j (AST a)) where
  atan2 = ixLiftA2 atan2

-- Numeric conversions

-- $conversions
-- Instance for 'Convert', 'Rounding'.
instance ( ScalarTy a, ScalarTy b
         , Convert '(AST a, AST b)
         , j ~ i, k ~ i, l ~ i
         )
         => Convert '( Program i j (AST a)
                     , Program k l (AST b)
                     ) where
  convert = ixFmap ( convert @'(AST a, AST b) )

instance ( ScalarTy a, ScalarTy b, Rounding '(AST a, AST b)
         , j ~ i, k ~ i, l ~ i
         )
         => Rounding '( Program i j (AST a)
                      , Program k l (AST b)
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
instance (ScalarTy a, Semiring a, j ~ i) => Semimodule Nat (Program i j (AST (V 0 a))) where
  type Scalar   (Program i j (AST (V 0 a)))       = Program i j (AST a)
  type OfDim    (Program i j (AST (V 0 a))) Nat n = Program i j (AST (V n a))
  type ValidDim (Program i j (AST (V 0 a))) Nat n = KnownNat n

  (^+^) = ixLiftA2 (^+^)
  (^*)  = ixLiftA2 (^*)

instance (ScalarTy a, Ring a, j ~ i) => LinearModule Nat (Program i j (AST (V 0 a))) where
  (^-^) = ixLiftA2 (^-^)

instance (ScalarTy a, Floating a, j ~ i) => Inner Nat (Program i j (AST (V 0 a))) where
  (^.^) = ixLiftA2 (^.^)
  normalise = ixFmap normalise

instance (ScalarTy a, Floating a, j ~ i) => Cross Nat (Program i j (AST (V 0 a))) where
  type CrossDim (Program i j (AST (V 0 a))) Nat n = ( n ~ 3 )
  cross = ixLiftA2 cross

-- Matrices

-- $matrices
-- Instance for 'Matrix'.

type instance VectorOf (Program i j (AST (M 0 0 a))) = Program i j (AST (V 0 a))

instance (ScalarTy a, Floating a, j ~ i) => Matrix Nat (Program i j (AST (M 0 0 a))) where
  type OfDims (Program i j (AST (M 0 0 a))) Nat '(m,n) = Program i j (AST (M m n a))

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
