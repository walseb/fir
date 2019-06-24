{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE InstanceSigs            #-}
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
Module: FIR.Instances.Codensity

This module, together with "FIR.Instances.AST",
provides most of the user-facing syntax for constructing
and manipulating values in the EDSL.

This is done through type class overloading, here in the form of
orphan instances for types of the form @Codensity AST (AST a := j) i@
(representing stateful values in the EDSL).

-}

module FIR.Instances.Codensity
  ( -- * Monadic control operations
    when, unless, while
  , locally, embed, locallyPair, embedPair

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

    -- * Instances

    -- ** Syntactic type class
    -- $syntactic

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
  )
import Data.Int
  ( Int32 )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(Proxy) )
import Data.Type.Equality
  ( type (==) )
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
  )
import FIR.ASTState
  ( FunctionInfo, Definedness(..)
  , FunctionContext(..), ASTState(ASTState)
  , TLInterface
  , ExecutionContext
  , EntryPointInfo(EntryPointInfo)
  )
import FIR.Binding
  ( BindingsMap
  , FunctionType, Var
  , Permissions
  )
import FIR.Definition
  ( Definition
  , KnownDefinitions
  , DefinitionFunctions, DefinitionEntryPoints
  , StartBindings, EndBindings
  )
import FIR.Instances.AST
  ( WhichConversion(conversion) ) -- plus instances
import FIR.Instances.Bindings
  ( AddBinding, AddFunBinding
  , Has
  , ValidFunDef, FunctionTypes
  , FunctionDefinitionStartState, FunctionDefinitionEndState
  , ValidEntryPoint
  , EntryPointStartState, EntryPointEndState
  , SetInterface, GetExecutionInfo
  , LookupImageProperties
  , ValidImageRead, ValidImageWrite
  , Embeddable
  )
import FIR.Instances.Images
  ( ImageTexel )
import FIR.Instances.Optics
  ( User, Assigner, KnownOptic, opticSing
  , StatefulOptic
  )
import FIR.Pipeline
  ( ShaderStage(ShaderStage) )
import FIR.Prim.Image
  ( ImageProperties, ImageOperands
  , ImageData, ImageCoordinates
  )
import FIR.Prim.Singletons
  ( PrimTy, ScalarTy, KnownVars )
import FIR.Synonyms
  ( pattern NoOperands )
import Math.Algebra.Class
  ( AdditiveMonoid(..), AdditiveGroup(..)
  , Semiring(..), Ring
  , DivisionRing(..)
  , Signed(..), Archimedean(..)
  , Floating(..), RealFloat(..)
  , Convert(..), Rounding(..)
  )
import Math.Linear
  ( Semimodule(..), Module(..)
  , Inner(..), Cross(..)
  , Matrix(..), VectorOf
  , V, M
  )
import Math.Logic.Bits
  ( Bits(..), BitShift(..) )
import Math.Logic.Class
  ( Eq(..), Boolean(..)
  , Choose(..), ifThenElse
  , Ord(..)
  )
import qualified SPIRV.PrimOp as SPIRV
  ( GeomPrimOp(..) )
import qualified SPIRV.Stage  as SPIRV

--------------------------------------------------------------------------
-- * Monadic control operations

when :: forall i. AST Bool -> Codensity AST (AST () := i) i -> Codensity AST (AST () := i) i
when b action
  = if b
    then action
    else ixPure (Lit ()) :: Codensity AST (AST () := i) i

unless :: forall i. AST Bool -> Codensity AST (AST () := i) i -> Codensity AST (AST () := i) i
unless b action
  = if b
    then ixPure (Lit ()) :: Codensity AST (AST () := i) i
    else action

locally :: forall i j r. Syntactic r => Codensity AST (r := j) i -> Codensity AST (r := i) i
locally = fromAST Locally

embed :: forall i j r. (Embeddable i j, Syntactic r)
      => Codensity AST (r := i) i -> Codensity AST (r := j) j
embed = fromAST Embed

-- temporary helpers until I find a nice typeclass for this
locallyPair :: forall i a b j
            .  Codensity AST ( (AST a, AST b) := j ) i
            -> Codensity AST ( (AST a, AST b) := i ) i
locallyPair p =
  ixLiftA2 (,)
    ( locally . ixFmap fst $ p )
    ( locally . ixFmap snd $ p )

embedPair :: forall i a b j
            .  (Embeddable i j)
            => Codensity AST ( (AST a, AST b) := i ) i
            -> Codensity AST ( (AST a, AST b) := j ) j
embedPair p =
  ixLiftA2 (,)
    ( embed @i @j . ixFmap fst $ p )
    ( embed @i @j . ixFmap snd $ p )

while :: ( GHC.Stack.HasCallStack
         , i' ~ i, i'' ~ i
         , l ~ (AST () := j)
         , b ~ (AST Bool := i)
         , r ~ (AST () := i)
         )
      => Codensity AST b (i :: ASTState)
      -> Codensity AST l i'
      -> Codensity AST r i''
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
def :: forall k ps a i.
       ( GHC.Stack.HasCallStack
       , KnownSymbol k
       , Known Permissions ps
       , PrimTy a
       )
    => AST a -- ^ Initial value.
    -> Codensity AST (AST a := AddBinding k (Var ps a) i) i
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
fundef :: forall name as b j_bds i r.
           ( GHC.Stack.HasCallStack
           , Syntactic r
           , Internal r ~ FunctionType as b
           , KnownSymbol name
           , KnownVars as
           , PrimTy b
           , ValidFunDef name as i j_bds
           , '(as, b) ~ FunctionTypes name i
           )
        => Codensity AST
              ( AST b := FunctionDefinitionEndState name as j_bds i)
              ( FunctionDefinitionStartState name as i ) -- ^ Function body code.
        -> Codensity AST
              ( r := AddFunBinding name as b i )
              i
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
           => Codensity AST
                ( AST () := EntryPointEndState name stageInfo j_bds j_iface i )
                ( EntryPointStartState name stageInfo i )
           -> Codensity AST
                ( AST () := SetInterface name stageInfo j_iface i )
                i
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
            ( funs      :: [Symbol :-> FunctionInfo]     )
            ( eps       :: [ EntryPointInfo ]            )
            ( i         :: ASTState                      )
       . ( GHC.Stack.HasCallStack
         , KnownDefinitions defs
         , DefinitionFunctions   defs ~ funs
         , DefinitionEntryPoints defs ~ eps
         , funs ~ '[ ]
         , eps ~ '[ 'EntryPointInfo name stageInfo '( '[], '[]) 'Declared ]
         , i ~ 'ASTState (StartBindings defs) 'TopLevel funs eps
         , KnownSymbol name
         , stage ~ 'SPIRV.Stage ('SPIRV.ShaderStage shader)
         , Known SPIRV.Shader shader
         , Known (SPIRV.ExecutionInfo Nat stage) stageInfo
         , EndBindings defs ~ StartBindings defs
         , ValidEntryPoint name stageInfo ('ASTState (EndBindings defs) 'TopLevel funs eps) j_bds
         )
       => Codensity AST
          ( AST () := EntryPointEndState name stageInfo j_bds j_iface i )
          ( EntryPointStartState name stageInfo i )
       -> ShaderStage name shader defs (SetInterface name stageInfo j_iface i)
shader
  = ShaderStage
  . entryPoint @name @stage

-- Optics.

-- | /Use/ an optic, returning a monadic value read from the (indexed) state.
--
-- Like @use@ from the lens library, except the optic needs to be passed with a type application.
use :: forall optic.
             ( GHC.Stack.HasCallStack
             , KnownOptic optic, StatefulOptic optic
             , Gettable optic
             , Syntactic (CodUser optic)
             , Internal (CodUser optic) ~ User optic
             )
           => CodUser optic
use = fromAST ( Use @optic sLength opticSing )

-- | Assign a new value with an optic.
--
-- Like @assign@ from the lens library, except the optic needs to be passed with a type application.
assign :: forall optic.
             ( GHC.Stack.HasCallStack
             , KnownOptic optic, StatefulOptic optic
             , Settable optic
             , Syntactic (CodAssigner optic)
             , Internal (CodAssigner optic) ~ Assigner optic
             )
           => CodAssigner optic
assign = fromAST ( Assign @optic sLength opticSing )

-- *** Get, put, modify.

-- | Get the value of a variable.
-- Like @get@ for state monads, except a binding name needs to be specified with a type application.
--
-- Synonym for @use \@(Name k)@.
get :: forall (k :: Symbol) a (i :: ASTState).
       ( KnownSymbol k
       , Gettable (Name k :: Optic '[] i a)
       , a ~ Has k i
       )
    => Codensity AST (AST a := i) i
get = use @(Name k :: Optic '[] i a)

-- | Set the value of a variable.
-- Like @put@ for state monads, except a binding name needs to be specified with a type application.
--
-- Synonym for @assign \@(Name k)@.
put :: forall (k :: Symbol) a (i :: ASTState).
       ( KnownSymbol k
       , Settable (Name k :: Optic '[] i a)
       , a ~ Has k i
       )
    => AST a -> Codensity AST (AST () := i) i
put = assign @(Name k :: Optic '[] i a)

-- | Modify the value of a variable.
-- | Like @modify@ for state monads, except a binding name needs to be specified with a type application.
--
-- Synonym for @modifying \@(Name k)@.
modify :: forall (k :: Symbol) a (i :: ASTState).
          ( KnownSymbol k
          , Gettable (Name k :: Optic '[] i a)
          , Settable (Name k :: Optic '[] i a)
          , a ~ Has k i
          )
       => (AST a -> AST a) -> Codensity AST (AST () := i) i
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
              ( i       :: ASTState        )
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
          -> Codensity AST ( AST (ImageData props '[]) := i ) i
imageRead = use
          @( ImageTexel imgName
              :: Optic
                   '[ ImageOperands props '[], ImageCoordinates props '[] ]
                    i
                    ( ImageData props '[] )
           )
          NoOperands

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
                ( i       :: ASTState        )
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
           -> Codensity AST ( AST () := i ) i
imageWrite = assign
           @( ImageTexel imgName
                :: Optic
                    '[ ImageOperands props '[], ImageCoordinates props '[] ]
                     i
                     ( ImageData props '[] )
            )
           NoOperands

--------------------------------------------------------------------------
-- geometry shader primitive instructions

-- | Geometry shader: write the current output values at the current vertex index.
emitVertex :: forall (i :: ASTState).
              ( ExecutionContext i ~ Just SPIRV.Geometry )
           => Codensity AST ( AST () := i ) i
emitVertex = primOp @i @SPIRV.EmitGeometryVertex

-- | Geometry shader: end the current primitive and pass to the next one.
endPrimitive :: forall (i :: ASTState).
                ( ExecutionContext i ~ Just SPIRV.Geometry )
             =>  Codensity AST ( AST () := i ) i
endPrimitive = primOp @i @SPIRV.EndGeometryPrimitive

--------------------------------------------------------------------------
-- type synonyms for use/assign

type family ListVariadicCod
              ( is :: [Type]   )
              ( s  :: ASTState )
              ( a  :: Type     )
            = ( r  :: Type     )
            | r -> is s a where
  ListVariadicCod '[]         s a = Codensity AST (AST a := s) s
  ListVariadicCod ( i ': is ) s a = AST i -> ListVariadicCod is s a


-- recall (defined in FIR.Instances.Optics):
-- type User     (g :: Optic is s a) = ListVariadicIx is                s a
-- type Assigner (g :: Optic is s a) = ListVariadicIx (is `Postpend` s) s ()

type CodUser     (optic :: Optic is s a) = ListVariadicCod  is               s a
type CodAssigner (optic :: Optic is s a) = ListVariadicCod (is `Postpend` a) s ()

--------------------------------------------------------------------------
-- modifying

type family VariadicCodModifier
              ( is :: [Type]  )
              ( s  :: ASTState )
              ( a  :: Type    )
            = ( r  :: Type    )
            | r -> is s a
            where
  VariadicCodModifier '[]       s a = (AST a -> AST a) -> Codensity AST (AST () := s) s
  VariadicCodModifier (i ': is) s a = AST i -> VariadicCodModifier is s a

type CodModifier (optic :: Optic is s a) = VariadicCodModifier is s a

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
       , Syntactic (CodUser optic)
       , Internal (CodUser optic) ~ User optic
       , Syntactic (CodAssigner optic)
       , Internal (CodAssigner optic) ~ Assigner optic
       , Modifier (Indices optic) (Whole optic) (Part optic)
       )
    => CodModifier optic
modifying
  = modifier @(Indices optic) @(Whole optic) @(Part optic)
      ( use    @optic )
      ( assign @optic )

class Modifier is s a where
  modifier :: ListVariadicCod      is               s a
           -> ListVariadicCod     (is `Postpend` a) s ()
           -> VariadicCodModifier  is               s a

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
-- 'Boolean', 'Choose',
--
-- 'Eq', 'Ord' (note: not the "Prelude" type classes).
instance Boolean (Codensity AST (AST Bool := i) i) where
  true  = ixPure true
  false = ixPure false
  (&&)  = ixLiftA2 (&&)
  (||)  = ixLiftA2 (||)
  not   = ixFmap not

instance ( PrimTy a
         , t ~ ( Codensity AST (AST a := j) i )
         , f ~ ( Codensity AST (AST a := k) i )
         , r ~ ( AST a := i )
         ) =>
  Choose  ( AST Bool )
         '( t
          , f
          , Codensity AST r i
          ) where
  choose = fromAST IfM

instance ( PrimTy a, Eq a, Logic a ~ Bool
         , x ~ (AST a := i)
         )
  => Eq (Codensity AST x i) where
  type Logic (Codensity AST x i) = Codensity AST (AST Bool := i) i
  (==) = ixLiftA2 (==)
  (/=) = ixLiftA2 (/=)

instance ( ScalarTy a, Ord a, Logic a ~ Bool
         , x ~ (AST a := i)
         )
  => Ord (Codensity AST x i) where
  type Ordering (Codensity AST x i) = Codensity AST (AST Int32 := i) i
  compare = ixLiftA2 compare
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
         , x ~ (AST a := i)
         , Bits a
         ) => Bits (Codensity AST x i) where
  (.&.) = ixLiftA2 (.&.)
  (.|.) = ixLiftA2 (.|.)
  xor   = ixLiftA2 xor
  complement = ixFmap complement
  zeroBits   = ixPure zeroBits

instance ( ScalarTy a, ScalarTy s
         , BitShift '(a,s)
         , x ~ (AST a := i)
         , t ~ (AST s := i)
         )
  => BitShift '(Codensity AST x i, Codensity AST t i) where
  shiftL = ixLiftA2 shiftL
  shiftR = ixLiftA2 shiftR

instance ( ScalarTy a, ScalarTy s
         , BitShift '(a,s)
         , x ~ (AST a := i)
         )
  => BitShift '(Codensity AST x i, AST s) where
  shiftL a s = shiftL @'(Codensity AST x i, Codensity AST (AST s := i) i) a (ixPure s)
  shiftR a s = shiftR @'(Codensity AST x i, Codensity AST (AST s := i) i) a (ixPure s)


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
instance (ScalarTy a, AdditiveMonoid a, j ~ i) => AdditiveMonoid (Codensity AST (AST a := j) i) where
  (+)    = ixLiftA2 (+)
  zero   = ixPure zero
  fromInteger = ixPure . fromInteger
instance (ScalarTy a, Semiring a, j ~ i) => Semiring (Codensity AST (AST a := j) i) where
  (*)    = ixLiftA2 (*)
instance (ScalarTy a, AdditiveGroup a, j ~ i) => AdditiveGroup (Codensity AST (AST a := j) i) where
  (-)    = ixLiftA2 (-)
  negate = ixFmap negate  
instance (ScalarTy a, Signed a, j ~ i) => Signed (Codensity AST (AST a := j) i) where
  abs    = ixFmap abs
  signum = ixFmap signum
instance (ScalarTy a, DivisionRing a, j ~ i) => DivisionRing (Codensity AST (AST a := j) i) where
  (/)    = ixLiftA2 (/)
  fromRational = ixPure . fromRational
instance ( ScalarTy a
         , Archimedean a
         , Archimedean (AST a)
         , Logic a ~ Bool
         , j ~ i
         ) => Archimedean (Codensity AST (AST a := j) i) where
  mod    = ixLiftA2 mod
  rem    = ixLiftA2 rem
  div    = ixLiftA2 div

instance (ScalarTy a, Floating a, j ~ i) => Floating (Codensity AST (AST a := j) i) where
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

instance (ScalarTy a, RealFloat a, j ~ i) => RealFloat (Codensity AST (AST a := j) i ) where
  atan2 = ixLiftA2 atan2

-- Numeric conversions

-- $conversions
-- Instance for 'Convert', 'Rounding'.
instance ( ScalarTy a, ScalarTy b, WhichConversion a b (a==b)
         , j ~ i, k ~ i, l ~ i
         )
         => Convert '( Codensity AST (AST a := j) i
                     , Codensity AST (AST b := l) k
                     ) where
  convert = ixFmap (conversion @a @b @(a==b))

instance ( ScalarTy a, ScalarTy b, Rounding '(AST a, AST b)
         , j ~ i, k ~ i, l ~ i
         )
         => Rounding '( Codensity AST (AST a := j) i
                      , Codensity AST (AST b := l) k
                      ) where
  truncate = ixFmap truncate
  round    = ixFmap round
  floor    = ixFmap floor
  ceiling  = ixFmap ceiling

-- Vectors

-- $vectors
-- Instances for:
--
-- 'Semimodule', 'Module', 'Inner', 'Cross'.
instance (ScalarTy a, Semiring a, j ~ i) => Semimodule Nat (Codensity AST (AST (V 0 a) := j) i) where
  type Scalar   (Codensity AST (AST (V 0 a) := j) i)       = Codensity AST (AST      a  := j) i
  type OfDim    (Codensity AST (AST (V 0 a) := j) i) Nat n = Codensity AST (AST (V n a) := j) i
  type ValidDim (Codensity AST (AST (V 0 a) := j) i) Nat n = KnownNat n

  (^+^) = ixLiftA2 (^+^)
  (^*)  = ixLiftA2 (^*)

instance (ScalarTy a, Ring a, j ~ i) => Module Nat (Codensity AST (AST (V 0 a) := j) i) where
  (^-^) = ixLiftA2 (^-^)

instance (ScalarTy a, Floating a, j ~ i) => Inner Nat (Codensity AST (AST (V 0 a) := j) i) where
  (^.^) = ixLiftA2 (^.^)
  normalise = ixFmap normalise

instance (ScalarTy a, Floating a, j ~ i) => Cross Nat (Codensity AST (AST (V 0 a) := j) i) where
  type CrossDim (Codensity AST (AST (V 0 a) := j) i) Nat n = ( n ~ 3 )
  cross = ixLiftA2 cross

-- Matrices

-- $matrices
-- Instance for 'Matrix'.

type instance VectorOf (Codensity AST (AST (M 0 0 a) := j) i) = (Codensity AST (AST (V 0 a) := j) i)

instance (ScalarTy a, Floating a, j ~ i) => Matrix Nat (Codensity AST (AST (M 0 0 a) := j) i) where
  type OfDims (Codensity AST (AST (M 0 0 a) := j) i) Nat '(m,n) = Codensity AST (AST (M m n a) := j) i

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
