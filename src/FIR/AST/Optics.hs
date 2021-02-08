{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.AST.Optics

AST operations involving optics (view, set, use, assign).
-}

module FIR.AST.Optics where

-- base
import Data.Kind
  ( Type )
import qualified GHC.Stack
  ( HasCallStack )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( pattern VF )

-- fir
import {-# SOURCE #-} Control.Type.Optic
  ( Optic
  , Gettable, ReifiedGetter
  , Settable, ReifiedSetter
  , Indices
  )
import Data.Type.List
  ( SLength, Snoc )
import FIR.AST.Display
  ( Display(toTreeArgs), named )
import FIR.AST.Type
  ( AugType(Val, (:-->)), Eff )
import FIR.ProgramState
  ( ProgramState )
import {-# SOURCE #-} FIR.Syntax.Optics
  ( KnownOptic, SOptic, showSOptic )

------------------------------------------------------------

pattern Use    lg optic = VF ( UseF    lg optic )
pattern Assign lg optic = VF ( AssignF lg optic )
pattern View   lg optic = VF ( ViewF   lg optic )
pattern Set    lg optic = VF ( SetF    lg optic )


data OpticF ( ast :: AugType -> Type ) ( t :: AugType ) where
  -- | /Use/ an optic, returning a monadic value read from the (indexed) state.
  --
  -- Like @use@ from the lens library.
  UseF
    :: forall optic ast
    .  ( GHC.Stack.HasCallStack
       , KnownOptic optic, Gettable optic
       )
    => SLength ( Indices optic ) -- ^ Singleton for the number of run-time indices.
    -> SOptic optic              -- ^ Singleton for the optic.
    -> OpticF ast ( AugUser optic )

  -- | /Assign/ a new value with an optic.
  --
  -- Like @assign@ from the lens library.
  AssignF
    :: forall optic ast
    .  ( GHC.Stack.HasCallStack
       , KnownOptic optic, Settable optic
       )
    => SLength ( Indices optic ) -- ^ Singleton for the number of run-time indices.
    -> SOptic optic              -- ^ Singleton for the optic.
    -> OpticF ast ( AugAssigner optic )

  -- | /View/: access a value using an optic.
  --
  -- Like @view@ from the lens library.
  ViewF
    :: forall optic ast
    .  ( GHC.Stack.HasCallStack
       , KnownOptic optic, ReifiedGetter optic
       )
    => SLength ( Indices optic ) -- ^ Singleton for the number of run-time indices.
    -> SOptic optic              -- ^ Singleton for the optic.
    -> OpticF ast ( AugViewer optic )

  -- | /Set/: set a value using an optic.
  --
  -- Like @set@ from the lens library.
  SetF
    :: forall optic ast
    .  ( GHC.Stack.HasCallStack
       , KnownOptic optic, ReifiedSetter optic
       )
    => SLength ( Indices optic ) -- ^ Singleton for the number of run-time indices.
    -> SOptic optic              -- ^ Singleton for the optic.
    -> OpticF ast ( AugSetter optic )

------------------------------------------------------------
-- type synonyms for augmented types of optical operations

type AugUser     (g :: Optic as i b) = AugListVariadicIx i as b
type AugViewer   (g :: Optic is s a) = AugListVariadic   (is `Snoc` s) a
type AugAssigner (g :: Optic as i b) = AugListVariadicIx i (as `Snoc` b) ()
type AugSetter   (g :: Optic is s a) = AugListVariadic   (is `Snoc` a `Snoc` s) s

type family AugListVariadic
              ( as :: [Type] )
              ( b  :: Type   )
            = ( r :: AugType )
            | r -> as b where
  AugListVariadic '[]       b = Val b
  AugListVariadic (a ': as) b = Val a :--> AugListVariadic as b

type family AugListVariadicIx
              ( i  :: ProgramState )
              ( as :: [Type]  )
              ( b  :: Type    )
            = ( r  :: AugType )
            | r -> as i b  where
  AugListVariadicIx i '[]       b = Eff i i b
  AugListVariadicIx i (a ': as) b = Val a :--> AugListVariadicIx i as b

------------------------------------------------------------
-- displaying

instance Display (OpticF ast) where
  toTreeArgs = named \ case
    UseF    _ o ->    "Use @(" ++ showSOptic o ++ ")"
    AssignF _ o -> "Assign @(" ++ showSOptic o ++ ")"
    ViewF   _ o ->   "View @(" ++ showSOptic o ++ ")"
    SetF    _ o ->    "Set @(" ++ showSOptic o ++ ")"
