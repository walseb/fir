{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module: FIR.AST.ControlFlow

AST operations involving control flow (conditional branching, selections, loops).
-}

module FIR.AST.ControlFlow where

-- base
import Data.Kind
  ( Type )
import qualified GHC.Stack
  ( HasCallStack )

-- containers
import Data.Tree
 ( Tree(Node) )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( pattern VF )

-- fir
import Control.Arrow.Strength
  ( secondF )
import FIR.AST.Display
  ( Display(toTreeArgs), named )
import FIR.AST.Type
  ( AugType(Val, (:-->)), Eff )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( PrimTy, IntegralTy )

------------------------------------------------------------

pattern If             = VF IfF
pattern IfM            = VF IfMF
pattern Switch  s d cs = VF (SwitchF  s d cs)
pattern SwitchM s d cs = VF (SwitchMF s d cs)
pattern While          = VF WhileF

-- | If-then-else statement.
data IfF ( ast :: AugType -> Type ) ( t :: AugType ) where
  IfF
    :: ( GHC.Stack.HasCallStack
       , PrimTy a
       )
    => IfF ast ( Val Bool :--> Val a :--> Val a :--> Val a )

-- | Monadic if-then-else.
data IfMF ( ast :: AugType -> Type ) ( t :: AugType ) where
  IfMF
    :: GHC.Stack.HasCallStack
    => IfMF ast ( Eff i i Bool :--> Eff i j a :--> Eff i k a :--> Eff i i a )

-- | Switch statement.
data SwitchF ( ast :: AugType -> Type ) ( t :: AugType ) where
  SwitchF
    :: ( GHC.Stack.HasCallStack
       , PrimTy a
       , IntegralTy s
       )
    => ast ( Val s ) -- ^ Scrutinee.
    -> ast ( Val a ) -- ^ Default case.
    -> [ ( s, ast (Val a) ) ] -- ^ Cases.
    -> SwitchF ast ( Val a )

-- | Monadic switch statement.
data SwitchMF ( ast :: AugType -> Type ) ( t :: AugType ) where
  SwitchMF
    :: ( GHC.Stack.HasCallStack
       , PrimTy a
       , IntegralTy s
       )
    => ast ( Eff i i s ) -- ^ Scrutinee.
    -> ast ( Eff i i a ) -- ^ Default case.
    -> [ ( s, ast ( Eff i i a ) ) ] -- ^ Cases.
    -> SwitchMF ast ( Eff i i a )
  -- ( SwitchM could be generalised by allowing different return types in the branches.)

-- | While loop.
data WhileF ( ast :: AugType -> Type ) ( t :: AugType ) where
  WhileF
    :: GHC.Stack.HasCallStack
    => WhileF ast ( Eff i i Bool :--> Eff i j () :--> Eff i i () )

------------------------------------------------------------
-- displaying

instance Display (IfF ast) where
  toTreeArgs = named (const "If")
instance Display (IfMF ast) where
  toTreeArgs = named (const "IfM")
instance Display ast => Display (SwitchF ast) where
  toTreeArgs as ( SwitchF s d cs ) = do
    scrut <- toTreeArgs [] s
    def   <- toTreeArgs [] d
    cases <- traverse ( secondF ( toTreeArgs [] ) ) cs
    let
      allCases
        =  map ( \(v,a) -> Node ("Case " ++ show v) [a] ) cases
        ++ [ Node "Default" [def] ]
    return (Node "Switch" (scrut : allCases ++ as))
instance Display ast => Display (SwitchMF ast) where
  toTreeArgs as (SwitchMF s d cs) = do
    scrut <- toTreeArgs [] s
    def   <- toTreeArgs [] d
    cases <- traverse ( secondF ( toTreeArgs [] ) ) cs
    let
      allCases
        =  map ( \(v,a) -> Node ("Case " ++ show v) [a] ) cases
        ++ [ Node "Default" [def] ]
    return (Node "SwitchM" (scrut : allCases ++ as))
instance Display (WhileF ast) where
  toTreeArgs = named (const "While")
