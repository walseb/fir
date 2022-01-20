{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures -Wno-missing-signatures #-}

{-# LANGUAGE BlockArguments      #-}
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
import Data.Proxy
  ( Proxy(..) )
import qualified GHC.Stack
  ( HasCallStack )
import GHC.TypeNats
  ( KnownNat, natVal )

-- containers
import Data.Tree
 ( Tree(Node) )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( EGADT, type (:<!), pattern VF )

-- fir
import Control.Arrow.Strength
  ( secondF )
import FIR.AST.Display
  ( Display(toTreeArgs) )
import FIR.AST.Type
  ( AugType(Val, (:-->)), Eff )
import {-# SOURCE #-} FIR.Prim.Types
  ( PrimTy, IntegralTy )
import FIR.Validation.CFG
  ( InLoop )

------------------------------------------------------------

pattern If :: forall a fs
           .  ( GHC.Stack.HasCallStack, PrimTy a, SelectionF :<! fs )
           => EGADT fs ( Val Bool :--> Val a :--> Val a :--> Val a )
pattern If              = VF IfF
pattern IfM             = VF IfMF
pattern Switch  s d cs  = VF (SwitchF  s d cs)
pattern SwitchM s d cs  = VF (SwitchMF s d cs)

pattern While           = VF WhileF
pattern Loop            = VF LoopF
pattern Break         n = VF (BreakF n)
pattern BreakContinue n = VF (BreakContinueF n)

data SelectionF ( ast :: AugType -> Type ) ( t :: AugType ) where
  -- | If-then-else statement.
  IfF
    :: ( GHC.Stack.HasCallStack
       , PrimTy a
       )
    => SelectionF ast ( Val Bool :--> Val a :--> Val a :--> Val a )

  -- | Monadic if-then-else.
  IfMF
    :: GHC.Stack.HasCallStack
    => SelectionF ast ( Eff i i Bool :--> Eff i j a :--> Eff i k a :--> Eff i i a )

  -- | Switch statement.
  SwitchF
    :: ( GHC.Stack.HasCallStack
       , PrimTy a
       , IntegralTy s
       )
    => ast ( Val s ) -- ^ Scrutinee.
    -> ast ( Val a ) -- ^ Default case.
    -> [ ( s, ast (Val a) ) ] -- ^ Cases.
    -> SelectionF ast ( Val a )

  -- | Monadic switch statement.
  SwitchMF
    :: ( GHC.Stack.HasCallStack
       , PrimTy a
       , IntegralTy s
       )
    => ast ( Eff i i s ) -- ^ Scrutinee.
    -> ast ( Eff i i a ) -- ^ Default case.
    -> [ ( s, ast ( Eff i i a ) ) ] -- ^ Cases.
    -> SelectionF ast ( Eff i i a )
  -- ( SwitchM could be generalised by allowing different return types in the branches.)

data LoopF ( ast :: AugType -> Type ) ( t :: AugType ) where
  -- | While loop.
  WhileF
    :: GHC.Stack.HasCallStack
    => LoopF ast ( Eff i i Bool :--> Eff (InLoop i) j () :--> Eff i i () )

  -- | Loop.
  LoopF
    :: GHC.Stack.HasCallStack
    => LoopF ast ( Eff (InLoop i) j () :--> Eff i i () )

  -- | Break out of the given number of loops.
  BreakF
    :: ( KnownNat n, GHC.Stack.HasCallStack )
    => Proxy n
    -> LoopF ast ( Eff i i () )

  -- | Break out of the given number of loops, and continue from the resulting loop.
  BreakContinueF
    :: ( KnownNat n, GHC.Stack.HasCallStack )
    => Proxy n
    -> LoopF ast ( Eff i i () )

------------------------------------------------------------
-- displaying

instance Display ast => Display (SelectionF ast) where
  toTreeArgs as IfF  = pure ( Node "If" as )
  toTreeArgs as IfMF = pure ( Node "IfM" as )
  toTreeArgs as ( SwitchF s d cs ) = do
    scrut <- toTreeArgs [] s
    def   <- toTreeArgs [] d
    cases <- traverse ( secondF ( toTreeArgs [] ) ) cs
    let
      allCases
        =  map ( \(v,a) -> Node ("Case " ++ show v) [a] ) cases
        ++ [ Node "Default" [def] ]
    return (Node "Switch" (scrut : allCases ++ as))
  toTreeArgs as (SwitchMF s d cs) = do
    scrut <- toTreeArgs [] s
    def   <- toTreeArgs [] d
    cases <- traverse ( secondF ( toTreeArgs [] ) ) cs
    let
      allCases
        =  map ( \(v,a) -> Node ("Case " ++ show v) [a] ) cases
        ++ [ Node "Default" [def] ]
    return (Node "SwitchM" (scrut : allCases ++ as))

instance Display ast => Display (LoopF ast) where
  toTreeArgs as WhileF = pure ( Node "While" as )
  toTreeArgs as LoopF  = pure ( Node "Loop"  as )
  toTreeArgs as ( BreakF n ) = pure ( Node ( "Break @" ++ show ( natVal n ) ) as )
  toTreeArgs as ( BreakContinueF n ) =
    let
      nm = case natVal n of
        0 -> "Continue"
        i -> "BreakContinue @" ++ show i
    in pure ( Node nm as )
