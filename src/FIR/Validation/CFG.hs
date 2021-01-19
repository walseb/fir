{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Validation.CFG

Validation module providing checks for control-flow based operations.

-}

module FIR.Validation.CFG
  ( InLoop
  , ValidBreak, ValidContinue
  )
  where

-- base
import Data.Kind
  ( Constraint )
import GHC.TypeLits
  ( TypeError
  , ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat, CmpNat )

-- fir
import Data.Type.Nat
  ( Succ )
import FIR.ProgramState
  ( ProgramState(ProgramState), CFGState(..) )

-------------------------------------------------

-- | Add one to the loop nesting level.
type family InLoop ( i :: ProgramState ) = ( i' :: ProgramState ) | i' -> i where
  InLoop ( 'ProgramState bds ctx cfg funs eps iface rayQueries bk ) =
    'ProgramState bds ctx ( CFGStateAddLoop cfg ) funs eps iface rayQueries bk

type family CFGStateAddLoop ( cfg :: CFGState ) = ( cfg' :: CFGState ) | cfg' -> cfg where
  CFGStateAddLoop ( 'CFGState currLoopLevel ) = 'CFGState ( Succ currLoopLevel )


-- | Check whether a break statement is valid.
type family ValidBreak ( n :: Nat ) ( i :: ProgramState ) :: Constraint where
  ValidBreak 0 _ = ()
  ValidBreak n ( 'ProgramState _ _ ('CFGState currLoopDepth) _ _ _ _ _ ) =
    EnsureValidBreakNb n currLoopDepth ( n `CmpNat` currLoopDepth )

type family EnsureValidBreakNb ( n :: Nat ) ( currLoopDepth :: Nat ) ( cmp :: Ordering ) :: Constraint where
  EnsureValidBreakNb n currLoopDepth GT =
    TypeError
      (    Text "Cannot break out of " :<>: ShowType n :<>: Text " loops."
      :$$: Text "Current loop nesting level is only " :<>: ShowType currLoopDepth :<>: Text "."
      )
  EnsureValidBreakNb _ _ _ = ()

-- | Check whether a continue statement is valid.
type family ValidContinue ( n :: Nat ) ( i :: ProgramState ) :: Constraint where
  ValidContinue n ( 'ProgramState _ _ ('CFGState currLoopDepth) _ _ _ _ _ ) =
    EnsureValidBreakContinue n currLoopDepth ( n `CmpNat` currLoopDepth )

type family EnsureValidBreakContinue ( n :: Nat ) ( currLoopDepth :: Nat ) ( cmp :: Ordering ) :: Constraint where
  EnsureValidBreakContinue _ _             LT = ()
  EnsureValidBreakContinue 0 currLoopDepth _ =
    TypeError
      ( Text "Unexpected 'continue' statement: not inside a loop" )
  EnsureValidBreakContinue n currLoopDepth _ =
    TypeError
      (    Text "'breakContinue': cannot continue after breaking out of " :<>: ShowType n :<>: Text " loops."
      :$$: Text "Current loop nesting level is only " :<>: ShowType currLoopDepth :<>: Text "."
      )
