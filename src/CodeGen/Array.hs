{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module: CodeGen.Array

This module defines the AST for a loop creating an array, to be processed by the code generator to define loops over arrays.

This is done here with the user-facing syntax of the library, instead of wiring in the definition.
-}

module CodeGen.Array
  ( createArray )
  where

-- base
import Prelude
  ( ($), Bool(..) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( KnownSymbol )
import GHC.TypeNats
  ( KnownNat )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Control.Monad.Indexed
  ( pure, (>>), (>>=) )
import Control.Type.Optic
  ( Name, AnIndex, (:.:) )
import Data.Type.Known
  ( knownValue )
import Data.Type.Map
  ( (:->)((:->)) )
import Math.Algebra.Class
  ( AdditiveMonoid(fromInteger, (+)) )
import Math.Logic.Class
  ( Ord((<)) )
import FIR.AST
  ( AST, Code
  , Syntactic(toAST)
  , pattern Lit
  )
import FIR.AST.Type
  ( Eff )
import FIR.Binding
  ( Var, RW )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Types
  ( PrimTy, HasOpaqueType )
import FIR.ProgramState
  ( ProgramState(ProgramState) )
import FIR.Syntax.AST
  ( )
import FIR.Syntax.Program
  ( def, get, put, assign
  , locally, while
  )
import FIR.Validation.Bindings
  ( Has, CanGet, CanPut
  , ValidDef, AddBinding
  )

----------------------------------------------------------------------------

-- | Code for a loop creating a new array.
--
-- Assumes that the array has already been defined,
-- but allows it to be uninitialised.
--
-- Runs in an environment with no local state.
createArray :: forall n arrName ixName a i j ctx funs eps g_iface rayQs bkend.
             ( KnownNat n
             , KnownSymbol ixName
             , KnownSymbol arrName
             , PrimTy a
             , i ~ ( 'ProgramState '[ arrName ':-> Var RW (Array n a) ] ctx funs eps g_iface rayQs bkend )
             , j ~ AddBinding ixName (Var RW Word32) i
             , Has ixName  j ~ Word32
             , Has arrName j ~ Array n a
             , CanGet ixName  j
             , CanPut ixName  j
             , CanGet arrName j
             , CanPut arrName j
             , ValidDef ixName i Word32
             )
          => ( Code Word32 -> Code a )
          -> AST ( Eff i i (Array n a) )
createArray arrayFunction
  | Refl <- ( unsafeCoerce Refl :: HasOpaqueType a :~: False ) -- bypass validity check
  = toAST $ locally @i @j do
      _ <- def @ixName @RW @Word32 @i 0
      while ( get @ixName < pure (Lit arrayLg) ) do
        i <- get @ixName
        assign @(Name arrName :.: AnIndex Word32)
          i (arrayFunction i)
        put @ixName (i+1)
      get @arrName
        where arrayLg :: Word32
              arrayLg = knownValue @n
