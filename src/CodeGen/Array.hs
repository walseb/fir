{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module CodeGen.Array
  ( createArray )
  where

-- base
import Prelude
  ( ($) )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( KnownSymbol )
import GHC.TypeNats
  ( KnownNat )

-- fir
import Control.Monad.Indexed
  ( (:=), pure, return, (>>), (>>=) )
import Control.Type.Optic
  ( Optic, Name, AnIndex, (:.:) )
import Data.Type.Known
  ( knownValue )
import Data.Type.Map
  ( (:->)((:->)) )
import Math.Algebra.Class
  ( AdditiveMonoid(fromInteger, (+)) )
import Math.Logic.Class
  ( Ord((<)) )
import FIR.AST
  ( AST(Lit), Syntactic(toAST) )
import FIR.Binding
  ( Var, RW )
import FIR.Instances.AST
  ( )
import FIR.Instances.Codensity
  ( def, get, put, assign
  , locally, while
  )
import FIR.Instances.Bindings
  ( Has, CanGet, CanPut
  , AddBinding
  )
import FIR.ASTState
  ( ASTState(..) )
import FIR.Prim.Array
  ( Array )
import FIR.Prim.Singletons
  ( PrimTy )

----------------------------------------------------------------------------

-- | Code for a loop creating a new array.
--
-- Assumes that the array has already been defined,
-- but allows it to be uninitialised.
--
-- Runs in an environment with no local state.
-- This __unsafely__ assumes that @ixName@ is not already defined
-- in the current state.
createArray :: forall n arrName ixName a i j ctx funs eps.
             ( KnownNat n
             , KnownSymbol ixName
             , KnownSymbol arrName
             , PrimTy a
             , i ~ ( 'ASTState '[ arrName ':-> Var RW (Array n a) ] ctx funs eps )
             , j ~ AddBinding ixName (Var RW Word32) i
             , Has ixName j ~ Word32
             , Has arrName j ~ Array n a
             , CanGet ixName  j
             , CanPut ixName  j
             , CanGet arrName j
             , CanPut arrName j
             )
          => ( AST Word32 -> AST a ) 
          -> AST ( (Array n a := i) i )
createArray arrayFunction = toAST $ locally @i @j do
  def @ixName @RW @Word32 @i 0
  while ( get @ixName < pure (Lit arrayLg) ) do
    i <- get @ixName
    assign @(Name arrName :.: (AnIndex Word32 :: Optic _ (Array n a) _))
      i (arrayFunction i)
    put @ixName (i+1)
  get @arrName
    where arrayLg :: Word32
          arrayLg = knownValue @n
