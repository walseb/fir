{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module FIR.Syntax.Eval where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy )
import GHC.Exts
  ( Any )
import GHC.TypeLits
  ( Symbol )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map as Map
  
-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import Control.Monad.Indexed
  ( (:=), Codensity(..) )
import Control.Type.Optic
  ( ReifiedGetter(view), ReifiedSetter(set) )
import Data.Type.Map
  ( (:->)((:->)) )
import FIR.AST
  ( AST(..) )
import FIR.Binding
  ( Binding(..), Var, Fun )
import FIR.Module
  ( Program )
import FIR.Prim.Op
  ( PrimOp(op) )
import FIR.ProgramState
  ( ProgramState )
import FIR.Syntax.Optics
  ( SOptic )

------------------------------------------------------------

{-
runIxState :: Program i j a -> Bindings i -> ( a, Bindings j )
runIxState = undefined

evalIxState :: Program i j a -> Bindings i -> a
evalIxState prog bds = fst ( runIxState prog bds )

execIxState :: Program i j a -> Bindings i -> Bindings j
execIxState prog bds = snd ( runIxState prog bds )
-}

newtype Bindings = Bindings (Map ShortText Any)


evalCodensity :: forall (i :: ProgramState) (k :: ProgramState) (a :: Type)
              .  Codensity AST (a := k) i -> Codensity ((->) Bindings) (a := k) i
evalCodensity ( Codensity k ) = Codensity ( runEval k )
  where
    runEval :: forall (q :: ProgramState -> Type)
         .  ( ( forall (j :: ProgramState). (a := k) j ->        AST (q j) ) ->        AST (q i) )
         -> ( ( forall (j :: ProgramState). (a := k) j -> Bindings -> q j  ) -> Bindings -> q i  )
    runEval l f bds = eval q
      where
        g :: forall (j :: ProgramState). (a := k) j -> AST (q j)
        g a = Value (f a bds)
        q :: AST (q i)
        q = l g
        eval :: AST (q i) -> q i
        eval = undefined



{-
runState :: Bindings i -> AST a -> ( a, Bindings j )
runState bds ( Lam f ) = ( \a -> f (Value a), bds )
runState bds ( f :$ a ) =
  case runState bds f of
    ( g, bds2 ) ->
      case runState bds2 a of
        ( b, bds3 ) ->
          ( g b, bds3 )
runState bds (Lit a) = (a, bds)
runState bds (Value a) = (a, bds)
runState bds (PrimOp ( _ :: Proxy a ) ( _ :: Proxy op ) ) = ( op @_ @_ op a, bds )
runState bds Return = ( AtKey , bds )
runState bds Bind = ???
runState bds ( Def ( _ :: Proxy k ) _ ) = ( id, 
runState _ (MkID _) = error "cannot evaluate SPIR-V instruction ID"
runState _ AnyImageOperand = error "image operands not supported"


pattern AnyImageOperand :: AST a
pattern AnyImageOperand ( isImageOperand -> True )

isImageOperand :: AST a -> Bool
isImageOperand (Proj          {}) = True
isImageOperand (Dref          {}) = True
isImageOperand (Bias          {}) = True
isImageOperand (LOD           {}) = True
isImageOperand (MinLOD        {}) = True
isImageOperand (Grad          {}) = True
isImageOperand (ConstOffsetBy {}) = True
isImageOperand (OffsetBy      {}) = True
isImageOperand (Gather        {}) = True
isImageOperand (SampleNo      {}) = True
isImageOperand _                  = False
-}
{-
eval :: AST a -> a
--eval ( Lam f ) = eval . f
eval ( f :$ a ) = eval f $ eval a
eval ( Lit x ) = x
eval ( PrimOp ( _ :: Proxy a ) ( _ :: Proxy op ) ) = op @_ @_ @op @a

eval ( View _ (_ :: SOptic optic) ) = view @optic
eval ( Set  _ (_ :: SOptic optic) ) = set  @optic
eval If  = \b t f -> if b then t else f
--eval IfM = ???
--eval ( Switch  s d cs ) = ... easy
--eval ( SwitchM s d cs ) = ???
--eval While = \c body -> ???

eval _ = error "unsupported"
-}