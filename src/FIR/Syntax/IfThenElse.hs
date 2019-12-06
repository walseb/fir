{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Syntax.IfThenElse

This module defines instances used to provide rebindable if-then-else statements.
-}

module FIR.Syntax.IfThenElse
  ( when, unless )
  where

-- base
import Data.Kind
  ( Type )

-- fir
import Control.Monad.Indexed
  ( (:=), ixPure )
import FIR.AST
  ( AST
      ( (:$), Lit
      , If, IfM
      , Return
      )
  , Syntactic(Internal, fromAST)
  )
import FIR.Module
  ( Program )
import FIR.Prim.Singletons
  ( PrimTy )
import FIR.Syntax.AST
  ( )
import FIR.Syntax.Program
  ( )
import Math.Logic.Class
  ( Choose(choose), Choosing )

--------------------------------------------------------------------------------------

when :: forall i. AST Bool -> Program i i (AST ()) -> Program i i (AST ())
when b action
  = choose b
      ( action )
      ( ixPure (Lit ()) :: Program i i (AST ()) )

unless :: forall i. AST Bool -> Program i i (AST ()) -> Program i i (AST ())
unless b action
  = choose b
      ( ixPure (Lit ()) :: Program i i (AST ()) )
      ( action )

--------------------------------------------------------------------------------------
-- | The 'Choose' instances provide syntax for rebindable 'ifThenElse'.
--
-- We uses a helper type class 'Chooser' in an attempt to improve type inference.
instance {-# OVERLAPPABLE #-}
         ( b ~ Bool
         , Syntactic z
         , Chooser x y z (WhichChoice (Internal z))
         )
       => Choose (AST b) '(x, y, z) where
  choose = chosen @x @y @z @(WhichChoice (Internal z))
instance ( b ~ AST Bool
         , j ~ i, k ~ i, l ~ i
         , Syntactic z
         , x ~ Program i j1 z
         , y ~ Program i j2 z
         )
       => Choose (Program k l b) '(x, y, Program i j z) where
  choose = fromAST IfM
instance ( b ~ Bool
         , j ~ i
         , Syntactic z
         , x ~ Program i j1 z
         , y ~ Program i j2 z
         )
       => Choose (AST b) '(x, y, Program i j z) where
  choose c = fromAST IfM ( Return :$ c )
instance {-# INCOHERENT #-}
         ( b ~ Program i i (AST Bool)
         , j ~ i
         , Syntactic z
         , x ~ Program i j1 z
         , y ~ Program i j2 z
         )
       => Choose b '(x, y, Program i j z) where
  choose = fromAST IfM


data Choice = PureChoice | ImpureChoice
class Chooser x y z (choice :: Choice) where
  chosen :: Choosing (AST Bool) '(x,y,z)
instance ( Syntactic z
         , PrimTy (Internal z)
         , x ~ z, y ~ z
         )
       => Chooser x y z PureChoice where
  chosen = fromAST If
instance ( x ~ Program i j1 x'
         , y ~ Program i j2 y'
         , z ~ Program i i  z'
         , Syntactic z'
         , PrimTy (Internal z')
         , x' ~ z'
         , y' ~ z'
         )
      => Chooser x y z ImpureChoice where
  chosen c = fromAST IfM ( Return :$ c )

type family WhichChoice (z :: Type) :: Choice where
  WhichChoice ( (_ := _) _ )
    = ImpureChoice
  WhichChoice _
    = PureChoice
