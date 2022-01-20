{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
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
  ( Code
  , Syntactic(toAST, fromAST)
  , SyntacticVal, InternalType
  , pattern (:$), pattern Lit
  , pattern If, pattern IfM
  , pattern Return
  )
import FIR.Module
  ( Program )
import FIR.Prim.Types
  ( PrimTy )
import FIR.Syntax.AST
  ( )
import FIR.Syntax.Program
  ( )
import Math.Logic.Class
  ( Choose(choose), Choosing )

--------------------------------------------------------------------------------------

when :: forall i. Code Bool -> Program i i (Code ()) -> Program i i (Code ())
when b action
  = choose b
      ( action )
      ( ixPure (Lit ()) :: Program i i (Code ()) )

unless :: forall i. Code Bool -> Program i i (Code ()) -> Program i i (Code ())
unless b action
  = choose b
      ( ixPure (Lit ()) :: Program i i (Code ()) )
      ( action )

--------------------------------------------------------------------------------------
-- | The 'Choose' instances provide syntax for rebindable 'ifThenElse'.
--
-- We uses a helper type class 'Chooser' in an attempt to improve type inference.
instance {-# OVERLAPPABLE #-}
         ( b ~ Bool
         , Syntactic z
         , Chooser x y z (WhichChoice (InternalType z))
         )
       => Choose (Code b) '(x, y, z) where
  choose = chosen @x @y @z @(WhichChoice (InternalType z))
instance ( b ~ Code Bool
         , j ~ i, k ~ i, l ~ i
         , SyntacticVal z
         , x ~ Program i j1 z
         , y ~ Program i j2 z
         )
       => Choose (Program k l b) '(x, y, Program i j z) where
  choose c t f = fromAST ( IfM :$ toAST c :$ toAST t :$ toAST f )
instance ( b ~ Bool
         , j ~ i
         , SyntacticVal z
         , x ~ Program i j1 z
         , y ~ Program i j2 z
         )
       => Choose (Code b) '(x, y, Program i j z) where
  choose c t f = fromAST ( IfM :$ ( Return :$ c ) :$ toAST t :$ toAST f )
instance {-# INCOHERENT #-}
         ( b ~ Program i i (Code Bool)
         , j ~ i
         , SyntacticVal z
         , x ~ Program i j1 z
         , y ~ Program i j2 z
         )
       => Choose b '(x, y, Program i j z) where
  choose c t f = fromAST ( IfM :$ toAST c :$ toAST t :$ toAST f )


data Choice = PureChoice | ImpureChoice
class Chooser x y z (choice :: Choice) where
  chosen :: Choosing (Code Bool) '(x,y,z)
instance ( SyntacticVal z
         , PrimTy (InternalType z)
         , x ~ z, y ~ z
         )
       => Chooser x y z PureChoice where
  chosen = fromAST ( If @(InternalType z) )
instance ( x ~ Program i j1 x'
         , y ~ Program i j2 y'
         , z ~ Program i i  z'
         , SyntacticVal z'
         , PrimTy (InternalType z')
         , x' ~ z'
         , y' ~ z'
         )
      => Chooser x y z ImpureChoice where
  chosen c t f = fromAST ( IfM :$ ( Return :$ c ) :$ toAST t :$ toAST f )

type family WhichChoice (z :: Type) :: Choice where
  WhichChoice ( (_ := _) _ )
    = ImpureChoice
  WhichChoice _
    = PureChoice
