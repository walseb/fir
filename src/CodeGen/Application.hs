{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

{-|
Module: CodeGen.Application

Functionality for keeping track of function arguments, such as patterns for the application of a function to several arguments.
-}

module CodeGen.Application
  ( UAST(..), UCode(..)
  , ASTs(.., SnocAST)
  , Codes, pattern ConsCode
  , Application(.., Nullary)
  , traverseASTs
  ) where

-- base
import Data.Kind
  ( Type )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Data.Constraint.All
  ( All(allDict)
  , AllDict(ConsDict)
  )
import Data.Type.List
  ( Head, Tail, Snoc )
import FIR.AST
  ( AST, Code )
import FIR.AST.Type
  ( AugType, FunTys, Nullary, MapVal )

----------------------------------------------------------------------------
-- existential data types to emulate untyped AST

-- UAST = Untyped AST
data UAST where
  UAST :: AST a -> UAST
deriving stock instance Show UAST

data UCode where
  UCode :: Nullary a => AST a -> UCode
deriving stock instance Show UCode

----------------------------------------------------------------------------
-- typed list of ASTs

infixr 5 `ConsAST`
infixl 5 `SnocAST`

data ASTs (is :: [AugType]) :: Type where
  NilAST  :: ASTs '[]
  ConsAST :: AST i -> ASTs is -> ASTs (i ': is)

{-# COMPLETE SnocAST #-}
pattern SnocAST :: ASTs as -> AST a -> ASTs (as `Snoc` a)
pattern SnocAST as a <- ( unsnocAST -> (as, a) )
  where
    SnocAST NilAST           a = a `ConsAST` NilAST
    SnocAST (x `ConsAST` xs) a = unsafeCoerce ( x `ConsAST` (xs `SnocAST` a) )
      --                          ^^^^
      -- should be able to get rid of this @unsafeCoerce@

unsnocAST :: ASTs (as `Snoc` a) -> ( ASTs as, AST a )
unsnocAST NilAST               = error "impossible"
unsnocAST (b `ConsAST` NilAST) = unsafeCoerce ( NilAST, b )
unsnocAST (b `ConsAST` bs    ) =
  case unsnocAST ( unsafeCoerce bs ) of
    ( cs, c ) -> ( unsafeCoerce (b `ConsAST` cs), c )

type Codes as = ASTs (MapVal as)

data Application (ast :: AugType -> Type) (f :: AugType) (r :: AugType) where
  Applied :: FunTys f as r => ast f -> ASTs as -> Application ast f r

pattern Nullary :: FunTys f '[] r => ast f -> Application ast f r
pattern Nullary v = Applied v NilAST

traverseASTs
  :: forall c f as b
  .  ( Applicative f, All c as )
  => ( forall a. c a => AST a -> f b )
  -> ASTs as
  -> f [b]
traverseASTs f as = go allDict as
  where
    go :: AllDict c xs -> ASTs xs -> f [b]
    go _        NilAST           = pure []
    go ConsDict (x `ConsAST` xs) = (:) <$> f x <*> go allDict xs

----------------------------------------------------------------------------
-- Workaround for pattern matching not correctly propagating type family injectivity annotations
-- (See [GHC trac #16436](https://gitlab.haskell.org/ghc/ghc/issues/16436).)

infixr 5 `ConsCode`

pattern ConsCode :: () => ( as ~ ( Head as ': Tail as ) )
                 => Code (Head as) -> Codes (Tail as) -> Codes as
pattern ConsCode b bs <- ( getConsCode -> Just (GetConsCode b bs) )
  where
    ConsCode b bs = ConsAST b bs

data GetConsCode as where
  GetConsCode :: ( as ~ (Head as ': Tail as) ) => Code (Head as) -> Codes (Tail as) -> GetConsCode as

getConsCode :: forall as. Codes as -> Maybe (GetConsCode as)
getConsCode NilAST         = Nothing
getConsCode (ConsAST a as) = case unsafeCoerce Refl :: ( as :~: (Head as ': Tail as) ) of
  Refl -> Just (GetConsCode a as)
