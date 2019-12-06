{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

{-|
Module: CodeGen.Application

Functionality for keeping track of function arguments, such as patterns for the application of a function to several arguments.
-}

module CodeGen.Application
  ( UAST(UAST), UASTs(NilUAST, SnocUAST)
  , ASTs(NilAST, ConsAST)
  , traverseASTs, astsLength
  , pattern UApplied
  , unsafeRetypeUASTs
  , pattern Applied
  )
  where

-- base
import Data.Kind
  ( Type )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Data.Function.Variadic
  ( ListVariadic )
import Data.Type.List
  ( SLength(SZero,SSucc)
  , Snoc
  )
import FIR.AST
  ( AST((:$)) )

----------------------------------------------------------------------------
-- existential data types to emulate untyped AST

-- UAST = Untyped AST
data UAST where
  UAST :: AST a -> UAST
deriving stock instance Show UAST

infixl 5 `SnocUAST`

-- snoc list of untyped ASTs
-- this representation matches up with the 'function application' pattern synonym
data UASTs where
  NilUAST  :: UASTs
  SnocUAST :: UASTs -> AST a -> UASTs
deriving stock instance Show UASTs

----------------------------------------------------------------------------
-- typed list of ASTs

infixr 5 `ConsAST`
infixl 5 `SnocAST`

data ASTs (is :: [Type]) :: Type where
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

foldrASTs :: (forall a. AST a -> b -> b) -> b -> ASTs as -> b
foldrASTs _ b0 NilAST           = b0
foldrASTs f b0 (a `ConsAST` as) = f a ( foldrASTs f b0 as )

astsLength :: ASTs is -> Int
astsLength = foldrASTs (const succ) 0

traverseASTs :: Applicative f => (forall a. AST a -> f b) -> ASTs as -> f [b]
traverseASTs _ NilAST           = pure []
traverseASTs f (a `ConsAST` as) = (:) <$> f a <*> traverseASTs f as


-- unfortunate workaround to obtain optic indices using untyped machinery
unsafeRetypeUASTs :: forall as. SLength as -> UASTs -> Maybe (ASTs as)
unsafeRetypeUASTs lg as
  | correctLength lg as = Just ( unsafeRetypeAcc as NilAST )
  | otherwise = Nothing
  where unsafeRetypeAcc :: UASTs -> ASTs bs -> ASTs as
        unsafeRetypeAcc NilUAST bs = unsafeCoerce bs
        unsafeRetypeAcc (xs `SnocUAST` x) bs = unsafeRetypeAcc xs (x `ConsAST` bs)
        correctLength :: SLength xs -> UASTs -> Bool
        correctLength SZero NilUAST = True
        correctLength (SSucc l) (xs `SnocUAST` _) = correctLength l xs
        correctLength _ _ = False

----------------------------------------------------------------------------
-- pattern for applied function with any number of arguments (untyped)

pattern UApplied :: AST a -> UASTs -> AST b
pattern UApplied f as <- (unapplyU . UAST -> (UAST f,as))

unapplyU :: UAST -> (UAST, UASTs)
unapplyU (UAST (f :$ a))
  = case unapplyU (UAST f) of
        (UAST g, as) -> (UAST g, as `SnocUAST` a)
unapplyU (UAST f) = (UAST f, NilUAST)

----------------------------------------------------------------------------
-- typed version

pattern Applied :: AST (ListVariadic as b) -> ASTs as -> AST b
pattern Applied f as <- (unapply -> AnApplication f as)

data AnApplication b where
  AnApplication :: AST (ListVariadic as b) -> ASTs as -> AnApplication b

unapply :: AST b -> AnApplication b
unapply (f :$ a)
  = case unapply f of
        AnApplication (g :: AST (ListVariadic as (a ->b))) as
          -> AnApplication (unsafeCoerce g :: AST (ListVariadic (as `Snoc` a) b)) (as `SnocAST` a)
unapply f = AnApplication (unsafeCoerce f :: ListVariadic '[] b) NilAST
