{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module CodeGen.Application where

-- base
import Control.Arrow
  ( second )
import Data.Kind
  ( Type )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import {-# SOURCE #-} CodeGen.CodeGen
  ( codeGen )
import CodeGen.Instruction
  ( ID )
import CodeGen.Monad
  ( CGMonad )
import Data.Function.Variadic
  ( ListVariadic )
import Data.Type.List
  ( SLength(SZero,SSucc)
  , Snoc
  )
import FIR.AST
  ( AST((:$)) )
import qualified SPIRV.PrimTy as SPIRV

----------------------------------------------------------------------------
-- existential data types to emulate untyped AST

-- UAST = Untyped AST
data UAST where
  UAST :: AST a -> UAST

deriving instance Show UAST

infixl 5 `SnocUAST`

-- snoc list of untyped ASTs
-- this representation matches up with the 'function application' pattern synonym
data UASTs where
  NilUAST  :: UASTs
  SnocUAST :: UASTs -> AST a -> UASTs

deriving instance Show UASTs

uastsLength :: UASTs -> Int
uastsLength NilUAST            = 0
uastsLength (as `SnocUAST` _ ) = 1 + uastsLength as

uastsHeadTail :: UASTs -> Maybe (UAST, UASTs)
uastsHeadTail NilUAST           = Nothing
uastsHeadTail (as `SnocUAST` a) = Just (go a as)
    where go :: AST a -> UASTs -> (UAST, UASTs)
          go b NilUAST           = (UAST b, NilUAST)
          go b (cs `SnocUAST` c) = second (`SnocUAST` b) (go c cs)

----------------------------------------------------------------------------
-- typed list of ASTs

infixr 5 `ConsAST`

data ASTs (is :: [Type]) :: Type where
  NilAST  :: ASTs '[]
  ConsAST :: AST i -> ASTs is -> ASTs (i ': is)

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

pattern Applied :: AST a -> UASTs -> AST b
pattern Applied f as <- (unapply . UAST -> (UAST f,as))

unapply :: UAST -> (UAST, UASTs)
unapply (UAST (f :$ a))
  = case unapply (UAST f) of
        (UAST g, as) -> (UAST g, as `SnocUAST` a)
unapply (UAST f) = (UAST f, NilUAST)

----------------------------------------------------------------------------
-- experimental typed version

pattern Applied' :: AST (ListVariadic as b) -> ASTs as -> AST b
pattern Applied' f as <- (unapply' -> AnApplication f as)

data AnApplication b where
  AnApplication :: AST (ListVariadic as b) -> ASTs as -> AnApplication b

unapply' :: AST b -> AnApplication b
unapply' (f :$ a)
  = case unapply' f of
        AnApplication (g :: AST (ListVariadic as (a ->b))) as
          -> AnApplication (unsafeCoerce g :: AST (ListVariadic (as `Snoc` a) b)) (as `snocAST` a)
unapply' f = AnApplication (unsafeCoerce f :: ListVariadic '[] b) NilAST

snocAST :: ASTs as -> AST a -> ASTs (as `Snoc` a)
snocAST NilAST           a = a `ConsAST` NilAST
snocAST (x `ConsAST` xs) a = unsafeCoerce ( x `ConsAST` (xs `snocAST` a) )

----------------------------------------------------------------------------
-- code generation for the existential AST data types

codeGenUASTs :: UASTs -> CGMonad [ (ID, SPIRV.PrimTy) ]
codeGenUASTs = sequence . reverse . go
    where go :: UASTs -> [ CGMonad (ID, SPIRV.PrimTy) ]
          go NilUAST           = []
          go (as `SnocUAST` a) = codeGen a : go as

codeGenUAST :: UAST -> CGMonad (ID, SPIRV.PrimTy)
codeGenUAST (UAST a) = codeGen a
