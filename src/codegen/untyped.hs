{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

module CodeGen.Untyped where

-- base
import Control.Arrow(second)

-- fir
import {-# SOURCE #-} CodeGen.CodeGen(codeGen)
import CodeGen.Instruction(ID)
import CodeGen.Monad(CGMonad)
import FIR.AST(AST((:$)))
import qualified SPIRV.PrimTy as SPIRV

----------------------------------------------------------------------------
-- existential data types to emulate untyped AST

-- UAST = Untyped AST
data UAST where
  UAST :: AST a -> UAST

deriving instance Show UAST

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
-- pattern for applied function with any number of arguments

pattern Applied :: AST a -> UASTs -> AST b
pattern Applied f as <- (unapply . UAST -> (UAST f,as))

unapply :: UAST -> (UAST, UASTs)
unapply (UAST (f :$ a))
  = case unapply (UAST f) of
        (UAST g, as) -> (UAST g, as `SnocUAST` a)
unapply (UAST f) = (UAST f, NilUAST)

----------------------------------------------------------------------------
-- code generation for the existential AST data types

codeGenUASTs :: UASTs-> CGMonad [ (ID, SPIRV.PrimTy)  ]
codeGenUASTs = sequence . reverse . go
    where go :: UASTs -> [ CGMonad (ID, SPIRV.PrimTy) ]
          go NilUAST           = []
          go (as `SnocUAST` a) = codeGen a : go as

codeGenUAST :: UAST -> CGMonad (ID, SPIRV.PrimTy)
codeGenUAST (UAST a) = codeGen a
