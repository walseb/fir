{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

module CodeGen.AST where

-- base
import Control.Arrow(second)
import Data.Kind(Type)

-- fir
import FIR.AST(AST((:$)))

----------------------------------------------------------------------------
-- existential data types to emulate untyped AST

data AnyAST where
  AnyAST :: AST a -> AnyAST

deriving instance Show AnyAST

data ASTList where
  NilAST  :: ASTList
  SnocAST :: ASTList -> AST a -> ASTList

deriving instance Show ASTList

astListLength :: ASTList -> Int
astListLength NilAST            = 0
astListLength (as `SnocAST` _ ) = 1 + astListLength as

astListHeadTail :: ASTList -> Maybe (AnyAST, ASTList)
astListHeadTail NilAST           = Nothing
astListHeadTail (as `SnocAST` a) = Just (go a as)
    where go :: AST a -> ASTList -> (AnyAST, ASTList)
          go b NilAST           = (AnyAST b, NilAST)
          go b (cs `SnocAST` c) = second (`SnocAST` b) (go c cs)

-- internal data type to deal with run-time indices
-- the user-facing interface is through variadic functions
data ASTIndexList (is :: [Type]) :: Type where
  INil  :: ASTIndexList '[]
  ICons :: AST i -> ASTIndexList is -> ASTIndexList (i ': is)

----------------------------------------------------------------------------
-- pattern for applied function with any number of arguments

pattern Applied :: AST a -> ASTList -> AST b
pattern Applied f as <- (unapply . AnyAST -> (AnyAST f,as))

unapply :: AnyAST -> (AnyAST, ASTList)
unapply (AnyAST (f :$ a))
  = case unapply (AnyAST f) of
        (AnyAST g, as) -> (AnyAST g, as `SnocAST` a)
unapply (AnyAST f) = (AnyAST f, NilAST)
