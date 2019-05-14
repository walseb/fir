{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies    #-}

module FIR.AST
  ( AST
  , Syntactic(Internal, toAST, fromAST)
  )
  where

data AST a
type role AST nominal

class Syntactic a where
  type Internal a
  toAST :: a -> AST (Internal a)
  fromAST :: AST (Internal a) -> a
