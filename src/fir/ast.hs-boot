{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RoleAnnotations #-}

module FIR.AST(AST) where

data AST a where
type role AST nominal