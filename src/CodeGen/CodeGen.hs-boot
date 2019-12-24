{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module CodeGen.CodeGen where

-- base
import Data.Kind
  ( Type )

-- fir
import CodeGen.Application
  ( Application )
import CodeGen.Instruction
  ( ID )
import CodeGen.Monad
  ( CGMonad )
import FIR.AST.Type
  ( AugType, Nullary )
import qualified SPIRV.PrimTy as SPIRV

----------------------------------------------------------------------------
-- export the 'CodeGen' type class and instance for the AST
-- this allows auxiliary code generation code to recursively call 'codeGen'

class CodeGen (ast :: AugType -> Type) where
  codeGenArgs :: Nullary r => Application ast f r -> CGMonad (ID, SPIRV.PrimTy)

codeGen :: ( CodeGen f, Nullary a ) => f a -> CGMonad (ID, SPIRV.PrimTy)
