module CodeGen.CodeGen (codeGen) where

-- fir
import CodeGen.Instruction
  ( ID )
import CodeGen.Monad
  ( CGMonad )
import FIR.AST
  ( AST )
import qualified SPIRV.PrimTy as SPIRV

----------------------------------------------------------------------------
-- export the 'codeGen' function
-- this allows auxiliary code generation code to recursively call 'codeGen'

codeGen :: AST a -> CGMonad (ID, SPIRV.PrimTy)
