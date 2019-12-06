{-|
Module: CodeGen.PrimOps

Code generation for primitive operations (e.g. arithmetic operations).
-}

module CodeGen.PrimOps (primOp) where

-- base
import Control.Monad
  ( void )

-- lens
import Control.Lens
  ( view )

-- fir
import CodeGen.Binary
  ( instruction )
import CodeGen.IDs
  ( typeID, extInstID )
import CodeGen.Instruction
  ( toArgs
  , ID(ID)
  , Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad, MonadFresh(fresh) )
import CodeGen.State
  ( _backend )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimOp    as SPIRV
import qualified SPIRV.PrimTy    as SPIRV

----------------------------------------------------------------------------
-- primops

primOp :: SPIRV.PrimOp -> [ (ID, SPIRV.PrimTy) ] -> CGMonad (ID, SPIRV.PrimTy)
primOp prim as = do

  bk <- view _backend
  let (op, retTy) = SPIRV.opAndReturnType bk prim

  case op of
    SPIRV.Op.ExtCode extInst _
      -> void (extInstID extInst) -- ensure extended instruction set is declared
    _ -> pure ()

  case retTy of
    SPIRV.Unit -> do
      instruction
        Instruction
          { operation = op
          , resTy = Nothing
          , resID = Nothing
          , args = toArgs (map fst as)
          }
      pure (ID 0, retTy) -- ID should not be used
    _ -> do
      resTyID <- typeID retTy
      v <- fresh
      instruction
        Instruction
          { operation = op
          , resTy = Just resTyID
          , resID = Just v
          , args = toArgs (map fst as)
          }
      pure (v, retTy)
