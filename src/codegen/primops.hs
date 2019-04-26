module CodeGen.PrimOps (primOp) where

-- base
import Control.Monad
  ( void )

-- lens
import Control.Lens
  ( use )

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.IDs
  ( typeID, extInstID )
import CodeGen.Instruction
  ( toArgs
  , ID(ID)
  , Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad
  , MonadFresh(fresh)
  , liftPut )
import CodeGen.State
  ( _knownExtInsts )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimOp    as SPIRV
import qualified SPIRV.PrimTy    as SPIRV

----------------------------------------------------------------------------
-- primops

primOp :: SPIRV.PrimOp -> [ (ID, SPIRV.PrimTy) ] -> CGMonad (ID, SPIRV.PrimTy)
primOp prim as =
  let (op,retTy) = SPIRV.opAndReturnType prim
  in do
      case op of
        SPIRV.Op.ExtCode extInst _
          -> void (extInstID extInst) -- ensure extended instruction set is declared
        _ -> pure ()

      extInsts <- use _knownExtInsts

      case retTy of
        SPIRV.Unit -> do
          liftPut $ putInstruction extInsts
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
          liftPut $ putInstruction extInsts
            Instruction
              { operation = op
              , resTy = Just resTyID
              , resID = Just v
              , args = toArgs (map fst as)
              }
          pure (v, retTy)
