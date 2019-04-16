module CodeGen.PrimOps (primOp) where

-- base
import Control.Monad
  ( void )

-- containers
import qualified Data.Map as Map

-- lens
import Control.Lens
  ( use )

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.IDs
  ( typeID, extInstID )
import CodeGen.Instruction
  ( Args(EndArgs), toArgs
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
primOp prim as
  -- unfortunate special case for geometry primitive operations
  | SPIRV.GeomOp _ <- prim = do
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = op
          , resTy     = Nothing
          , resID     = Nothing
          , args      = EndArgs
          }
      pure ( ID 0, SPIRV.Unit ) -- ID should not be used
  -- main case
  | otherwise = do
      case op of
        SPIRV.Op.ExtCode extInst _
          -> void (extInstID extInst)
        _ -> pure ()

      extInsts <- use _knownExtInsts

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

    where (op,retTy) = SPIRV.opAndReturnType prim
