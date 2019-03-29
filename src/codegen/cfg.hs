module CodeGen.CFG where

-- containers
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens
  ( assign )

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.Instruction
  ( Args(Arg, EndArgs)
  , ID, Instruction(..)
  )
import CodeGen.Monad
  ( MonadFresh(fresh)
  , CGMonad, liftPut
  )
import CodeGen.State
  ( _currentBlock )
import qualified SPIRV.Operation as SPIRV.Op

----------------------------------------------------------------------------
-- blocks and branching

block :: ID -> CGMonad ()
block blockID = do
  liftPut $ putInstruction Map.empty
    Instruction
      { operation = SPIRV.Op.Label
      , resTy = Nothing
      , resID = Just blockID
      , args  = EndArgs
      }
  assign _currentBlock (Just blockID)

newBlock :: CGMonad ()
newBlock = fresh >>= block

branch :: ID -> CGMonad ()
branch branchID
  = liftPut $ putInstruction Map.empty
      Instruction
       { operation = SPIRV.Op.Branch
       , resID = Nothing
       , resTy = Nothing
       , args  = Arg branchID EndArgs
       }

branchConditional :: ID -> ID -> ID -> CGMonad ()
branchConditional b t f
  = liftPut $ putInstruction Map.empty
      Instruction
        { operation = SPIRV.Op.BranchConditional
        , resTy = Nothing
        , resID = Nothing
        , args  = Arg b
                $ Arg t
                $ Arg f EndArgs
        }
