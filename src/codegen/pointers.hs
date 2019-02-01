{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Pointers
  ( Safeness(Safe, Unsafe)
  , newPointer, accessChain
  , load, loadInstruction
  , store, storeInstruction
  ) where

-- containers
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except(throwError)

-- lens
import Control.Lens(use, assign)

-- text-utf8
import Data.Text(Text)

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad
  , MonadFresh(fresh)
  , liftPut
  )
import CodeGen.State
  ( FunctionContext(TopLevel, EntryPoint)
  , _functionContext
  , _interfaceBinding
  )
import qualified SPIRV.Operation       as SPIRV.Op
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.Storage         as Storage

----------------------------------------------------------------------------
-- safeness of pointer access (whether indices known to be within bounds)

data Safeness
  = Unsafe
  | Safe
  deriving ( Eq, Show )

instance Semigroup Safeness where
  Safe <> x = x
  _    <> _ = Unsafe

----------------------------------------------------------------------------
-- creating new pointers

newPointer :: SPIRV.PointerTy -> CGMonad ID
newPointer ptrTy@(SPIRV.PointerTy storage _)
  = do  ptrTyID <- typeID (SPIRV.pointerTy ptrTy) -- ensure the pointer type is declared
        v <- fresh
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Variable
            , resTy     = Just ptrTyID
            , resID     = Just v
            , args      = Arg storage EndArgs
            }
        pure v

-- create a pointer into a composite object with a list of successive indices
accessChain :: Safeness -> (ID, SPIRV.PointerTy) -> SPIRV.PrimTy -> [ID] -> CGMonad (ID, SPIRV.PrimTy)
accessChain safe (basePtrID, SPIRV.PointerTy storage _) eltTy indices
  = let opAccessChain
          = case safe of
                Safe -> SPIRV.Op.InBoundsAccessChain
                _    -> SPIRV.Op.AccessChain
        accessPtrTy = SPIRV.Pointer storage eltTy
    in do
      accessPtrTyID <- typeID accessPtrTy
      v <- fresh
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = opAccessChain
          , resTy     = Just accessPtrTyID
          , resID     = Just v
          , args      = Arg basePtrID
                      $ toArgs indices
          }
      pure (v, accessPtrTy)

----------------------------------------------------------------------------
-- load/store through pointers

load :: (Text, ID) -> SPIRV.PointerTy -> CGMonad (ID, SPIRV.PrimTy)
load (loadeeName, loadeeID) ptrTy@(SPIRV.PointerTy storage ty)
  = do
      _ <- typeID (SPIRV.pointerTy ptrTy) -- ensure the pointer type is declared
      context <- use _functionContext
      case context of
        TopLevel -> throwError "codeGen: load operation not allowed at top level"
        EntryPoint stage entryPointName
          | storage == Storage.Input
          -- add this variable to the interface of the entry point
          -> assign ( _interfaceBinding stage entryPointName loadeeName ) (Just loadeeID)
        _ -> pure ()
      loadInstruction ty loadeeID

loadInstruction :: SPIRV.PrimTy -> ID -> CGMonad (ID, SPIRV.PrimTy)
loadInstruction ty loadeeID
  = do  tyID <- typeID ty
        v <- fresh
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Load
            , resTy = Just tyID
            , resID = Just v
            , args  = Arg loadeeID EndArgs
            }
        pure (v, ty)

store :: (Text, ID) -> ID -> SPIRV.PointerTy -> CGMonad ()
store (storeeName, storeeID) pointerID ptrTy@(SPIRV.PointerTy storage _)
  = do
      _ <- typeID (SPIRV.pointerTy ptrTy) -- ensure the pointer type is declared
      context <- use _functionContext
      case context of
        TopLevel -> throwError "codeGen: store operation not allowed at top level"
        EntryPoint stage entryPointName
          | storage == Storage.Output
          -- add this variable to the interface of the entry point
          -> assign ( _interfaceBinding stage entryPointName storeeName ) (Just pointerID)
        _ -> pure ()
      storeInstruction pointerID storeeID

storeInstruction :: ID -> ID -> CGMonad ()
storeInstruction pointerID storeeID
  = do  liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Store
            , resTy = Nothing
            , resID = Nothing
            , args = Arg pointerID
                   $ Arg storeeID EndArgs
            }