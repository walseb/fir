{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module CodeGen.Pointers
  ( Safeness(Safe, Unsafe)
  , Indices(RTInds, CTInds)
  , newVariable, temporaryVariable, declareVariable
  , accessChain
  , load, loadInstruction
  , store, storeInstruction
  ) where

-- base
import Data.Word
  ( Word32 )

-- containers
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except
  ( throwError )

-- lens
import Control.Lens
  ( use, assign )

-- text-utf8
import Data.Text
  ( Text )

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.IDs
  ( typeID, constID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad
  , MonadFresh(fresh)
  , liftPut
  , tryToUse
  )
import CodeGen.State
  ( FunctionContext(TopLevel, EntryPoint)
  , PointerState(Fresh)
  , _functionContext
  , _interfaceBinding
  , _localVariable
  , _temporaryPointer
  )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
import qualified SPIRV.Storage   as Storage

----------------------------------------------------------------------------
-- safeness of pointer access (whether indices known to be within bounds)

data Safeness
  = Unsafe
  | Safe
  deriving ( Eq, Show )

instance Semigroup Safeness where
  Safe <> x = x
  _    <> _ = Unsafe

data Indices
  = CTInds [Word32]      -- compile-time indices
  | RTInds Safeness [ID] -- run-time indices
  deriving Show

----------------------------------------------------------------------------
-- creating pointers

newVariable :: SPIRV.PointerTy -> CGMonad ID
newVariable ptrTy
  = do
      v <- fresh
      assign ( _localVariable v ) (Just ptrTy)
      pure v

temporaryVariable :: ID -> SPIRV.PointerTy -> CGMonad (ID, PointerState)
temporaryVariable baseID ptrTy
  = tryToUse ( _temporaryPointer baseID )
      id
      ( \v ->
          do
            assign ( _localVariable v ) (Just ptrTy)
            storeInstruction v baseID
            pure (v, Fresh)
      )

declareVariable :: ID -> SPIRV.PointerTy -> CGMonad ()
declareVariable v ptrTy@(SPIRV.PointerTy storage _)
  = do  ptrTyID <- typeID (SPIRV.pointerTy ptrTy)
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Variable
            , resTy     = Just ptrTyID
            , resID     = Just v
            , args      = Arg storage EndArgs
            }

indicesIDs :: Indices -> CGMonad ( Safeness, [ID] )
indicesIDs (CTInds ws)
  = ( Safe, ) <$> traverse constID ws
indicesIDs (RTInds safe is) = pure ( safe, is )

-- create a pointer into a composite object with a list of successive indices
accessChain :: (ID, SPIRV.PointerTy) -> SPIRV.PrimTy -> Indices -> CGMonad (ID, SPIRV.PointerTy)
accessChain (basePtrID, SPIRV.PointerTy storage _) eltTy indices
  = do
      (safe, is) <- indicesIDs indices
      let opAccessChain
            = case safe of
                  Safe -> SPIRV.Op.InBoundsAccessChain
                  _    -> SPIRV.Op.AccessChain
          accessPtrTy = SPIRV.PointerTy storage eltTy
      accessPtrTyID <- typeID (SPIRV.pointerTy accessPtrTy)
      v <- fresh
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = opAccessChain
          , resTy     = Just accessPtrTyID
          , resID     = Just v
          , args      = Arg basePtrID
                      $ toArgs is
          }
      pure (v, accessPtrTy)

----------------------------------------------------------------------------
-- load/store through pointers

load :: (Text, ID) -> SPIRV.PointerTy -> CGMonad (ID, SPIRV.PrimTy)
load (loadeeName, loadeeID) (SPIRV.PointerTy storage ty)
  = do
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
store (storeeName, storeeID) pointerID (SPIRV.PointerTy storage _)
  = do
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
  = liftPut $ putInstruction Map.empty
      Instruction
        { operation = SPIRV.Op.Store
        , resTy = Nothing
        , resID = Nothing
        , args = Arg pointerID
               $ Arg storeeID EndArgs
        }