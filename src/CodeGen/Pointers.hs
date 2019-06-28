{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

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

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.Composite
  ( accessedTy )
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
  , note
  )
import CodeGen.State
  ( PointerState(Fresh)
  , _functionContext
  , _interfaceBinding
  , _localVariable
  , _temporaryPointer
  , _knownConstants
  )
import FIR.ASTState
  ( FunctionContext(TopLevel, InEntryPoint) )
import FIR.Prim.Singletons
  ( SPrimTy(..), SScalarTy(..)
  , PrimTy(primTySing), ScalarTy(scalarTySing)
  , AConstant(AConstant)
  )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
import qualified SPIRV.Stage     as SPIRV
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
accessChain :: (ID, SPIRV.PointerTy) -> Indices -> CGMonad (ID, SPIRV.PointerTy)
accessChain (basePtrID, SPIRV.PointerTy storage baseTy) indices
  = do
      (safe, is) <- indicesIDs indices
      -- We need to compute the accessee type from the pointer,
      -- because creating it from scratch using typeID risks creating
      -- an incorrectly decorated object.
      -- (Remember that, for instance, differently decorated structs are incompatible in SPIR-V,
      -- even though they appear identical on the Haskell side.)
      eltTy <-
        note ( "'accessChain': could not compute accessee type.\n\
               \base: " <> ShortText.pack (show baseTy) <> "\n\
               \indices: " <> ShortText.pack (show indices) <> "."
             )
          =<< ( flip accessedTy baseTy <$> reverseLookupIndices indices )
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


reverseLookupIndices :: Indices -> CGMonad [ Maybe Word32 ]
reverseLookupIndices (CTInds   is) = pure (map Just is)
reverseLookupIndices (RTInds _ is) = traverse reverseConstantLookup is

reverseConstantLookup :: ID -> CGMonad (Maybe Word32)
reverseConstantLookup c = do
  lookups <- filter ( (== Just c) . resID . snd ) . Map.assocs <$> use _knownConstants
  case lookups of
    ( (AConstant (a :: ty), _) : _ )
      -> case primTySing @ty of
            SScalar ->
              case scalarTySing @ty of
                SWord8  -> pure . Just $ fromIntegral a
                SWord16 -> pure . Just $ fromIntegral a
                SWord32 -> pure . Just $ fromIntegral a
                SWord64 -> pure . Just $ fromIntegral a
                SInt8   -> pure . Just $ fromIntegral a
                SInt16  -> pure . Just $ fromIntegral a
                SInt32  -> pure . Just $ fromIntegral a
                SInt64  -> pure . Just $ fromIntegral a
                _       -> pure Nothing
            _ -> pure Nothing
    _ -> pure Nothing

----------------------------------------------------------------------------
-- load/store through pointers

load :: (ShortText, ID) -> SPIRV.PointerTy -> CGMonad (ID, SPIRV.PrimTy)
load (loadeeName, loadeeID) (SPIRV.PointerTy storage ty)
  = do
      context <- use _functionContext
      case context of
        TopLevel -> throwError "codeGen: load operation not allowed at top level"
        InEntryPoint entryPointName stageInfo _
          | storage == Storage.Input
          -- add this variable to the interface of the entry point
          -> assign
                ( _interfaceBinding entryPointName (SPIRV.modelOf stageInfo) loadeeName )
                ( Just loadeeID )
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

store :: (ShortText, ID) -> ID -> SPIRV.PointerTy -> CGMonad ()
store (storeeName, storeeID) pointerID (SPIRV.PointerTy storage _)
  = do
      context <- use _functionContext
      case context of
        TopLevel -> throwError "codeGen: store operation not allowed at top level"
        InEntryPoint entryPointName stageInfo _
          | storage == Storage.Output
          -- add this variable to the interface of the entry point
          -> assign
                ( _interfaceBinding entryPointName (SPIRV.modelOf stageInfo) storeeName )
                ( Just pointerID )
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