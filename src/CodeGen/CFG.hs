{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module CodeGen.CFG
  ( newBlock
  , selection, ifM, switch, while, locally
  )
  where

-- base
import Control.Arrow
  ( first )
import Control.Monad
  ( when, unless, forM )
import Data.Word
  ( Word32 )

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens
  ( use, assign, modifying )

-- mtl
import Control.Monad.Except
  ( throwError )
import Control.Monad.Reader
  ( ask )
import Control.Monad.State
  ( get, put )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Application
  ( UAST(UAST) )
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( codeGen )
import CodeGen.Debug
  ( whenAsserting )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( Args(Arg, EndArgs), toArgs
  , ID(ID), Instruction(..)
  , Pair(Pair)
  )
import CodeGen.Monad
  ( MonadFresh(fresh)
  , CGMonad, runCGMonad
  , note
  )
import CodeGen.Phi
  ( phiInstruction, phiInstructions )
import CodeGen.State
  ( CGState(currentBlock, localBindings)
  , _currentBlock
  , _localBindings, _localBinding
  )
import FIR.AST
  ( AST )
import FIR.Prim.Singletons
  ( IntegralTy )
import qualified SPIRV.Control   as SPIRV
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
  ( PrimTy(Unit, Boolean) )

----------------------------------------------------------------------------
-- blocks and branching

block :: ID -> CGMonad ()
block blockID = do
  instruction
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
  = instruction
      Instruction
       { operation = SPIRV.Op.Branch
       , resID = Nothing
       , resTy = Nothing
       , args  = Arg branchID EndArgs
       }

branchConditional :: ID -> ID -> ID -> CGMonad ()
branchConditional b t f
  = instruction
      Instruction
        { operation = SPIRV.Op.BranchConditional
        , resTy = Nothing
        , resID = Nothing
        , args  = Arg b
                $ Arg t
                $ Arg f EndArgs
        }

select :: ID -> (ID, SPIRV.PrimTy) -> (ID, SPIRV.PrimTy) -> CGMonad (ID, SPIRV.PrimTy)
select c (o1, ty1) (o2, ty2) = do
  ( whenAsserting . when (ty1 /= ty2) )
    ( throwError
      ( "select: objects of different types: \n\
        \object 1: " <> ShortText.pack (show ty1) <> "\n\
        \object 2: " <> ShortText.pack (show ty2) <> "."
      )
    )
  v <- fresh
  resTyID <- typeID ty1
  instruction
    Instruction
      { operation = SPIRV.Op.Select
      , resTy     = Just resTyID
      , resID     = Just v
      , args      = Arg c
                  $ Arg o1
                  $ Arg o2
                  EndArgs
      }
  pure (v, ty1)

selectionMerge :: ID -> Maybe SPIRV.SelectionControl -> CGMonad ()
selectionMerge mergeBlockID mbControl
  = instruction
      Instruction
        { operation = SPIRV.Op.SelectionMerge
        , resTy     = Nothing
        , resID     = Nothing
        , args      = Arg mergeBlockID
                    $ Arg mbControl
                    EndArgs
        }
loopMerge :: ID -> ID -> SPIRV.LoopControl Word32 -> CGMonad ()
loopMerge mergeBlockID loopBlockID loopControl
  = instruction
      Instruction
         { operation = SPIRV.Op.LoopMerge
         , resTy = Nothing
         , resID = Nothing
         , args  = Arg mergeBlockID
                 $ Arg loopBlockID
                 $ Arg loopControl
                 EndArgs
         }

multiWaySwitch :: IntegralTy s => ID -> ID -> [Pair s ID] -> CGMonad ()
multiWaySwitch scrut defBlock ids
  = instruction
      Instruction
        { operation = SPIRV.Op.Switch
        , resTy     = Nothing
        , resID     = Nothing
        , args      = Arg scrut
                    $ Arg defBlock
                    $ toArgs ids
        }

----------------------------------------------------------------------------
-- code generation for selections (pure or branching)

selection :: AST Bool -> AST a -> AST a -> CGMonad (ID, SPIRV.PrimTy)
selection cond x y = do
  (condID, condTy) <- codeGen cond
  ( whenAsserting . unless ( condTy == SPIRV.Boolean) )
    ( throwError
    $  "codeGen: 'select' expected boolean conditional, but got "
    <> ShortText.pack (show condTy)
    )
  o1 <- codeGen x
  o2 <- codeGen y
  select condID o1 o2

ifM :: AST b -> AST t -> AST f -> CGMonad (ID, SPIRV.PrimTy)
ifM cond bodyTrue bodyFalse
  = branchingSelection conditionalHeader [UAST bodyTrue, UAST bodyFalse]
      where
        conditionalHeader :: [ID] -> ID -> CGMonad ()
        conditionalHeader [trueBlockID, falseBlockID] mergeBlockID = do
          (condID, condTy) <- locally (codeGen cond)
          ( whenAsserting . unless ( condTy == SPIRV.Boolean ) )
            ( throwError
            $  "codeGen: 'if' expected boolean conditional, but got "
            <> ShortText.pack (show condTy)
            )
          selectionMerge mergeBlockID SPIRV.NoSelectionControl
          branchConditional condID trueBlockID falseBlockID
        conditionalHeader ids _ =
          throwError
            ( "codeGen: if statement with "
            <> ShortText.pack ( show ( length ids ))
            <> " branches"
            )

switch :: IntegralTy t => AST s -> AST a -> [(t, UAST)] -> CGMonad (ID, SPIRV.PrimTy)
switch scrut def cases
  = branchingSelection switchHeader (UAST def : map snd cases)
      where
        switchHeader :: [ID] -> ID -> CGMonad ()
        switchHeader [] _ = whenAsserting (throwError "codeGen: 'switch' statement missing default case")
        switchHeader ( defaultBlockID : caseBlockIDs ) mergeBlockID = do
          (scrutID, _) <- locally (codeGen scrut)
          selectionMerge mergeBlockID SPIRV.NoSelectionControl
          multiWaySwitch scrutID defaultBlockID
            (zipWith ( \(t,_) i -> Pair (t,i) ) cases caseBlockIDs)

branchingSelection :: ( [ID] -> ID -> CGMonad () ) -> [ UAST ] -> CGMonad (ID, SPIRV.PrimTy)
branchingSelection mkHeader cases = do
  headerBlockID  <- fresh
  caseBlockIDs   <- traverse (\x -> (,x) <$> fresh) cases
  mergeBlockID   <- fresh
  bindingsBefore <- use _localBindings
  branch headerBlockID

  -- header block
  block headerBlockID
  mkHeader ( map fst caseBlockIDs ) mergeBlockID

  -- case blocks
  (bodies, endBlocks, endBindings) <- unzip3 <$>
    forM caseBlockIDs \ (caseBlockID, UAST caseBody) -> do
      block caseBlockID
      assign _localBindings bindingsBefore
      bodyRes <- codeGen caseBody
      bodyEndBlock <- note ( "codeGen: branch statement escaped CFG" )
        =<< use _currentBlock
      bodyBindings <- use _localBindings
      branch mergeBlockID
      pure (bodyRes, bodyEndBlock, bodyBindings)

  -- reset local bindings to what they were outside the branches
  assign _localBindings bindingsBefore

  -- merge block
  block mergeBlockID
  phiBindings <- phiInstructions ( `Map.member` bindingsBefore ) endBlocks endBindings
  -- update local bindings to refer to the phi instructions
  Map.traverseWithKey
    ( \ bdName bdID -> modifying ( _localBinding bdName ) ( fmap $ first (const bdID) ) )
    phiBindings

  -- return the result
  resType <- case map snd bodies of
    [] -> throwError "codeGen: empty list of branches in conditional"
    (ty : tys)
      -> do ( whenAsserting . unless ( all (== ty) tys ) )
              ( throwError "codeGen: branches of conditional return different types" )
            pure ty
  if resType == SPIRV.Unit
  then pure (ID 0, resType) -- ID should never be used
  else do
    res <- fresh
    phiInstruction
      ( res, resType )
      ( zipWith ( \(t,_) i -> Pair (t,i)) bodies endBlocks )
    pure (res, resType)

----------------------------------------------------------------------------
-- code generation for "while" loop

while :: AST c -> AST b -> CGMonad (ID, SPIRV.PrimTy)
while cond loopBody = do
  beforeBlockID <- note ( "codeGen: while loop outside of a block" )
                   =<< use _currentBlock
  headerBlockID <- fresh
  loopBlockID   <- fresh
  mergeBlockID  <- fresh -- block where control flow merges back
  branch headerBlockID

  -- Need to perform code generation for the loop block first,
  -- as we need to know which phi instructions to put in the header.
  -- However, the loop block (also called the continue block)
  -- needs to appear after the header block in the CFG.
  ctxt  <- ask
  state <- get
  let bindingsBefore :: Map ShortText (ID, SPIRV.PrimTy)
      bindingsBefore  = localBindings state

      -- The first CGState is the one we pass manually and that we want.
      -- The second CGState has the wrong "currentBlock" information,
      -- because we branched to the header block at the end.
      loopGenOutput :: Either ShortText (CGState, CGState, ByteString)
      loopGenOutput
        = runCGMonad ctxt state
            do  block loopBlockID
                _ <- codeGen loopBody
                endState <- get
                branch headerBlockID
                pure endState

  (loopEndState, _) -- (loopEndState,loopBodyASM)
    <- case loopGenOutput of
          Left  err     -> throwError err
          Right (s,_,a) -> pure (s,a)
  let mbLoopEndBlockID = currentBlock  loopEndState
      loopEndBindings  = localBindings loopEndState
  loopEndBlockID <- note ( "codeGen: while loop escaped CFG" )
                    mbLoopEndBlockID

  -- Update the state to be the state at the end of the loop
  -- (e.g. don't forget about new constants that were defined),
  -- but reset bindings to what they were before the loop block.
  -- This is because all bindings within the loop remain local to it.
  put loopEndState
  assign _localBindings bindingsBefore

  -- header block
  block headerBlockID
  phiLocalBindings <-
    phiInstructions
      ( const True )
      [ beforeBlockID , loopEndBlockID  ]
      [ bindingsBefore, loopEndBindings ] -- need loopEndBindings

  -- update local bindings to refer to the phi instructions
  Map.traverseWithKey
    ( \ bdName bdID -> modifying ( _localBinding bdName ) ( fmap $ first (const bdID) ) )
    phiLocalBindings
  updatedLocalBindings <- use _localBindings
  (condID, condTy) <- locally (codeGen cond)
  updatedState <- get
  ( whenAsserting . when ( condTy /= SPIRV.Boolean ) )
    ( throwError
    $  "codeGen: 'while' expected boolean conditional, but got "
    <> ShortText.pack (show condTy)
    )
  loopMerge mergeBlockID loopBlockID SPIRV.NoLoopControl
  branchConditional condID loopBlockID mergeBlockID

  -- writing the loop block proper
  {- can't simply use code gen we already did,
  because the bindings don't refer to the phi instructions as they should
  -- liftPut $ Binary.putLazyByteString loopBodyASM
  -}
  -- do code generation a second time for the loop body,
  -- but this time with updated local bindings referring to the phi instructions
  put state
  assign _localBindings updatedLocalBindings
  block loopBlockID
  _ <- codeGen loopBody
  branch headerBlockID
  put updatedState -- account for what happened in the loop header (e.g. IDs of phi instructions)

  -- merge block (first block after the loop)
  block mergeBlockID
  pure (ID 0, SPIRV.Unit) -- ID should never be used

----------------------------------------------------------------------------
-- helper for resetting local bindings

locally :: CGMonad a -> CGMonad a
locally action = do
  bindingsBefore <- use _localBindings
  res <- action
  assign _localBindings bindingsBefore
  pure res
