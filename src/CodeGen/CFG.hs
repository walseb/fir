{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-|
Module: CodeGen.CFG

Code generation for control flow operations: branching, selections, loops.

See also "CodeGen.Phi" for ϕ-functions necessary for such constructs when using SSA form.
-}

module CodeGen.CFG
  ( newBlock, locally, embed )
  where

-- base
import Prelude
  hiding ( break )
import Control.Arrow
  ( first, second )
import Control.Monad
  ( forM, unless, when )
import Data.Functor
  ( ($>) )
import Data.Maybe
  ( catMaybes, fromMaybe )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( fromList, member )
import qualified Data.Sequence as Seq
  ( Seq(..), drop, lookup )

-- lens
import Control.Lens
  ( view, use, assign, modifying )

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
  ( ASTs(NilAST, ConsAST), UCode(UCode)
  , Application(Applied)
  )
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
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
  ( CGState(localBindings)
  , ContinueOrMergeID(..), LoopBlockIDs(..)
  , _currentBlock, _loopBlockIDs, _earlyExit, _earlyExits
  , _localBindings, _localBinding
  , _spirvVersion, _rayQueries
  )
import Data.Containers.Traversals
  ( traverseWithKey_ )
import Data.Type.Known
  ( knownValue )
import Data.Type.Map
  ( Keys )
import FIR.AST
  ( AST, Code
  , SelectionF(..), LoopF(..), StateF(..)
  , pattern (:$), pattern Return
  )
import FIR.AST.Type
  ( AugType(Val), Eff, Nullary )
import FIR.Prim.Types
  ( IntegralTy, primTy )
import FIR.ProgramState
  ( Bindings )
import qualified SPIRV.Control   as SPIRV
  ( SelectionControl, pattern NoSelectionControl
  , LoopControl, pattern NoLoopControl
  )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
  ( PrimTy(..) )
import qualified SPIRV.Version as SPIRV
  ( Version(..) )

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

instance CodeGen AST => CodeGen (SelectionF AST) where

  -- if statement
  codeGenArgs (Applied IfF (c `ConsAST` (t :: Code a) `ConsAST ` f `ConsAST` NilAST)) = do
    ver <- view _spirvVersion
    if canUseSelection (primTy @a) ver
    then selection c        t         f
    else ifM       c (UCode t) (UCode f)
      where
        canUseSelection :: SPIRV.PrimTy -> SPIRV.Version -> Bool
        canUseSelection (SPIRV.Scalar    _) _ = True
        canUseSelection (SPIRV.Pointer _ _) _ = True
        canUseSelection ty ver
          | ver < SPIRV.Version 1 4 = False
          | SPIRV.Matrix {} <- ty   = True
          | SPIRV.Vector {} <- ty   = True
          | SPIRV.Array  {} <- ty   = True
          | SPIRV.Struct {} <- ty   = True
          | otherwise               = False

  -- monadic if statement
  codeGenArgs (Applied IfMF (c `ConsAST` t `ConsAST ` f `ConsAST` NilAST)) = ifM c (UCode t) (UCode f)

  -- switch statement
  codeGenArgs (Applied (SwitchF scrut def cases) NilAST) =
    codeGen
      ( SwitchMF
       ( Return :$ scrut )
       ( Return :$ def   )
       ( map (second (Return :$)) cases )
      )

  -- monadic switch statement
  codeGenArgs (Applied (SwitchMF scrut def cases) NilAST) = switch scrut def (map (second UCode) cases)


selection :: ( CodeGen AST, Nullary a ) => Code Bool -> AST a -> AST a -> CGMonad (ID, SPIRV.PrimTy)
selection cond x y = do
  (condID, condTy) <- codeGen cond
  ( whenAsserting . unless ( condTy == SPIRV.Boolean) )
    ( throwError
    $  "codeGen: ASSERT failed, 'select' expected boolean conditional, but got "
    <> ShortText.pack (show condTy)
    )
  o1 <- codeGen x
  o2 <- codeGen y
  select condID o1 o2

ifM :: ( CodeGen AST, Nullary b ) => AST b -> UCode -> UCode -> CGMonad (ID, SPIRV.PrimTy)
ifM cond bodyTrue bodyFalse
  = branchingSelection conditionalHeader [bodyTrue, bodyFalse]
      where
        conditionalHeader :: [ID] -> ID -> CGMonad ()
        conditionalHeader [trueBlockID, falseBlockID] mergeBlockID = do
          (condID, condTy) <- locally (codeGen cond)
          ( whenAsserting . unless ( condTy == SPIRV.Boolean ) )
            ( throwError
            $  "codeGen: ASSERT failed, 'if' expected boolean conditional, but got "
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

switch :: ( CodeGen AST, IntegralTy t, Nullary s, Nullary a)
       => AST s -> AST a -> [(t, UCode)] -> CGMonad (ID, SPIRV.PrimTy)
switch scrut def cases
  = branchingSelection switchHeader (UCode def : map snd cases)
      where
        switchHeader :: [ID] -> ID -> CGMonad ()
        switchHeader [] _ = whenAsserting (throwError "codeGen: ASSERT failed, 'switch' statement missing default case")
        switchHeader ( defaultBlockID : caseBlockIDs ) mergeBlockID = do
          (scrutID, _) <- locally (codeGen scrut)
          selectionMerge mergeBlockID SPIRV.NoSelectionControl
          multiWaySwitch scrutID defaultBlockID
            (zipWith ( \(t,_) i -> Pair (t,i) ) cases caseBlockIDs)

branchingSelection :: CodeGen AST => ( [ID] -> ID -> CGMonad () ) -> [ UCode ] -> CGMonad (ID, SPIRV.PrimTy)
branchingSelection mkHeader cases = do
  headerBlockID  <- fresh
  caseBlockIDs   <- traverse (\x -> (,x) <$> fresh) cases
  mergeBlockID   <- fresh
  bindingsBefore <- use _localBindings
  exitBefore     <- use _earlyExit
  branch headerBlockID

  -- header block
  block headerBlockID
  mkHeader ( map fst caseBlockIDs ) mergeBlockID

  -- Do the code-generation for each of the cases.
  (bodies, endBlocks, endBindings) <-
    unzip3 . catMaybes <$>
      forM caseBlockIDs \ (caseBlockID, UCode caseBody) -> do
        assign _localBindings bindingsBefore
        assign _earlyExit Nothing
        block caseBlockID
        bodyRes  <- codeGen caseBody
        endBlock <-
          note "codeGen: branch statement escaped CFG"
            =<< use _currentBlock
        branch mergeBlockID
        earlyExit <- use _earlyExit
        case earlyExit of
          Just _ -> pure Nothing
          _ -> do
            bodyBindings <- use _localBindings
            pure $ Just (bodyRes, endBlock, bodyBindings)

  -- reset local bindings to what they were outside the branches
  assign _localBindings bindingsBefore
  assign _earlyExit     exitBefore

  -- merge block
  -- emit the appropriate ϕ-instructions
  block mergeBlockID
  phiBindings <- case endBindings of
    []                    -> pure Map.empty
    [ singleEndBindings ] -> pure . fmap fst $ singleEndBindings `Map.intersection` bindingsBefore
    _                     -> phiInstructions ( `Map.member` bindingsBefore ) endBlocks endBindings
  -- update local bindings to refer to the ϕ-instructions
  traverseWithKey_
    ( \ bdName bdID -> modifying ( _localBinding bdName ) ( fmap $ first (const bdID) ) )
    phiBindings

  -- return the result
  resType <- case map snd bodies of
    [] -> throwError "codeGen: empty list of branches in conditional"
    (ty : tys)
      -> do ( whenAsserting . unless ( all (== ty) tys ) )
              ( throwError "codeGen: ASSERT failed, branches of conditional return different types" )
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
-- code generation for loops

instance CodeGen AST => CodeGen (LoopF AST) where
  codeGenArgs ( Applied WhileF ( c `ConsAST` body `ConsAST` NilAST ) ) = while ( Just c ) body
  codeGenArgs ( Applied LoopF              ( body `ConsAST` NilAST ) ) = while ( Nothing :: Maybe ( AST ( Val Bool ) ) ) body
  codeGenArgs ( Applied ( BreakF         ( _ :: Proxy n ) ) NilAST   ) = break ( knownValue @n )
  codeGenArgs ( Applied ( BreakContinueF ( _ :: Proxy n ) ) NilAST   ) = breakContinue ( knownValue @n )

while :: ( CodeGen AST, Nullary c, Nullary b ) => Maybe ( AST c ) -> AST b -> CGMonad (ID, SPIRV.PrimTy)
while mbCond loopBody = do
  beforeLoopBlockIDs <- use _loopBlockIDs
  beforeEarlyExits   <- use _earlyExits
  beforeEarlyExit    <- use _earlyExit
  beforeRayQueries   <- use _rayQueries
  beforeBlockID      <- note "codeGen: loop outside of a block"
                        =<< use _currentBlock
  headerBlockID      <- fresh
  loopBlockID        <- fresh
  mergeBlockID       <- fresh -- block where control flow merges back
  continueBlockID    <- fresh -- block at the end of the loop body from which we can continue for further loops
  branch headerBlockID

  -- Need to perform code generation for the loop block first,
  -- as we need to know which ϕ-instructions to put in the header.
  -- However, the loop block needs to appear after the header block in the CFG.
  ctxt  <- ask
  state <- get
  let
    bindingsBefore :: Map ShortText (ID, SPIRV.PrimTy)
    bindingsBefore = localBindings state

    -- Perform code-generation for the loop block.
    --
    -- Factored out because we do this code-generation twice.
    codeGenLoop :: CGMonad ( Map ID ( ContinueOrMergeID, Map ShortText (ID, SPIRV.PrimTy) ) )
    codeGenLoop = do
      early <- use _earlyExit
      block loopBlockID
      modifying _loopBlockIDs ( LoopBlockIDs { continueBlockID, mergeBlockID } Seq.:<| )
      _ <- codeGen loopBody
      loopEndBindings <- use _localBindings
      loopEndBlockID <-
        note "codeGen: loop statement escaped CFG?"
          =<< use _currentBlock
      loopEarlyExits <- use _earlyExits
      modifying _loopBlockIDs (Seq.drop 1)
      assign _earlyExit early
      branch continueBlockID
      block  continueBlockID
      let
        -- Gather up all the blocks that immediately precede the continue block
        -- in the control-flow graph.
        -- These will be
        --   - the block at the end of the loop body,
        --   - the early-exit continue statements that go to this continue block.
        predContinueBlockIDs, earlyContinueBlockIDs :: [ ID ]
        predContinueBlockIDs = loopEndBlockID : earlyContinueBlockIDs
        predContinueBindings, earlyContinueBindings :: [ Map ShortText ( ID, SPIRV.PrimTy ) ]
        predContinueBindings = loopEndBindings : earlyContinueBindings
        ( earlyContinueBlockIDs, earlyContinueBindings )
          = unzip
          . Map.toList
          . Map.mapMaybe \ case
              ( ContinueBlockID continueID', earlyExitBds )
                | continueID' == continueBlockID
                -> Just earlyExitBds
              _ -> Nothing
          $ loopEarlyExits
      -- If there are early-exit continue blocks, we need ϕ-instructions.
      unless ( null earlyContinueBlockIDs ) do
        phiMergeLocalBindings <-
          phiInstructions
            ( const True )
            predContinueBlockIDs
            predContinueBindings
        -- update local bindings to refer to the ϕ-instructions
        traverseWithKey_
          ( \ bdName bdID -> modifying ( _localBinding bdName ) ( fmap $ first (const bdID) ) )
          phiMergeLocalBindings

      branch headerBlockID

      return loopEarlyExits

    dryRunLoopGenOutput :: Either ShortText (_earlyExits, CGState, ByteString)
    dryRunLoopGenOutput = runCGMonad ctxt state codeGenLoop

  dryRunLoopEndState
    <- case dryRunLoopGenOutput of
          Left  err     -> throwError err
          Right (_,s,_) -> pure s
  let
    loopEndBindings :: Map ShortText (ID, SPIRV.PrimTy)
    loopEndBindings = localBindings dryRunLoopEndState

  -- Update the state to be the state at the end of the loop
  -- (e.g. don't forget about new constants that were defined),
  -- but reset bindings to what they were before the loop block.
  -- This is because all bindings within the loop remain local to it.
  put dryRunLoopEndState
  assign _localBindings bindingsBefore
  assign _loopBlockIDs  beforeLoopBlockIDs
  assign _earlyExits    beforeEarlyExits
  assign _earlyExit     beforeEarlyExit
  assign _rayQueries    beforeRayQueries

  -- header block
  block headerBlockID
  phiHeaderLocalBindings <-
    phiInstructions
      ( const True )
      [ beforeBlockID , continueBlockID ]
      [ bindingsBefore, loopEndBindings ] -- need loopEndBindings

  -- update local bindings to refer to the ϕ-instructions
  traverseWithKey_
    ( \ bdName bdID -> modifying ( _localBinding bdName ) ( fmap $ first (const bdID) ) )
    phiHeaderLocalBindings
  updatedLocalBindings <- use _localBindings
  headerEndState <-
    case mbCond of
      Just cond -> do
        (condID, condTy) <- locally (codeGen cond)
        headerEndState <- get
        ( whenAsserting . when ( condTy /= SPIRV.Boolean ) )
          ( throwError
          $  "codeGen: ASSERT failed, while loop expected boolean conditional, but got "
          <> ShortText.pack (show condTy)
          )
        loopMerge mergeBlockID continueBlockID SPIRV.NoLoopControl
        branchConditional condID loopBlockID mergeBlockID
        pure headerEndState
      Nothing -> do
        headerEndState <- get
        loopMerge mergeBlockID continueBlockID SPIRV.NoLoopControl
        branch loopBlockID
        pure headerEndState

  -- writing the loop block proper
  {- can't simply use code gen we already did,
  because the bindings don't refer to the ϕ-instructions as they should
  -- liftPut $ Binary.putLazyByteString loopBodyASM
  -}
  -- do code generation a second time for the loop body,
  -- but this time with updated local bindings referring to the ϕ-instructions
  put state
  assign _localBindings updatedLocalBindings
  finalEarlyExits <- codeGenLoop
  put headerEndState -- account for what happened in the loop header (e.g. IDs of ϕ-instructions)

  -- merge block (first block after the loop)
  block mergeBlockID

  -- Gather up all the blocks that immediately precede the merge block in the CFG,
  -- in order to emit the appropriate ϕ-instructions.
  -- The relevant blocks are:
  --   - the header block, for a while loop (i.e. with a conditional in the header);
  --   - the early-exit break statements that go to this merge block.
  let
    headerEndBindings :: Map ShortText ( ID, SPIRV.PrimTy )
    headerEndBindings = localBindings headerEndState
    predMergeBlockIDs, earlyMergeBlockIDs :: [ ID ]
    predMergeBindings, earlyMergeBindings :: [ Map ShortText ( ID, SPIRV.PrimTy ) ]
    ( predMergeBlockIDs, predMergeBindings )
      | Nothing <- mbCond
      = ( earlyMergeBlockIDs, earlyMergeBindings )
      | otherwise
      = ( headerBlockID : earlyMergeBlockIDs, headerEndBindings : earlyMergeBindings )
    ( earlyMergeBlockIDs, earlyMergeBindings )
      = unzip
      . Map.toList
      . Map.mapMaybe \ case
          ( MergeBlockID mergeID', earlyExitBds )
            | mergeID' == mergeBlockID
            -> Just earlyExitBds
          _ -> Nothing
      $ finalEarlyExits

  phiBindings <- case predMergeBindings of
    [] -> pure Map.empty
    [ singleEndBindings ]
      -> pure . fmap fst $ singleEndBindings `Map.intersection` bindingsBefore
    _ -> phiInstructions
          ( const True )
          predMergeBlockIDs
          predMergeBindings
  -- update local bindings to refer to the ϕ-instructions
  traverseWithKey_
    ( \ bdName bdID -> modifying ( _localBinding bdName ) ( fmap $ first (const bdID) ) )
    phiBindings

  pure (ID 0, SPIRV.Unit) -- ID should never be used

break :: Word32 -> CGMonad (ID, SPIRV.PrimTy)
break 0 = pure (ID 0, SPIRV.Unit) -- ID should never be used
break nbLoopsToBreak = do
  loopBlockIDs <- use _loopBlockIDs
  earlyExitBlockID <-
    case Seq.lookup ( fromIntegral nbLoopsToBreak - 1 ) loopBlockIDs of
      Just ( LoopBlockIDs { mergeBlockID } )
        -> branch mergeBlockID $> MergeBlockID mergeBlockID
      _ -> throwError
            ( "codeGen: 'break' statement inside too few loops.\n\
              \breaking " <> ShortText.pack ( show nbLoopsToBreak ) <> " loops,\n\
              \but only inside " <> ShortText.pack ( show ( length loopBlockIDs ) ) <> " loops."
            )
  assign _earlyExit ( Just earlyExitBlockID )
  currentBlockID <-
    note "codeGen: 'break' statement outside of a block"
      =<< use _currentBlock
  currentBindings <- use _localBindings
  modifying _earlyExits ( Map.insert currentBlockID ( earlyExitBlockID, currentBindings ) )
  pure (ID 0, SPIRV.Unit) -- ID should never be used

breakContinue :: Word32 -> CGMonad (ID, SPIRV.PrimTy)
breakContinue nbLoopsToBreak = do
  loopBlockIDs <- use _loopBlockIDs
  earlyExitBlockID <-
    case Seq.lookup ( fromIntegral nbLoopsToBreak ) loopBlockIDs of
      Just ( LoopBlockIDs { continueBlockID } )
        -> branch continueBlockID $> ContinueBlockID continueBlockID
      _ -> throwError
            ( "codeGen: 'breakContinue' statement inside too few loops.\n\
              \continuing from start of loop after breaking " <> ShortText.pack ( show nbLoopsToBreak ) <> " loops,\n\
              \but only inside " <> ShortText.pack ( show ( length loopBlockIDs ) ) <> " loops."
            )
  assign _earlyExit ( Just earlyExitBlockID )
  currentBlockID <-
    note "codeGen: 'breakContinue' statement outside of a block"
      =<< use _currentBlock
  currentBindings <- use _localBindings
  modifying _earlyExits ( Map.insert currentBlockID ( earlyExitBlockID, currentBindings ) )
  pure (ID 0, SPIRV.Unit) -- ID should never be used

----------------------------------------------------------------------------
-- code-generation for locally / embed

instance CodeGen AST => CodeGen (StateF AST) where
  codeGenArgs (Applied LocallyF (a `ConsAST` NilAST)) =
    locally (codeGen a)
  codeGenArgs (Applied EmbedF   ( ( a :: AST ( Eff i i a ) ) `ConsAST` NilAST)) =
    embed ( Set.fromList $ knownValue @(Keys (Bindings i)) ) (codeGen a)

locally :: CGMonad a -> CGMonad a
locally action = do
  bindingsBefore <- use _localBindings
  res <- action
  bindingsAfter <- use _localBindings

  -- keep the updated values of local bindings,
  -- but only for bindings that were defined before the "locally" block
  assign _localBindings ( bindingsAfter `Map.intersection` bindingsBefore )

  whenAsserting do
    let
      weirdos :: Map ShortText ( (ID, SPIRV.PrimTy), (ID, SPIRV.PrimTy) )
      weirdos = Map.mapMaybe id
              $ Map.intersectionWith ( \ ( id1, ty1 ) ( id2, ty2 ) -> if ty1 == ty2 then Nothing else Just ( ( id1, ty1 ), ( id2, ty2 ) ) )
                bindingsBefore bindingsAfter
    unless ( null weirdos ) $
      throwError ( "'codeGen': ASSERT failed, error in code-generation for 'locally'\nweirdos = " <> ShortText.pack ( show weirdos ) )

  pure res

embed :: Set ShortText -> CGMonad a -> CGMonad a
embed innerStartNames action = do
  bindingsBefore <- use _localBindings
  assign _localBindings ( Map.restrictKeys bindingsBefore innerStartNames )
  res <- action
  bindingsAfter <- use _localBindings

  -- discard the updated values of local bindings that shadowed previous bindings
  let
    newBindings :: Map ShortText (ID, SPIRV.PrimTy)
    newBindings =
      Map.mapWithKey
        ( \ name aft -> fromMaybe aft (shadows name innerStartNames bindingsBefore ) )
        (  bindingsAfter `Map.intersection` bindingsBefore
        <> Map.withoutKeys bindingsBefore innerStartNames
        )

  assign _localBindings newBindings
  pure res

shadows :: ShortText -> Set ShortText -> Map ShortText (ID, SPIRV.PrimTy) -> Maybe (ID, SPIRV.PrimTy)
shadows name innerStartNames prevs
  | not ( name `Set.member` innerStartNames )
  , Just prev <- Map.lookup name prevs
  = Just prev
  | otherwise
  = Nothing
