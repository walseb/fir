{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: CodeGen.Functions

Code generation for function and entry-point definitions, and function calls.
-}

module CodeGen.Functions ( ) where

-- base
import Data.Foldable
  ( traverse_ )
import Data.Maybe
  ( fromMaybe )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )
import qualified GHC.Stack
  ( callStack )

-- binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- containers
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens
  ( view, use, assign )

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

-- fir
import CodeGen.Application
  ( ASTs(NilAST, ConsAST)
  , Application(Applied)
  , traverseASTs
  )
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
import CodeGen.CFG
  ( newBlock )
import CodeGen.Debug
  ( whenDebugging, putSrcInfo )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, TyID
  , Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad, runCGMonad
  , MonadFresh(fresh)
  , note
  , liftPut
  , createIDRec
  )
import CodeGen.Pointers
  ( declareVariable )
import CodeGen.State
  ( CGState(localVariables)
  , _functionContext
  , _knownBinding
  , _localBindings, _localBinding
  , _localVariables
  , _interface
  , _entryPoint
  , _userFunction
  , requireCapabilities
  )
import Data.Containers.Traversals
  ( traverseWithKey_ )
import Data.Type.Known
  ( knownValue )
import FIR.AST
  ( AST
  , FunDefF(..), FunCallF(..), DefEntryPointF(..)
  )
import FIR.AST.Type
  ( Nullary )
import FIR.Binding
  ( Permissions )
import FIR.Prim.Singletons
  ( primTy, knownVars )
import FIR.ProgramState
  ( FunctionContext(..)
  , VLFunctionContext
  , VLInterface
  )
import qualified SPIRV.Control      as SPIRV
  ( FunctionControl, pattern NoFunctionControl )
import qualified SPIRV.Operation    as SPIRV.Op
import qualified SPIRV.PrimTy       as SPIRV
  ( PrimTy(..) )
import qualified SPIRV.Requirements as SPIRV
  ( executionModelCapabilities )
import qualified SPIRV.Stage        as SPIRV
  ( ExecutionModel, ExecutionInfo
  , modelOf
  )

----------------------------------
-- code-generation for functions and entry points

instance CodeGen AST => CodeGen (FunDefF AST) where
  codeGenArgs (Applied (FunDefF (_ :: Proxy name) (_ :: Proxy as) (_ :: Proxy b)) (body `ConsAST` NilAST)) =
    let as     = knownVars  @as
        retTy  = primTy     @b
        name   = knownValue @name
    in do
      whenDebugging ( putSrcInfo GHC.Stack.callStack )
      control <- fromMaybe SPIRV.NoFunctionControl <$> view ( _userFunction name )
      funID   <- declareFunction name control as retTy (codeGen body)
      pure ( funID , SPIRV.Function (map (fst . snd) as) retTy )

instance CodeGen AST => CodeGen (FunCallF AST) where
  codeGenArgs (Applied (FunCallF (_ :: Proxy name) ( _ :: Proxy as ) ( _ :: Proxy b )) as) = do
    let
      funName = knownValue @name
      retTy   = primTy @b
    retTyID <- typeID retTy
    mbFunc <-  use ( _knownBinding funName )
    func <- note
              (  "codeGen: function " <> funName
              <> "\" not bound to any ID."
              )
              mbFunc
    retID <-
      declareFunctionCall
        ( retTyID, retTy )
        func
        =<< traverseASTs @Nullary codeGen as
    pure ( retID, retTy )

instance CodeGen AST => CodeGen (DefEntryPointF AST) where
  codeGenArgs (Applied (DefEntryPointF (_ :: Proxy name) (_ :: Proxy stageInfo)) (body `ConsAST` NilAST))
    = let name      = knownValue @name
          stageInfo = knownValue @stageInfo
      in do
        whenDebugging ( putSrcInfo GHC.Stack.callStack )
        entryPointID <- declareEntryPoint name stageInfo Nothing (codeGen body) -- TODO
        pure ( entryPointID, SPIRV.Function [] SPIRV.Unit )

----------------------------------
-- dealing with function context

inFunctionContext :: ShortText -> [(ShortText, (SPIRV.PrimTy, Permissions))] -> CGMonad a -> CGMonad a
inFunctionContext functionName as
  = inContext ( InFunction functionName as ) as

inEntryPointContext
  :: ShortText
  -> SPIRV.ExecutionInfo Word32 model
  -> Maybe VLInterface
  -> CGMonad a
  -> CGMonad a
inEntryPointContext modelName modelInfo mbIface
  = inContext ( InEntryPoint modelName modelInfo mbIface ) []

inContext :: forall a. VLFunctionContext -> [(ShortText, (SPIRV.PrimTy, Permissions))] -> CGMonad a -> CGMonad a
inContext context as body
  = do
      outsideBindings <- use _localBindings
      assign _functionContext context
      traverse_ (uncurry declareArgument) as
      newBlock

      -- need to perform codeGen first to obtain variables defined within this function
      -- and then declare these variables first before the rest of the body
      cgContext <- ask
      cgState   <- get
      let bodyGenOutput :: Either ShortText (a, CGState, ByteString)
          bodyGenOutput = runCGMonad cgContext cgState body
      (a, bodyState, bodyASM)
        <- case bodyGenOutput of
            Left err -> throwError err
            Right (b,s,asm) -> pure (b,s,asm)

      put bodyState

      -- declare the local variables for this function
      traverseWithKey_ declareVariable (localVariables bodyState)

      -- emit assembly for the function body
      liftPut $ Binary.putLazyByteString bodyASM

      -- reset local bindings after end of function
      assign _functionContext TopLevel
      assign _localBindings   outsideBindings
      assign _localVariables  Map.empty
      pure a

----------------------------------
-- declaring function instructions

declareFunctionCall :: (TyID, SPIRV.PrimTy)
                    ->   (ID, SPIRV.PrimTy)
                    -> [ (ID, SPIRV.PrimTy) ]
                    -> CGMonad ID
declareFunctionCall res func argIDs
  = do v <- fresh
       instruction
         Instruction
           { operation = SPIRV.Op.FunctionCall
           , resTy = Just (fst res)
           , resID = Just v
           , args  = Arg (fst func)
                   $ toArgs (map fst argIDs)
           }
       pure v

declareFunction :: ShortText
                -> SPIRV.FunctionControl
                -> [(ShortText, (SPIRV.PrimTy, Permissions))]
                -> SPIRV.PrimTy
                -> CGMonad (ID, SPIRV.PrimTy)
                -> CGMonad ID
declareFunction funName control as b body
  = createIDRec ( _knownBinding funName )
      ( do resTyID <- typeID b
           fnTyID  <- typeID funTy
           pure (resTyID, fnTyID)
      )
      ( \(resTyID,fnTyID) v -> do
        instruction
          Instruction
            { operation = SPIRV.Op.Function
            , resTy     = Just resTyID
            , resID     = Just v
            , args      = Arg control
                        $ Arg fnTyID EndArgs
            }
        (retValID, _) <- inFunctionContext funName as body
        case b of
          SPIRV.Unit
            -> instruction
                 Instruction
                   { operation = SPIRV.Op.Return
                   , resTy = Nothing
                   , resID = Nothing
                   , args  = EndArgs
                   }
          _ -> instruction
                 Instruction
                   { operation = SPIRV.Op.ReturnValue
                   , resTy = Nothing
                   , resID = Just retValID
                   , args  = EndArgs
                   }
        instruction
          Instruction
            { operation = SPIRV.Op.FunctionEnd
            , resTy     = Nothing
            , resID     = Nothing
            , args      = EndArgs
            }
        pure (v, funTy)
      )
  where
    funTy :: SPIRV.PrimTy
    funTy = SPIRV.Function ( map (fst . snd) as ) b


declareArgument :: ShortText -> (SPIRV.PrimTy, Permissions) -> CGMonad ID
declareArgument argName (argTy, _)
  = createIDRec ( _localBinding argName )
     ( ( , argTy) <$> typeID argTy )
     ( \(argTyID,_) v -> do
        instruction
          Instruction
            { operation = SPIRV.Op.FunctionParameter
            , resTy = Just argTyID
            , resID = Just v
            , args = EndArgs
            }
        pure (v, argTy)
     )

declareEntryPoint
  :: ShortText
  -> SPIRV.ExecutionInfo Word32 model
  -> Maybe VLInterface
  -> CGMonad r
  -> CGMonad ID
declareEntryPoint modelName modelInfo mbIface body
  = createIDRec ( _knownBinding modelName )
      ( do unitTyID <- typeID SPIRV.Unit
           fnTyID  <- typeID funTy
           pure (unitTyID, fnTyID)
      )
      ( \(unitTyID,fnTyID) v -> do
        -- declare entry point ID proper
        assign ( _entryPoint modelName model ) (Just v)
        -- initialise entry point with empty interface
        -- (loading/storing should add to the interface as needed)
        assign ( _interface modelName model ) (Just Map.empty)
        -- set the required capabilities
        requireCapabilities ( SPIRV.executionModelCapabilities model )

        instruction
          Instruction
            { operation = SPIRV.Op.Function
            , resTy     = Just unitTyID
            , resID     = Just v
            , args      = Arg SPIRV.NoFunctionControl
                        $ Arg fnTyID EndArgs
            }
        _ <- inEntryPointContext modelName modelInfo mbIface body
        instruction
          Instruction
            { operation = SPIRV.Op.Return
            , resTy = Nothing
            , resID = Nothing
            , args  = EndArgs
            }
        instruction
          Instruction
            { operation = SPIRV.Op.FunctionEnd
            , resTy     = Nothing
            , resID     = Nothing
            , args      = EndArgs
            }
        pure (v, funTy)
      )
  where
    model :: SPIRV.ExecutionModel
    model = SPIRV.modelOf modelInfo

    funTy :: SPIRV.PrimTy
    funTy = SPIRV.Function [] SPIRV.Unit
