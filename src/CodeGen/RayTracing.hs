{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module: CodeGen.Debug

Code generation for ray-tracing instructions.
-}

module CodeGen.RayTracing
  ( )
  where

-- base
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )

-- lens
import Control.Lens
  ( use, assign )

-- mtl
import Control.Monad.Except
  ( throwError )

-- fir
import CodeGen.Application
  ( ASTs(..), Application(..)
  , traverseASTs
  )
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
import CodeGen.IDs
  ( constID, globalID )
import CodeGen.Instruction
  ( ID(ID), Instruction(..), Args(..) )
import CodeGen.Monad
  ( MonadFresh(fresh), note )
import CodeGen.Pointers
  ( declareVariable )
import CodeGen.PrimOps
  ( operationInstruction )
import CodeGen.State
  ( requireCapability
  , _functionContext, _rayQuery
  )
import FIR.AST
  ( AST )
import FIR.AST.Exts
  ( RayF(..) )
import FIR.AST.Type
  ( Nullary )
import FIR.Prim.RayTracing
  ( RayQuery(..), IntersectionType(..) )
import FIR.Prim.Types
  ( primTy )
import FIR.ProgramState
  ( FunctionContext(InEntryPoint) )
import qualified SPIRV.Capability as SPIRV
import qualified SPIRV.Operation  as SPIRV.Op
import qualified SPIRV.PrimTy     as SPIRV
import qualified SPIRV.Stage      as SPIRV
import qualified SPIRV.Storage    as Storage
  ( StorageClass(..) )

-------------------------------------------------------------------------------

instance CodeGen AST => CodeGen (RayF AST) where

  codeGenArgs ( Applied ( TraceRayF {..} ) NilAST ) = do
    ctxt <- use _functionContext
    case ctxt of
      InEntryPoint modelName modelInfo _-> do
        let
          model :: SPIRV.ExecutionModel
          model = SPIRV.modelOf modelInfo
        accelerationStructure <- fst <$> codeGen  traceRayFAccelerationStructure
        rayFlags              <- fst <$> codeGen  traceRayFRayFlags
        cullMask              <- fst <$> codeGen  traceRayFCullMask
        sbt_offset            <- fst <$> codeGen  traceRayFSBTOffset
        sbt_stride            <- fst <$> codeGen  traceRayFSBTStride
        missIndex             <- fst <$> codeGen  traceRayFMissIndex
        rayOrigin             <- fst <$> codeGen  traceRayFRayOrigin
        rayTMin               <- fst <$> codeGen  traceRayFRayTMin
        rayDirection          <- fst <$> codeGen  traceRayFRayDirection
        rayTMax               <- fst <$> codeGen  traceRayFRayTMax
        mbRayPayloadID   <- fmap fst <$> globalID traceRayFRayPayloadName ( Just ( modelName, model ) )
        rayPayloadID <-
          note
            ( "codeGen: could not find RayPayload ID with name " <> traceRayFRayPayloadName  )
            mbRayPayloadID
        instruction
          Instruction
            { operation = SPIRV.Op.TraceRay
            , resTy = Nothing
            , resID = Nothing
            , args  = Arg accelerationStructure
                    $ Arg rayFlags
                    $ Arg cullMask
                    $ Arg sbt_offset
                    $ Arg sbt_stride
                    $ Arg missIndex
                    $ Arg rayOrigin
                    $ Arg rayTMin
                    $ Arg rayDirection
                    $ Arg rayTMax
                    $ Arg rayPayloadID
                    $ EndArgs
            }
        pure (ID 0, SPIRV.Unit) -- ID should not be used
      _ -> throwError "codeGen: 'traceRay' outside of a ray shader"

  codeGenArgs ( Applied ( ExecuteCallableF {..} ) NilAST ) = do
    ctxt <- use _functionContext
    case ctxt of
      InEntryPoint modelName modelInfo _-> do
        let
          model :: SPIRV.ExecutionModel
          model = SPIRV.modelOf modelInfo
        sbtIndex              <- fst <$> codeGen  executeCallableFSBTIndex
        mbCallableDataID <- fmap fst <$> globalID executeCallableFDataName ( Just ( modelName, model ) )
        callableDataID <-
          note
            ( "codeGen: could not find CallableData ID with name " <> executeCallableFDataName )
            mbCallableDataID
        instruction
          Instruction
            { operation = SPIRV.Op.ExecuteCallable
            , resTy = Nothing
            , resID = Nothing
            , args  = Arg sbtIndex
                    $ Arg callableDataID
                    $ EndArgs
            }
        pure (ID 0, SPIRV.Unit) -- ID should not be used
      _ -> throwError "codeGen: 'executeCallable' outside of a ray shader"

  codeGenArgs ( Applied ( RayQueryInitializeF {..} ) NilAST ) = do
    let
      RayQuery rayQueryName = rayQueryFRayQueryName
      rayQueryPtrTy = SPIRV.PointerTy Storage.Private SPIRV.RayQuery
    v <- fresh
    declareVariable v rayQueryPtrTy
    assign ( _rayQuery rayQueryName ) ( Just v )

    requireCapability SPIRV.RayQueryKHR

    accelerationStructure <- fst <$> codeGen rayQueryFAccelerationStructure
    rayFlags              <- fst <$> codeGen rayQueryFRayFlags
    cullMask              <- fst <$> codeGen rayQueryFCullMask
    rayOrigin             <- fst <$> codeGen rayQueryFRayOrigin
    rayTMin               <- fst <$> codeGen rayQueryFRayTMin
    rayDirection          <- fst <$> codeGen rayQueryFRayDirection
    rayTMax               <- fst <$> codeGen rayQueryFRayTMax
    instruction
      Instruction
        { operation = SPIRV.Op.RayQueryInitialize
        , resTy = Nothing
        , resID = Nothing
        , args  = Arg v
                $ Arg accelerationStructure
                $ Arg rayFlags
                $ Arg cullMask
                $ Arg rayOrigin
                $ Arg rayTMin
                $ Arg rayDirection
                $ Arg rayTMax
                $ EndArgs
        }
    pure (ID 0, SPIRV.Unit) -- ID should not be used

  codeGenArgs ( Applied ( RayQueryPrimOpF ( _ :: Proxy a ) rayOpName ( RayQuery rayQueryName ) mbInterTy ) as ) = do
    requireCapability SPIRV.RayQueryKHR
    v <- note ( "'codeGen': no ray query variable named " <> rayQueryName )
            =<< use ( _rayQuery rayQueryName )

    argIDs <- traverseASTs @Nullary ( fmap fst . codeGen ) as
    case mbInterTy of
      Nothing ->
        operationInstruction rayOpName ( primTy @a ) ( v : argIDs )
      Just interTy -> do
        interTyLit <- constID ( case interTy of { Candidate -> (0 :: Word32); Committed -> (1 :: Word32) } )
        operationInstruction rayOpName ( primTy @a ) ( v : interTyLit : argIDs )
