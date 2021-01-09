{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module: FIR.Syntax.RayTracing

Ray-tracing and ray-query operations, such as 'traceRay', 'reportIntersection', 'executeCallable'...
-}

module FIR.Syntax.RayTracing where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(..) )
import Data.Word
  ( Word32, Word64 )
import GHC.TypeLits
  ( Symbol )
import Unsafe.Coerce
  ( unsafeCoerce )

-- fir
import Data.Type.Known
  ( Known, knownValue )
import Data.Type.Map
  ( Lookup )
import FIR.AST
import FIR.AST.Type
  ( AugType(..), Eff )
import FIR.Module
  ( Program )
import FIR.Prim.RayTracing
  ( AccelerationStructure
  , RayQuery(..), RayQueryProceedState(..)
  , RayQueryCandidateIntersection(..), RayQueryCommittedIntersection(..)
  , IntersectionType(..)
  )
import FIR.Prim.Singletons
  ( PrimTy )
import FIR.ProgramState
  ( ProgramState, ExecutionContext, RayQueries )
import FIR.Syntax.Program
  ( ) -- Syntactic instance for 'Program'
import FIR.Validation.RayTracing
  ( CanTraceRay, CanExecuteCallable
  , NewRayQuery, RayQueryProceed, RayQueryCandidateState, RayQueryCommittedState
  , Terminate, CanGenerateIntersection, CanConfirmIntersection
  , HasRayQuery, CanGetIntersectionT, HasSomeIntersection, HasSomeTriangleIntersection
  )
import Math.Linear
  ( V, M )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimOp    as SPIRV
  ( RayPrimOp(..) )
import qualified SPIRV.Stage     as SPIRV
  ( AnyHit, Intersection )

--------------------------------------------------------------------------
-- ray tracing instructions

data RayInfo
  = RayInfo
  { rayFlags     :: Code Word32
  , rayOrigin    :: Code ( V 3 Float )
  , rayDirection :: Code ( V 3 Float )
  , rayTMin      :: Code Float
  , rayTMax      :: Code Float
  , cullMask     :: Code Word32 -- ^ Only the least significant 8 bits are used.
  }

data RayShaderInfo
  = RayShaderInfo
  { bindingTableOffset :: Code Word32 -- ^ Only the least significant 4 bits are used.
  , bindingTableStride :: Code Word32 -- ^ Only the least significant 4 bits are used.
  , missShaderIndex    :: Code Word32 -- ^ Only the least significant 16 bits are used.
  }

traceRay
  :: forall
      ( rayPayloadName :: Symbol       )
      ( i              :: ProgramState )
  . ( Known Symbol rayPayloadName
    , CanTraceRay  rayPayloadName i
    )
  => Code AccelerationStructure
  -> RayInfo
  -> RayShaderInfo
  -> Program i i ( Code () )
traceRay accelerationStructure ( RayInfo { .. } ) ( RayShaderInfo { .. } ) = fromAST $
  TraceRay
    { traceRayAccelerationStructure = accelerationStructure
    , traceRayRayFlags              = rayFlags
    , traceRayCullMask              = cullMask
    , traceRaySBTOffset             = bindingTableOffset
    , traceRaySBTStride             = bindingTableStride
    , traceRayMissIndex             = missShaderIndex
    , traceRayRayOrigin             = rayOrigin
    , traceRayRayTMin               = rayTMin
    , traceRayRayDirection          = rayDirection
    , traceRayRayTMax               = rayTMax
    , traceRayPayloadID             = knownValue @rayPayloadName
    }

reportIntersection 
  :: forall (i :: ProgramState)
  .  ( ExecutionContext i ~ Just SPIRV.Intersection )
  => Code Float  -- ^ HitT
  -> Code Word32 -- ^ Hit kind.
  -> Program i i ( Code Bool )
reportIntersection = primOp @i @SPIRV.RT_ReportIntersection

ignoreIntersection
  :: forall (i :: ProgramState)
  .  ( ExecutionContext i ~ Just SPIRV.AnyHit )
  => Program i i ( Code () )
ignoreIntersection = primOp @i @SPIRV.RT_IgnoreIntersection

terminateRay
  :: forall (i :: ProgramState)
  .  ( ExecutionContext i ~ Just SPIRV.AnyHit )
  => Program i i ( Code () )
terminateRay = primOp @i @SPIRV.RT_TerminateRay

executeCallable
  :: forall
        ( callableDataName :: Symbol       )
        ( i                :: ProgramState )
  . ( Known Symbol callableDataName
    , CanExecuteCallable callableDataName i
    )
  => Code Word32 -- ^ Index into the shader binding table.
  -> Program i i ( Code () )
executeCallable bindingTableOffset = fromAST $
  ExecuteCallable bindingTableOffset ( knownValue @callableDataName )

accelerationStructureFromDeviceAddress :: Code Word64 -> Code AccelerationStructure
accelerationStructureFromDeviceAddress
  = primOp @'() @'SPIRV.RT_AccelerationStructureFromDeviceAddress

--------------------------------------------------------------------------
-- ray query instructions

initialize
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery )
  => Code AccelerationStructure
  -> RayInfo
  -> Program i ( NewRayQuery rayQuery i ) ( Code () )
initialize accelStructure ( RayInfo {..} ) = fromAST $
  RayQueryInitialize
    { rayQueryRayQueryName          = RayQuery (knownValue @rayQuery)
    , rayQueryAccelerationStructure = accelStructure
    , rayQueryRayFlags              = rayFlags
    , rayQueryCullMask              = cullMask
    , rayQueryRayOrigin             = rayOrigin
    , rayQueryRayTMin               = rayTMin
    , rayQueryRayDirection          = rayDirection
    , rayQueryRayTMax               = rayTMax
    }

ifProceeding
  :: forall
      ( rayQuery :: Symbol       )
      ( a        :: Type         )
      ( i        :: ProgramState )
      ( j1       :: ProgramState )
      ( j2       :: ProgramState )
  .  ( Known Symbol rayQuery, PrimTy a )
  => Program ( RayQueryProceed rayQuery ( Incomplete Nothing ) i ) j1 ( Code a )
  -> Program ( RayQueryProceed rayQuery     Complete           i ) j2 ( Code a )
  -> Program i i ( Code a )
ifProceeding proceed don't =
  fromAST
    ( IfM :$ ( RayQueryPrimOp (Proxy @Bool) SPIRV.Op.RayQueryProceed ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Eff i i Bool ) )
          :$ ( unsafeCoerce $ toAST proceed )
          :$ ( unsafeCoerce $ toAST don't   )
    )

withCandidateIntersectionType
  :: forall
      ( rayQuery :: Symbol       )
      ( a        :: Type         )
      ( i        :: ProgramState )
      ( j1       :: ProgramState )
      ( j2       :: ProgramState )
  .  ( Known Symbol rayQuery, PrimTy a )
  => Program ( RayQueryCandidateState rayQuery TriangleCandidateIntersection i ) j1 ( Code a )
  -> Program ( RayQueryCandidateState rayQuery     AABBCandidateIntersection i ) j2 ( Code a )
  -> Program i i ( Code a )
withCandidateIntersectionType withTriangle withAABB =
  fromAST
    ( SwitchM
        ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetIntersectionType ( RayQuery $ knownValue @rayQuery ) ( Just Candidate ) :: AST ( Eff i i Word32 ) )
        ( unsafeCoerce $ toAST withTriangle )
        [ ( 1, unsafeCoerce $ toAST withAABB ) ]
    )

withCommittedIntersectionType
  :: forall
      ( rayQuery :: Symbol       )
      ( a        :: Type         )
      ( i        :: ProgramState )
      ( j1       :: ProgramState )
      ( j2       :: ProgramState )
      ( j3       :: ProgramState )
  .  ( Known Symbol rayQuery, PrimTy a )
  => Program ( RayQueryCommittedState rayQuery NoIntersection        i ) j1 ( Code a )
  -> Program ( RayQueryCommittedState rayQuery TriangleIntersection  i ) j2 ( Code a )
  -> Program ( RayQueryCommittedState rayQuery GeneratedIntersection i ) j3 ( Code a )
  -> Program i i ( Code a )
withCommittedIntersectionType noInter tri gen =
  fromAST
    ( SwitchM
        ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetIntersectionType ( RayQuery $ knownValue @rayQuery ) ( Just Committed ) :: AST ( Eff i i Word32 ) )
        ( unsafeCoerce $ toAST noInter )
        [ ( 1, unsafeCoerce $ toAST tri )
        , ( 2, unsafeCoerce $ toAST gen )
        ]
    )

terminate
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery )
  => Program i ( Terminate rayQuery i ) ( Code () )
terminate = fromAST . ( unsafeCoerce :: AST ( Eff i i () ) -> AST ( Eff i ( Terminate rayQuery i ) () ) ) $
  ( RayQueryPrimOp (Proxy @()) SPIRV.Op.RayQueryTerminate ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Eff i i () ) )

generateIntersection
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery
     , CanGenerateIntersection rayQuery ( Lookup rayQuery ( RayQueries i ) )
     )
  => Code Float -> Program i i ( Code () )
generateIntersection = fromAST
  ( RayQueryPrimOp (Proxy @()) SPIRV.Op.RayQueryGenerateIntersection ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Val Float :--> Eff i i () ) )

confirmIntersection
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery
     , CanConfirmIntersection rayQuery ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code () )
confirmIntersection = fromAST
  ( RayQueryPrimOp (Proxy @()) SPIRV.Op.RayQueryConfirmIntersection ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Eff i i () ) )

getRayTMin
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery
     , HasRayQuery "getRayTMin" rayQuery ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Float )
getRayTMin = fromAST
  ( RayQueryPrimOp (Proxy @Float) SPIRV.Op.RayQueryGetRayTMin ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Eff i i Float ) )

getRayFlags
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery
     , HasRayQuery "getRayFlags" rayQuery ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Word32 )
getRayFlags = fromAST
  ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetRayFlags ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Eff i i Word32 ) )

getWorldRayDirection
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery
     , HasRayQuery "getWorldRayDirection" rayQuery ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code ( V 3 Float ) )
getWorldRayDirection = fromAST
  ( RayQueryPrimOp (Proxy @(V 3 Float)) SPIRV.Op.RayQueryGetWorldRayDirection ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Eff i i ( V 3 Float ) ) )

getWorldRayOrigin
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery
     , HasRayQuery "getWorldRayOrigin" rayQuery ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code ( V 3 Float ) )
getWorldRayOrigin = fromAST
  ( RayQueryPrimOp (Proxy @(V 3 Float)) SPIRV.Op.RayQueryGetWorldRayOrigin ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Eff i i ( V 3 Float ) ) )

getIntersectionCandidateAABBOpaque
  :: forall
       ( rayQuery :: Symbol       )
       ( i        :: ProgramState )
  .  ( Known Symbol rayQuery
     , HasRayQuery "getIntersectionCandidateAABBOpaque" rayQuery ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Bool )
getIntersectionCandidateAABBOpaque = fromAST
  ( RayQueryPrimOp (Proxy @Bool) SPIRV.Op.RayQueryGetIntersectionCandidateAABBOpaque ( RayQuery $ knownValue @rayQuery ) Nothing :: AST ( Eff i i Bool ) )

getIntersectionT
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , CanGetIntersectionT rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Word32 )
getIntersectionT = fromAST
  ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetIntersectionT ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i Word32 ) )

getIntersectionInstanceCustomIndex
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionInstanceCustomIndex" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Word32 )
getIntersectionInstanceCustomIndex = fromAST
  ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetIntersectionInstanceCustomIndex ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i Word32 ) )

getIntersectionInstanceId
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionInstanceId" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Word32 )
getIntersectionInstanceId = fromAST
  ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetIntersectionInstanceId ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i Word32 ) )

getIntersectionInstanceShaderBindingTableRecordOffset
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionInstanceShaderBindingTableRecordOffset" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Word32 )
getIntersectionInstanceShaderBindingTableRecordOffset = fromAST
  ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetIntersectionInstanceShaderBindingTableRecordOffset ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i Word32 ) )

getIntersectionGeometryIndex
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionGeometryIndex" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Word32 )
getIntersectionGeometryIndex = fromAST
  ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetIntersectionGeometryIndex ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i Word32 ) )

getIntersectionPrimitiveIndex
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionPrimitiveIndex" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Word32 )
getIntersectionPrimitiveIndex = fromAST
  ( RayQueryPrimOp (Proxy @Word32) SPIRV.Op.RayQueryGetIntersectionPrimitiveIndex ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i Word32 ) )

getIntersectionObjectRayDirection
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionObjectRayDirection" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code ( V 3 Float ) )
getIntersectionObjectRayDirection = fromAST
  ( RayQueryPrimOp (Proxy @(V 3 Float)) SPIRV.Op.RayQueryGetIntersectionObjectRayDirection ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i ( V 3 Float ) ) )

getIntersectionObjectRayOrigin
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionObjectRayOrigin" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code ( V 3 Float ) )
getIntersectionObjectRayOrigin = fromAST
  ( RayQueryPrimOp (Proxy @(V 3 Float)) SPIRV.Op.RayQueryGetIntersectionObjectRayOrigin ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i ( V 3 Float ) ) )

getIntersectionObjectToWorld
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionObjectToWorld" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code ( M 3 4 Float ) )
getIntersectionObjectToWorld = fromAST
  ( RayQueryPrimOp (Proxy @( M 3 4 Float)) SPIRV.Op.RayQueryGetIntersectionObjectToWorld( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i ( M 3 4 Float ) ) )

getIntersectionWorldToObject
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeIntersection "getIntersectionWorldToObject" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code ( M 3 4 Float ) )
getIntersectionWorldToObject = fromAST
  ( RayQueryPrimOp (Proxy @( M 3 4 Float)) SPIRV.Op.RayQueryGetIntersectionWorldToObject ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i ( M 3 4 Float ) ) )

getIntersectionBarycentrics
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeTriangleIntersection "getIntersectionBarycentrics" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code ( V 2 Float ) )
getIntersectionBarycentrics = fromAST
  ( RayQueryPrimOp (Proxy @(V 2 Float)) SPIRV.Op.RayQueryGetIntersectionBarycentrics ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i ( V 2 Float ) ) )

getIntersectionFrontFace
  :: forall
       ( rayQuery :: Symbol           )
       ( interTy  :: IntersectionType )
       ( i        :: ProgramState     )
  .  ( Known Symbol rayQuery
     , Known IntersectionType interTy
     , HasSomeTriangleIntersection "getIntersectionFrontFace" rayQuery interTy ( Lookup rayQuery ( RayQueries i ) )
     )
  => Program i i ( Code Bool )
getIntersectionFrontFace = fromAST
  ( RayQueryPrimOp (Proxy @Bool) SPIRV.Op.RayQueryGetIntersectionFrontFace ( RayQuery $ knownValue @rayQuery ) ( Just $ knownValue @interTy ) :: AST ( Eff i i Bool ) )
