{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.AST.Exts

Additional operations, such as ray tracing operations and Debug Printf.
-}

module FIR.AST.Exts where

-- base
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )

-- containers
import Data.Tree
 ( Tree(Node) )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( pattern VF )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

-- fir
import Data.Constraint.All
  ( All )
import FIR.AST.Display
  ( Display(toTreeArgs), named )
import FIR.AST.Type
  ( AugType(..), Eff
  , Nullary, FunArgs, FunRes
  )
import FIR.Prim.RayTracing
  ( AccelerationStructure
  , RayQuery(..), IntersectionType(..)
  )
import FIR.Prim.Types
  ( PrimTy )
import FIR.ProgramState
  ( ProgramState )
import Math.Linear
  ( V )
import qualified SPIRV.Operation as SPIRV
  ( Operation, showOperation )

------------------------------------------------------------

pattern DebugPrintf formatString = VF ( DebugPrintfF formatString )
pattern
  TraceRay
    { traceRayAccelerationStructure
    , traceRayRayFlags
    , traceRayCullMask
    , traceRaySBTOffset
    , traceRaySBTStride
    , traceRayMissIndex
    , traceRayRayOrigin
    , traceRayRayTMin
    , traceRayRayDirection
    , traceRayRayTMax
    , traceRayPayloadID
    } = VF
      ( TraceRayF
          traceRayAccelerationStructure
          traceRayRayFlags
          traceRayCullMask
          traceRaySBTOffset
          traceRaySBTStride
          traceRayMissIndex
          traceRayRayOrigin
          traceRayRayTMin
          traceRayRayDirection
          traceRayRayTMax
          traceRayPayloadID
      )
pattern ExecuteCallable sbtIndex dataID = VF ( ExecuteCallableF sbtIndex dataID )
pattern
  RayQueryInitialize
    { rayQueryRayQueryName
    , rayQueryAccelerationStructure
    , rayQueryRayFlags
    , rayQueryCullMask
    , rayQueryRayOrigin
    , rayQueryRayTMin
    , rayQueryRayDirection
    , rayQueryRayTMax
    } = VF
      ( RayQueryInitializeF
          rayQueryRayQueryName
          rayQueryAccelerationStructure
          rayQueryRayFlags
          rayQueryCullMask
          rayQueryRayOrigin
          rayQueryRayTMin
          rayQueryRayDirection
          rayQueryRayTMax
      )
pattern RayQueryPrimOp px rayQueryOp rayQueryName mbIntersectionType =
  VF ( RayQueryPrimOpF px rayQueryOp rayQueryName mbIntersectionType )


-- | Printf debugging statement (SPIR-V non-semantic extension).
data DebugPrintfF ( ast :: AugType -> Type ) ( t :: AugType ) where
  DebugPrintfF
    :: All Nullary ( FunArgs ( PrintfAugType i as ) )
    => ShortText  -- ^ PrintF format string
    -> DebugPrintfF ast ( PrintfAugType i as )

type family PrintfAugType ( i :: ProgramState ) ( as :: [ Type ] ) = ( r :: AugType ) | r -> as i where
  PrintfAugType i '[]         = Eff i i ()
  PrintfAugType i ( a ': as ) = Val a :--> PrintfAugType i as


data RayF ( ast :: AugType -> Type ) ( t :: AugType ) where
  -- | Trace a ray into the given acceleration structure.
  TraceRayF ::
    { traceRayFAccelerationStructure :: ast ( Val AccelerationStructure )
    , traceRayFRayFlags              :: ast ( Val Word32 )
    , traceRayFCullMask              :: ast ( Val Word32 )
    , traceRayFSBTOffset             :: ast ( Val Word32 )
    , traceRayFSBTStride             :: ast ( Val Word32 )
    , traceRayFMissIndex             :: ast ( Val Word32 )
    , traceRayFRayOrigin             :: ast ( Val ( V 3 Float ) )
    , traceRayFRayTMin               :: ast ( Val Float )
    , traceRayFRayDirection          :: ast ( Val ( V 3 Float ) )
    , traceRayFRayTMax               :: ast ( Val Float )
    , traceRayFRayPayloadName        :: ShortText
    } -> RayF ast ( Eff i i () )

  -- | Invoke a callable shader.
  ExecuteCallableF ::
    { executeCallableFSBTIndex :: ast ( Val Word32 )
    , executeCallableFDataName :: ShortText
    } -> RayF ast ( Eff i i () )

  -- | Initialise a ray query.
  RayQueryInitializeF ::
    { rayQueryFRayQueryName          :: RayQuery
    , rayQueryFAccelerationStructure :: ast ( Val AccelerationStructure )
    , rayQueryFRayFlags              :: ast ( Val Word32 )
    , rayQueryFCullMask              :: ast ( Val Word32 )
    , rayQueryFRayOrigin             :: ast ( Val ( V 3 Float ) )
    , rayQueryFRayTMin               :: ast ( Val Float )
    , rayQueryFRayDirection          :: ast ( Val ( V 3 Float ) )
    , rayQueryFRayTMax               :: ast ( Val Float )
    } -> RayF ast ( Eff i j () )

  -- | Ray query operations.
  RayQueryPrimOpF
    :: ( All Nullary ( FunArgs opTy )
       , FunRes opTy ~ Eff i i a
       , PrimTy a
       )
    => Proxy a
    -> SPIRV.Operation
    -> RayQuery
    -> Maybe IntersectionType
    -> RayF ast opTy

------------------------------------------------------------
-- displaying

instance Display ast => Display (DebugPrintfF ast) where
  toTreeArgs = named \(DebugPrintfF formatString) ->
    "DebugPrintf \"" <> ShortText.unpack formatString <> "\""

instance Display ast => Display (RayF ast) where

  toTreeArgs as (TraceRayF {..}) = do
    accelerationStructure <- toTreeArgs [] traceRayFAccelerationStructure
    rayFlags              <- toTreeArgs [] traceRayFRayFlags
    cullMask              <- toTreeArgs [] traceRayFCullMask
    sbtOffset             <- toTreeArgs [] traceRayFSBTOffset
    sbtStride             <- toTreeArgs [] traceRayFSBTStride
    missIndex             <- toTreeArgs [] traceRayFMissIndex
    rayOrigin             <- toTreeArgs [] traceRayFRayOrigin
    rayTMin               <- toTreeArgs [] traceRayFRayTMin
    rayDirection          <- toTreeArgs [] traceRayFRayDirection
    rayTMax               <- toTreeArgs [] traceRayFRayTMax
    let
      rayPayloadName = ShortText.unpack traceRayFRayPayloadName
    pure $
      Node "TraceRay"
        ( accelerationStructure
        : rayFlags
        : cullMask
        : sbtOffset
        : sbtStride
        : missIndex
        : rayOrigin
        : rayTMin
        : rayDirection
        : rayTMax
        : Node rayPayloadName []
        : as
        )

  toTreeArgs as (ExecuteCallableF {..}) = do
    sbtIndex <- toTreeArgs [] executeCallableFSBTIndex
    let
      callableDataName = ShortText.unpack executeCallableFDataName
    pure $
      Node "ExecuteCallable"
        ( sbtIndex
        : Node callableDataName []
        : as
        )

  toTreeArgs as (RayQueryInitializeF {..}) = do
    let
      RayQuery rayQueryName = rayQueryFRayQueryName
    accelerationStructure <- toTreeArgs [] rayQueryFAccelerationStructure
    rayFlags              <- toTreeArgs [] rayQueryFRayFlags
    cullMask              <- toTreeArgs [] rayQueryFCullMask
    rayOrigin             <- toTreeArgs [] rayQueryFRayOrigin
    rayTMin               <- toTreeArgs [] rayQueryFRayTMin
    rayDirection          <- toTreeArgs [] rayQueryFRayDirection
    rayTMax               <- toTreeArgs [] rayQueryFRayTMax
    pure $
      Node "RayQueryInitialize"
        ( Node ( ShortText.unpack rayQueryName ) []
        : accelerationStructure
        : rayFlags
        : cullMask
        : rayOrigin
        : rayTMin
        : rayDirection
        : rayTMax
        : as
        )

  toTreeArgs as (RayQueryPrimOpF _ rayQueryOp (RayQuery rayQueryName) mbInterType) =
    pure $
      Node ( SPIRV.showOperation rayQueryOp )
        ( Node ( ShortText.unpack rayQueryName ) []
        : ( case mbInterType of
             Nothing -> id
             Just Candidate -> ( Node "Candidate" [] : )
             Just Committed -> ( Node "Committed" [] : )
          ) as
        )
