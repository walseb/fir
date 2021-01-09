{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Validation.RayTracing

Validation of ray-tracing and ray-query operations.
-}


module FIR.Validation.RayTracing
  ( -- * Ray tracing validation
    CanTraceRay, CanExecuteCallable
    -- * Ray query validation
    -- ** Modifying ray query state
  , NewRayQuery, RayQueryProceed, RayQueryCandidateState, RayQueryCommittedState, Terminate
    -- ** Validating ray query state
  , CanGenerateIntersection, CanConfirmIntersection
  , HasRayQuery, CanGetIntersectionT, HasSomeIntersection, HasSomeTriangleIntersection
  )
  where

-- base
import Data.Kind
  ( Type, Constraint )
import Data.Type.Bool
  ( If )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Type.Error
  ( Try )
import Data.Type.Map
  ( (:->)((:->)), Insert, LookupAndLookup )
import Data.Type.Maybe
  ( IfNothingThen )
import FIR.Prim.RayTracing
  ( RayQueryState(..), RayQueryProceedState(..)
  , IntersectionType(..), RayQueryCandidateIntersection(..), RayQueryCommittedIntersection(..)
  )
import FIR.ProgramState
  ( ProgramState(..), FunctionContext(..)
  , TLInterfaceVariable
  )
import qualified SPIRV.Stage      as SPIRV
import qualified SPIRV.Storage    as SPIRV

--------------------------------------------------------------------------
-- Ray tracing validation.

-- | Check that a 'traceRay' instruction is valid:
--
--   - the execution model is a ray generation, closest hit or miss shader,
--   - @payload@ indeed refers to a variable with ray payload storage class.
type family CanTraceRay
             ( payload :: Symbol       )
             ( i       :: ProgramState )
          :: Constraint
          where
  CanTraceRay payload
    ( 'ProgramState
       _
       ('InEntryPoint _
         ( info :: SPIRV.ExecutionInfo Nat ( 'SPIRV.Stage ( 'SPIRV.RayStage shader ) ) )
         _
       )
       _ _ iface _ _
    ) = If ( StageIsCompatibleWithRayPayload shader )
          ( HasRayPayloadBinding payload
              ( IfNothingThen
                 ( LookupAndLookup ('SPIRV.RayStorage (SPIRV.RayPayload SPIRV.Incoming)) payload iface )
                 ( LookupAndLookup ('SPIRV.RayStorage (SPIRV.RayPayload SPIRV.Lifetime)) payload iface )
              )
          )
          ( TypeError
            (    Text "'traceRay' instruction not allowed in ray-tracing shader "
            :<>: Text ( SPIRV.StageName ( 'SPIRV.RayStage shader ) )
            :<>: Text "."
            )
          )
  CanTraceRay _ _ = TypeError
    ( Text "Cannot call 'traceRay' here: not within a ray-tracing shader." )

type family StageIsCompatibleWithRayPayload ( shader :: SPIRV.RayShader ) :: Bool where
  StageIsCompatibleWithRayPayload SPIRV.RayGenerationShader = 'True
  StageIsCompatibleWithRayPayload SPIRV.ClosestHitShader    = 'True
  StageIsCompatibleWithRayPayload SPIRV.MissShader          = 'True
  StageIsCompatibleWithRayPayload _                         = 'False


type family HasRayPayloadBinding
              ( callable :: Symbol                    )
              ( mbVar    :: Maybe TLInterfaceVariable )
            :: Constraint
            where
  HasRayPayloadBinding payload 'Nothing =
    TypeError
      (    Text "Cannot use payload named " :<>: ShowType payload :<>: Text " in this call to 'traceRay'."
      :$$: Text "No interface variable with 'RayPayload' storage class could be found with this name."
      )
  HasRayPayloadBinding payload ( 'Just '( decs, ty ) ) =
    Try
      ( ValidPayloadType payload ty )

{-
type family PayloadLocation ( payload :: Symbol ) ( decs :: [ SPIRV.Decoration Nat ] ) :: Either ErrorMessage Constraint where
  PayloadLocation payload '[] = Left
    ( Text "Ray payload interface variable named " :<>: ShowType payload
    :$$: Text "is missing a 'Location' decoration."
    )
  PayloadLocation _ ( SPIRV.Location _ ': _ ) = Right ( () :: Constraint )
  PayloadLocation payload ( _ ': decs ) = PayloadLocation payload decs
-}

type family ValidPayloadType ( payload :: Symbol ) ( ty :: Type ) :: Either ErrorMessage Constraint where
  ValidPayloadType _ _ = Right ( () :: Constraint ) -- TODO


-- | Check that an 'executeCallable' instruction is valid:
--
--  - the execution model is a ray generation, closest hit, miss shader or callable shader,
--  - @callableData@ indeed refers to a variable with callable data storage class.
type family CanExecuteCallable
              ( callableData :: Symbol       )
              ( i            :: ProgramState )
           :: Constraint
            where
  CanExecuteCallable callable 
    ( 'ProgramState
       _
       ('InEntryPoint _
         ( info :: SPIRV.ExecutionInfo Nat ( 'SPIRV.Stage ( 'SPIRV.RayStage shader ) ) )
         _
       )
       _ _ iface _ _
    ) = If ( StageIsCompatibleWithCallableData shader )
          ( HasCallableDataBinding callable
              ( IfNothingThen
                 ( LookupAndLookup ('SPIRV.RayStorage (SPIRV.CallableData SPIRV.Incoming)) callable iface )
                 ( LookupAndLookup ('SPIRV.RayStorage (SPIRV.CallableData SPIRV.Lifetime)) callable iface )
              )
          )
          ( TypeError
            (    Text "'executeCallable' instruction not allowed in ray-tracing shader "
            :<>: Text ( SPIRV.StageName ( 'SPIRV.RayStage shader ) )
            :<>: Text "."
            )
          )
  CanExecuteCallable _ _ = TypeError
    ( Text "Cannot call 'executeCallable' here: not within a ray-tracing shader." )

type family StageIsCompatibleWithCallableData ( shader :: SPIRV.RayShader ) :: Bool where
  StageIsCompatibleWithCallableData SPIRV.RayGenerationShader = 'True
  StageIsCompatibleWithCallableData SPIRV.ClosestHitShader    = 'True
  StageIsCompatibleWithCallableData SPIRV.MissShader          = 'True
  StageIsCompatibleWithCallableData SPIRV.CallableShader      = 'True
  StageIsCompatibleWithCallableData _                         = 'False

type family HasCallableDataBinding
              ( callable :: Symbol )
              ( bd :: Maybe TLInterfaceVariable )
            :: Constraint
            where
  HasCallableDataBinding callable  'Nothing =
    TypeError
      (    Text "Cannot use callable data named " :<>: ShowType callable :<>: Text " in this call to 'executeCallable'."
      :$$: Text "No interface variable with 'CallableData' storage class could be found with this name."
      )
  HasCallableDataBinding callable  ( 'Just '( decs, ty ) ) =
    Try
      ( ValidCallableDataType callable ty )

{-
type family CallableDataLocation ( payload :: Symbol ) ( decs :: [ SPIRV.Decoration Nat ] ) :: Either ErrorMessage Constraint where
  CallableDataLocation callable '[] = Left
    ( Text "Callable data interface variable named " :<>: ShowType callable
    :$$: Text "is missing a 'Location' decoration."
    )
  CallableDataLocation _ ( SPIRV.Location _ ': _ ) = Right ( () :: Constraint )
  CallableDataLocation callable ( _ ': decs ) = CallableDataLocation callable decs
-}

type family ValidCallableDataType ( callable :: Symbol ) ( ty :: Type ) :: Either ErrorMessage Constraint where
  ValidCallableDataType _ _ = Right ( () :: Constraint ) -- TODO


--------------------------------------------------------------------------
-- Ray query validation.

-- | Start a new ray query.
type family NewRayQuery ( rayQuery :: Symbol ) ( i :: ProgramState ) :: ProgramState where
  NewRayQuery rayQuery ( 'ProgramState bds ctx fns eps iface rayQueries bk ) =
    'ProgramState bds ctx fns eps iface ( Insert rayQuery ( 'RayQueryState 'Nothing 'Nothing ) rayQueries ) bk

-- | Validation for ray query 'ifProceeding' function:
--
--   - check that the ray query referred to has been initialized,
--   - check that the ray query has not already completed
--   - sets the ray query proceed state going forwards.
type family RayQueryProceed
              ( rayQuery :: Symbol               )
              ( st       :: RayQueryProceedState )
              ( i        :: ProgramState         )
           :: ProgramState where
  RayQueryProceed rayQuery st ( 'ProgramState bds ctx fns eps iface rayQueries bk ) =
    'ProgramState bds ctx fns eps iface ( SetProceedState rayQuery st rayQueries) bk

type family SetProceedState
              ( rayQuery   :: Symbol                       )
              ( st         :: RayQueryProceedState         )
              ( rayQueries :: [ Symbol :-> RayQueryState ] )
           :: [ Symbol :-> RayQueryState ]
           where
  SetProceedState rayQuery _ '[] =
    TypeError
      ( Text "'ifProceeding': no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  SetProceedState rayQuery st ( ( rayQuery ':-> 'RayQueryState ( 'Just Complete ) _ ) ': _ ) =
    TypeError
      (    Text "'ifProceeding': cannot proceed with ray query named "  :<>: ShowType rayQuery :<>: Text ","
      :$$: Text "as this ray query has already completed."
      )
  SetProceedState rayQuery st ( ( rayQuery ':-> 'RayQueryState _ commState ) ': rayQueries ) =
    ( rayQuery ':-> 'RayQueryState ( 'Just st ) commState ) ': rayQueries
  SetProceedState rayQuery st ( otherRayQuery ': rayQueries )
    = otherRayQuery ': SetProceedState rayQuery st rayQueries

-- | Validation for ray query 'withCandidateIntersection' function:
--
--   - check that the ray query referred to has been initialized,
--   - check that the ray query has not already completed,
--   - sets the candidate intersection type going forwards.
type family RayQueryCandidateState
              ( rayQuery :: Symbol                        )
              ( st       :: RayQueryCandidateIntersection )
              ( i        :: ProgramState                  )
           :: ProgramState where
  RayQueryCandidateState rayQuery st ( 'ProgramState bds ctx fns eps iface rayQueries bk ) =
    'ProgramState bds ctx fns eps iface ( SetCandidateState rayQuery st rayQueries) bk

type family SetCandidateState
              ( rayQuery   :: Symbol                        )
              ( st         :: RayQueryCandidateIntersection )
              ( rayQueries :: [ Symbol :-> RayQueryState ]  )
           :: [ Symbol :-> RayQueryState ]
           where
  SetCandidateState rayQuery _ '[] =
    TypeError
      ( Text "'withCandidateIntersection': no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  SetCandidateState rayQuery st ( ( rayQuery ':-> 'RayQueryState 'Nothing _ ) ': _ ) =
    TypeError
      (    Text "'withCandidateIntersection': cannot get candidate intersection type for ray query named "  :<>: ShowType rayQuery :<>: Text ","
      :$$: Text "as it has not been decided whether to proceed with this ray query."
      :$$: Text "Possible fix: use 'ifProceeding' to determine whether ray query proceeds."
      )
  SetCandidateState rayQuery st ( ( rayQuery ':-> 'RayQueryState ( 'Just Complete ) _ ) ': _ ) =
    TypeError
      (    Text "'withCandidateIntersection': cannot get candidate intersection type for ray query named "  :<>: ShowType rayQuery :<>: Text ","
      :$$: Text "as this ray query has already completed."
      )
  SetCandidateState rayQuery st ( ( rayQuery ':-> 'RayQueryState _ commState ) ': rayQueries ) =
    ( rayQuery ':-> 'RayQueryState ( 'Just ( Incomplete ( 'Just st ) ) ) commState ) ': rayQueries
  SetCandidateState rayQuery st ( otherRayQuery ': rayQueries )
    = otherRayQuery ': SetCandidateState rayQuery st rayQueries

-- | Validation for ray query 'withCommittedIntersection' function:
--
--   - check that the ray query referred to has been initialized,
--   - sets the committed intersection type going forwards.
type family RayQueryCommittedState
              ( rayQuery :: Symbol                        )
              ( st       :: RayQueryCommittedIntersection )
              ( i        :: ProgramState                  )
           :: ProgramState where
  RayQueryCommittedState rayQuery st ( 'ProgramState bds ctx fns eps iface rayQueries bk ) =
    'ProgramState bds ctx fns eps iface ( SetCommittedState rayQuery st rayQueries) bk

type family SetCommittedState
              ( rayQuery   :: Symbol                        )
              ( st         :: RayQueryCommittedIntersection )
              ( rayQueries :: [ Symbol :-> RayQueryState ]  )
           :: [ Symbol :-> RayQueryState ]
           where
  SetCommittedState rayQuery _ '[] =
    TypeError
      ( Text "'withCommittedIntersection': no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  SetCommittedState rayQuery st ( ( rayQuery ':-> 'RayQueryState procState _ ) ': rayQueries ) =
    ( rayQuery ':-> 'RayQueryState procState ( 'Just st ) ) ': rayQueries
  SetCommittedState rayQuery st ( otherRayQuery ': rayQueries )
    = otherRayQuery ': SetCommittedState rayQuery st rayQueries

-- | Validation for ray query 'terminate' instruction.
--
--   - check that the ray query referred to has already been initialized,
--   - set it to completed state.
type family Terminate
              ( rayQuery :: Symbol       )
              ( i        :: ProgramState )
           :: ProgramState where
  Terminate rayQuery ( 'ProgramState bds ctx fns eps iface rayQueries bk ) =
    'ProgramState bds ctx fns eps iface ( TerminateRayQuery rayQuery rayQueries) bk

type family TerminateRayQuery
              ( rayQuery   :: Symbol                       )
              ( rayQueries :: [ Symbol :-> RayQueryState ] )
           :: [ Symbol :-> RayQueryState ]
           where
  TerminateRayQuery rayQuery '[] =
    TypeError
      ( Text "'terminate': no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  TerminateRayQuery rayQuery ( ( rayQuery ':-> 'RayQueryState ( 'Just Complete ) _ ) ': _ ) =
    TypeError
      (    Text "'terminate': cannot terminate ray query named "  :<>: ShowType rayQuery :<>: Text ","
      :$$: Text "as it has already completed."
      )
  TerminateRayQuery rayQuery ( ( rayQuery ':-> 'RayQueryState _ comm ) ': rayQueries ) =
    ( rayQuery ':-> 'RayQueryState ( 'Just 'Complete ) comm ) ': rayQueries
  TerminateRayQuery rayQuery ( otherRayQuery ': rayQueries )
    = otherRayQuery ': TerminateRayQuery rayQuery rayQueries


-- | Validation for ray query "generateIntersection":
--
--   - check that the ray query referred to has already been initialized,
--   - check that we have a candidate AABB intersection.
type family CanGenerateIntersection ( rayQuery :: Symbol ) ( lk :: Maybe RayQueryState ) :: Constraint where
  CanGenerateIntersection rayQuery 'Nothing = TypeError
    ( Text "'generateIntersection': no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  CanGenerateIntersection _ ( Just ( 'RayQueryState ( 'Just ( 'Incomplete ( 'Just AABBCandidateIntersection ) ) ) _ ) ) = ( () :: Constraint )
  CanGenerateIntersection rayQuery _ = TypeError
    (    Text "'generateIntersection' cannot be called here on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as the candidate intersection type is not known to be an AABB."
    :$$: Text "Possible fix: use 'withCandidateIntersection' to determine candidate intersection type."
    )

-- | Validation for ray query "confirmIntersection":
--
--   - check that the ray query referred to has already been initialized,
--   - check that we have a candidate triangle intersection.
type family CanConfirmIntersection ( rayQuery :: Symbol ) ( lk :: Maybe RayQueryState ) :: Constraint where
  CanConfirmIntersection rayQuery 'Nothing = TypeError
    ( Text "'confirmIntersection': no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  CanConfirmIntersection _ ( Just ( 'RayQueryState ( 'Just ( 'Incomplete ( 'Just TriangleCandidateIntersection ) ) ) _ ) ) = ( () :: Constraint )
  CanConfirmIntersection rayQuery _ = TypeError
    (    Text "'confirmIntersection' cannot be called here on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as the candidate intersection type is not known to be a triangle."
    :$$: Text "Possible fix: use 'withCandidateIntersection' to determine candidate intersection type."
    )

-- | Check that a given ray query name has been initialised.
type family HasRayQuery ( callerName :: Symbol ) ( rayQuery :: Symbol ) ( lk :: Maybe RayQueryState ) :: Constraint where
  HasRayQuery callerName rayQuery 'Nothing = TypeError
    ( ShowType callerName :<>: Text ": no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  HasRayQuery _ _ _ = ( () :: Constraint )

-- | Validation for ray query 'getIntersectionT':
--
--   - check that the ray query referred to has already been initialized,
--   - for a candidate intersection, check that we have a candidate triangle intersection,
--   - for a committed intersection, check that we have some committed intersection.
type family CanGetIntersectionT ( rayQuery :: Symbol ) ( interTy :: IntersectionType ) ( lk :: Maybe RayQueryState ) :: Constraint where
  CanGetIntersectionT rayQuery _ Nothing = TypeError
    ( Text "'getIntersectionT': no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  CanGetIntersectionT rayQuery Candidate ( Just ( 'RayQueryState ( 'Just ( 'Incomplete _ ) ) _ ) ) = ( () :: Constraint )
  CanGetIntersectionT rayQuery Candidate _ = TypeError
    (    Text "'getIntersectionT' cannot be called here with a 'Candidate' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as it has not been decided whether to proceed with this ray query."
    :$$: Text "Possible fix: use 'ifProceeding' to determine whether ray query proceeds."
    )
  CanGetIntersectionT rayQuery Committed ( Just ( 'RayQueryState _ Nothing ) ) = TypeError
    (    Text "'getIntersectionT' cannot be called here with a 'Committed' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as the committed intersection type has not been determined."
    :$$: Text "Possible fix: use 'withCommittedIntersection' to determine committed intersection type."
    )
  CanGetIntersectionT rayQuery Committed ( Just ( 'RayQueryState _ ( Just NoIntersection ) ) ) = TypeError
    (    Text "'getIntersectionT' cannot be called here with a 'Committed' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as there is no currently 'Committed' intersection type."
    )
  CanGetIntersectionT rayQuery Committed _ = ( () :: Constraint )

-- | Check that an intersection has occurred in the ray query state.
type family HasSomeIntersection
              ( callerName :: Symbol              )
              ( rayQuery   :: Symbol              )
              ( interTy    :: IntersectionType    )
              ( lk         :: Maybe RayQueryState )
           :: Constraint where
  HasSomeIntersection callerName rayQuery _  'Nothing = TypeError
    ( ShowType callerName :<>: Text ": no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  HasSomeIntersection callerName rayQuery Candidate ( 'Just ( 'RayQueryState ( 'Just ( 'Incomplete _ ) ) _ ) ) = ( () :: Constraint )
  HasSomeIntersection callerName rayQuery Candidate _ = TypeError
    (    ShowType callerName :<>: Text " cannot be called here with a 'Candidate' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as it has not been decided whether to proceed with this ray query."
    :$$: Text "Possible fix: use 'ifProceeding' to determine whether ray query proceeds."
    )
  HasSomeIntersection callerName rayQuery Committed ( Just ( 'RayQueryState _ Nothing ) ) = TypeError
    (    ShowType callerName :<>: Text " cannot be called here with a 'Committed' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as the committed intersection type has not been determined."
    :$$: Text "Possible fix: use 'withCommittedIntersection' to determine committed intersection type."
    )
  HasSomeIntersection callerName rayQuery Committed ( Just ( 'RayQueryState _ ( Just NoIntersection ) ) ) = TypeError
    (    ShowType callerName :<>: Text " cannot be called here with a 'Committed' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as there is no currently 'Committed' intersection type."
    )
  HasSomeIntersection _ _ Committed _ = ( () :: Constraint )

-- | Check that a triangle intersection has occurred in the ray query state.
type family HasSomeTriangleIntersection
              ( callerName :: Symbol              )
              ( rayQuery   :: Symbol              )
              ( interTy    :: IntersectionType    )
              ( lk         :: Maybe RayQueryState )
           :: Constraint where
  HasSomeTriangleIntersection callerName rayQuery _  'Nothing = TypeError
    ( ShowType callerName :<>: Text ": no ray query found with name " :<>: ShowType rayQuery :<>: Text "." )
  HasSomeTriangleIntersection callerName rayQuery Candidate ( 'Just ( 'RayQueryState 'Nothing _ ) ) = TypeError
    (    ShowType callerName :<>: Text " cannot be called here with a 'Candidate' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as it has not been decided whether to proceed with this ray query."
    :$$: Text "Possible fix: use 'ifProceeding' to determine whether ray query proceeds."
    )
  HasSomeTriangleIntersection callerName rayQuery Candidate ( 'Just ( 'RayQueryState ( 'Just ( 'Incomplete 'Nothing ) ) _ ) ) = TypeError
    (    ShowType callerName :<>: Text " cannot be called here with a 'Candidate' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as the candidate intersection type has not been determined."
    :$$: Text "Possible fix: use 'withCandidateIntersection' to determine candidate intersection type."
    )
  HasSomeTriangleIntersection callerName rayQuery Candidate ( 'Just ( 'RayQueryState ( 'Just ( 'Incomplete ( 'Just TriangleCandidateIntersection ) ) ) _ ) ) =
    ( () :: Constraint )
  HasSomeTriangleIntersection callerName rayQuery Candidate _ = TypeError
    (    ShowType callerName :<>: Text " cannot be called here with a 'Candidate' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as the candidate intersection type is not 'Triangle'."
    )
  HasSomeTriangleIntersection callerName rayQuery Committed ( Just ( 'RayQueryState _ Nothing ) ) = TypeError
    (    ShowType callerName :<>: Text " cannot be called here with a 'Committed' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as the committed intersection type has not been determined."
    :$$: Text "Possible fix: use 'withCommittedIntersection' to determine committed intersection type."
    )
  HasSomeTriangleIntersection _ _ Committed ( Just ( 'RayQueryState _ ( Just TriangleIntersection ) ) ) = ( () :: Constraint )
  HasSomeTriangleIntersection callerName rayQuery Committed ( Just ( 'RayQueryState _ ( Just NoIntersection ) ) ) = TypeError
    (    ShowType callerName :<>: Text " cannot be called here with a 'Committed' intersection type"
    :$$: Text "on ray query named " :<>: ShowType rayQuery :<>: Text ","
    :$$: Text "as the committed intersection type is not 'Triangle'."
    )
