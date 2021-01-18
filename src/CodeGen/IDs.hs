{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module: CodeGen.IDs

This module defines operations to obtain instruction IDs from the code generator state, such as:
  
  * 'typeID', which obtains the ID of a type, recursively defining new IDs for types that have not yet been defined,
  * 'constID', which obtains the ID of a constant value.

Suppose for instance we need an ID for the type @M 4 3 Float@, a matrix with 4 rows and 3 columns.    
In SPIR-V, matrices are represented in column-major order, so the (informal) process to obtain an ID for this matrix is as follows:

  1. query whether this matrix type has already been defined. If so, use that ID: we're done. Otherwise:
  2. query whether the column type @V 4 Float@ has already been defined.
     If so, define a new ID with @OpTypeMatrix vec4FloatID 3@. Otherwise:
  3. query whether the entry type @Float@ has already been defined.
     If so, define a column vector ID @OpTypeVector floatID 4@ and then a matrix ID from that as in 2. Otherwise:
  4. define a new ID for entry type, then one for the column type, then one for the matrix type.

This process lends itself extremely well to a recursive traversal.

-}

module CodeGen.IDs
  ( extInstID
  , typeID
  , constID
  , bindingID
  , undefID
  , builtinID
  , globalID
  , stringLitID
  )
  where

-- base
import Control.Arrow
  ( second )
import Control.Monad
  ( void )
import Data.Coerce
  ( coerce )
import Data.Foldable
  ( traverse_, for_ )
import Data.Maybe
  ( fromJust )
import Data.Semigroup
  ( First(First) )
import Data.Word
  ( Word32 )

-- containers
import qualified Data.Set as Set

-- lens
import Control.Lens
  ( Lens', view, use, assign )

-- mtl
import Control.Monad.Except
  ( MonadError, throwError )
import Control.Monad.State
  ( MonadState )
import Control.Monad.Reader
  ( MonadReader )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import CodeGen.Instruction
  ( ID(ID), TyID(TyID, tyID)
  , Instruction(..)
  , Args(Arg, EndArgs), toArgs
  )
import CodeGen.Monad
  ( MonadFresh
  , createID, createIDRec
  , tryToUse, tryToUseWith
  , note
  )
import CodeGen.State
  ( CGState, CGContext
  , _functionContext
  , _knownExtInst
  , _knownStringLit
  , _knownBinding
  , _localBinding
  , _knownType
  , _knownConstant
  , _knownUndefined
  , _builtin
  , _usedGlobal
  , _userGlobal
  , _backend
  , _interfaceBinding
  , _spirvVersion
  , requireCapabilities
  , addName, addMemberName
  , addDecorations
  , addMemberDecoration, addMemberDecorations
  )
import FIR.Builtin
  ( modelBuiltins, builtinCapabilities )
import FIR.Layout
  ( inferPointerLayout )
import FIR.Prim.RayTracing
  ( AccelerationStructure(..) )
import FIR.Prim.Struct
  ( traverseStruct )
import FIR.Prim.Types
  ( PrimTy(primTySing), primTy
  , SPrimTy(..)
  , aConstant
  )
import FIR.ProgramState
  ( FunctionContext(InEntryPoint), executionContext )
import Math.Linear
  ( M(unM) )
import qualified SPIRV.Builtin      as SPIRV
import qualified SPIRV.Decoration   as SPIRV
import qualified SPIRV.Extension    as SPIRV
  ( ExtInst )
import qualified SPIRV.Image        as SPIRV.Image
import qualified SPIRV.Operation    as SPIRV.Op
import qualified SPIRV.PrimTy       as SPIRV
import qualified SPIRV.PrimTy       as SPIRV.PrimTy
import           SPIRV.PrimTy
  ( AggregateUsage(..) )
import qualified SPIRV.Requirements as SPIRV
  ( primTyCapabilities )
import qualified SPIRV.ScalarTy     as SPIRV
import qualified SPIRV.Stage        as SPIRV
import qualified SPIRV.Storage      as Storage
import qualified SPIRV.Version      as SPIRV

----------------------------------------------------------------------------
-- instructions generated along the way that need to be floated to the top

-- get extended instruction set ID (or create one if none exist)
extInstID :: (MonadState CGState m, MonadFresh ID m)
          => SPIRV.ExtInst -> m ID
extInstID extInst = 
  tryToUse ( _knownExtInst extInst )
    id
    pure

-- get an ID for a given type ( result ID of corresponding type constructor instruction )
-- ( if one is known use it, otherwise recursively create fresh IDs for necessary types )
typeID :: forall m.
          ( MonadReader CGContext m
          , MonadState  CGState   m
          , MonadFresh  ID        m
          , MonadError  ShortText m -- only needed for the constant instruction call for array length
          )
       => SPIRV.PrimTy -> m TyID
typeID ty = TyID <$>
  tryToUseWith _knownPrimTy
    ( coerce . fromJust . resID ) -- type constructor instructions always have a result ID
    do requireCapabilities ( SPIRV.primTyCapabilities ty )
       bk <- view _backend
       case ty of

        SPIRV.Matrix m n a -> 
          createIDRec _knownPrimTy
            ( tyID <$> typeID (SPIRV.Vector m (SPIRV.Scalar a)) ) -- column type
            ( \ colID 
                  -> mkTyConInstruction ( Arg colID $ Arg n EndArgs )
            )

        SPIRV.Vector n a ->
          createIDRec _knownPrimTy
            ( tyID <$> typeID a ) -- element type
            ( \ eltID 
                  -> mkTyConInstruction ( Arg eltID $ Arg n EndArgs )
            )

        SPIRV.Function as b ->
          createIDRec _knownPrimTy
            ( do asIDs <- traverse typeID as -- types of function arguments
                 bID   <- typeID b           -- return type of function
                 pure (asIDs, bID)
            )
            ( \ (asIDs, bID) 
                  -> mkTyConInstruction ( Arg bID $ toArgs asIDs )
            )

        SPIRV.Array l a decs _ ->
          createIDRec _knownPrimTy
            ( do lgID  <- constID l -- array size is the result of a constant instruction
                 eltID <- typeID  a --     as opposed to being a literal number
                 pure (eltID, lgID) -- (I suppose this is to do with specialisation constants)
            )
            ( \(eltTyID, lgID) v -> do
                -- decorate the array (only include layout decorations)
                addDecorations v (Set.filter SPIRV.isLayoutDecoration decs)
                mkTyConInstruction ( Arg eltTyID $ Arg lgID EndArgs ) v
            )

        SPIRV.RuntimeArray a decs _ ->
          createIDRec _knownPrimTy
            ( typeID a )
            ( \eltTyID v -> do
              -- decorate the array (only include layout decorations)
              addDecorations v (Set.filter SPIRV.isLayoutDecoration decs)
              mkTyConInstruction ( Arg eltTyID EndArgs ) v
            )

        SPIRV.Unit                  -> createID _knownPrimTy ( mkTyConInstruction EndArgs )
        SPIRV.Boolean               -> createID _knownPrimTy ( mkTyConInstruction EndArgs )
        SPIRV.AccelerationStructure -> createID _knownPrimTy ( mkTyConInstruction EndArgs )
        SPIRV.RayQuery              -> createID _knownPrimTy ( mkTyConInstruction EndArgs )

        SPIRV.Scalar (SPIRV.Integer s w)
          | SPIRV.Signed <- s
          , SPIRV.OpenCL <- bk
          -> tyID <$> typeID ( SPIRV.Scalar (SPIRV.Integer SPIRV.Unsigned w) ) -- no "sign semantics" in OpenCL

        SPIRV.Scalar (SPIRV.Integer s w)
          -> createID _knownPrimTy
                ( mkTyConInstruction
                    ( Arg (SPIRV.width w) $ Arg (SPIRV.signedness s) EndArgs )
                )

        SPIRV.Scalar (SPIRV.Floating w)
          -> createID _knownPrimTy
                ( mkTyConInstruction ( Arg (SPIRV.width w) EndArgs ) )

        SPIRV.Struct as decs structUsage
          -> createIDRec _knownPrimTy
                ( traverse (fmap tyID . typeID . (\(_,y,_) -> y)) as ) -- return IDs of struct member types
                ( \eltIDs structTyID ->
                  do
                    let labelledElts :: [ (Maybe ShortText, Word32, SPIRV.Decorations) ]
                        labelledElts =
                          ( zipWith
                            ( \i (k, _, eltDecs) -> (k, i, eltDecs) )
                            [0..]
                            as
                          )

                    -- add information: name of each struct field, and decorations
                    for_ labelledElts
                      ( \ (mbFieldName, i, eltDecs) -> do
                        traverse_ ( addMemberName (TyID structTyID) i ) mbFieldName
                        addMemberDecorations (TyID structTyID) i eltDecs
                        -- unfortunate workaround for built-in decorations with structs
                        case structUsage of
                          ForInputBuiltins  -> do
                            for_ ( SPIRV.readBuiltin =<< mbFieldName )
                              ( addMemberDecoration (TyID structTyID) i . SPIRV.Builtin )
                            addName structTyID "gl_in_PerVertex"
                          ForOutputBuiltins -> do
                            for_ ( SPIRV.readBuiltin =<< mbFieldName )
                              ( addMemberDecoration (TyID structTyID) i . SPIRV.Builtin )
                            addName structTyID "gl_out_PerVertex"
                          NotForBuiltins -> pure ()
                      )

                    -- decorate the overall struct (only include layout decorations)
                    addDecorations structTyID (Set.filter SPIRV.isLayoutDecoration decs)

                    -- declare the type
                    mkTyConInstruction (toArgs eltIDs) structTyID
                )

        SPIRV.Pointer storage a ->
          createIDRec _knownPrimTy
            ( typeID a )
            ( \ tyID -> mkTyConInstruction ( Arg storage $ Arg tyID EndArgs ) )

        SPIRV.PrimTy.Image (SPIRV.Image.Image { .. })
          -> createIDRec _knownPrimTy
               ( typeID (SPIRV.Scalar texelComponent) )  -- get the type ID of the image 'component' type
               ( \componentID
                   -> mkTyConInstruction
                        ( Arg componentID
                        $ Arg dimensionality
                        $ Arg hasDepth
                        $ Arg arrayness
                        $ Arg multiSampling
                        $ Arg imageUsage
                        $ Arg imageFormat
                        EndArgs
                        )
               )

        SPIRV.Sampler -> createID _knownPrimTy ( mkTyConInstruction EndArgs )

        SPIRV.SampledImage imageTy
          -> createIDRec _knownPrimTy
               ( typeID (SPIRV.PrimTy.Image imageTy) )
               ( \ imgTyID -> mkTyConInstruction (Arg imgTyID EndArgs ) )

  where _knownPrimTy :: Lens' CGState (Maybe Instruction)
        _knownPrimTy = _knownType ty

        mkTyConInstruction :: Args -> ID -> m Instruction
        mkTyConInstruction flds v
          = pure 
              Instruction
                { operation = SPIRV.tyOp ty
                , resTy     = Nothing
                , resID     = Just v
                , args      = flds
                }

-- get the ID for a given constant, or create one if none exist
-- this is the crucial location where we make use of singletons to perform type-case
constID :: forall m a.
           ( MonadReader CGContext m
           , MonadState  CGState   m
           , MonadFresh  ID        m
           , MonadError  ShortText m
           , PrimTy a
           )
        => a -> m ID
constID a =
  tryToUseWith _knownAConstant
    ( fromJust . resID ) -- constant definition instructions always have a result ID
    do resTyID <- typeID (primTy @a) -- start off by getting an ID for the type!
       let mkConstantInstruction 
             :: SPIRV.Op.Operation -> Args -> ID -> m Instruction
           mkConstantInstruction op flds v
             = pure 
                 Instruction
                   { operation = op
                   , resTy     = Just resTyID
                   , resID     = Just v
                   , args      = flds
                   }
       case primTySing @a of

        SMatrix {} ->
          createIDRec _knownAConstant
            ( traverse constID . unM $ a ) -- get the ID for each column
            ( \ cols -> 
                  mkConstantInstruction 
                    SPIRV.Op.ConstantComposite 
                    ( toArgs cols )
            )

        SVector {} ->
          createIDRec _knownAConstant
            ( traverse constID a ) -- get the result ID for each component
            ( \ eltIDs -> 
                  mkConstantInstruction 
                    SPIRV.Op.ConstantComposite
                    (toArgs eltIDs)
            )

        SScalar {}
          -> createID _knownAConstant
              ( mkConstantInstruction
                  SPIRV.Op.Constant
                  ( Arg a EndArgs )
              )

        SUnit
          -> pure (ID 0) -- should not be used 
            {- create _knownAConstant
                ( mkConstantInstruction
                    SPIRV.Op.ConstantNull
                    EndArgs
                )
              -}
              {-
              throwError
                "constId: called on Unit type.\n\
                \Unit has a unique value, \
                \and as such does not need to be constructed."
              -}

        SBool -> 
          createID _knownAConstant
            ( mkConstantInstruction
                ( if a
                  then SPIRV.Op.ConstantTrue
                  else SPIRV.Op.ConstantFalse
                )
                EndArgs
            )

        SRuntimeArray {} ->
            throwError
              "'constID': cannot construct run-time arrays.\n\
              \Runtime arrays are only available through entry-point interfaces."

        SAccelerationStructure {} ->
          createIDRec _knownAConstant
            ( constID ( case a of { AccelerationStructureFromWord64 handle -> handle } ) )
            ( \ handleID ->
              mkConstantInstruction
                SPIRV.Op.ConvertUToAccelerationStructure
                ( Arg handleID EndArgs )
            )

        SArray {} ->
          createIDRec _knownAConstant
            ( traverse constID a )
            ( \ eltIDs ->
                  mkConstantInstruction
                    SPIRV.Op.ConstantComposite
                    ( toArgs eltIDs )
            )
        
        SStruct {} ->
          createIDRec _knownAConstant
            ( traverseStruct constID a :: m [ID] )
            ( \ eltIDs ->
                  mkConstantInstruction
                    SPIRV.Op.ConstantComposite
                    ( toArgs eltIDs )
            )

  where _knownAConstant :: Lens' CGState (Maybe Instruction)
        _knownAConstant = _knownConstant ( aConstant a )


-- left-biased semigroup operation
infixl 6 <<?>
(<<?>) :: forall x. Maybe x -> Maybe x -> Maybe x
(<<?>) = coerce ( (<>) @(Maybe (First x)) )

bindingID :: forall m.
             ( MonadState  CGState   m
             , MonadReader CGContext m
             , MonadError  ShortText m
             , MonadFresh  ID        m
             ) => ShortText -> m (ID, SPIRV.PrimTy)
bindingID varName
  = do  ctxt <- use _functionContext
        bk <- view _backend
        case ctxt of
          InEntryPoint modelName modelInfo _
            | Just ptrTy <- lookup varName (modelBuiltins modelInfo)
              ->
                do -- this is a built-in variable, use 'builtinID'
                  builtin <- builtinID modelName modelInfo varName ptrTy
                  -- requirement
                  let requirements = builtinCapabilities bk varName
                  requireCapabilities requirements
                  -- note that 'builtinID' sets the necessary decorations for the builtin
                  pure (builtin, SPIRV.pointerTy ptrTy)
          _ ->
            do -- this is not a built-in variable, obtain the binding ID
              mbLoc   <- use ( _localBinding varName )
              mbKnown <- use ( _knownBinding varName )
              mbGlob  <-
                fmap ( second SPIRV.pointerTy )
                  <$> globalID varName (executionContext ctxt)

              -- return the binding ID (throwing an error if none was found)
              note
                ( "codeGen: no binding with name " <> varName )
                ( mbLoc <<?> mbKnown <<?> mbGlob )

undefID :: ( MonadReader CGContext m
           , MonadState  CGState   m
           , MonadError  ShortText m
           , MonadFresh  ID        m
           )
        => SPIRV.PrimTy -> m ID
undefID ty =
  tryToUse ( _knownUndefined ty )
    fst
    ( \v -> do
      tyID <- typeID ty
      pure (v,tyID)
    )

-- | Get the ID for a built-in variable.
--
-- Automatically switches on this built-in in the interface of the relevant entry-point,
-- by using the '_builtin' lens.
builtinID :: ( MonadReader CGContext m
             , MonadState  CGState   m
             , MonadError  ShortText m
             , MonadFresh  ID        m
             )
          => ShortText -> SPIRV.ExecutionInfo Word32 model -> ShortText -> SPIRV.PointerTy -> m ID
builtinID modelName modelInfo builtinName ptrTy =
  tryToUse ( _builtin modelName modelInfo builtinName )
    id
    ( \ v -> do
      -- Ensure that relevant pointer type is declared when this builtin is first used.
      void ( typeID $ SPIRV.pointerTy ptrTy )
      pure v
    )

-- | Get the ID for a user-defined global variable,
-- adding this global to the list of 'used' global variables if it isn't already there,
-- adding the required decorations, and (manually) adding this global variable
-- to the relevant stage interface if necessary.
globalID :: ( MonadState  CGState   m
            , MonadReader CGContext m
            , MonadError  ShortText m
            , MonadFresh  ID        m
            )
         => ShortText -> Maybe (ShortText, SPIRV.ExecutionModel) -> m ( Maybe (ID, SPIRV.PointerTy) )
globalID globalName mbModel
  = do ver <- view _spirvVersion
       mbGlobID <- view ( _userGlobal globalName )
       case mbGlobID of
         Nothing
           -> pure Nothing
         Just (ptrTy, decs)
           -> do
                (ident, laidOutPtrTy) <-
                  tryToUse ( _usedGlobal globalName )
                    id
                    ( \ v -> do
                      -- Declare the user-provided decorations for this global.
                      -- For struct/arrays, layout decorations apply to the underlying object.
                      -- In other situations, we apply the decorations directly to the variable ID.
                      ptrDecs <- case ptrTy of
                        SPIRV.PointerTy _ (SPIRV.Struct       {}) -> pure decs
                        SPIRV.PointerTy _ (SPIRV.Array        {}) -> pure decs
                        SPIRV.PointerTy _ (SPIRV.RuntimeArray {}) -> pure decs
                        _                                         -> pure Set.empty
                      addDecorations v (Set.filter (not . SPIRV.isLayoutDecoration) decs)

                      -- Ensure that relevant pointer type is declared when this global variable is first used.
                      -- We must ensure correct layout.
                      -- In this case, this means giving full layout information for the underlying struct,
                      -- such as @ArrayStride@, @MatrixStride@ and @Offset@ decorations.
                      laidOutPtrTy <- inferPointerLayout NotForBuiltins ptrDecs ptrTy
                      void ( typeID $ SPIRV.pointerTy laidOutPtrTy )
                      pure (v, laidOutPtrTy)
                    )
                case (mbModel, laidOutPtrTy) of
                  ( Just (model, modelName), SPIRV.PointerTy storage _ )
                    |  ver >= SPIRV.Version 1 4
                    || storage == Storage.Input
                    || storage == Storage.Output
                      -> assign ( _interfaceBinding model modelName globalName ) ( Just ident )
                  _   -> pure ()
                pure ( Just (ident, laidOutPtrTy) )

stringLitID :: (MonadState CGState m, MonadFresh ID m)
            => ShortText -> m ID
stringLitID literal =
  tryToUse ( _knownStringLit literal )
    id
    pure
