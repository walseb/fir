{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module CodeGen.IDs where

-- base
import Control.Arrow
  ( second )
import Data.Coerce
  ( coerce )
import Data.Foldable
  ( traverse_, toList )
import Data.Maybe
  ( fromJust, fromMaybe )
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

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- fir
import CodeGen.Instruction
  ( ID(ID), Instruction(..)
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
  , FunctionContext(EntryPoint)
  , _functionContext
  , _knownExtInst
  , _knownStringLit
  , _knownBinding
  , _localBinding
  , _knownType
  , _knownConstant
  , _builtin
  , _usedGlobal
  , _userGlobal
  , _entryPointExecutionModes
  , _interfaceBinding
  , addCapabilities, addMemberName, addMemberDecoration, addDecorations
  )
import FIR.Builtin
  ( stageBuiltins )
import FIR.Prim.Singletons
  ( PrimTy(primTySing), primTy
  , SPrimTy(..)
  , aConstant
  )
import FIR.Prim.Struct
  ( traverseStruct )
import Math.Linear
  ( M(unM), Matrix(transpose) )
import qualified SPIRV.Builtin    as SPIRV
  ( Builtin(Position,PointSize) )
import qualified SPIRV.Capability as SPIRV
  ( primTyCapabilities )
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.Extension  as SPIRV
import qualified SPIRV.Image      as SPIRV
  ( Image(..) )
import qualified SPIRV.Image      as SPIRV.Image
import qualified SPIRV.Operation  as SPIRV.Op
import qualified SPIRV.PrimTy     as SPIRV
import qualified SPIRV.PrimTy     as SPIRV.PrimTy
import qualified SPIRV.ScalarTy   as SPIRV
import qualified SPIRV.Stage      as SPIRV
import qualified SPIRV.Storage    as Storage

----------------------------------------------------------------------------
-- instructions generated along the way that need to be floated to the top

-- get extended instruction set ID (or create one if none exist)
extInstID :: (MonadState CGState m, MonadFresh ID m)
          => SPIRV.ExtInst -> m ID
extInstID extInst = 
  tryToUse ( _knownExtInst extInst )
    ( fromJust . resID ) -- ExtInstImport instruction always has a result ID
    ( \ v -> pure
      Instruction
        { operation = SPIRV.Op.ExtInstImport
        , resTy     = Nothing
        , resID     = Just v
        , args      = Arg ( SPIRV.extInstName extInst )
                      EndArgs
        }
    )

-- get an ID for a given type ( result ID of corresponding type constructor instruction )
-- ( if one is known use it, otherwise recursively create fresh IDs for necessary types )
typeID :: forall m.
          ( MonadState CGState m
          , MonadFresh ID m
          , MonadError Text m -- only needed for the constant instruction call for array length
          )
       => SPIRV.PrimTy -> m ID
typeID ty =
  tryToUseWith _knownPrimTy
    ( fromJust . resID ) -- type constructor instructions always have a result ID
    do addCapabilities ( SPIRV.primTyCapabilities ty )
       case ty of

        SPIRV.Matrix m n a -> 
          createIDRec _knownPrimTy
            ( typeID (SPIRV.Vector m (SPIRV.Scalar a)) ) -- column type
            ( \ colID 
                  -> mkTyConInstruction ( Arg colID $ Arg n EndArgs )
            )

        SPIRV.Vector n a ->
          createIDRec _knownPrimTy
            ( typeID a ) -- element type
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

        SPIRV.Array l a ->
          createIDRec _knownPrimTy
            ( do lgID  <- constID l -- array size is the result of a constant instruction
                 eltID <- typeID  a --     as opposed to being a literal number
                 pure (eltID, lgID) -- (I suppose this is to do with specialisation constants)
            )
            ( \(eltID, lgId)
                -> mkTyConInstruction ( Arg eltID $ Arg lgId EndArgs ) 
            )

        SPIRV.RuntimeArray a ->
          createIDRec _knownPrimTy
            ( typeID a )
            ( \eltID -> mkTyConInstruction ( Arg eltID EndArgs ) )

        SPIRV.Unit    -> createID _knownPrimTy ( mkTyConInstruction EndArgs )
        SPIRV.Boolean -> createID _knownPrimTy ( mkTyConInstruction EndArgs )

        SPIRV.Scalar (SPIRV.Integer s w)
          -> createID _knownPrimTy
                ( mkTyConInstruction 
                  ( Arg (SPIRV.width w)
                  $ Arg (SPIRV.signedness s) EndArgs
                  )
                )

        SPIRV.Scalar (SPIRV.Floating w)
          -> createID _knownPrimTy
                ( mkTyConInstruction ( Arg (SPIRV.width w) EndArgs ) )

        SPIRV.Struct as
          -> createIDRec _knownPrimTy
                ( traverse (typeID . snd) as ) -- return IDs of struct member types
                ( \eltIDs structTyID 
                    -> do let labelledElts :: [ (Word32, Text) ]
                              labelledElts
                                = ( zipWith
                                    ( \i (k,_) -> (i,k) )
                                    [0..]
                                    as
                                  )

                          -- add annotations: name of each struct field
                          traverse_
                            ( uncurry (addMemberName structTyID) )
                            labelledElts

                          -- workaround for builtin decoration complexities:
                          -- add "builtin" decorations when relevant
                          --   (for gl_in, gl_out, gl_PerVertex)
                          ctxt <- use _functionContext
                          case ctxt of
                            EntryPoint stage _
                              | stage `elem` [ SPIRV.TessellationControl
                                             , SPIRV.TessellationEvaluation
                                             , SPIRV.Geometry
                                             ]
                              -> traverse_
                                  ( \case { (i, "gl_Position"  )
                                              -> addMemberDecoration
                                                    structTyID
                                                    i
                                                    ( SPIRV.Builtin SPIRV.Position  )
                                          ; (i, "gl_PointSize" )
                                              -> addMemberDecoration
                                                    structTyID
                                                    i
                                                    ( SPIRV.Builtin SPIRV.PointSize )
                                          ; _ -> pure ()
                                          }
                                  )
                                  labelledElts
                            _ -> pure ()

                          -- declare the type
                          mkTyConInstruction (toArgs eltIDs) structTyID
                )

        SPIRV.Pointer storage a ->
          createIDRec _knownPrimTy
            ( typeID a )
            ( \ tyID -> mkTyConInstruction
                          ( Arg storage $ Arg tyID EndArgs )
            )

        SPIRV.PrimTy.Image (SPIRV.Image.Image { .. })
          -> createIDRec _knownPrimTy
               ( typeID (SPIRV.Scalar component) )  -- get the type ID of the image 'component' type
               ( \componentID
                   -> mkTyConInstruction
                        (   Arg componentID
                          $ Arg dimensionality
                          $ Arg hasDepth
                          $ Arg arrayness
                          $ Arg multiSampling
                          $ Arg imageUsage -- whether the image is sampled or serves as storage
                          $ Arg imageFormat EndArgs
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
           ( MonadState CGState m
           , MonadFresh ID m
           , MonadError Text m
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
            ( traverse constID . unM . transpose $ a ) -- get the ID for each column
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
              "constID: cannot construct run-time arrays.\n\
              \Runtime arrays are only available through uniforms."
        
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

builtinID :: (MonadState CGState m, MonadFresh ID m)
          => SPIRV.Stage -> Text -> Text -> m ID
builtinID stage stageName builtinName =
  tryToUse ( _builtin stage stageName builtinName )
    id
    pure

-- left-biased semigroup operation
infixl 6 <<?>
(<<?>) :: forall x. Maybe x -> Maybe x -> Maybe x
(<<?>) = coerce ( (<>) @(Maybe (First x)) )

bindingID :: forall m.
             ( MonadState CGState m
             , MonadReader CGContext m
             , MonadError Text m
             , MonadFresh ID m
             ) => Text -> m (ID, SPIRV.PrimTy)
bindingID varName
  = do  ctxt <- use _functionContext

        -- is this a built-in variable?
        (mbBuiltin, mbStage) :: (Maybe SPIRV.PointerTy, Maybe ( SPIRV.Stage, Text ))
          <- case ctxt of
            EntryPoint stage stageName ->
              do
                modes <- toList . fromMaybe Set.empty <$> use ( _entryPointExecutionModes stage stageName )
                let mbBuiltinTy = lookup varName (stageBuiltins stage modes)
                pure ( mbBuiltinTy, Just ( stage, stageName) )
            _ ->
              pure (Nothing, Nothing)

        case (mbBuiltin, mbStage) of
          (Just ptrTy, Just (stage, stageName)) ->
            -- this is a built-in variable, use 'builtinID'
            do
              builtin <- builtinID stage stageName varName
              let ptrPrimTy = SPIRV.pointerTy ptrTy
              _ <- typeID ptrPrimTy -- ensure pointer type is declared
              -- note that 'builtinID' sets the necessary decorations for the builtin
              pure (builtin, ptrPrimTy)

          _ -> do
            -- this is not a built-in variable, obtain the binding ID
            mbLoc   <- use ( _localBinding varName )
            mbKnown <- use ( _knownBinding varName )
            mbGlob  <-
              do  glob <- fmap (second SPIRV.pointerTy) <$> globalID varName mbStage
                  -- declare pointer type (if necessary)
                  mapM_ (typeID . snd) glob
                  pure glob

            bd@(bdID,_)
                <- note
                    ( "codeGen: no binding with name " <> varName )
                    ( mbLoc <<?> mbKnown <<?> mbGlob )

            -- add the user decorations for this binding if necessary
            decorations <- fmap snd <$> view ( _userGlobal varName )
            case decorations of
              Nothing   -> pure ()
              Just decs -> addDecorations bdID decs

            -- return the binding ID
            pure bd

-- get the ID for a global variable,
-- adding this global to the list of 'used' global variables if it isn't already there
-- adding this global to the stage interface if applicable
globalID :: ( MonadState CGState m
            , MonadReader CGContext m
            , MonadFresh ID m
            )
         => Text -> Maybe (SPIRV.Stage, Text) -> m ( Maybe (ID, SPIRV.PointerTy) )
globalID globalName mbStage
  = do glob <- view ( _userGlobal globalName )
       case glob of
         Nothing
           -> pure Nothing
         Just (ptrTy, _)
           -> do
                ident <- tryToUse ( _usedGlobal globalName ) fst ( pure . ( , ptrTy ) )
                case (mbStage, ptrTy) of
                  ( Just (stage, stageName), SPIRV.PointerTy storage _ )
                    | storage == Storage.Input || storage == Storage.Output
                      -> assign ( _interfaceBinding stage stageName globalName ) ( Just ident )
                  _   -> pure ()
                pure ( Just (ident, ptrTy) )

stringLitID :: (MonadState CGState m, MonadFresh ID m)
          => Text -> m ID
stringLitID literal =
  tryToUse ( _knownStringLit literal )
    id
    pure
