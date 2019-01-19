{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module CodeGen.IDs where

-- base
import Data.Coerce(coerce)
import Data.Foldable(traverse_)
import Data.Maybe(fromJust)
import Data.Semigroup(First(First))
import Data.Word(Word32)

-- lens
import Control.Lens(Lens', view, use)

-- mtl
import Control.Monad.Except(MonadError, throwError)
import Control.Monad.State(MonadState)
import Control.Monad.Reader(MonadReader)

-- text-utf8
import Data.Text(Text)

-- fir
import CodeGen.Instruction
  ( ID(ID), Instruction(..)
  , Args(Arg, EndArgs), toArgs
  )
import CodeGen.Monad
  ( MonadFresh
  , create, createRec
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
  , addCapabilities, addMemberName, addMemberDecoration, addDecorations
  )
import FIR.Builtin(stageBuiltins)
import FIR.Prim.Singletons
  ( PrimTy(primTySing), primTy
  , SPrimTy(..)
  , aConstant
  )
import FIR.Prim.Struct(traverseStruct)
import Math.Linear(M(unM), Matrix(transpose))
import qualified SPIRV.Builtin    as SPIRV(Builtin(Position,PointSize))
import qualified SPIRV.Capability as SPIRV(primTyCapabilities)
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.Extension  as SPIRV
import qualified SPIRV.Operation  as SPIRV.Op
import qualified SPIRV.PrimTy     as SPIRV
import qualified SPIRV.Stage      as SPIRV

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
          createRec _knownPrimTy
            ( typeID (SPIRV.Vector m (SPIRV.Scalar a)) ) -- column type
            ( \ colID 
                  -> mkTyConInstruction ( Arg colID $ Arg n EndArgs )
            )

        SPIRV.Vector n a ->
          createRec _knownPrimTy
            ( typeID a ) -- element type
            ( \ eltID 
                  -> mkTyConInstruction ( Arg eltID $ Arg n EndArgs )
            )

        SPIRV.Function as b ->
          createRec _knownPrimTy
            ( do asIDs <- traverse typeID as -- types of function arguments
                 bID   <- typeID b           -- return type of function
                 pure (asIDs, bID)
            )
            ( \ (asIDs, bID) 
                  -> mkTyConInstruction ( Arg bID $ toArgs asIDs )
            )

        SPIRV.Array l a ->
          createRec _knownPrimTy
            ( do lgID  <- constID l -- array size is the result of a constant instruction
                 eltID <- typeID  a --     as opposed to being a literal number
                 pure (eltID, lgID) -- (I suppose this is to do with specialisation constants)
            )
            ( \(eltID, lgId)
                -> mkTyConInstruction ( Arg eltID $ Arg lgId EndArgs ) 
            )

        SPIRV.RuntimeArray a ->
          createRec _knownPrimTy
            ( typeID a )
            ( \eltID -> mkTyConInstruction ( Arg eltID EndArgs ) )

        SPIRV.Unit    -> create _knownPrimTy ( mkTyConInstruction EndArgs )
        SPIRV.Boolean -> create _knownPrimTy ( mkTyConInstruction EndArgs )

        SPIRV.Scalar (SPIRV.Integer s w)
          -> create _knownPrimTy 
                ( mkTyConInstruction 
                  ( Arg (SPIRV.width w)
                  $ Arg (SPIRV.signedness s) EndArgs
                  )
                )

        SPIRV.Scalar (SPIRV.Floating w)
          -> create _knownPrimTy 
                ( mkTyConInstruction ( Arg (SPIRV.width w) EndArgs ) )

        SPIRV.Struct as
          -> createRec _knownPrimTy
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
                          --   (for gl_in, gl_out, gl_perVertex)
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
          createRec _knownPrimTy
            ( typeID a )
            ( \ tyID -> mkTyConInstruction
                          ( Arg storage $ Arg tyID EndArgs )
            )

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
          createRec _knownAConstant
            ( traverse constID . unM . transpose $ a ) -- get the ID for each column
            ( \ cols -> 
                  mkConstantInstruction 
                    SPIRV.Op.ConstantComposite 
                    ( toArgs cols )
            )

        SVector {} ->
          createRec _knownAConstant
            ( traverse constID a ) -- get the result ID for each component
            ( \ eltIDs -> 
                  mkConstantInstruction 
                    SPIRV.Op.ConstantComposite
                    (toArgs eltIDs)
            )

        SScalar {}
          -> create _knownAConstant
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
          create _knownAConstant
            ( mkConstantInstruction
                ( if a
                  then SPIRV.Op.ConstantTrue
                  else SPIRV.Op.ConstantFalse
                )
                EndArgs
            )

        SRuntimeArray {} ->
            throwError
              "constID: cannot construct runtime arrays.\n\
              \Runtime arrays are only available through uniforms."
        
        SArray {} ->
          createRec _knownAConstant
            ( traverse constID a )
            ( \ eltIDs ->
                  mkConstantInstruction
                    SPIRV.Op.ConstantComposite
                    ( toArgs eltIDs )
            )
        
        SStruct {} ->
          createRec _knownAConstant
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

bindingID :: ( MonadState CGState m
             , MonadReader CGContext m
             , MonadError Text m
             , MonadFresh ID m
             ) => Text -> m (ID, SPIRV.PrimTy)
bindingID varName
  = do  ctxt <- use _functionContext
        case ctxt of
          EntryPoint stage stageName
            | Just ptrTy <- lookup varName (stageBuiltins stage)
            -> do builtin <- builtinID stage stageName varName
                  -- note that 'builtinID' sets the necessary decorations for the builtin
                  pure (builtin, ptrTy)
                
          _ -> do -- obtain the binding ID
                  loc     <- use ( _localBinding varName )
                  known   <- use ( _knownBinding varName )
                  glob    <- globalID varName

                  bd@(bdID,_)
                      <- note
                          ( "codeGen: no binding with name " <> varName )
                          ( loc <<?> known <<?> glob )

                  -- add the user decorations for this binding if necessary
                  decorations <- fmap snd <$> view ( _userGlobal varName )
                  case decorations of
                    Nothing   -> pure ()
                    Just decs -> addDecorations bdID decs

                  -- return the binding ID
                  pure bd

globalID :: ( MonadState CGState m
            , MonadReader CGContext m
            , MonadFresh ID m
            )
         => Text -> m ( Maybe (ID, SPIRV.PrimTy) )
globalID globalName
  = do glob <- view ( _userGlobal globalName )
       case glob of
         Nothing
           -> pure Nothing
         Just (ptrTy, _)
           -> do ident <- tryToUse ( _usedGlobal globalName )
                            fst
                            ( pure . ( , ptrTy ) )
                 pure ( Just (ident, ptrTy) )

stringLitID :: (MonadState CGState m, MonadFresh ID m)
          => Text -> m ID
stringLitID literal =
  tryToUse ( _knownStringLit literal )
    id
    pure
