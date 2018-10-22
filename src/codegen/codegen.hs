{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CodeGen.CodeGen where

-- base
import Data.Maybe(fromJust)
import Data.Word(Word32)

-- bytestring
import Data.ByteString.Lazy(ByteString)

-- mtl
import Control.Monad.State.Class(MonadState)

-- lens
import Control.Lens(Lens')

-- text-utf8
import Data.Text(Text)

-- fir
import CodeGen.Monad( CGState, CGContext
                    , CGMonad
                    , MonadFresh
                    , create, createRec
                    , tryToUse, tryToUseWith
                    , _knownExtInst
                    , _knownType
                    , _knownConstant
                    )
import CodeGen.Instruction ( Args(..), prependArg, toArgs
                           , ID, Instruction(..)
                           )
import CodeGen.Declarations(putASM)
import FIR.AST(AST(..))
import FIR.PrimTy(PrimTy(primTySing), primTy, sPrimTy, SPrimTy(..), aConstant)
import Math.Linear(M(unM), Matrix(transpose))
import qualified SPIRV.Extension as SPIRV
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV

----------------------------------------------------------------------------
-- main code generator

codeGen :: AST a -> CGMonad ()
codeGen = error "nope"

runCodeGen :: CGContext -> AST a -> Either Text ByteString
runCodeGen context = putASM context . codeGen

----------------------------------------------------------------------------
-- instructions generated along the way that need to be floated to the top

-- get extended instruction set ID (or create one if none exist)
extInstID :: (MonadState CGState m, MonadFresh ID m)
           => SPIRV.ExtInst -> m ID
extInstID extInst = 
  tryToUse ( _knownExtInst extInst )
    ( fromJust . resID ) -- ExtInstImport instruction always has a result ID
    ( \ v -> pure $ Instruction
      { operation = SPIRV.Op.ExtInstImport
      , resTy     = Nothing
      , resID     = Just v
      , args      = Arg ( SPIRV.extInstName extInst ) -- TODO: 'Put' instance for strings might be wrong
                    EndArgs 
      }
  )

-- get an ID for a given type ( result ID of corresponding type constructor instruction )
-- ( if one is known use it, otherwise recursively create fresh IDs for necessary types )
typeID :: forall m. (MonadState CGState m, MonadFresh ID m)
       => SPIRV.PrimTy -> m ID
typeID ty =
  tryToUseWith _knownPrimTy
    ( fromJust . resID ) -- type constructor instructions always have a result ID
    $ case ty of

        SPIRV.Matrix m _ a -> 
          createRec _knownPrimTy
            ( typeID (SPIRV.Vector m a) ) -- column type
            ( \ colID -> pure . prependArg colID . mkTyConInstruction )
        
        SPIRV.Vector _ a ->
          createRec _knownPrimTy
            ( typeID a ) -- element type
            ( \ eltID -> pure . prependArg eltID . mkTyConInstruction )
        
        _ -> create _knownPrimTy ( pure . mkTyConInstruction )
           
  where _knownPrimTy :: Lens' CGState (Maybe Instruction)
        _knownPrimTy = _knownType ty

        op :: SPIRV.Op.Operation
        someTyConArgs :: [Word32]
        (op, someTyConArgs) = SPIRV.tyAndSomeTyConArgs ty

        mkTyConInstruction :: ID -> Instruction
        mkTyConInstruction v = Instruction
           { operation = op
           , resTy     = Nothing
           , resID     = Just v
           , args      = toArgs someTyConArgs
           }

constID :: forall m a. 
           ( MonadState CGState m, MonadFresh ID m
           , PrimTy a
           )
        => a -> m ID
constID a =
  tryToUseWith _knownAConstant
    ( fromJust . resID ) -- constant definition instructions always have a result ID
    $ case primTySing @a of

        SMatrix m n eltTySing ->
          createRec _knownAConstant
            ( toArgs <$> ( traverse constID . unM . transpose $ a ) ) -- get the ID for each column
            $ \ cols -> mkConstantInstruction 
                          SPIRV.Op.ConstantComposite 
                          ( SPIRV.Matrix 
                              (SPIRV.sDim $ SPIRV.natSDim m) 
                              (SPIRV.sDim $ SPIRV.natSDim n)
                              (sPrimTy eltTySing)
                          )
                          cols

        SVector n eltTySing -> 
          createRec _knownAConstant
            ( toArgs <$> traverse constID a ) -- get the result ID for each component
            $ \ elts -> mkConstantInstruction 
                          SPIRV.Op.ConstantComposite
                            ( SPIRV.Vector 
                                (SPIRV.sDim $ SPIRV.natSDim n) 
                                (sPrimTy eltTySing)
                            ) 
                            elts

        SUnit -> error "Error: 'constId' called on Unit type.\n\
                       \Unit has a unique value, and as such does not need to be constructed."

        SBool -> 
          create _knownAConstant
            $ mkConstantInstruction
                ( if a
                  then SPIRV.Op.ConstantTrue
                  else SPIRV.Op.ConstantFalse
                )
                SPIRV.Boolean
                EndArgs

        -- scalar (by elimination)
        _ -> create _knownAConstant 
              ( mkConstantInstruction SPIRV.Op.Constant (primTy @a) (Arg a EndArgs) )

  where _knownAConstant :: Lens' CGState (Maybe Instruction)
        _knownAConstant = _knownConstant ( aConstant a )

        mkConstantInstruction :: SPIRV.Op.Operation -> SPIRV.PrimTy -> Args -> ID -> m Instruction
        mkConstantInstruction op ty flds v = 
          do resTypeID <- typeID ty
             pure Instruction
                    { operation = op
                    , resTy     = Just resTypeID
                    , resID     = Just v
                    , args      = flds
                    }