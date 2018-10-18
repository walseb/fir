{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CodeGen.CodeGen where

-- base
import Data.Maybe(fromJust)
import Data.Word(Word32)

-- bytestring
import Data.ByteString.Lazy(ByteString)

-- mtl
import Control.Monad.State.Class(MonadState)

-- text-utf8
import qualified Data.Text as Text

-- lens
import Control.Lens(Lens')

-- text-utf8
import Data.Text(Text)

-- fir
import AST.AST(AST(..))
import CodeGen.Monad( CGState, CGContext
                    , CGMonad
                    , MonadFresh
                    , create, createRec
                    , tryToUse, tryToUseWith
                    , _knownExt, _knownType
                    )
import CodeGen.Instruction ( Args(..), prependArg, argsList
                           , ID, Instruction(..)
                           )
import CodeGen.Declarations(putASM)
import qualified SPIRV.Types   as SPIRV
import qualified SPIRV.OpCodes as SPIRV

----------------------------------------------------------------------------
-- main code generator

codeGen :: AST a -> CGMonad ()
codeGen = error "nope"

runCodeGen :: CGContext -> AST a -> Either Text ByteString
runCodeGen context = putASM context . codeGen

----------------------------------------------------------------------------
-- instructions generated along the way that need to be floated to the top

-- get extended instruction set ID (or create one if none exist)
extInstrID :: (MonadState CGState m, MonadFresh ID m)
           => SPIRV.Extension -> m ID
extInstrID ext = 
  tryToUse ( _knownExt ext )
    ( fromJust . resID ) -- ExtInstImport instruction always has a result ID
    ( \ v -> Instruction
      { name  = "ExtInstImport"
      , code  = SPIRV.OpCode 11
      , resTy = Nothing
      , resID = Just v
      , args  = Arg ( SPIRV.extensionName ext ) -- TODO: 'Put' instance for strings might be wrong
                EndArgs 
      }
  )

-- get an ID for a given type ( result ID of corresponding type constructor instruction )
-- ( if one is known use it, otherwise recursively create fresh IDs for necessary types )
typeID :: forall m. (MonadState CGState m, MonadFresh ID m)
       => SPIRV.PrimTy -> m ID
typeID primTy =
  tryToUseWith _knownPrimTy
    ( fromJust . resID ) -- type constructor instructions always have a result ID
    $ case primTy of

        SPIRV.Mat n _ a -> 
          createRec _knownPrimTy
            ( typeID (SPIRV.Vec n a) ) -- column type
            ( \ colID -> prependArg colID . mkTyConInstruction )
        
        SPIRV.Vec _ a ->
          createRec _knownPrimTy
            ( typeID a ) -- element type
            ( \ eltID -> prependArg eltID . mkTyConInstruction )
        
        _ -> create _knownPrimTy mkTyConInstruction
           
  where _knownPrimTy :: Lens' CGState (Maybe Instruction)
        _knownPrimTy = _knownType primTy

        ty :: SPIRV.Ty
        someTyConArgs :: [Word32]
        (ty, someTyConArgs) = SPIRV.tyAndSomeTyConArgs primTy

        mkTyConInstruction :: ID -> Instruction
        mkTyConInstruction v = Instruction
           { name = "Type" <> Text.pack ( show ty )
           , code = SPIRV.opTypeCode ty
           , resTy = Nothing
           , resID = Just v
           , args  = argsList someTyConArgs
           }