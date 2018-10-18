{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module CodeGen.AST where

-- base
import Control.Arrow(right)
import Control.Category((>>>))
import Data.Maybe(fromJust)
import Data.Word(Word32)

-- binary
import Data.Binary(Binary)
import qualified Data.Binary     as Binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy(ByteString)

-- containers
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

-- mtl
import Control.Monad.Except(MonadError, ExceptT, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader(MonadReader, ReaderT, runReaderT)
import Control.Monad.State.Lazy(StateT, runStateT)
import Control.Monad.State.Class(MonadState)

-- transformers
import Control.Monad.Trans.Class(MonadTrans(lift))

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

-- lens
import Control.Lens( Lens', lens
                   , use, assign, (<<%=)
                   , at
                   )

-- fir
import Control.Arrow.Strength
--import AST(AST(..))
import CodeGen.Instruction ( Args(..), prependArg, argsList
                           , ID(..), Instruction(..)
                           , EntryPoint(..)
                           )
import CodeGen.Binary ( putHeader
                      , putCapabilities
                      , putExtendedInstructions
                      , putMemoryModel
                      , putEntryPoints
                      , putExecutionModes
                      , putTyDecs
                      )
--import qualified SPIRV.PrimOps      as SPIRV
import qualified SPIRV.Types        as SPIRV
import qualified SPIRV.OpCodes      as SPIRV
import qualified SPIRV.Capabilities as SPIRV

----------------------------------------------------------------------------

data CGState
  = CGState
    { currentID          :: ID
    , neededCapabilities :: [ SPIRV.Capability ]
    , knownExts          :: Map SPIRV.Extension Instruction
    , knownTypes         :: Map SPIRV.PrimTy    Instruction
    , knownBindings      :: Map Text            ID
    }

initialState :: CGState
initialState = CGState
  { currentID          = ID 1
  , neededCapabilities = [] 
  , knownExts          = Map.empty
  , knownTypes         = Map.empty
  , knownBindings      = Map.empty 
  }

data CGContext
  = CGContext
    { entryPoints :: Map Text (EntryPoint Text) }

-- bespoke monad for code generation
type CGMonad 
  = FreshSuccT CGState    -- supply of fresh variable IDs using CGState
      ( ReaderT CGContext -- context for code generation
        ( StateT CGState  -- state (including for instance a map of which types have been defined)
          ( ExceptT Text  -- for errors during code-generation
              Binary.PutM -- to write instructions in binary form
          )
        )
      )
deriving instance MonadReader CGContext CGMonad
deriving instance MonadError  Text      CGMonad
-- other instances automatically derived from the definition of FreshSuccT:
-- Functor, Applicative, Monad, MonadState CGState, MonadFresh ID

putCG :: Binary a => a -> CGMonad ()
putCG = lift . lift . lift . lift . Binary.put -- apologies

----------------------------------------------------------------------------
-- lenses

_currentID :: Lens' CGState ID
_currentID = lens currentID ( \s v -> s { currentID = v } )

_knownExts :: Lens' CGState (Map SPIRV.Extension Instruction)
_knownExts = lens knownExts ( \s v -> s { knownExts = v } )

_knownExt :: SPIRV.Extension -> Lens' CGState (Maybe Instruction)
_knownExt ext = _knownExts . at ext

_knownTypes :: Lens' CGState (Map SPIRV.PrimTy Instruction)
_knownTypes = lens knownTypes ( \s v -> s { knownTypes = v } )

_knownType :: SPIRV.PrimTy -> Lens' CGState (Maybe Instruction)
_knownType primTy = _knownTypes . at primTy

_knownBindings :: Lens' CGState (Map Text ID)
_knownBindings = lens knownBindings ( \s v -> s { knownBindings = v } )

_knownBinding :: Text -> Lens' CGState (Maybe ID)
_knownBinding binding = _knownBindings . at binding

----------------------------------------------------------------------------
-- fresh name generation

class HasSupply v s where
  supply :: Lens' s v

instance HasSupply ID CGState where
  supply = _currentID

class Monad m => MonadFresh v m where
  fresh :: m v

newtype FreshSuccT s m a = FreshSuccT { runFreshSuccT :: m a }

deriving instance Functor       m => Functor       (FreshSuccT s m)
deriving instance Applicative   m => Applicative   (FreshSuccT s m)
deriving instance Monad         m => Monad         (FreshSuccT s m)
deriving instance MonadState  s m => MonadState  s (FreshSuccT s m)

instance MonadTrans (FreshSuccT s) where
  lift = FreshSuccT

instance (MonadState s m, HasSupply v s, Enum v) => MonadFresh v (FreshSuccT s m) where
  fresh = supply <<%= succ -- <<%= means: "modify, and return the _old_ value"

create :: ( MonadFresh v m, MonadState s m )
       => Lens' s (Maybe a) -> (v -> a) -> m v
create _key mk
  = do v <- fresh
       let a = mk v
       assign _key ( Just a )
       pure v

createRec :: ( MonadFresh v m, MonadState s m )
          => Lens' s (Maybe a) -> m b -> (b -> v -> a) -> m v
createRec _key before mk
  = do b <- before
       v <- fresh
       let a = mk b v
       assign _key ( Just a )
       pure v

tryToUseWith :: ( MonadFresh v m, MonadState s m )
             => Lens' s (Maybe a) -> (a -> v) -> m v -> m v
tryToUseWith _key f creation =
  use _key >>= maybe creation (pure . f)

tryToUse :: ( MonadFresh v m, MonadState s m )
         => Lens' s (Maybe a) -> (a -> v) -> (v -> a) -> m v
tryToUse _key f mk = tryToUseWith _key f (create _key mk)

----------------------------------------------------------------------------
-- code generation

-- TODO: as we go along, all given binding names must be given an ID
-- use this to turn [ EntryPoint String ] into [ EntryPoint ID ]
-- ( both the entry point ID and the interface element IDs )

{-
codeGen :: AST a -> CGMonad ()
codeGen = error "lol"
-}

runExceptTPutM :: ExceptT e Binary.PutM a -> Either e (a, ByteString)
runExceptTPutM = runExceptT >>> uncurry leftStrength . Binary.runPutM

runCG :: CGContext -> CGMonad r -> Either Text (r, CGState, ByteString)
runCG context
  =   runFreshSuccT  
  >>> ( `runReaderT` context      )
  >>> ( `runStateT`  initialState )
  >>> runExceptTPutM
  >>> right ( \((r,s),b) -> (r,s,b) )


putDecs :: CGContext -> CGState -> ExceptT Text Binary.PutM ()
putDecs CGContext { entryPoints }
     CGState   { currentID
               , neededCapabilities
               , knownExts
               , knownTypes
               , knownBindings
               } 
  = do entryPointsWithIDs <- identifyEntryPoints knownBindings entryPoints
       lift $
         do putHeader ( idNumber currentID )
            putCapabilities neededCapabilities
            putExtendedInstructions knownExts
            putMemoryModel
            putEntryPoints    entryPointsWithIDs
            putExecutionModes entryPointsWithIDs
            putTyDecs         knownTypes
  
spirv_asm :: CGContext -> CGMonad r -> Either Text ByteString
spirv_asm context mr
  = case runCG context mr of

      Right (_, cgState, body)
        -> case runExceptTPutM $ putDecs context cgState of
              Right ((), decs) -> Right ( decs <> body )
              Left err         -> Left err

      Left err -> Left err


-- TODO: move this somewhere sensible
note :: MonadError e m => e -> Maybe a -> m a
note _ (Just a) = pure a
note e Nothing  = Except.throwError e


identifyEntryPoint
  :: forall m. ( MonadError Text m )
  => Map Text ID 
  -> EntryPoint Text
  -> m (EntryPoint ID)
identifyEntryPoint
  bindingIDs
  entryPoint@EntryPoint
    { entryPointName      = entryName
    , entryPointInterface = interface
    }  
  = do
      entryID <- 
        note
          (    "No entry point with name "
            <> entryName
            <> " was found during code generation."
          )
          ( Map.lookup entryName bindingIDs )

      interfaceIDs <- 
        traverse
          ( \ bindingName -> note
            (    "No binding with name "
              <> bindingName
              <> " was found during code generation."
            )
            ( Map.lookup bindingName bindingIDs )
          )
          interface
      
      pure ( entryPoint 
              { entryPointID        = entryID
              , entryPointInterface = interfaceIDs
              }
           )

identifyEntryPoints
  :: forall m. ( MonadError Text m )
  => Map Text ID
  -> Map Text (EntryPoint Text)
  -> m (Map Text (EntryPoint ID))
identifyEntryPoints = traverse . identifyEntryPoint


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
