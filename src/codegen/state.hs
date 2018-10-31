{-# LANGUAGE RankNTypes                 #-}

module CodeGen.State where

-- base
import Data.Maybe(fromMaybe)

-- containers
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

-- lens
import Control.Lens( Lens', lens
                   , at
                   , view, set
                   )

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

-- fir
import CodeGen.Instruction( ID(ID)
                          , Instruction
                          )
import FIR.Builtin(Stage(..), stageBuiltins)
import FIR.PrimTy(AConstant)
import qualified SPIRV.Capability as SPIRV
import qualified SPIRV.Extension  as SPIRV
import qualified SPIRV.PrimTy     as SPIRV
import qualified SPIRV.Storage    as SPIRV

----------------------------------------------------------------------------
-- code generator monad

-- code generator state
-- this consists of information we need to keep track of along the way,
-- for instance which types have been declared
data CGState
  = CGState
    { currentID           :: ID
    , currentBlock        :: Maybe ID
    , functionContext     :: FunctionContext
    , neededCapabilities  :: Set               SPIRV.Capability
    , knownExtInsts       :: Map SPIRV.ExtInst Instruction
    , interfaces          :: Map (Stage, Text) (Set Text)
    , annotations         :: Set               Instruction
    , knownTypes          :: Map SPIRV.PrimTy  Instruction
    , knownConstants      :: Map AConstant     Instruction
    , usedGlobals         :: Map Text          (ID, (SPIRV.PrimTy, SPIRV.StorageClass))
    , knownBindings       :: Map Text          (ID, SPIRV.PrimTy)
    , localBindings       :: Map Text          (ID, SPIRV.PrimTy)
    }
  deriving Show

data FunctionContext
  = TopLevel
  | Function [(Text, SPIRV.PrimTy)] -- argument names & types
  | EntryPoint Stage Text -- stage, and stage name
  deriving ( Eq, Show )

data VariableContext
  = KnownBinding
  | UserGlobal
  | LocalBinding
  | Builtin
  deriving ( Eq, Show )

initialState :: CGState
initialState = CGState
  { currentID           = ID 1
  , currentBlock        = Nothing
  , functionContext     = TopLevel
  , neededCapabilities  = Set.empty
  , knownExtInsts       = Map.empty
  , interfaces          = Map.empty
  , annotations         = Set.empty
  , knownTypes          = Map.empty
  , knownConstants      = Map.empty
  , usedGlobals         = Map.empty
  , knownBindings       = Map.empty
  , localBindings       = Map.empty
  }

data CGContext
  = CGContext
     { userGlobals :: Map Text SPIRV.PrimTy
     }

emptyContext :: CGContext
emptyContext = CGContext { userGlobals = Map.empty }

----------------------------------------------------------------------------
-- lenses

_currentID :: Lens' CGState ID
_currentID = lens currentID ( \s v -> s { currentID = v } )

_currentBlock :: Lens' CGState ( Maybe ID )
_currentBlock = lens currentBlock ( \s v -> s { currentBlock = v } )

_functionContext :: Lens' CGState FunctionContext
_functionContext = lens functionContext ( \s v -> s { functionContext = v } )

_knownExtInsts :: Lens' CGState (Map SPIRV.ExtInst Instruction)
_knownExtInsts = lens knownExtInsts ( \s v -> s { knownExtInsts = v } )

_knownExtInst :: SPIRV.ExtInst -> Lens' CGState (Maybe Instruction)
_knownExtInst ext = _knownExtInsts . at ext

_usedGlobals :: Lens' CGState (Map Text (ID, (SPIRV.PrimTy, SPIRV.StorageClass)))
_usedGlobals = lens usedGlobals ( \s v -> s { usedGlobals = v } )

_usedGlobal :: Text -> Lens' CGState (Maybe (ID, (SPIRV.PrimTy, SPIRV.StorageClass)))
_usedGlobal name = _usedGlobals . at name

_interfaces :: Lens' CGState (Map (Stage, Text) (Set Text))
_interfaces = lens interfaces ( \s v -> s { interfaces = v } )

_interface :: Stage -> Text -> Lens' CGState (Maybe (Set Text))
_interface stage stageName = _interfaces . at (stage, stageName)

_builtin :: Stage -> Text -> Text -> Lens' CGState (Maybe ID)
_builtin stage stageName builtinName
  = lens
      ( \s -> case view _interfaceBuiltin s of
                Just () -> fst <$> view _usedBuiltin s
                Nothing -> Nothing
      )
      ( \s mb_i -> case mb_i of
         Nothing -> s
         Just i  -> set _interfaceBuiltin (Just ())
                  . set _usedBuiltin      (Just (i,(ty,storage)))
                  $ s
      )
  where affineTraverse :: (Monoid a, Functor f) => (a -> f b) -> (Maybe a -> f (Maybe b))
        affineTraverse f Nothing  = fmap Just (f mempty)
        affineTraverse f (Just a) = fmap Just (f a)

        _interfaceBuiltin :: Lens' CGState (Maybe ())
        _interfaceBuiltin = _interface stage stageName
                          . affineTraverse
                          . at builtinName

        _usedBuiltin :: Lens' CGState (Maybe (ID, (SPIRV.PrimTy, SPIRV.StorageClass)))
        _usedBuiltin = _usedGlobal builtinName

        ty :: SPIRV.PrimTy
        storage :: SPIRV.StorageClass
        (ty, storage) =
          fromMaybe
            ( error ( "_builtin: builtin with name " ++ Text.unpack builtinName ++ " cannot be found,\n\
                        \among builtins for " ++ show stage ++ " stage named " ++ Text.unpack stageName
                    )
            )
            ( lookup builtinName (stageBuiltins stage) )


_knownTypes :: Lens' CGState (Map SPIRV.PrimTy Instruction)
_knownTypes = lens knownTypes ( \s v -> s { knownTypes = v } )

_knownType :: SPIRV.PrimTy -> Lens' CGState (Maybe Instruction)
_knownType primTy = _knownTypes . at primTy

_knownConstants :: Lens' CGState (Map AConstant Instruction)
_knownConstants = lens knownConstants ( \s v -> s { knownConstants = v } )

_knownConstant :: AConstant -> Lens' CGState (Maybe Instruction)
_knownConstant constant = _knownConstants . at constant

_knownBindings :: Lens' CGState (Map Text (ID, SPIRV.PrimTy))
_knownBindings = lens knownBindings ( \s v -> s { knownBindings = v } )

_knownBinding :: Text -> Lens' CGState (Maybe (ID, SPIRV.PrimTy))
_knownBinding binding = _knownBindings . at binding

_localBindings :: Lens' CGState (Map Text (ID, SPIRV.PrimTy))
_localBindings = lens localBindings ( \s v -> s { localBindings = v } )

_localBinding :: Text -> Lens' CGState (Maybe (ID, SPIRV.PrimTy))
_localBinding binding = _localBindings . at binding



_userGlobals :: Lens' CGContext (Map Text SPIRV.PrimTy)
_userGlobals = lens userGlobals ( \c v -> c { userGlobals = v } )

_userGlobal :: Text -> Lens' CGContext (Maybe SPIRV.PrimTy)
_userGlobal global = _userGlobals . at global