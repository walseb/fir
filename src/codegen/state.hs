{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE RankNTypes       #-}

module CodeGen.State where

-- base
import Data.Foldable
  ( traverse_, toList )
import Data.Maybe
  ( fromMaybe )
import Data.Word
  ( Word32 )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set

-- lens
import Control.Lens
  ( Lens', lens
  , at
  , view, set
  , assign, modifying
  )

-- mtl
import Control.Monad.State
  ( MonadState )

-- text-utf8
import "text-utf8" Data.Text
  ( Text )
import qualified "text-utf8" Data.Text as Text

-- fir
import CodeGen.Instruction
  ( ID(ID)
  , Instruction
  )
import FIR.Builtin
  ( stageBuiltins, builtinDecorations )
import FIR.Prim.Singletons
  ( AConstant )
import qualified SPIRV.Capability      as SPIRV
import qualified SPIRV.Decoration      as SPIRV
import qualified SPIRV.ExecutionMode   as SPIRV
import qualified SPIRV.Extension       as SPIRV
import qualified SPIRV.FunctionControl as SPIRV
import qualified SPIRV.Image           as SPIRV
import qualified SPIRV.PrimTy          as SPIRV
import qualified SPIRV.Stage           as SPIRV

----------------------------------------------------------------------------
-- code generator monad

-- code generator state
-- this consists of information we need to keep track of along the way,
-- for instance which types have been declared
data CGState
  = CGState
    -- current ID number (increases by 1 each time a new ID is needed)
      { currentID           :: ID
    -- ID of the current block in the CFG (if inside a block)
      , currentBlock        :: Maybe ID
      , functionContext     :: FunctionContext
      , neededCapabilities  :: Set                     SPIRV.Capability
      , knownExtInsts       :: Map SPIRV.ExtInst       Instruction
      , knownStringLits     :: Map Text                ID
      , names               :: Set                     (ID, Either Text (Word32, Text))
      -- entry-point interfaces, keeping track of which global variables are used
      , interfaces          :: Map (SPIRV.Stage, Text) (Map Text ID)
      , executionModes      :: Map (SPIRV.Stage, Text) (Set (SPIRV.ExecutionMode Word32))
      , decorations         :: Map ID                  (Set (SPIRV.Decoration    Word32))
      , memberDecorations   :: Map (ID, Word32)        (Set (SPIRV.Decoration    Word32))
      -- map of all types used
      , knownTypes          :: Map SPIRV.PrimTy        Instruction
      -- map of all constants used
      , knownConstants      :: Map AConstant           Instruction
      -- which top-level global (input/output) variables have been used
      , usedGlobals         :: Map Text                (ID, SPIRV.PointerTy)
      -- top-level bindings available, such as input/output variables and top-level functions
      , knownBindings       :: Map Text                (ID, SPIRV.PrimTy   )
      -- variables declared by the user in the program
      , localBindings       :: Map Text                (ID, SPIRV.PrimTy   )
      -- IDs of locally declared variables (floated to the top of the function definition)
      , localVariables      :: Map ID                  SPIRV.PointerTy
      -- pointer ID associated to a given ID
      -- used to keep track of auxiliary temporary pointers
      -- (e.g. a pointer created for a runtime access chain operation)
      , temporaryPointers   :: Map ID                  (ID, PointerState)
      }
  deriving Show

data FunctionContext
  = TopLevel
  | Function [(Text, SPIRV.PrimTy)] -- argument names & types
  | EntryPoint SPIRV.Stage Text     -- stage, and stage name
  deriving ( Eq, Show )

data PointerState
  = Fresh
  | Modified
  deriving ( Eq, Show )

initialState :: CGState
initialState
  = CGState
      { currentID           = ID 1
      , currentBlock        = Nothing
      , functionContext     = TopLevel
      , neededCapabilities  = Set.empty
      , knownExtInsts       = Map.empty
      , knownStringLits     = Map.empty
      , names               = Set.empty
      , interfaces          = Map.empty
      , executionModes      = Map.empty
      , decorations         = Map.empty
      , memberDecorations   = Map.empty
      , knownTypes          = Map.empty
      , knownConstants      = Map.empty
      , usedGlobals         = Map.empty
      , knownBindings       = Map.empty
      , localBindings       = Map.empty
      , localVariables      = Map.empty
      , temporaryPointers   = Map.empty
      }

data CGContext
  = CGContext
     { -- user defined inputs/outputs (not builtins)
       userGlobals
          :: Map Text (SPIRV.PointerTy, Set (SPIRV.Decoration Word32) )
       -- user defined functions (not entry points)
     , userFunctions
          :: Map Text SPIRV.FunctionControl
       -- entry points
     , userEntryPoints
          :: Map Text (SPIRV.Stage, Set (SPIRV.ExecutionMode Word32) )
       -- images
     , userImages
          :: Map Text SPIRV.Image
     , debugMode :: Bool
     }

emptyContext :: CGContext
emptyContext
  = CGContext
      { userGlobals     = Map.empty
      , userFunctions   = Map.empty
      , userEntryPoints = Map.empty
      , userImages      = Map.empty
      , debugMode       = True
      }

----------------------------------------------------------------------------
-- useful function to deal with nested data structures
-- such as 'Map a (Map b c)'

affineTraverse :: (Monoid a, Functor f) => (a -> f b) -> (Maybe a -> f (Maybe b))
affineTraverse f Nothing  = fmap Just (f mempty)
affineTraverse f (Just a) = fmap Just (f a)

----------------------------------------------------------------------------
-- lenses

_currentID :: Lens' CGState ID
_currentID = lens currentID ( \s v -> s { currentID = v } )

_currentBlock :: Lens' CGState ( Maybe ID )
_currentBlock = lens currentBlock ( \s v -> s { currentBlock = v } )

_functionContext :: Lens' CGState FunctionContext
_functionContext = lens functionContext ( \s v -> s { functionContext = v } )

_neededCapabilities :: Lens' CGState (Set SPIRV.Capability)
_neededCapabilities = lens neededCapabilities ( \s v -> s { neededCapabilities = v } )

_neededCapability :: SPIRV.Capability -> Lens' CGState (Maybe ())
_neededCapability capability = _neededCapabilities . at capability

_knownExtInsts :: Lens' CGState (Map SPIRV.ExtInst Instruction)
_knownExtInsts = lens knownExtInsts ( \s v -> s { knownExtInsts = v } )

_knownExtInst :: SPIRV.ExtInst -> Lens' CGState (Maybe Instruction)
_knownExtInst ext = _knownExtInsts . at ext

_knownStringLits :: Lens' CGState (Map Text ID)
_knownStringLits = lens knownStringLits ( \s v -> s { knownStringLits = v } )

_knownStringLit :: Text -> Lens' CGState (Maybe ID)
_knownStringLit lit = _knownStringLits . at lit

_names :: Lens' CGState ( Set (ID, Either Text (Word32, Text)) )
_names = lens names ( \s v -> s { names = v } )

_usedGlobals :: Lens' CGState (Map Text (ID, SPIRV.PointerTy))
_usedGlobals = lens usedGlobals ( \s v -> s { usedGlobals = v } )

_usedGlobal :: Text -> Lens' CGState (Maybe (ID, SPIRV.PointerTy))
_usedGlobal name = _usedGlobals . at name

_interfaces :: Lens' CGState (Map (SPIRV.Stage, Text) (Map Text ID))
_interfaces = lens interfaces ( \s v -> s { interfaces = v } )

_interface :: SPIRV.Stage -> Text -> Lens' CGState (Maybe (Map Text ID))
_interface stage stageName = _interfaces . at (stage, stageName)

_interfaceBinding :: SPIRV.Stage -> Text -> Text -> Lens' CGState (Maybe ID)
_interfaceBinding stage stageName varName
  = _interface stage stageName
  . affineTraverse
  . at varName

_builtin :: SPIRV.Stage -> Text -> Text -> Lens' CGState (Maybe ID)
_builtin stage stageName builtinName
  = lens
      ( view _interfaceBuiltin )
      ( \s mb_i -> case mb_i of
         Nothing -> s
         Just i  -> set _interfaceBuiltin           (Just i)
                  . set ( _usedGlobal builtinName ) (Just (i, builtinTy s))
                  . set ( _decorate i ) (Just $ builtinDecorations builtinName)
                  $ s
      )
  where _interfaceBuiltin :: Lens' CGState (Maybe ID)
        _interfaceBuiltin = _interfaceBinding stage stageName builtinName

        builtinTy :: CGState -> SPIRV.PointerTy
        builtinTy s =
          let modes :: [SPIRV.ExecutionMode Word32]
              modes = fromMaybe []
                        ( toList <$> Map.lookup (stage, stageName) (executionModes s) )
          in
            fromMaybe
              ( error
                ( "_builtin: builtin with name " ++ Text.unpack builtinName ++ " cannot be found,\n\
                    \among builtins for " ++ show stage ++ " stage named " ++ Text.unpack stageName
                )
              )
              ( lookup builtinName $ stageBuiltins stage modes )

_executionModes :: Lens' CGState (Map (SPIRV.Stage, Text) (Set (SPIRV.ExecutionMode Word32)))
_executionModes = lens executionModes ( \s v -> s { executionModes = v } )

_entryPointExecutionModes
  :: SPIRV.Stage
  -> Text
  -> Lens'
        CGState
        ( Maybe ( Set (SPIRV.ExecutionMode Word32) ) )
_entryPointExecutionModes stage stageName = _executionModes . at (stage, stageName)

_decorations :: Lens' CGState (Map ID (Set (SPIRV.Decoration Word32)))
_decorations = lens decorations ( \s v -> s { decorations = v } )

_decorate :: ID -> Lens' CGState (Maybe (Set (SPIRV.Decoration Word32)))
_decorate bindingID = _decorations . at bindingID

_memberDecorations
  :: Lens'
        CGState
        ( Map (ID, Word32) ( Set ( SPIRV.Decoration Word32 ) ) )
_memberDecorations = lens memberDecorations ( \s v -> s { memberDecorations = v } )

_memberDecorate
  :: ID
  -> Word32
  -> Lens'
        CGState
        ( Maybe ( Set ( SPIRV.Decoration Word32 ) ) )
_memberDecorate bindingID index
  = _memberDecorations
  . at (bindingID, index)

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

_localVariables :: Lens' CGState (Map ID SPIRV.PointerTy)
_localVariables = lens localVariables ( \s v -> s { localVariables = v } )

_localVariable :: ID -> Lens' CGState (Maybe SPIRV.PointerTy)
_localVariable v = _localVariables . at v

_temporaryPointers :: Lens' CGState (Map ID (ID, PointerState))
_temporaryPointers = lens temporaryPointers ( \s v -> s { temporaryPointers = v } )

_temporaryPointer :: ID -> Lens' CGState (Maybe (ID, PointerState))
_temporaryPointer v = _temporaryPointers . at v


_userGlobals
  :: Lens' CGContext
        ( Map Text
            ( SPIRV.PointerTy
            , Set ( SPIRV.Decoration Word32 )
            )
        )
_userGlobals = lens userGlobals ( \c v -> c { userGlobals = v } )

_userGlobal
  :: Text
  -> Lens' CGContext
        ( Maybe
            ( SPIRV.PointerTy
            , Set ( SPIRV.Decoration Word32 )
            )
        )
_userGlobal global = _userGlobals . at global

_userFunctions :: Lens' CGContext ( Map Text SPIRV.FunctionControl )
_userFunctions = lens userFunctions ( \c v -> c { userFunctions = v } )

_userFunction :: Text -> Lens' CGContext ( Maybe SPIRV.FunctionControl )
_userFunction function = _userFunctions . at function

_userEntryPoints :: Lens' CGContext ( Map Text ( SPIRV.Stage, Set (SPIRV.ExecutionMode Word32) ) )
_userEntryPoints = lens userEntryPoints ( \c v -> c { userEntryPoints = v } )

_userEntryPoint :: Text -> Lens' CGContext ( Maybe ( SPIRV.Stage, Set (SPIRV.ExecutionMode Word32) ) )
_userEntryPoint entryPoint = _userEntryPoints . at entryPoint

_userImages :: Lens' CGContext ( Map Text SPIRV.Image )
_userImages = lens userImages ( \c v -> c { userImages = v } )

_userImage :: Text -> Lens' CGContext ( Maybe SPIRV.Image )
_userImage image = _userImages . at image

_debugMode :: Lens' CGContext Bool
_debugMode = lens debugMode ( \c v -> c { debugMode = v } )

-----------------------------------------------------------------------------
-- various utility functions to update state

addCapabilities :: forall t m.
                  ( Traversable t, MonadState CGState m )
                => t SPIRV.Capability -> m ()
addCapabilities
  = traverse_ ( \cap -> assign ( _neededCapability cap ) (Just ()) )

addMemberName :: MonadState CGState m 
              => ID -> Word32 -> Text -> m ()
addMemberName structTyID index name
  = modifying _names
      ( Set.insert (structTyID, Right (index,name)) )

addDecorations :: MonadState CGState m
               => ID -> Set (SPIRV.Decoration Word32) -> m ()
addDecorations bdID decs
  = modifying ( _decorate bdID )
      ( Just . maybe decs (Set.union decs) )

addMemberDecoration :: MonadState CGState m
                    => ID -> Word32 -> SPIRV.Decoration Word32 -> m ()
addMemberDecoration structID index dec
  = modifying ( _memberDecorate structID index )
      ( Just . maybe (Set.singleton dec) (Set.insert dec) )
