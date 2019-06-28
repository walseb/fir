{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE RankNTypes       #-}

{-|
Module: CodeGen.State

This module defines the structure of the state used by the code generation,
in two parts:

  - 'CGContext', consisting of reified type-level data provided at the start
  of code-generation (with a 'Control.Monad.Reader.ReaderT' transformer),
  - 'CGState', consisting of state accumulated during code-generation,
  recording which objects have been declared and their IDs.

This module manually provides lenses for these. Most of these could be derived
automatically, but a few of them are slightly trickier, requiring an affine traversal.

These custom lenses also allow terse state modifications.
For instance, usage of a built-in variable simultaneously obtains an ID for the variable,
adds itself to the relevant entry point interface, and sets the necessary decorations.

-}

module CodeGen.State where

-- base
import Data.Foldable
  ( traverse_ )
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
  ( ID(ID), TyID(tyID)
  , Instruction
  )
import FIR.Builtin
  ( modelBuiltins, builtinDecorations )
import FIR.ASTState
  ( FunctionContext(TopLevel), VLFunctionContext ) -- value-level function context
import FIR.Prim.Singletons
  ( AConstant )
import qualified SPIRV.Capability    as SPIRV
import qualified SPIRV.Control       as SPIRV
import qualified SPIRV.Decoration    as SPIRV
import qualified SPIRV.ExecutionMode as SPIRV
import qualified SPIRV.Extension     as SPIRV
import qualified SPIRV.Image         as SPIRV
import qualified SPIRV.PrimTy        as SPIRV
import qualified SPIRV.Stage         as SPIRV

----------------------------------------------------------------------------
-- * Code generation state and context

-- | Code generation state.
--
-- Consists of information that code generation needs to keep track of along the way,
-- for instance which types have been declared.
data CGState
  = CGState
      {
      -- | Current ID number (increases by 1 each time a new ID is needed).
      currentID             :: ID

      -- | ID of the current block in the CFG (if inside a block).
      , currentBlock        :: Maybe ID

      -- | Current function context: top-level, within a function, within an entry point.
      , functionContext     :: VLFunctionContext

      -- | Capability requirements that have been declared.
      , neededCapabilities  :: Set                             SPIRV.Capability

      -- | IDs of all used extended instruction sets.
      , knownExtInsts       :: Map SPIRV.ExtInst               ID

      -- | IDs of constant text literals.
      , knownStringLits     :: Map Text                        ID

      -- | IDs which have been annotated with a name / member name.
      , names               :: Set                             (ID, Either Text (Word32, Text))

      -- | Entry point IDs.
      , entryPoints         :: Map (Text, SPIRV.ExecutionModel) ID

      -- | Entry point interfaces, keeping track of which global variables are used.
      , interfaces          :: Map (Text, SPIRV.ExecutionModel) (Map Text ID)

      -- | Decorations for given types.
      , decorations         :: Map ID                          SPIRV.Decorations

      -- | Decorations for members of a given struct type.
      , memberDecorations   :: Map (TyID, Word32)              SPIRV.Decorations

      -- | Map of all declared types.
      , knownTypes          :: Map SPIRV.PrimTy                Instruction

      -- | Map of all declared constants.
      , knownConstants      :: Map AConstant                   Instruction

      -- | Map of all types who have a corresponding "Undefined" instruction.
      , knownUndefineds     :: Map SPIRV.PrimTy                (ID, TyID)

      -- | Which top-level global (input/output) variables have been used?
      , usedGlobals         :: Map Text                        (ID, SPIRV.PointerTy)

      -- | Top-level bindings available, such as top-level functions.
      , knownBindings       :: Map Text                        (ID, SPIRV.PrimTy)

      -- | Variables declared by the user in the program.
      , localBindings       :: Map Text                        (ID, SPIRV.PrimTy)

      -- | IDs of locally declared variables (floated to the top of the function definition).
      , localVariables      :: Map ID                          SPIRV.PointerTy

      -- | Pointer ID associated to a given ID.
      -- Used to keep track of auxiliary temporary pointers
      -- (e.g. a pointer created for a runtime access chain operation).
      , temporaryPointers   :: Map ID                          (ID, PointerState)
      }
  deriving Show

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
      , entryPoints         = Map.empty
      , interfaces          = Map.empty
      , decorations         = Map.empty
      , memberDecorations   = Map.empty
      , knownTypes          = Map.empty
      , knownConstants      = Map.empty
      , knownUndefineds     = Map.empty
      , usedGlobals         = Map.empty
      , knownBindings       = Map.empty
      , localBindings       = Map.empty
      , localVariables      = Map.empty
      , temporaryPointers   = Map.empty
      }


-- | Code generation context.
--
-- Consists of information provided at the start of code generation,
-- as provided by the user-provided type annotations of
-- top-level functions, entry points and input/output variables.
data CGContext
  = CGContext
     { -- | User defined inputs/outputs (not builtins).
       userGlobals
          :: Map Text (SPIRV.PointerTy, SPIRV.Decorations)

       -- | User defined functions (not entry points).
     , userFunctions
          :: Map Text SPIRV.FunctionControl

       -- | User defined entry points.
     , userEntryPoints
          :: Map (Text, SPIRV.ExecutionModel) SPIRV.ExecutionModes

       -- | User defined images.
     , userImages
          :: Map Text SPIRV.Image

       -- | Whether to turn on debug mode,
       -- which adds extra source information in the generated SPIR-V assembly.
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
-- * Lenses

-- | Affine traversal relative to 'Maybe'.
--
-- Useful function to deal with nested data structures
-- such as @Map a (Map b c)@
affineTraverse :: (Monoid a, Functor f) => (a -> f b) -> (Maybe a -> f (Maybe b))
affineTraverse f Nothing  = fmap Just (f mempty)
affineTraverse f (Just a) = fmap Just (f a)


_currentID :: Lens' CGState ID
_currentID = lens currentID ( \s v -> s { currentID = v } )

_currentBlock :: Lens' CGState ( Maybe ID )
_currentBlock = lens currentBlock ( \s v -> s { currentBlock = v } )

_functionContext :: Lens' CGState VLFunctionContext
_functionContext = lens functionContext ( \s v -> s { functionContext = v } )

_neededCapabilities :: Lens' CGState (Set SPIRV.Capability)
_neededCapabilities = lens neededCapabilities ( \s v -> s { neededCapabilities = v } )

_neededCapability :: SPIRV.Capability -> Lens' CGState (Maybe ())
_neededCapability capability = _neededCapabilities . at capability

_knownExtInsts :: Lens' CGState (Map SPIRV.ExtInst ID)
_knownExtInsts = lens knownExtInsts ( \s v -> s { knownExtInsts = v } )

_knownExtInst :: SPIRV.ExtInst -> Lens' CGState (Maybe ID)
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

_entryPoints :: Lens' CGState (Map (Text, SPIRV.ExecutionModel) ID)
_entryPoints = lens entryPoints ( \s v -> s { entryPoints = v } )

_entryPoint :: Text -> SPIRV.ExecutionModel -> Lens' CGState (Maybe ID)
_entryPoint name stage = _entryPoints . at (name, stage)

_interfaces :: Lens' CGState (Map (Text, SPIRV.ExecutionModel) (Map Text ID))
_interfaces = lens interfaces ( \s v -> s { interfaces = v } )

_interface :: Text -> SPIRV.ExecutionModel -> Lens' CGState (Maybe (Map Text ID))
_interface stageName stage = _interfaces . at (stageName, stage)

_interfaceBinding :: Text -> SPIRV.ExecutionModel -> Text -> Lens' CGState (Maybe ID)
_interfaceBinding stageName stage varName
  = _interface stageName stage
  . affineTraverse
  . at varName

_builtin :: Text -> SPIRV.ExecutionInfo Word32 stage -> Text -> Lens' CGState (Maybe ID)
_builtin stageName executionInfo builtinName
  = lens
      ( view _interfaceBuiltin )
      ( \s mb_i -> case mb_i of
         Nothing -> s
         Just i  -> set _interfaceBuiltin           ( Just i              )
                  . set ( _usedGlobal builtinName ) ( Just (i, builtinTy) )
                  . set ( _decorate   i           ) ( Just decs           )
                  $ s
      )
  where executionModel :: SPIRV.ExecutionModel
        executionModel = SPIRV.modelOf executionInfo

        _interfaceBuiltin :: Lens' CGState (Maybe ID)
        _interfaceBuiltin = _interfaceBinding stageName executionModel builtinName

        decs :: SPIRV.Decorations
        decs = builtinDecorations builtinName

        builtinTy :: SPIRV.PointerTy
        builtinTy =
          fromMaybe
            ( error
              ( "_builtin: builtin with name " ++ Text.unpack builtinName ++ " cannot be found,\n\
                  \among builtins for " ++ show executionModel ++ " named " ++ Text.unpack stageName
              )
            )
            ( lookup builtinName $ modelBuiltins executionInfo )


_decorations :: Lens' CGState (Map ID SPIRV.Decorations)
_decorations = lens decorations ( \s v -> s { decorations = v } )

_decorate :: ID -> Lens' CGState (Maybe SPIRV.Decorations)
_decorate bindingID = _decorations . at bindingID

_memberDecorations :: Lens' CGState ( Map (TyID, Word32) SPIRV.Decorations )
_memberDecorations = lens memberDecorations ( \s v -> s { memberDecorations = v } )

_memberDecorate :: TyID -> Word32 -> Lens' CGState ( Maybe SPIRV.Decorations )
_memberDecorate bindingTyID index
  = _memberDecorations
  . at (bindingTyID, index)

_knownTypes :: Lens' CGState (Map SPIRV.PrimTy Instruction)
_knownTypes = lens knownTypes ( \s v -> s { knownTypes = v } )

_knownType :: SPIRV.PrimTy -> Lens' CGState (Maybe Instruction)
_knownType primTy = _knownTypes . at primTy

_knownConstants :: Lens' CGState (Map AConstant Instruction)
_knownConstants = lens knownConstants ( \s v -> s { knownConstants = v } )

_knownConstant :: AConstant -> Lens' CGState (Maybe Instruction)
_knownConstant constant = _knownConstants . at constant

_knownUndefineds :: Lens' CGState (Map SPIRV.PrimTy (ID, TyID))
_knownUndefineds = lens knownUndefineds ( \s v -> s { knownUndefineds = v } )

_knownUndefined :: SPIRV.PrimTy -> Lens' CGState (Maybe (ID,TyID))
_knownUndefined primTy = _knownUndefineds . at primTy

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
            , SPIRV.Decorations
            )
        )
_userGlobals = lens userGlobals ( \c v -> c { userGlobals = v } )

_userGlobal
  :: Text
  -> Lens' CGContext
        ( Maybe
            ( SPIRV.PointerTy
            , SPIRV.Decorations
            )
        )
_userGlobal global = _userGlobals . at global

_userFunctions :: Lens' CGContext ( Map Text SPIRV.FunctionControl )
_userFunctions = lens userFunctions ( \c v -> c { userFunctions = v } )

_userFunction :: Text -> Lens' CGContext ( Maybe SPIRV.FunctionControl )
_userFunction function = _userFunctions . at function

_userEntryPoints :: Lens' CGContext ( Map (Text, SPIRV.ExecutionModel) SPIRV.ExecutionModes )
_userEntryPoints = lens userEntryPoints ( \c v -> c { userEntryPoints = v } )

_userEntryPoint :: Text -> SPIRV.ExecutionModel -> Lens' CGContext ( Maybe SPIRV.ExecutionModes )
_userEntryPoint name stage = _userEntryPoints . at (name, stage)

_userImages :: Lens' CGContext ( Map Text SPIRV.Image )
_userImages = lens userImages ( \c v -> c { userImages = v } )

_userImage :: Text -> Lens' CGContext ( Maybe SPIRV.Image )
_userImage image = _userImages . at image

_debugMode :: Lens' CGContext Bool
_debugMode = lens debugMode ( \c v -> c { debugMode = v } )

-----------------------------------------------------------------------------
-- * Helper state update functions

addCapabilities :: forall t m.
                  ( Traversable t, MonadState CGState m )
                => t SPIRV.Capability -> m ()
addCapabilities
  = traverse_ ( \cap -> assign ( _neededCapability cap ) (Just ()) )

addName :: MonadState CGState m
        => ID -> Text -> m ()
addName bdID name
  = modifying _names
      ( Set.insert (bdID, Left name) )

addMemberName :: MonadState CGState m 
              => TyID -> Word32 -> Text -> m ()
addMemberName structTyID index name
  = modifying _names
      ( Set.insert (tyID structTyID, Right (index,name)) )

addDecoration :: MonadState CGState m
              => ID -> SPIRV.Decoration Word32 -> m ()
addDecoration bdID dec
  = modifying ( _decorate bdID )
      ( Just . maybe (Set.singleton dec) (Set.insert dec) )

addDecorations :: MonadState CGState m
               => ID -> SPIRV.Decorations -> m ()
addDecorations bdID decs
  = modifying ( _decorate bdID )
      ( Just . maybe decs (Set.union decs) )

addMemberDecoration :: MonadState CGState m
                    => TyID -> Word32 -> SPIRV.Decoration Word32 -> m ()
addMemberDecoration structTyID index dec
  = modifying ( _memberDecorate structTyID index )
      ( Just . maybe (Set.singleton dec) (Set.insert dec) )

addMemberDecorations :: MonadState CGState m
                     => TyID -> Word32 -> SPIRV.Decorations -> m ()
addMemberDecorations structTyID index decs
  = modifying ( _memberDecorate structTyID index )
      ( Just . maybe decs (Set.union decs) )
