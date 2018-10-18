{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module CodeGen.AST where

-- base
import Data.Maybe(fromJust)
import Data.Word(Word32)

-- binary
import qualified Data.Binary.Put as Binary

-- mtl
import Control.Monad.State.Lazy(StateT)
import Control.Monad.State.Class(MonadState)

-- transformers
import Control.Monad.Trans.Class(MonadTrans(lift))

-- containers
import Data.Map.Strict(Map)

-- lens
import Control.Lens( Lens', lens
                   , use, assign, (<<%=)
                   , at
                   )

-- fir
--import AST(AST(..))
import CodeGen.Instruction ( Args(..), prependArg, argsList
                           , ID(..), Instruction(..)
                           )

--import qualified SPIRV.PrimOps      as SPIRV
import qualified SPIRV.Types        as SPIRV
import qualified SPIRV.OpCodes      as SPIRV
import qualified SPIRV.Capabilities as SPIRV

----------------------------------------------------------------------------

data CGState
  = CGState
    { currentID          :: ID
    , knownTypes         :: Map SPIRV.PrimTy    Instruction
    , knownExts          :: Map SPIRV.Extension Instruction
    , neededCapabilities :: [ SPIRV.Capability ]
    }

-- bespoke monad for code generation
-- efficiently writes instructions using the Binary.PutM monad
-- keeps track of state (e.g. a map of which types have been defined) using StateT
type CGMonad a = FreshSuccT CGState (StateT CGState Binary.PutM) a

----------------------------------------------------------------------------
-- lenses

_currentID :: Lens' CGState ID
_currentID = lens currentID ( \s v -> s { currentID = v } )

_knownTypes :: Lens' CGState (Map SPIRV.PrimTy Instruction)
_knownTypes = lens knownTypes ( \s v -> s { knownTypes = v } )

_knownType :: SPIRV.PrimTy -> Lens' CGState (Maybe Instruction)
_knownType primTy = _knownTypes . at primTy

_knownExts :: Lens' CGState (Map SPIRV.Extension Instruction)
_knownExts = lens knownExts ( \s v -> s { knownExts = v } )

_knownExt :: SPIRV.Extension -> Lens' CGState (Maybe Instruction)
_knownExt ext = _knownExts . at ext

----------------------------------------------------------------------------
-- fresh name generation

class HasSupply v s where
  supply :: Lens' s v

instance HasSupply ID CGState where
  supply = _currentID

class Monad m => MonadFresh v m where
  fresh :: m v

newtype FreshSuccT s m a = FreshSuccT { freshSuccT :: m a }

deriving instance Functor      m => Functor      (FreshSuccT s m)
deriving instance Applicative  m => Applicative  (FreshSuccT s m)
deriving instance Monad        m => Monad        (FreshSuccT s m)
deriving instance MonadState s m => MonadState s (FreshSuccT s m)

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

-- TODO: get IDs for all names of functions defined, and for variables at the start
-- use this to turn [ EntryPoint String ] into [ EntryPoint ID ]



-- get an ID for a given type
-- ( if one is known use it, otherwise recursively get fresh IDs for necessary types )
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
           
  where _knownPrimTy :: Lens' CGState (Maybe Instruction) -- commenting this out gives a type error, weird...
        _knownPrimTy = _knownType primTy

        ty :: SPIRV.Ty
        someTyConArgs :: [Word32]
        (ty, someTyConArgs) = SPIRV.tyAndSomeTyConArgs primTy

        mkTyConInstruction :: ID -> Instruction
        mkTyConInstruction v = Instruction
           { name = "Type" ++ show ty
           , code = SPIRV.opTypeCode ty
           , resTy = Nothing
           , resID = Just v
           , args  = argsList someTyConArgs
           }


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