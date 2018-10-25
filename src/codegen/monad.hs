{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module CodeGen.Monad where

-- base
import Control.Arrow(right)
import Control.Category((>>>))

-- binary
import Data.Binary(Binary)
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy(ByteString)

-- containers
import Data.Map(Map)
import qualified Data.Map as Map

-- lens
import Control.Lens( Lens', lens
                   , use, assign, (<<%=)
                   , at
                   )

-- text-utf8
import Data.Text(Text)

-- transformers
import Control.Monad.Except(MonadError , ExceptT, runExceptT)
import Control.Monad.Reader(MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState , StateT , runStateT )
import Control.Monad.Trans.Class(MonadTrans, lift)

-- fir
import Control.Arrow.Strength(leftStrength)
import CodeGen.Instruction( ID(ID)
                          , Instruction
                          , EntryPoint
                          )
import FIR.Builtin(Stage)
import FIR.PrimTy(AConstant)
import qualified SPIRV.Capability as SPIRV
import qualified SPIRV.Extension  as SPIRV
import qualified SPIRV.PrimTy     as SPIRV

----------------------------------------------------------------------------
-- code generator monad

-- code generator state
-- this consists of information we need to keep track of along the way,
-- for instance which types have been declared
data CGState
  = CGState
    { currentID          :: ID
    , functionContext    :: FunctionContext
    , neededCapabilities :: [ SPIRV.Capability ]
    , knownExtInsts      :: Map SPIRV.ExtInst Instruction
    , annotations        :: [Instruction]
    , knownBindings      :: Map Text          ID
    , knownTypes         :: Map SPIRV.PrimTy  Instruction
    , knownConstants     :: Map AConstant     Instruction    
    }

data FunctionContext
  = TopLevel
  | Function [Text] -- argument names
  | EntryPoint Stage
  deriving ( Eq, Show )

initialState :: CGState
initialState = CGState
  { currentID          = ID 1
  , functionContext    = TopLevel
  , neededCapabilities = []
  , knownExtInsts      = Map.empty
  , annotations        = []
  , knownBindings      = Map.empty
  , knownTypes         = Map.empty
  , knownConstants     = Map.empty  
  }

data CGContext
  = CGContext
    { entryPoints :: Map Text (EntryPoint Text) }

-- bespoke monad for code generation
type CGMonad 
  = FreshSuccT CGState    -- supply of fresh variable IDs using CGState (see below)
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

_functionContext :: Lens' CGState FunctionContext
_functionContext = lens functionContext ( \s v -> s { functionContext = v } )

_knownExtInsts :: Lens' CGState (Map SPIRV.ExtInst Instruction)
_knownExtInsts = lens knownExtInsts ( \s v -> s { knownExtInsts = v } )

_knownExtInst :: SPIRV.ExtInst -> Lens' CGState (Maybe Instruction)
_knownExtInst ext = _knownExtInsts . at ext

_knownBindings :: Lens' CGState (Map Text ID)
_knownBindings = lens knownBindings ( \s v -> s { knownBindings = v } )

_knownBinding :: Text -> Lens' CGState (Maybe ID)
_knownBinding binding = _knownBindings . at binding

_knownTypes :: Lens' CGState (Map SPIRV.PrimTy Instruction)
_knownTypes = lens knownTypes ( \s v -> s { knownTypes = v } )

_knownType :: SPIRV.PrimTy -> Lens' CGState (Maybe Instruction)
_knownType primTy = _knownTypes . at primTy

_knownConstants :: Lens' CGState (Map AConstant Instruction)
_knownConstants = lens knownConstants ( \s v -> s { knownConstants = v } )

_knownConstant :: AConstant -> Lens' CGState (Maybe Instruction)
_knownConstant constant = _knownConstants . at constant

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
       => Lens' s (Maybe a) -> (v -> m a) -> m v
create _key mk
  = do v <- fresh
       a <- mk v
       assign _key ( Just a )
       pure v

createRec :: ( MonadFresh v m, MonadState s m )
          => Lens' s (Maybe a) -> m b -> (b -> v -> m a) -> m v
createRec _key before mk
  = do b <- before
       v <- fresh
       a <- mk b v
       assign _key ( Just a )
       pure v

tryToUseWith :: ( MonadFresh v m, MonadState s m )
             => Lens' s (Maybe a) -> (a -> v) -> m v -> m v
tryToUseWith _key f creation =
  use _key >>= maybe creation (pure . f)

tryToUse :: ( MonadFresh v m, MonadState s m )
         => Lens' s (Maybe a) -> (a -> v) -> (v -> m a) -> m v
tryToUse _key f mk = tryToUseWith _key f (create _key mk)

----------------------------------------------------------------------------
-- running the code generation monad

runExceptTPutM :: ExceptT e Binary.PutM a -> Either e (a, ByteString)
runExceptTPutM = runExceptT >>> uncurry leftStrength . Binary.runPutM

runCGMonad :: CGContext -> CGMonad r -> Either Text (r, CGState, ByteString)
runCGMonad context
  =   runFreshSuccT  
  >>> ( `runReaderT` context      )
  >>> ( `runStateT`  initialState )
  >>> runExceptTPutM
  >>> right ( \((r,s),b) -> (r,s,b) )

