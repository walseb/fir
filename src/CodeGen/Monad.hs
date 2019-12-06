{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-|
Module: CodeGen.Monad

Defines the monad used by the code generator, including the monadic state required for code generation.
-}

module CodeGen.Monad
  ( MonadFresh(fresh), runFreshSuccT
  , CGMonad, putCG
  , liftPut
  , create, createID
  , createRec, createIDRec
  , tryToUse, tryToUseWith
  , runExceptTPutM, runCGMonad
  , note
  )
  where

-- base
import Control.Arrow
  ( right )
import Control.Category
  ( (>>>) )

-- binary
import Data.Binary
  ( Binary )
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- lens
import Control.Lens
  ( Lens'
  , use, assign, (<<%=)
  )

-- mtl
import Control.Monad.Except
  ( MonadError , ExceptT, runExceptT, throwError )
import Control.Monad.Reader
  ( MonadReader, ReaderT, runReaderT )
import Control.Monad.State
  ( MonadState , StateT , runStateT )

-- text-short
import Data.Text.Short
  ( ShortText )

-- transformers
import Control.Monad.Trans.Class
  ( MonadTrans, lift )

-- fir
import Control.Arrow.Strength
  ( leftStrength )
import CodeGen.Instruction
  ( ID )
import CodeGen.State
  ( CGState, CGContext
  , _currentID
  )

----------------------------------------------------------------------------
-- monad for code generation

type CGMonad 
  = FreshSuccT CGState        -- supply of fresh variable IDs using CGState (see below)
      ( ReaderT CGContext     -- context for code generation
        ( StateT CGState      -- state (including for instance a map of which types have been defined)
          ( ExceptT ShortText -- for errors during code generation
              Binary.PutM     -- to write instructions in binary form
          )
        )
      )
deriving newtype instance MonadReader CGContext CGMonad
deriving newtype instance MonadError  ShortText CGMonad
-- other instances automatically derived from the definition of FreshSuccT:
-- Functor, Applicative, Monad, MonadState CGState, MonadFresh ID

liftPut :: Binary.PutM a -> CGMonad a
liftPut = lift . lift . lift . lift  -- apologies

putCG :: Binary a => a -> CGMonad ()
putCG = liftPut . Binary.put

----------------------------------------------------------------------------
-- fresh name generation

class HasSupply v s where
  supply :: Lens' s v

instance HasSupply ID CGState where
  supply = _currentID

instance HasSupply ID ID where
  supply = id

class Monad m => MonadFresh v m where
  fresh :: m v

newtype FreshSuccT s m a = FreshSuccT { runFreshSuccT :: m a }

deriving newtype instance Functor       m => Functor       (FreshSuccT s m)
deriving newtype instance Applicative   m => Applicative   (FreshSuccT s m)
deriving newtype instance Monad         m => Monad         (FreshSuccT s m)
deriving newtype instance MonadState  s m => MonadState  s (FreshSuccT s m)

instance MonadTrans (FreshSuccT s) where
  lift = FreshSuccT

instance (MonadState s m, HasSupply v s, Enum v) => MonadFresh v (FreshSuccT s m) where
  fresh = supply <<%= succ -- <<%= means: "modify, and return the _old_ value"

create :: ( MonadFresh v m, MonadState s m )
       => Lens' s (Maybe a) -> (v -> m a) -> m (v, a)
create _key mk
  = do v <- fresh
       a <- mk v
       assign _key ( Just a )
       pure (v, a)

createID :: ( MonadFresh v m, MonadState s m )
         => Lens' s (Maybe a) -> (v -> m a) -> m v
createID _key mk = fst <$> create _key mk

createRec :: ( MonadFresh v m, MonadState s m )
          => Lens' s (Maybe a) -> m b -> (b -> v -> m a) -> m (v, a)
createRec _key before mk
  = do b <- before
       v <- fresh
       a <- mk b v
       assign _key ( Just a )
       pure (v, a)

createIDRec :: ( MonadFresh v m, MonadState s m )
          => Lens' s (Maybe a) -> m b -> (b -> v -> m a) -> m v
createIDRec _key before mk = fst <$> createRec _key before mk

-- | Try to use a given lens to obtain a value.
--
-- If using the lens returns Nothing, create the value instead, using 'create'.
tryToUse :: ( MonadFresh v m, MonadState s m )
         => Lens' s (Maybe a) -> (a -> b) -> (v -> m a) -> m b
tryToUse _key f mk = tryToUseWith _key f ( f . snd <$> create _key mk )

-- | Like 'tryToUse', but allowing for a custom creation procedure.
tryToUseWith :: ( MonadState s m )
             => Lens' s (Maybe a) -> (a -> b) -> m b -> m b
tryToUseWith _key f creation =
  use _key >>= maybe creation (pure . f)

----------------------------------------------------------------------------
-- running the code generation monad

runExceptTPutM :: ExceptT e Binary.PutM a -> Either e (a, ByteString)
runExceptTPutM = runExceptT >>> uncurry leftStrength . Binary.runPutM

runCGMonad :: CGContext -> CGState -> CGMonad r -> Either ShortText (r, CGState, ByteString)
runCGMonad context state
  =   runFreshSuccT  
  >>> ( `runReaderT` context )
  >>> ( `runStateT`  state   )
  >>> runExceptTPutM
  >>> right ( \((r,s),b) -> (r,s,b) )

----------------------------------------------------------------------------
-- utility for exceptions

note :: forall m a e. MonadError e m => e -> Maybe a -> m a
note _ (Just a) = pure a
note e Nothing  = throwError e
