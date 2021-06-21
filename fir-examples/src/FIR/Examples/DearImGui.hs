{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module FIR.Examples.DearImGui where

-- base
import Control.Monad
  ( void )
import Data.Int
  ( Int32 )
import Data.IORef
  ( IORef, readIORef, newIORef )
import Data.Kind
  ( Constraint, Type )
import GHC.TypeLits
  ( Symbol )

-- dear-imgui
import qualified DearImGui

-- transformers
import Control.Monad.IO.Class
  ( MonadIO(liftIO) )

-- fir
import FIR
  ( Struct(..), (:->)(..) )

--------------------------------------------------------------------------------

data Controller a where
  Slider :: Controller Float
  Toggle :: Controller Int32

createController :: MonadIO m => String -> Controller a -> IORef a -> m ()
createController controllerName controllerType ref =
  case controllerType of
    Slider ->
      void $ DearImGui.sliderFloat controllerName ref 0.0 1.0
    Toggle -> pure () -- TODO

--------------------------------------------------------------------------------

data ControllerRef = InitValue | Ref | Value

type ControllerData :: ControllerRef -> Type -> Type
type family ControllerData ref a where
  ControllerData 'InitValue a = ( String, Controller a, a )
  ControllerData 'Ref       a = ( String, Controller a, IORef a )
  ControllerData 'Value     a = a

type ControllerInitValues :: [ Symbol :-> Type ] -> [ Symbol :-> Type ] -> Constraint
class ControllerInitValues as bs | as -> bs, bs -> as where
  controllerInitValues :: Struct as -> Struct bs
instance ControllerInitValues '[] '[] where
  controllerInitValues _ = End
instance ( ControllerInitValues as bs
         , k1 ~ k2
         , v ~ ( String, Controller a, a )
         , r ~ a
         )
      => ControllerInitValues ( ( k1 ':-> v ) ': as ) ( ( k2 ':-> r ) ': bs )
      where
  controllerInitValues ( ( _, _, a ) :& as ) =
    a :& controllerInitValues as

type CreateControllerRefs :: [ Symbol :-> Type ] -> [ Symbol :-> Type ] -> Constraint
class CreateControllerRefs as bs | as -> bs, bs -> as where
  createControllerRefs :: MonadIO m => Struct as -> m ( Struct bs )
instance CreateControllerRefs '[] '[] where
  createControllerRefs _ = pure End
instance ( CreateControllerRefs as bs
         , k1 ~ k2
         , v ~ ( String, Controller a, a )
         , r ~ ( String, Controller a, IORef a )
         )
      => CreateControllerRefs ( ( k1 ':-> v ) ': as ) ( ( k2 ':-> r ) ': bs )
      where
  createControllerRefs ( ( nm, ct, a ) :& as ) = do
    ref  <- liftIO $ newIORef a
    refs <- createControllerRefs as
    pure ( ( nm, ct, ref ) :& refs )

type CreateControllers :: [ Symbol :-> Type ] -> Constraint
class CreateControllers as where
  createControllers :: MonadIO m => Struct as -> m ()
instance CreateControllers '[] where
  createControllers _ = pure ()
instance ( CreateControllers as
         , r ~ ( String, Controller a, IORef a )
         )
      => CreateControllers ( ( k ':-> r ) ': as )
      where
  createControllers ( ( nm, ct, r ) :& rs ) =
    createController nm ct r *> createControllers rs

type ReadControllers :: [ Symbol :-> Type ] -> [ Symbol :-> Type ] -> Constraint
class ReadControllers as bs | as -> bs, bs -> as where
  readControllers :: MonadIO m => Struct as -> m ( Struct bs )
instance ReadControllers '[] '[] where
  readControllers _ = pure End
instance ( ReadControllers as bs
         , k1 ~ k2
         , r ~ ( String, Controller a, IORef a )
         , b ~ a
         )
      => ReadControllers ( ( k1 ':-> r ) ': as ) ( ( k2 ':-> b ) ': bs )
      where
  readControllers ( ( _, _, r ) :& rs ) = do
    a  <- liftIO $ readIORef r
    as <- readControllers rs
    pure ( a :& as )

